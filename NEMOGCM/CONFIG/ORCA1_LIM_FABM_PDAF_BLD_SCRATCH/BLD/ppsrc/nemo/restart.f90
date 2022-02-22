












MODULE restart
   !!======================================================================
   !!                     ***  MODULE  restart  ***
   !! Ocean restart :  write the ocean restart file
   !!======================================================================
   !! History :  OPA  !  1999-11  (M. Imbard)  Original code
   !!   NEMO     1.0  !  2002-08  (G. Madec)  F90: Free form
   !!            2.0  !  2006-07  (S. Masson)  use IOM for restart
   !!            3.3  !  2010-04  (M. Leclair, G. Madec)  modified LF-RA
   !!            - -  !  2010-10  (C. Ethe, G. Madec) TRC-TRA merge (T-S in 4D)
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   rst_opn    : open the ocean restart file
   !!   rst_write  : write the ocean restart file
   !!   rst_read   : read the ocean restart file
   !!----------------------------------------------------------------------
   USE oce             ! ocean dynamics and tracers 
   USE dom_oce         ! ocean space and time domain
   USE phycst          ! physical constants
   USE in_out_manager  ! I/O manager
   USE iom             ! I/O module
   USE ioipsl, ONLY : ju2ymds    ! for calendar
   USE eosbn2          ! equation of state            (eos bn2 routine)
   USE trdmxl_oce      ! ocean active mixed layer tracers trends variables
   USE divcur          ! hor. divergence and curl      (div & cur routines)
   USE sbc_oce         ! for icesheet freshwater input variables
   USE timing

   IMPLICIT NONE
   PRIVATE

   PUBLIC   rst_opn         ! routine called by step module
   PUBLIC   rst_write       ! routine called by step module
   PUBLIC   rst_read        ! routine called by istate module
   PUBLIC   rst_read_open   ! routine called in rst_read and (possibly) in dom_vvl_init

   !! * Substitutions
   !!----------------------------------------------------------------------
   !!                    ***  domzgr_substitute.h90   ***
   !!----------------------------------------------------------------------
   !! ** purpose :   substitute fsdep. and fse.., the vert. depth and scale
   !!      factors depending on the vertical coord. used, using CPP macro.
   !!----------------------------------------------------------------------
   !! History :  1.0  !  2005-10  (A. Beckmann, G. Madec) generalisation to all coord.
   !!            3.1  !  2009-02  (G. Madec, M. Leclair)  pure z* coordinate
   !!----------------------------------------------------------------------

! s* or z*-coordinate (3D + time dependency) + use of additional now arrays (..._n)







! This part should be removed one day ...
! ... In that case all occurence of the above statement functions
!     have to be replaced in the code by xxx_n

   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.3 , NEMO Consortium (2010)
   !! $Id$
   !! Software governed by the CeCILL licence (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
   !!----------------------------------------------------------------------
   !!                   ***  vectopt_loop_substitute  ***
   !!----------------------------------------------------------------------
   !! ** purpose :   substitute the inner loop start/end indices with CPP macro
   !!                allow unrolling of do-loop (useful with vector processors)
   !!----------------------------------------------------------------------
   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.7 , NEMO Consortium (2014)
   !! $Id$
   !! Software governed by the CeCILL licence (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.3 , NEMO Consortium (2010)
   !! $Id$
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE rst_opn( kt )
      !!---------------------------------------------------------------------
      !!                   ***  ROUTINE rst_opn  ***
      !!                     
      !! ** Purpose : + initialization (should be read in the namelist) of nitrst 
      !!              + open the restart when we are one time step before nitrst
      !!                   - restart header is defined when kt = nitrst-1
      !!                   - restart data  are written when kt = nitrst
      !!              + define lrst_oce to .TRUE. when we need to define or write the restart
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt     ! ocean time-step
      INTEGER             ::   iyear, imonth, iday
      REAL (wp)           ::   zsec
      REAL (wp)           ::   zfjulday
      !!
      CHARACTER(LEN=20)   ::   clkt     ! ocean time-step deine as a character
      CHARACTER(LEN=50)   ::   clname   ! ocean output restart file name
      CHARACTER(LEN=150)  ::   clpath   ! full path to ocean output restart file
      !!----------------------------------------------------------------------
      !
      IF( kt == nit000 ) THEN   ! default definitions
         lrst_oce = .FALSE.   
         IF( ln_rst_list ) THEN
            nrst_lst = 1
            nitrst = nstocklist( nrst_lst )
         ELSE
            nitrst = nitend
         ENDIF
      ENDIF

      ! frequency-based restart dumping (nn_stock)
      IF( .NOT. ln_rst_list .AND. MOD( kt - 1, nstock ) == 0 ) THEN   
         ! we use kt - 1 and not kt - nit000 to keep the same periodicity from the beginning of the experiment
         nitrst = kt + nstock - 1                  ! define the next value of nitrst for restart writing
         IF( nitrst > nitend )   nitrst = nitend   ! make sure we write a restart at the end of the run
      ENDIF
      ! to get better performances with NetCDF format:
      ! we open and define the ocean restart file one time step before writing the data (-> at nitrst - 1)
      ! except if we write ocean restart files every time step or if an ocean restart file was writen at nitend - 1
      IF( kt == nitrst - 1 .OR. nstock == 1 .OR. ( kt == nitend .AND. .NOT. lrst_oce ) ) THEN
         IF( nitrst <= nitend .AND. nitrst > 0 ) THEN 
            IF ( ln_rstdate ) THEN
               zfjulday = fjulday + rdttra(1) / rday
               IF( ABS(zfjulday - REAL(NINT(zfjulday),wp)) < 0.1 / rday )   zfjulday = REAL(NINT(zfjulday),wp)   ! avoid truncation error
               CALL ju2ymds( zfjulday, iyear, imonth, iday, zsec )           
               WRITE(clkt, '(i4.4,2i2.2)') iyear, imonth, iday
            ELSE
               ! beware of the format used to write kt (default is i8.8, that should be large enough...)
               IF( nitrst > 999999999 ) THEN   ;   WRITE(clkt, *       ) nitrst
               ELSE                            ;   WRITE(clkt, '(i8.8)') nitrst
               ENDIF
            ENDIF
            ! create the file
            clname = TRIM(cexper)//"_"//TRIM(ADJUSTL(clkt))//"_"//TRIM(cn_ocerst_out)
            clpath = TRIM(cn_ocerst_outdir)
            IF( clpath(LEN_TRIM(clpath):) /= '/' ) clpath = TRIM(clpath) // '/'
            IF(lwp) THEN
               WRITE(numout,*)
               SELECT CASE ( jprstlib )
               CASE ( jprstdimg )   ;   WRITE(numout,*)                            &
                   '             open ocean restart binary file: ',TRIM(clpath)//clname
               CASE DEFAULT         ;   WRITE(numout,*)                            &
                   '             open ocean restart NetCDF file: ',TRIM(clpath)//clname
               END SELECT
               IF ( snc4set%luse )      WRITE(numout,*) '             opened for NetCDF4 chunking and compression'
               IF( kt == nitrst - 1 ) THEN   ;   WRITE(numout,*) '             kt = nitrst - 1 = ', kt
               ELSE                          ;   WRITE(numout,*) '             kt = '             , kt
               ENDIF
            ENDIF
            !
            CALL iom_open( TRIM(clpath)//TRIM(clname), numrow, ldwrt = .TRUE., kiolib = jprstlib )
            lrst_oce = .TRUE.
         ENDIF
      ENDIF
      !
   END SUBROUTINE rst_opn


   SUBROUTINE rst_write( kt )
      !!---------------------------------------------------------------------
      !!                   ***  ROUTINE rstwrite  ***
      !!                     
      !! ** Purpose :   Write restart fields in the format corresponding to jprstlib
      !!
      !! ** Method  :   Write in numrow when kt == nitrst in NetCDF
      !!              file, save fields which are necessary for restart
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt   ! ocean time-step
      !!----------------------------------------------------------------------
                     IF(nn_timing == 2)  CALL timing_start('iom_rstput')
                     CALL iom_rstput( kt, nitrst, numrow, 'rdt'    , rdt       )   ! dynamics time step
                     CALL iom_rstput( kt, nitrst, numrow, 'rdttra1', rdttra(1) )   ! surface tracer time step

                     CALL iom_rstput( kt, nitrst, numrow, 'ub'     , ub        )     ! before fields
                     CALL iom_rstput( kt, nitrst, numrow, 'vb'     , vb        )
                     CALL iom_rstput( kt, nitrst, numrow, 'tb'     , tsb(:,:,:,jp_tem) )
                     CALL iom_rstput( kt, nitrst, numrow, 'sb'     , tsb(:,:,:,jp_sal) )
                     CALL iom_rstput( kt, nitrst, numrow, 'rotb'   , rotb      )
                     CALL iom_rstput( kt, nitrst, numrow, 'hdivb'  , hdivb     )
                     CALL iom_rstput( kt, nitrst, numrow, 'sshb'   , sshb      )
                     !
                     CALL iom_rstput( kt, nitrst, numrow, 'un'     , un        )     ! now fields
                     CALL iom_rstput( kt, nitrst, numrow, 'vn'     , vn        )
                     CALL iom_rstput( kt, nitrst, numrow, 'tn'     , tsn(:,:,:,jp_tem) )
                     CALL iom_rstput( kt, nitrst, numrow, 'sn'     , tsn(:,:,:,jp_sal) )
                     CALL iom_rstput( kt, nitrst, numrow, 'rotn'   , rotn      )
                     CALL iom_rstput( kt, nitrst, numrow, 'hdivn'  , hdivn     )
                     CALL iom_rstput( kt, nitrst, numrow, 'sshn'   , sshn      )
                     CALL iom_rstput( kt, nitrst, numrow, 'rhop'   , rhop      )
                     IF( lk_oasis) THEN
                     ! nn_coupled_iceshelf_fluxes uninitialised unless lk_oasis=true
                     IF( nn_coupled_iceshelf_fluxes .eq. 1 ) THEN
                        CALL iom_rstput( kt, nitrst, numrow, 'greenland_icesheet_mass', greenland_icesheet_mass )
                        CALL iom_rstput( kt, nitrst, numrow, 'greenland_icesheet_timelapsed', greenland_icesheet_timelapsed )
                        CALL iom_rstput( kt, nitrst, numrow, 'greenland_icesheet_mass_roc', greenland_icesheet_mass_rate_of_change )
                        CALL iom_rstput( kt, nitrst, numrow, 'antarctica_icesheet_mass', antarctica_icesheet_mass )
                        CALL iom_rstput( kt, nitrst, numrow, 'antarctica_icesheet_timelapsed', antarctica_icesheet_timelapsed )
                        CALL iom_rstput( kt, nitrst, numrow, 'antarctica_icesheet_mass_roc', antarctica_icesheet_mass_rate_of_change )
                     ENDIF
                     ENDIF
                     IF(nn_timing == 2)  CALL timing_stop('iom_rstput')

      IF( kt == nitrst ) THEN
         CALL iom_close( numrow )     ! close the restart file (only at last time step)
!!gm         IF( .NOT. lk_trdmld )   lrst_oce = .FALSE.
!!gm  not sure what to do here   ===>>>  ask to Sebastian
         lrst_oce = .FALSE.
            IF( ln_rst_list ) THEN
               nrst_lst = MIN(nrst_lst + 1, SIZE(nstocklist,1))
               nitrst = nstocklist( nrst_lst )
            ENDIF
            lrst_oce = .FALSE.
      ENDIF
      !
   END SUBROUTINE rst_write


   SUBROUTINE rst_read_open
      !!---------------------------------------------------------------------- 
      !!                   ***  ROUTINE rst_read_open  ***
      !! 
      !! ** Purpose :   Open read files for restart (format fixed by jprstlib )
      !! 
      !! ** Method  :   Use a non-zero, positive value of numror to assess whether or not
      !!                the file has already been opened
      !!----------------------------------------------------------------------
      INTEGER        ::   jlibalt = jprstlib
      LOGICAL        ::   llok
      CHARACTER(lc)  ::   clpath   ! full path to ocean output restart file
      !!----------------------------------------------------------------------
      !
      IF( numror <= 0 ) THEN
         IF(lwp) THEN                                             ! Contol prints
            WRITE(numout,*)
            SELECT CASE ( jprstlib )
            CASE ( jpnf90    )   ;   WRITE(numout,*) 'rst_read : read oce NetCDF restart file'
            CASE ( jprstdimg )   ;   WRITE(numout,*) 'rst_read : read oce binary restart file'
            END SELECT
            IF ( snc4set%luse )      WRITE(numout,*) 'rst_read : configured with NetCDF4 support'
            WRITE(numout,*) '~~~~~~~~'
         ENDIF

         clpath = TRIM(cn_ocerst_indir)
         IF( clpath(LEN_TRIM(clpath):) /= '/' ) clpath = TRIM(clpath) // '/'
         IF ( jprstlib == jprstdimg ) THEN
           ! eventually read netcdf file (monobloc)  for restarting on different number of processors
           ! if {cn_ocerst_in}.nc exists, then set jlibalt to jpnf90
           INQUIRE( FILE = TRIM(cn_ocerst_indir)//'/'//TRIM(cn_ocerst_in)//'.nc', EXIST = llok )
           IF ( llok ) THEN ; jlibalt = jpnf90  ; ELSE ; jlibalt = jprstlib ; ENDIF
         ENDIF
         CALL iom_open( TRIM(clpath)//cn_ocerst_in, numror, kiolib = jlibalt )
      ENDIF
   END SUBROUTINE rst_read_open

   SUBROUTINE rst_read
      !!---------------------------------------------------------------------- 
      !!                   ***  ROUTINE rst_read  ***
      !! 
      !! ** Purpose :   Read files for restart (format fixed by jprstlib )
      !! 
      !! ** Method  :   Read in restart.nc file fields which are necessary for restart
      !!----------------------------------------------------------------------
      REAL(wp) ::   zrdt, zrdttra1
      INTEGER  ::   jk
      LOGICAL  ::   llok
      !!----------------------------------------------------------------------

      CALL rst_read_open           ! open restart for reading (if not already opened)

      ! Check dynamics and tracer time-step consistency and force Euler restart if changed
      IF(nn_timing == 2)  CALL timing_start('iom_rstget')
      IF( iom_varid( numror, 'rdt', ldstop = .FALSE. ) > 0 )   THEN
         CALL iom_get( numror, 'rdt', zrdt )
         IF( zrdt /= rdt )   neuler = 0
      ENDIF
      IF( iom_varid( numror, 'rdttra1', ldstop = .FALSE. ) > 0 )   THEN
         CALL iom_get( numror, 'rdttra1', zrdttra1 )
         IF( zrdttra1 /= rdttra(1) )   neuler = 0
      ENDIF
      ! 
      IF( iom_varid( numror, 'ub', ldstop = .FALSE. ) > 0 ) THEN
         CALL iom_get( numror, jpdom_autoglo, 'ub'     , ub      )   ! before fields
         CALL iom_get( numror, jpdom_autoglo, 'vb'     , vb      )
         CALL iom_get( numror, jpdom_autoglo, 'tb'     , tsb(:,:,:,jp_tem) )
         CALL iom_get( numror, jpdom_autoglo, 'sb'     , tsb(:,:,:,jp_sal) )
         CALL iom_get( numror, jpdom_autoglo, 'rotb'   , rotb    )
         CALL iom_get( numror, jpdom_autoglo, 'hdivb'  , hdivb   )
         CALL iom_get( numror, jpdom_autoglo, 'sshb'   , sshb    )
      ELSE
         neuler = 0
      ENDIF
      !
      CALL iom_get( numror, jpdom_autoglo, 'un'     , un      )   ! now    fields
      CALL iom_get( numror, jpdom_autoglo, 'vn'     , vn      )
      CALL iom_get( numror, jpdom_autoglo, 'tn'     , tsn(:,:,:,jp_tem) )
      CALL iom_get( numror, jpdom_autoglo, 'sn'     , tsn(:,:,:,jp_sal) )
      CALL iom_get( numror, jpdom_autoglo, 'sshn'   , sshn    )
      IF( iom_varid( numror, 'rotn', ldstop = .FALSE. ) > 0 ) THEN
         CALL iom_get( numror, jpdom_autoglo, 'rotn'   , rotn    )
         CALL iom_get( numror, jpdom_autoglo, 'hdivn'  , hdivn   )
      ELSE
         CALL div_cur( 0 )                              ! Horizontal divergence & Relative vorticity
      ENDIF
      IF( iom_varid( numror, 'rhop', ldstop = .FALSE. ) > 0 ) THEN
         CALL iom_get( numror, jpdom_autoglo, 'rhop'   , rhop    )   ! now    potential density
      ELSE
         CALL eos    ( tsn, rhd, rhop, gdept_n(:,:,:) )   
      ENDIF
      !
      IF( iom_varid( numror, 'greenland_icesheet_mass', ldstop = .FALSE. ) > 0 )   THEN
         CALL iom_get( numror, 'greenland_icesheet_mass', greenland_icesheet_mass )
         CALL iom_get( numror, 'greenland_icesheet_timelapsed', greenland_icesheet_timelapsed )
         CALL iom_get( numror, 'greenland_icesheet_mass_roc', greenland_icesheet_mass_rate_of_change )
      ELSE
         greenland_icesheet_mass = 0.0 
         greenland_icesheet_mass_rate_of_change = 0.0 
         greenland_icesheet_timelapsed = 0.0
      ENDIF
      IF( iom_varid( numror, 'antarctica_icesheet_mass', ldstop = .FALSE. ) > 0 )   THEN
         CALL iom_get( numror, 'antarctica_icesheet_mass', antarctica_icesheet_mass )
         CALL iom_get( numror, 'antarctica_icesheet_timelapsed', antarctica_icesheet_timelapsed )
         CALL iom_get( numror, 'antarctica_icesheet_mass_roc', antarctica_icesheet_mass_rate_of_change )
      ELSE
         antarctica_icesheet_mass = 0.0 
         antarctica_icesheet_mass_rate_of_change = 0.0 
         antarctica_icesheet_timelapsed = 0.0
      ENDIF
      IF(nn_timing == 2)  CALL timing_stop('iom_rstget')
      IF( neuler == 0 ) THEN                                  ! Euler restart (neuler=0)
         tsb  (:,:,:,:) = tsn  (:,:,:,:)                             ! all before fields set to now values
         ub   (:,:,:)   = un   (:,:,:)
         vb   (:,:,:)   = vn   (:,:,:)
         rotb (:,:,:)   = rotn (:,:,:)
         hdivb(:,:,:)   = hdivn(:,:,:)
         sshb (:,:)     = sshn (:,:)

         IF( lk_vvl ) THEN
            DO jk = 1, jpk
               e3t_b(:,:,jk) = e3t_n(:,:,jk)
            END DO
         ENDIF

      ENDIF
      !
   END SUBROUTINE rst_read

   !!=====================================================================
END MODULE restart
