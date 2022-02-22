












MODULE trcrst
   !!======================================================================
   !!                         ***  MODULE trcrst  ***
   !! TOP :   Manage the passive tracer restart
   !!======================================================================
   !! History :    -   !  1991-03  ()  original code
   !!             1.0  !  2005-03 (O. Aumont, A. El Moussaoui) F90
   !!              -   !  2005-10 (C. Ethe) print control
   !!             2.0  !  2005-10 (C. Ethe, G. Madec) revised architecture
   !!----------------------------------------------------------------------
   !!----------------------------------------------------------------------
   !!   'key_top'                                                TOP models
   !!----------------------------------------------------------------------
   !!----------------------------------------------------------------------
   !!   trc_rst :   Restart for passive tracer
   !!----------------------------------------------------------------------
   !!----------------------------------------------------------------------
   !!   'key_top'                                                TOP models
   !!----------------------------------------------------------------------
   !!   trc_rst_opn    : open  restart file
   !!   trc_rst_read   : read  restart file
   !!   trc_rst_wri    : write restart file
   !!----------------------------------------------------------------------
   USE oce_trc
   USE trc
   USE trcnam_trp
   USE iom
   USE ioipsl, ONLY : ju2ymds    ! for calendar
   USE daymod
   ! +++>>> FABM
   USE trcrst_fabm
   ! FABM <<<+++
   !! AXY (05/11/13): need these for MEDUSA to input/output benthic reservoirs
   USE par_medusa
   USE sms_medusa
   USE trcsms_medusa
   !!
   !!
   USE lbclnk          ! ocean lateral boundary conditions (or mpp link)
   USE sbc_oce, ONLY: lk_oasis 
   USE oce,     ONLY: CO2Flux_out_cpl, DMS_out_cpl, chloro_out_cpl  !! Coupling variable
   USE trcstat

   IMPLICIT NONE
   PRIVATE

   PUBLIC   trc_rst_opn       ! called by ???
   PUBLIC   trc_rst_read      ! called by ???
   PUBLIC   trc_rst_wri       ! called by ???
   PUBLIC   trc_rst_cal
   PUBLIC   trc_rst_stat

   !! * Substitutions
   !!----------------------------------------------------------------------
   !!                    ***  top_substitute.h90   ***
   !!----------------------------------------------------------------------
   !! ** purpose : Statement function file: to be include in all passive tracer modules
   !!----------------------------------------------------------------------
   !! History :   1.0  !  2004-03 (C. Ethe) Original code
   !!             2.0  !  2007-12 (C. Ethe, G. Madec) new architecture
   !!----------------------------------------------------------------------
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
   !!                   ***  ldfeiv_substitute.h90  ***
   !!----------------------------------------------------------------------
   !! ** purpose :   substitute fsaei. the eddy induced velocity coeff.
   !!      with a constant or 1D or 2D or 3D array, using CPP macro.
   !!----------------------------------------------------------------------
   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.3 , NEMO Consortium (2010)
   !! $Id$
   !! Software governed by the CeCILL licence (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
!   'traldf_c2d' :                           eiv: 2D coefficient
   !!----------------------------------------------------------------------
   !!                    *** ldftra_substitute.h90  ***
   !!----------------------------------------------------------------------
   !! ** purpose :   substitute fsaht. the eddy diffusivity coeff.
   !!      with a constant or 1D or 2D or 3D array, using CPP macro.
   !!----------------------------------------------------------------------
   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.3 , NEMO Consortium (2010)
   !! $Id$
   !! Software governed by the CeCILL licence (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
!   'key_traldf_c2d' :                 aht: 2D coefficient
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
   !! NEMO/TOP 3.3 , NEMO Consortium (2010)
   !! $Id$
   !! Software governed by the CeCILL licence (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
   
CONTAINS
   
   SUBROUTINE trc_rst_opn( kt )
      !!----------------------------------------------------------------------
      !!                    ***  trc_rst_opn  ***
      !!
      !! ** purpose  :   output of sea-trc variable in a netcdf file
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt       ! number of iteration
      INTEGER             ::   iyear, imonth, iday
      REAL (wp)           ::   zsec
      REAL (wp)           ::   zfjulday
      !
      CHARACTER(LEN=20)   ::   clkt     ! ocean time-step define as a character
      CHARACTER(LEN=50)   ::   clname   ! trc output restart file name
      CHARACTER(LEN=256)  ::   clpath   ! full path to ocean output restart file
      !!----------------------------------------------------------------------
      !
      IF( lk_offline ) THEN
         IF( kt == nittrc000 ) THEN
            lrst_trc = .FALSE.
            IF( ln_rst_list ) THEN
               nrst_lst = 1
               nitrst = nstocklist( nrst_lst )
            ELSE
               nitrst = nitend
            ENDIF
         ENDIF

         IF( .NOT. ln_rst_list .AND. MOD( kt - 1, nstock ) == 0 ) THEN
            ! we use kt - 1 and not kt - nittrc000 to keep the same periodicity from the beginning of the experiment
            nitrst = kt + nstock - 1                  ! define the next value of nitrst for restart writing
            IF( nitrst > nitend )   nitrst = nitend   ! make sure we write a restart at the end of the run
         ENDIF
      ELSE
         IF( kt == nittrc000 ) lrst_trc = .FALSE.
      ENDIF

      ! to get better performances with NetCDF format:
      ! we open and define the tracer restart file one tracer time step before writing the data (-> at nitrst - 2*nn_dttrc + 1)
      ! except if we write tracer restart files every tracer time step or if a tracer restart file was writen at nitend - 2*nn_dttrc + 1
      IF( kt == nitrst - 2*nn_dttrc .OR. nstock == nn_dttrc .OR. ( kt == nitend - nn_dttrc .AND. .NOT. lrst_trc ) ) THEN
         IF ( ln_rstdate ) THEN
            !! JPALM -- 22-12-2015 -- modif to get the good date on restart trc file name
            !!                     -- the condition to open the rst file is not the same than for the dynamic rst.
            !!                     -- here it - for an obscure reason - is open 2 time-step before the restart writing process
            !!                     instead of 1.
            !!                     -- i am not sure if someone forgot +1 in the if loop condition as
            !!                     it is writen in all comments nitrst -2*nn_dttrc + 1 and the condition is 
            !!                     nitrst - 2*nn_dttrc
            !!                     -- nevertheless we didn't wanted to broke something already working 
            !!                     and just adapted the part we added.
            !!                     -- So instead of calling ju2ymds( fjulday + (rdttra(1)) 
            !!                     we call ju2ymds( fjulday + (2*rdttra(1)) 
            !!--------------------------------------------------------------------      
            zfjulday = fjulday + (2*rdttra(1)) / rday
            IF( ABS(zfjulday - REAL(NINT(zfjulday),wp)) < 0.1 / rday )   zfjulday = REAL(NINT(zfjulday),wp)   ! avoid truncation error
            CALL ju2ymds( zfjulday + (2*rdttra(1)) / rday, iyear, imonth, iday, zsec )
            WRITE(clkt, '(i4.4,2i2.2)') iyear, imonth, iday
         ELSE
            ! beware of the format used to write kt (default is i8.8, that should be large enough)
            IF( nitrst > 1.0e9 ) THEN   ;   WRITE(clkt,*       ) nitrst
            ELSE                        ;   WRITE(clkt,'(i8.8)') nitrst
            ENDIF
         ENDIF
         ! create the file
         IF(lwp) WRITE(numout,*)
         clname = TRIM(cexper)//"_"//TRIM(ADJUSTL(clkt))//"_"//TRIM(cn_trcrst_out)
         clpath = TRIM(cn_trcrst_outdir)
         IF( clpath(LEN_TRIM(clpath):) /= '/' ) clpath = TRIM(clpath) // '/'
         IF(lwp) WRITE(numout,*) &
             '             open trc restart.output NetCDF file: ',TRIM(clpath)//clname
         CALL iom_open( TRIM(clpath)//TRIM(clname), numrtw, ldwrt = .TRUE., kiolib = jprstlib )
         lrst_trc = .TRUE.
      ENDIF
      !
   END SUBROUTINE trc_rst_opn

   SUBROUTINE trc_rst_read
      !!----------------------------------------------------------------------
      !!                    ***  trc_rst_opn  ***
      !!
      !! ** purpose  :   read passive tracer fields in restart files
      !!----------------------------------------------------------------------
      INTEGER  ::  jn, jl     
      !! AXY (05/11/13): temporary variables
      REAL(wp) ::    fq0,fq1,fq2

      !!----------------------------------------------------------------------
      !
      IF(lwp) WRITE(numout,*)
      IF(lwp) WRITE(numout,*) 'trc_rst_read : read data in the TOP restart file'
      IF(lwp) WRITE(numout,*) '~~~~~~~~~~~~'

      ! READ prognostic variables and computes diagnostic variable
      DO jn = 1, jptra
         CALL iom_get( numrtr, jpdom_autoglo, 'TRN'//ctrcnm(jn), trn(:,:,:,jn) )
         trn(:,:,:,jn) = trn(:,:,:,jn) * tmask(:,:,:)
      END DO

      DO jn = 1, jptra
         CALL iom_get( numrtr, jpdom_autoglo, 'TRB'//ctrcnm(jn), trb(:,:,:,jn) )
         trb(:,:,:,jn) = trb(:,:,:,jn) * tmask(:,:,:)
      END DO
      ! +++>>> FABM

      IF (lk_fabm) CALL trc_rst_read_fabm
      ! FABM <<<+++
      !
      !
      !! AXY (09/06/14): the ARCHER version of MEDUSA-2 does not include an equivalent
      !!                 call to MEDUSA-2 at this point; this suggests that the FCM
      !!                 version of NEMO date significantly earlier than the current
      !!                 version

      !
      !
      !
   END SUBROUTINE trc_rst_read

   SUBROUTINE trc_rst_wri( kt )
      !!----------------------------------------------------------------------
      !!                    ***  trc_rst_wri  ***
      !!
      !! ** purpose  :   write passive tracer fields in restart files
      !!----------------------------------------------------------------------
      INTEGER, INTENT( in ) ::   kt    ! ocean time-step index
      !!
      INTEGER  :: jn, jl
      REAL(wp) :: zarak0
      !! AXY (05/11/13): temporary variables
      REAL(wp) ::    fq0,fq1,fq2
      !!----------------------------------------------------------------------
      !
      CALL iom_rstput( kt, nitrst, numrtw, 'rdttrc1', rdttrc(1) )   ! surface passive tracer time step
      ! prognostic variables 
      ! -------------------- 
      DO jn = 1, jptra
         CALL iom_rstput( kt, nitrst, numrtw, 'TRN'//ctrcnm(jn), trn(:,:,:,jn) )
      END DO

      DO jn = 1, jptra
         CALL iom_rstput( kt, nitrst, numrtw, 'TRB'//ctrcnm(jn), trb(:,:,:,jn) )
      END DO
      ! +++>>> FABM
      IF (lk_fabm) CALL trc_rst_wri_fabm(kt)
      ! FABM <<<+++
      !
      !! AXY (09/06/14): the ARCHER version of MEDUSA-2 does not include an equivalent
      !!                 call to MEDUSA-2 at this point; this suggests that the FCM
      !!                 version of NEMO date significantly earlier than the current
      !!                 version

      !
      !
      !

      IF( kt == nitrst ) THEN
          CALL trc_rst_stat            ! statistics
          CALL iom_close( numrtw )     ! close the restart file (only at last time step)
          lrst_trc = .FALSE.
          IF( lk_offline .AND. ln_rst_list ) THEN
             nrst_lst = nrst_lst + 1
             nitrst = nstocklist( nrst_lst )
          ENDIF
      ENDIF
      !
   END SUBROUTINE trc_rst_wri 


   SUBROUTINE trc_rst_cal( kt, cdrw )
      !!---------------------------------------------------------------------
      !!                   ***  ROUTINE trc_rst_cal  ***
      !!
      !!  ** Purpose : Read or write calendar in restart file:
      !!
      !!  WRITE(READ) mode:
      !!       kt        : number of time step since the begining of the experiment at the
      !!                   end of the current(previous) run
      !!       adatrj(0) : number of elapsed days since the begining of the experiment at the
      !!                   end of the current(previous) run (REAL -> keep fractions of day)
      !!       ndastp    : date at the end of the current(previous) run (coded as yyyymmdd integer)
      !!
      !!   According to namelist parameter nrstdt,
      !!       nn_rsttr = 0  no control on the date (nittrc000 is  arbitrary).
      !!       nn_rsttr = 1  we verify that nittrc000 is equal to the last
      !!                   time step of previous run + 1.
      !!       In both those options, the  exact duration of the experiment
      !!       since the beginning (cumulated duration of all previous restart runs)
      !!       is not stored in the restart and is assumed to be (nittrc000-1)*rdt.
      !!       This is valid is the time step has remained constant.
      !!
      !!       nn_rsttr = 2  the duration of the experiment in days (adatrj)
      !!                    has been stored in the restart file.
      !!----------------------------------------------------------------------
      INTEGER         , INTENT(in) ::   kt         ! ocean time-step
      CHARACTER(len=*), INTENT(in) ::   cdrw       ! "READ"/"WRITE" flag
      !
      INTEGER  ::  jlibalt = jprstlib
      LOGICAL  ::  llok
      REAL(wp) ::  zkt, zrdttrc1
      REAL(wp) ::  zndastp

      ! Time domain : restart
      ! ---------------------

      IF( TRIM(cdrw) == 'READ' ) THEN

         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) 'trc_rst_cal : read the TOP restart file for calendar'
         IF(lwp) WRITE(numout,*) '~~~~~~~~~~~~'

         IF ( jprstlib == jprstdimg ) THEN
           ! eventually read netcdf file (monobloc)  for restarting on different number of processors
           ! if {cn_trcrst_in}.nc exists, then set jlibalt to jpnf90 
           INQUIRE( FILE = TRIM(cn_trcrst_indir)//'/'//TRIM(cn_trcrst_in)//'.nc', EXIST = llok )
           IF ( llok ) THEN ; jlibalt = jpnf90  ; ELSE ; jlibalt = jprstlib ; ENDIF
         ENDIF

         IF( ln_rsttr ) THEN
            CALL iom_open( TRIM(cn_trcrst_indir)//'/'//cn_trcrst_in, numrtr, kiolib = jlibalt )
            CALL iom_get ( numrtr, 'kt', zkt )   ! last time-step of previous run

            IF(lwp) THEN
               WRITE(numout,*) ' *** Info read in restart : '
               WRITE(numout,*) '   previous time-step                               : ', NINT( zkt )
               WRITE(numout,*) ' *** restart option'
               SELECT CASE ( nn_rsttr )
               CASE ( 0 )   ;   WRITE(numout,*) ' nn_rsttr = 0 : no control of nittrc000'
               CASE ( 1 )   ;   WRITE(numout,*) ' nn_rsttr = 1 : no control the date at nittrc000 (use ndate0 read in the namelist)'
               CASE ( 2 )   ;   WRITE(numout,*) ' nn_rsttr = 2 : calendar parameters read in restart'
               END SELECT
               WRITE(numout,*)
            ENDIF
            ! Control of date 
            IF( nittrc000  - NINT( zkt ) /= nn_dttrc .AND.  nn_rsttr /= 0 )                                  &
               &   CALL ctl_stop( ' ===>>>> : problem with nittrc000 for the restart',                 &
               &                  ' verify the restart file or rerun with nn_rsttr = 0 (namelist)' )
         ENDIF
         !
         IF( lk_offline ) THEN    
            !                                          ! set the date in offline mode
            IF( ln_rsttr .AND. nn_rsttr == 2 ) THEN
               CALL iom_get( numrtr, 'ndastp', zndastp ) 
               ndastp = NINT( zndastp )
               CALL iom_get( numrtr, 'adatrj', adatrj  )
             ELSE
               ndastp = ndate0 - 1     ! ndate0 read in the namelist in dom_nam
               adatrj = ( REAL( nittrc000-1, wp ) * rdttra(1) ) / rday
               ! note this is wrong if time step has changed during run
            ENDIF
            !
            IF(lwp) THEN
              WRITE(numout,*) ' *** Info used values : '
              WRITE(numout,*) '   date ndastp                                      : ', ndastp
              WRITE(numout,*) '   number of elapsed days since the begining of run : ', adatrj
              WRITE(numout,*)
            ENDIF
            !
            IF( ln_rsttr )  THEN   ;    neuler = 1
            ELSE                   ;    neuler = 0
            ENDIF
            !
            CALL day_init          ! compute calendar
            !
         ENDIF
         !
      ELSEIF( TRIM(cdrw) == 'WRITE' ) THEN
         !
         IF(  kt == nitrst ) THEN
            IF(lwp) WRITE(numout,*)
            IF(lwp) WRITE(numout,*) 'trc_wri : write the TOP restart file (NetCDF) at it= ', kt, ' date= ', ndastp
            IF(lwp) WRITE(numout,*) '~~~~~~~'
         ENDIF
         CALL iom_rstput( kt, nitrst, numrtw, 'kt'     , REAL( kt    , wp) )   ! time-step
         CALL iom_rstput( kt, nitrst, numrtw, 'ndastp' , REAL( ndastp, wp) )   ! date
         CALL iom_rstput( kt, nitrst, numrtw, 'adatrj' , adatrj            )   ! number of elapsed days since
         !                                                                     ! the begining of the run [s]
      ENDIF

   END SUBROUTINE trc_rst_cal


   SUBROUTINE trc_rst_stat
      !!----------------------------------------------------------------------
      !!                    ***  trc_rst_stat  ***
      !!
      !! ** purpose  :   Compute tracers statistics
      !!----------------------------------------------------------------------
      INTEGER  :: jk, jn
      REAL(wp) :: ztraf, zmin, zmax, zmean, zdrift
      REAL(wp), DIMENSION(jpi,jpj,jpk) :: zvol
      !!----------------------------------------------------------------------

      IF( lwp ) THEN
         WRITE(numout,*) 
         WRITE(numout,*) '           ----TRACER STAT----             '
         WRITE(numout,*) 
      ENDIF
      !
      DO jk = 1, jpk
         zvol(:,:,jk) = e1e2t(:,:) * e3t_a(:,:,jk) * tmask(:,:,jk)
      END DO
      !
      DO jn = 1, jptra
         ztraf = glob_sum( trn(:,:,:,jn) * zvol(:,:,:) )
         zmin  = MINVAL( trn(:,:,:,jn), mask= ((tmask*SPREAD(tmask_i,DIM=3,NCOPIES=jpk).NE.0.)) )
         zmax  = MAXVAL( trn(:,:,:,jn), mask= ((tmask*SPREAD(tmask_i,DIM=3,NCOPIES=jpk).NE.0.)) )
         IF( lk_mpp ) THEN
            CALL mpp_min( zmin )      ! min over the global domain
            CALL mpp_max( zmax )      ! max over the global domain
         END IF
         zmean  = ztraf / areatot
         zdrift = ( ( ztraf - trai(jn) ) / ( trai(jn) + 1.e-12 )  ) * 100._wp
         IF(lwp) WRITE(numout,9000) jn, TRIM( ctrcnm(jn) ), zmean, zmin, zmax, zdrift
      END DO
      IF(lwp) WRITE(numout,*) 
9000  FORMAT(' tracer nb :',i2,'    name :',a10,'    mean :',e18.10,'    min :',e18.10, &
      &      '    max :',e18.10,'    drift :',e18.10, ' %')
      !
   END SUBROUTINE trc_rst_stat





   !!----------------------------------------------------------------------
   !! NEMO/TOP 3.3 , NEMO Consortium (2010)
   !! $Id$
   !! Software governed by the CeCILL licence (NEMOGCM/NEMO_CeCILL.txt)
   !!======================================================================
END MODULE trcrst
