












MODULE trcstp
   !!======================================================================
   !!                       ***  MODULE trcstp  ***
   !! Time-stepping    : time loop of opa for passive tracer
   !!======================================================================
   !! History :  1.0  !  2004-03  (C. Ethe)  Original
   !!----------------------------------------------------------------------
   !!----------------------------------------------------------------------
   !!   trc_stp      : passive tracer system time-stepping
   !!----------------------------------------------------------------------
   USE oce_trc          ! ocean dynamics and active tracers variables
   USE sbc_oce
   USE trc
   USE trctrp           ! passive tracers transport
   USE trcsms           ! passive tracers sources and sinks
   USE prtctl_trc       ! Print control for debbuging
   USE trcdia
   USE trcwri
   USE trcrst
   USE trcstat
   USE trdtrc_oce
   USE trdmxl_trc
   USE iom
   USE in_out_manager
   USE trcsub

   IMPLICIT NONE
   PRIVATE

   PUBLIC   trc_stp    ! called by step

   REAL(wp), DIMENSION(:,:,:), SAVE, ALLOCATABLE ::   qsr_arr ! save qsr during TOP time-step
   REAL(wp) :: rdt_sampl
   INTEGER  :: nb_rec_per_day
   INTEGER  :: isecfst, iseclast
   LOGICAL  :: llnew

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
   !! NEMO/TOP 3.3 , NEMO Consortium (2010)
   !! $Id$
   !! Software governed by the CeCILL licence (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE trc_stp( kt )
      !!-------------------------------------------------------------------
      !!                     ***  ROUTINE trc_stp  ***
      !!                      
      !! ** Purpose : Time loop of opa for passive tracer
      !! 
      !! ** Method  : 
      !!              Compute the passive tracers trends 
      !!              Update the passive tracers
      !!-------------------------------------------------------------------

      USE dom_oce, ONLY: narea

      INTEGER, INTENT( in ) ::  kt      ! ocean time-step index
      INTEGER               ::  jk, jn  ! dummy loop indices
      REAL(wp)              ::  ztrai
      CHARACTER (len=25)    ::  charout 
      !!-------------------------------------------------------------------
      !
      IF( nn_timing == 1 )   CALL timing_start('trc_stp')
      !
      IF( kt == nittrc000 .AND. lk_trdmxl_trc )  CALL trd_mxl_trc_init    ! trends: Mixed-layer
      !
      IF( lk_vvl ) THEN                                                   ! update ocean volume due to ssh temporal evolution
         DO jk = 1, jpk
            cvol(:,:,jk) = e1e2t(:,:) * e3t_n(:,:,jk) * tmask(:,:,jk)
         END DO
         IF( lk_degrad )  cvol(:,:,:) = cvol(:,:,:) * facvol(:,:,:)       ! degrad option: reduction by facvol
         areatot         = glob_sum( cvol(:,:,:) )
      ENDIF
      !
      IF( l_trcdm2dc )   CALL trc_mean_qsr( kt )
      !    
      IF( nn_dttrc /= 1 )   CALL trc_sub_stp( kt )  ! averaging physical variables for sub-stepping
      !    
      IF( MOD( kt , nn_dttrc ) == 0 ) THEN      ! only every nn_dttrc time step
         !
         IF(ln_ctl) THEN
            WRITE(charout,FMT="('kt =', I4,'  d/m/y =',I2,I2,I4)") kt, nday, nmonth, nyear
            CALL prt_ctl_trc_info(charout)
         ENDIF
         !
         tra(:,:,:,:) = 0.e0
         !
                                   CALL trc_rst_opn  ( kt )       ! Open tracer restart file 
         IF( lrst_trc )            CALL trc_rst_cal  ( kt, 'WRITE' )   ! calendar
         IF( lk_iomput ) THEN  ;   CALL trc_wri      ( kt )       ! output of passive tracers with iom I/O manager
         ELSE                  ;   CALL trc_dia      ( kt )       ! output of passive tracers with old I/O manager
         ENDIF
                                   CALL trc_sms      ( kt )       ! tracers: sinks and sources
                                   CALL trc_trp      ( kt )       ! transport of passive tracers
         IF( kt == nittrc000 ) THEN
            CALL iom_close( numrtr )       ! close input tracer restart file
            IF(lwm) CALL FLUSH( numont )   ! flush namelist output
         ENDIF
         IF( lrst_trc )            CALL trc_rst_wri  ( kt )       ! write tracer restart file
         IF( lk_trdmxl_trc  )      CALL trd_mxl_trc  ( kt )       ! trends: Mixed-layer
         !
         IF( nn_dttrc /= 1   )     CALL trc_sub_reset( kt )       ! resetting physical variables when sub-stepping
         !
      ENDIF
      !
      IF (ln_ctl) THEN 
         ! The following code is very expensive since it involves multiple 
         ! reproducible global sums over all tracer fields and is potentially  
         ! called on every timestep. The results it produces are purely for 
         ! informational purposes and do not affect model evolution. 
         ! Hence we restrict its use by protecting it with the ln_ctl RTL 
         ! which should normally only be used under debugging conditions 
         ! and not in operational runs. We also need to restrict output  
         ! to the master PE since there's no point duplicating the same results 
         ! on all processors.    
         ztrai = 0._wp                                                   !  content of all tracers
         DO jn = 1, jptra
            ztrai = ztrai + glob_sum( trn(:,:,:,jn) * cvol(:,:,:)   )
         END DO
         IF( numstr /= -1 ) WRITE(numstr,9300) kt,  ztrai / areatot
9300     FORMAT(i10,D23.16)
      ENDIF
      !
      IF( nn_timing == 1 )   CALL timing_stop('trc_stp')
      !
   END SUBROUTINE trc_stp

   SUBROUTINE trc_mean_qsr( kt )
      !!----------------------------------------------------------------------
      !!             ***  ROUTINE trc_mean_qsr  ***
      !!
      !! ** Purpose :  Compute daily mean qsr for biogeochemical model in case
      !!               of diurnal cycle
      !!
      !! ** Method  : store in TOP the qsr every hour ( or every time-step if the latter 
      !!              is greater than 1 hour ) and then, compute the  mean with 
      !!              a moving average over 24 hours. 
      !!              In coupled mode, the sampling is done at every coupling frequency 
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt
      INTEGER  :: jn
      REAL(wp) :: zsecfst
      CHARACTER(len=1)               ::   cl1                      ! 1 character
      CHARACTER(len=2)               ::   cl2                      ! 2 characters

      IF( kt == nittrc000 ) THEN
         IF( ln_cpl )  THEN  
            rdt_sampl = 86400. / ncpl_qsr_freq
            nb_rec_per_day = ncpl_qsr_freq
         ELSE  
            rdt_sampl = MAX( 3600., rdt * nn_dttrc )
            nb_rec_per_day = INT( 86400 / rdt_sampl )
         ENDIF
         !
         IF( lwp ) THEN
            WRITE(numout,*) 
            WRITE(numout,*) ' Sampling frequency dt = ', rdt_sampl, 's','   Number of sampling per day  nrec = ', nb_rec_per_day
            WRITE(numout,*) 
         ENDIF
         !
         ALLOCATE( qsr_arr(jpi,jpj,nb_rec_per_day ) )
         !
         !                                            !* Restart: read in restart file
         IF( ln_rsttr .AND. nn_rsttr /= 0 .AND. iom_varid( numrtr, 'qsr_mean' , ldstop = .FALSE. ) > 0 &
                                          .AND. iom_varid( numrtr, 'qsr_arr_1', ldstop = .FALSE. ) > 0 &
                                          .AND. iom_varid( numrtr, 'zsecfst'  , ldstop = .FALSE. ) > 0 ) THEN
            IF(lwp) WRITE(numout,*) 'trc_qsr_mean:   qsr_mean read in the restart file'
            CALL iom_get( numrtr, jpdom_autoglo, 'qsr_mean', qsr_mean )   !  A mean of qsr
            CALL iom_get( numrtr, 'zsecfst', zsecfst )   !  A mean of qsr
            isecfst = INT( zsecfst )
            DO jn = 1, nb_rec_per_day 
             IF( jn <= 9 )  THEN
               WRITE(cl1,'(i1)') jn
               CALL iom_get( numrtr, jpdom_autoglo, 'qsr_arr_'//cl1, qsr_arr(:,:,jn) )   !  A mean of qsr
             ELSE
               WRITE(cl2,'(i2.2)') jn
               CALL iom_get( numrtr, jpdom_autoglo, 'qsr_arr_'//cl2, qsr_arr(:,:,jn) )   !  A mean of qsr
             ENDIF
           ENDDO
         ELSE                                         !* no restart: set from nit000 values
            IF(lwp) WRITE(numout,*) 'trc_qsr_mean:   qsr_mean set to nit000 values'
            isecfst  = nsec_year + nsec1jan000   !   number of seconds between Jan. 1st 00h of nit000 year and the middle of time step
            !
            qsr_mean(:,:) = qsr(:,:)
            DO jn = 1, nb_rec_per_day
               qsr_arr(:,:,jn) = qsr_mean(:,:)
            ENDDO
         ENDIF
         !
      ENDIF
      !
      iseclast = nsec_year + nsec1jan000
      !
      llnew   = ( iseclast - isecfst )  > INT( rdt_sampl )   !   new shortwave to store
      IF( llnew ) THEN
          IF( lwp ) WRITE(numout,*) ' New shortwave to sample for TOP at time kt = ', kt, &
             &                      ' time = ', (iseclast+rdt*nn_dttrc/2.)/3600.,'hours '
          isecfst = iseclast
          DO jn = 1, nb_rec_per_day - 1
             qsr_arr(:,:,jn) = qsr_arr(:,:,jn+1)
          ENDDO
          qsr_arr (:,:,nb_rec_per_day) = qsr(:,:)
          qsr_mean(:,:                ) = SUM( qsr_arr(:,:,:), 3 ) / nb_rec_per_day
      ENDIF
      !
      IF( lrst_trc ) THEN    !* Write the mean of qsr in restart file 
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) 'trc_mean_qsr : write qsr_mean in restart file  kt =', kt
         IF(lwp) WRITE(numout,*) '~~~~~~~'
          DO jn = 1, nb_rec_per_day 
             IF( jn <= 9 )  THEN
               WRITE(cl1,'(i1)') jn
               CALL iom_rstput( kt, nitrst, numrtw, 'qsr_arr_'//cl1, qsr_arr(:,:,jn) )
             ELSE
               WRITE(cl2,'(i2.2)') jn
               CALL iom_rstput( kt, nitrst, numrtw, 'qsr_arr_'//cl2, qsr_arr(:,:,jn) )
             ENDIF
         ENDDO
         CALL iom_rstput( kt, nitrst, numrtw, 'qsr_mean', qsr_mean(:,:) )
         zsecfst = REAL( isecfst, wp )
         CALL iom_rstput( kt, nitrst, numrtw, 'zsecfst', zsecfst )
      ENDIF
      !
   END SUBROUTINE trc_mean_qsr


   !!======================================================================
END MODULE trcstp
