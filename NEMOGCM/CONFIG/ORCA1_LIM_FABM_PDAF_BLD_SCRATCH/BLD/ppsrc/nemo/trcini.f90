












MODULE trcini
   !!======================================================================
   !!                         ***  MODULE trcini  ***
   !! TOP :   Manage the passive tracer initialization
   !!======================================================================
   !! History :   -   ! 1991-03 (O. Marti)  original code
   !!            1.0  ! 2005-03 (O. Aumont, A. El Moussaoui) F90
   !!            2.0  ! 2005-10 (C. Ethe, G. Madec) revised architecture
   !!            4.0  ! 2011-01 (A. R. Porter, STFC Daresbury) dynamical allocation
   !!             -   ! 2014-06 (A. Yool, J. Palmieri) adding MEDUSA-2
   !!----------------------------------------------------------------------
   !!----------------------------------------------------------------------
   !!   'key_top'                                                TOP models
   !!----------------------------------------------------------------------
   !!   trc_init  :   Initialization for passive tracer
   !!   top_alloc :   allocate the TOP arrays
   !!----------------------------------------------------------------------
   USE oce_trc         ! shared variables between ocean and passive tracers
   USE trc             ! passive tracers common variables
   USE trcrst          ! passive tracers restart
   USE trcnam          ! Namelist read
   USE trcini_cfc      ! CFC      initialisation
   USE trcini_pisces   ! PISCES   initialisation
   USE trcini_c14b     ! C14 bomb initialisation
   USE trcini_age      ! AGE      initialisation
   USE trcini_my_trc   ! MY_TRC   initialisation
   USE trcini_idtra    ! idealize tracer initialisation
   USE trcini_medusa   ! MEDUSA   initialisation
   USE par_medusa      ! MEDUSA   parameters (needed for elemental cycles)
   ! +++>>> FABM
   USE trcsms_fabm     ! FABM initialisation
   USE trcini_fabm     ! FABM initialisation
   ! FABM <<<FABM
   USE trcdta          ! initialisation from files
   USE daymod          ! calendar manager
   USE prtctl_trc      ! Print control passive tracers (prt_ctl_trc_init routine)
   USE trcsub          ! variables to substep passive tracers
   USE lib_mpp         ! distribued memory computing library
   USE sbc_oce
   USE trcice          ! tracers in sea ice
   USE trcbc,   only : trc_bc_init ! generalized Boundary Conditions
 
   IMPLICIT NONE
   PRIVATE
   
   PUBLIC   trc_init   ! called by opa

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
   !! NEMO/TOP 4.0 , NEMO Consortium (2011)
   !! $Id$
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS
   
   SUBROUTINE trc_init
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE trc_init  ***
      !!
      !! ** Purpose :   Initialization of the passive tracer fields 
      !!
      !! ** Method  : - read namelist
      !!              - control the consistancy 
      !!              - compute specific initialisations
      !!              - set initial tracer fields (either read restart 
      !!                or read data or analytical formulation
      !!---------------------------------------------------------------------
      INTEGER ::   ji, jj, jk, jn, jl    ! dummy loop indices
      CHARACTER (len=25) :: charout
      !!---------------------------------------------------------------------
      !
      IF( nn_timing == 1 )   CALL timing_start('trc_init')
      !
      IF(lwp) WRITE(numout,*)
      IF(lwp) WRITE(numout,*) 'trc_init : initial set up of the passive tracers'
      IF(lwp) WRITE(numout,*) '~~~~~~~'
      ! +++>>> FABM
      ! Allow FABM to update numbers of biogeochemical tracers, diagnostics (jptra etc.)
      IF( lk_fabm ) CALL nemo_fabm_configure
      ! FABM <<<+++

      CALL top_alloc()              ! allocate TOP arrays

      l_trcdm2dc = ln_dm2dc .OR. ( ln_cpl .AND. ncpl_qsr_freq /= 1 )
      l_trcdm2dc = l_trcdm2dc  .AND. .NOT. lk_offline
      IF( l_trcdm2dc .AND. lwp ) &
         &   CALL ctl_warn(' Coupling with passive tracers and used of diurnal cycle. &
         & Computation of a daily mean shortwave for some biogeochemical models) ')

      IF( nn_cla == 1 )   &
         &  CALL ctl_stop( ' Cross Land Advection not yet implemented with passive tracer ; nn_cla must be 0' )

      CALL trc_nam      ! read passive tracers namelists
      !
      IF(lwp) WRITE(numout,*)
      !
      IF( ln_rsttr .AND. .NOT. lk_offline ) CALL trc_rst_cal( nit000, 'READ' )   ! calendar
      !
      IF(lwp) WRITE(numout,*)
                                                              ! masked grid volume
      !                                                              ! masked grid volume
      DO jk = 1, jpk
         cvol(:,:,jk) = e1e2t(:,:) * e3t_n(:,:,jk) * tmask(:,:,jk)
      END DO
      IF( lk_degrad ) cvol(:,:,:) = cvol(:,:,:) * facvol(:,:,:)      ! degrad option: reduction by facvol
      !                                                              ! total volume of the ocean 
      areatot = glob_sum( cvol(:,:,:) )


      IF( lk_pisces  )       CALL trc_ini_pisces       ! PISCES  bio-model
      IF( lk_cfc     )       CALL trc_ini_cfc          ! CFC     tracers
      IF( lk_c14b    )       CALL trc_ini_c14b         ! C14 bomb  tracer
      IF( lk_age     )       CALL trc_ini_age          ! AGE       tracer
      IF( lk_my_trc  )       CALL trc_ini_my_trc       ! MY_TRC  tracers
      IF( lk_idtra   )       CALL trc_ini_idtra        ! Idealize tracers
      IF( lk_medusa  )       CALL trc_ini_medusa       ! MEDUSA  tracers
      ! +++>>> FABM
      IF( lk_fabm    )       CALL trc_ini_fabm         ! FABM    tracers
      ! FABM <<<+++

      CALL trc_ice_ini                                 ! Tracers in sea ice

      IF( ln_ctl ) THEN
         !
         IF (narea == 1) THEN  
            ! The tracer.stat file only contains global tracer sum values, if 
            ! it contains anything at all. Hence it only needs to be opened 
            ! and written to on the master PE, not on all PEs.  
            CALL ctl_opn( numstr, 'tracer.stat', 'REPLACE','FORMATTED',  & 
                          'SEQUENTIAL', -1, numout, lwp , narea ) 
         ENDIF  
         !
      ENDIF

      IF( ln_trcdta ) THEN
         CALL trc_dta_init(jptra)
      ENDIF

      IF( ln_rsttr ) THEN
        !
        CALL trc_rst_read              ! restart from a file
        !
      ELSE
        !
        IF( ln_trcdta .AND. nb_trcdta > 0 ) THEN  ! Initialisation of tracer from a file that may also be used for damping
            !
            DO jn = 1, jptra
               IF( ln_trc_ini(jn) ) THEN      ! update passive tracers arrays with input data read from file
                  jl = n_trc_index(jn) 
                  CALL trc_dta( nit000, sf_trcdta(jl), rf_trfac(jl) )   ! read tracer data at nit000
                  trn(:,:,:,jn) = sf_trcdta(jl)%fnow(:,:,:) 
                  IF( .NOT.ln_trcdmp .AND. .NOT.ln_trcdmp_clo ) THEN      !== deallocate data structure   ==!
                     !                                                    (data used only for initialisation)
                     IF(lwp) WRITE(numout,*) 'trc_dta: deallocate data arrays as they are only used to initialize the run'
                                                  DEALLOCATE( sf_trcdta(jl)%fnow )     !  arrays in the structure
                     IF( sf_trcdta(jl)%ln_tint )  DEALLOCATE( sf_trcdta(jl)%fdta )
                     !
                  ENDIF
               ENDIF
            ENDDO
            !
        ENDIF
        !
        trb(:,:,:,:) = trn(:,:,:,:)
        ! 
      ENDIF

     ! --->>> FABM
! Initialisation of FABM diagnostics and tracer boundary conditions (so that you can use initial condition as boundary)
      IF( lk_fabm )     THEN
          CALL trc_bc_init(jptra) 
      ENDIF
      ! FABM <<<+++

      tra(:,:,:,:) = 0._wp
      !
      IF( nn_dttrc /= 1 )        CALL trc_sub_ini      ! Initialize variables for substepping passive tracers
      !

      trai(:) = 0._wp                                                   ! initial content of all tracers
      DO jn = 1, jptra
         trai(jn) = trai(jn) + glob_sum( trn(:,:,:,jn) * cvol(:,:,:)   )
      END DO

      IF(lwp) THEN               ! control print
         WRITE(numout,*)
         WRITE(numout,*)
         WRITE(numout,*) '          *** Total number of passive tracer jptra = ', jptra
         WRITE(numout,*) '          *** Total volume of ocean                = ', areatot
         WRITE(numout,*) '          *** Total inital content of all tracers '
         WRITE(numout,*)
         !
         DO jn = 1, jptra
            WRITE(numout,9000) jn, TRIM( ctrcnm(jn) ), trai(jn)
         ENDDO
         WRITE(numout,*)
      ENDIF
      IF(lwp) WRITE(numout,*)
      IF(ln_ctl) THEN            ! print mean trends (used for debugging)
         CALL prt_ctl_trc_init
         WRITE(charout, FMT="('ini ')")
         CALL prt_ctl_trc_info( charout )
         CALL prt_ctl_trc( tab4d=trn, mask=tmask, clinfo=ctrcnm )
      ENDIF


      IF(lwp) THEN 
          WRITE(numout,*)
          WRITE(numout,*) 'trc_init : passive tracer set up completed'
          WRITE(numout,*) '~~~~~~~'
      ENDIF 

9000  FORMAT(' tracer nb : ',i2,'      name :',a10,'      initial content :',e18.10)
9010  FORMAT(' element:',a10,                     &
             ' 3d sum:',e18.10,' 2d sum:',e18.10, &
             ' total:',e18.10)
      !
      IF( nn_timing == 1 )   CALL timing_stop('trc_init')
      !
   END SUBROUTINE trc_init


   SUBROUTINE top_alloc
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE top_alloc  ***
      !!
      !! ** Purpose :   Allocate all the dynamic arrays of the OPA modules
      !!----------------------------------------------------------------------
      USE trcadv        , ONLY:   trc_adv_alloc          ! TOP-related alloc routines...
      USE trc           , ONLY:   trc_alloc
      USE trcnxt        , ONLY:   trc_nxt_alloc
      USE trczdf        , ONLY:   trc_zdf_alloc
      USE trdtrc_oce    , ONLY:   trd_trc_oce_alloc

      !
      INTEGER :: ierr
      !!----------------------------------------------------------------------
      !
      ierr =        trc_adv_alloc()          ! Start of TOP-related alloc routines...
      ierr = ierr + trc_alloc    ()
      ierr = ierr + trc_nxt_alloc()
      ierr = ierr + trc_zdf_alloc()
      ierr = ierr + trd_trc_oce_alloc()
      !
      IF( lk_mpp    )   CALL mpp_sum( ierr )
      IF( ierr /= 0 )   CALL ctl_stop( 'STOP', 'top_alloc : unable to allocate standard ocean arrays' )
      !
   END SUBROUTINE top_alloc


   !!======================================================================
END MODULE trcini
