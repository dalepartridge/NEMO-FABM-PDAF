












MODULE zdfmxl
   !!======================================================================
   !!                       ***  MODULE  zdfmxl  ***
   !! Ocean physics: mixed layer depth 
   !!======================================================================
   !! History :  1.0  ! 2003-08  (G. Madec)  original code
   !!            3.2  ! 2009-07  (S. Masson, G. Madec)  IOM + merge of DO-loop
   !!            3.7  ! 2012-03  (G. Madec)  make public the density criteria for trdmxl 
   !!             -   ! 2014-02  (F. Roquet)  mixed layer depth calculated using N2 instead of rhop 
   !!----------------------------------------------------------------------
   !!   zdf_mxl      : Compute the turbocline and mixed layer depths.
   !!----------------------------------------------------------------------
   USE oce             ! ocean dynamics and tracers variables
   USE dom_oce         ! ocean space and time domain variables
   USE zdf_oce         ! ocean vertical physics
   USE in_out_manager  ! I/O manager
   USE prtctl          ! Print control
   USE phycst          ! physical constants
   USE iom             ! I/O library
   USE eosbn2          ! for zdf_mxl_zint
   USE lib_mpp         ! MPP library
   USE wrk_nemo        ! work arrays
   USE timing          ! Timing
   USE trc_oce, ONLY : lk_offline ! offline flag

   IMPLICIT NONE
   PRIVATE

   PUBLIC   zdf_mxl       ! called by step.F90
   PUBLIC   zdf_mxl_alloc ! Used in zdf_tke_init

   INTEGER , PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   nmln    !: number of level in the mixed layer (used by TOP)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   hmld    !: mixing layer depth (turbocline)      [m]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   hmlp    !: mixed layer depth  (rho=rho0+zdcrit) [m]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   hmlpt   !: mixed layer depth at t-points        [m]
   REAL(wp), PUBLIC, ALLOCATABLE,       DIMENSION(:,:) ::   hmld_zint  !: vertically-interpolated mixed layer depth   [m] 
   REAL(wp), PUBLIC, ALLOCATABLE,       DIMENSION(:,:) ::   htc_mld    ! Heat content of hmld_zint
   LOGICAL, PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)    :: ll_found   ! Is T_b to be found by interpolation ? 
   LOGICAL, PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)  :: ll_belowml ! Flag points below mixed layer when ll_found=F

   REAL(wp), PUBLIC ::   rho_c = 0.01_wp    !: density criterion for mixed layer depth
   REAL(wp)         ::   avt_c = 5.e-4_wp   ! Kz criterion for the turbocline depth

   TYPE, PUBLIC :: MXL_ZINT   !: Structure for MLD defs
      INTEGER   :: mld_type   ! mixed layer type     
      REAL(wp)  :: zref       ! depth of initial T_ref
      REAL(wp)  :: dT_crit    ! Critical temp diff
      REAL(wp)  :: iso_frac   ! Fraction of rn_dT_crit used
   END TYPE MXL_ZINT

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
   !! NEMO/OPA 4.0 , NEMO Consortium (2011)
   !! $Id$
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   INTEGER FUNCTION zdf_mxl_alloc()
      !!----------------------------------------------------------------------
      !!               ***  FUNCTION zdf_mxl_alloc  ***
      !!----------------------------------------------------------------------
      zdf_mxl_alloc = 0      ! set to zero if no array to be allocated
      IF( .NOT. ALLOCATED( nmln ) ) THEN
         ALLOCATE( nmln(jpi,jpj), hmld(jpi,jpj), hmlp(jpi,jpj), hmlpt(jpi,jpj), hmld_zint(jpi,jpj),       &
        &          htc_mld(jpi,jpj),                                                                    &
        &          ll_found(jpi,jpj), ll_belowml(jpi,jpj,jpk), STAT= zdf_mxl_alloc )
         !
         IF( lk_mpp             )   CALL mpp_sum ( zdf_mxl_alloc )
         IF( zdf_mxl_alloc /= 0 )   CALL ctl_warn('zdf_mxl_alloc: failed to allocate arrays.')
         !
      ENDIF
   END FUNCTION zdf_mxl_alloc


   SUBROUTINE zdf_mxl( kt )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE zdfmxl  ***
      !!                   
      !! ** Purpose :   Compute the turbocline depth and the mixed layer depth
      !!              with density criteria.
      !!
      !! ** Method  :   The mixed layer depth is the shallowest W depth with 
      !!      the density of the corresponding T point (just bellow) bellow a
      !!      given value defined locally as rho(10m) + rho_c
      !!               The turbocline depth is the depth at which the vertical
      !!      eddy diffusivity coefficient (resulting from the vertical physics
      !!      alone, not the isopycnal part, see trazdf.F) fall below a given
      !!      value defined locally (avt_c here taken equal to 5 cm/s2 by default)
      !!
      !! ** Action  :   nmln, hmld, hmlp, hmlpt
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt   ! ocean time-step index
      !
      INTEGER  ::   ji, jj, jk      ! dummy loop indices
      INTEGER  ::   iikn, iiki, ikt ! local integer
      REAL(wp) ::   zN2_c           ! local scalar
      INTEGER, POINTER, DIMENSION(:,:) ::   imld   ! 2D workspace
      !!----------------------------------------------------------------------
      !
      IF( nn_timing == 1 )  CALL timing_start('zdf_mxl')
      !
      CALL wrk_alloc( jpi,jpj, imld )

      IF( kt <= nit000 ) THEN
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) 'zdf_mxl : mixed layer depth'
         IF(lwp) WRITE(numout,*) '~~~~~~~ '
         !                             ! allocate zdfmxl arrays
         IF( zdf_mxl_alloc() /= 0 )   CALL ctl_stop( 'STOP', 'zdf_mxl : unable to allocate arrays' )
      ENDIF

      ! w-level of the mixing and mixed layers
      nmln(:,:)  = nlb10               ! Initialization to the number of w ocean point
      hmlp(:,:)  = 0._wp               ! here hmlp used as a dummy variable, integrating vertically N^2
      zN2_c = grav * rho_c * r1_rau0   ! convert density criteria into N^2 criteria
      DO jk = nlb10, jpkm1
         DO jj = 1, jpj                ! Mixed layer level: w-level 
            DO ji = 1, jpi
               ikt = mbkt(ji,jj)
               hmlp(ji,jj) = hmlp(ji,jj) + MAX( rn2b(ji,jj,jk) , 0._wp ) * e3w_n(ji,jj,jk)
               IF( hmlp(ji,jj) < zN2_c )   nmln(ji,jj) = MIN( jk , ikt ) + 1   ! Mixed layer level
            END DO
         END DO
      END DO
      !
      ! w-level of the turbocline
      imld(:,:) = mbkt(:,:) + 1        ! Initialization to the number of w ocean point
      DO jk = jpkm1, nlb10, -1         ! from the bottom to nlb10 
         DO jj = 1, jpj
            DO ji = 1, jpi
               IF( avt (ji,jj,jk) < avt_c * wmask(ji,jj,jk) )   imld(ji,jj) = jk      ! Turbocline 
            END DO
         END DO
      END DO
      ! depth of the mixing and mixed layers
      DO jj = 1, jpj
         DO ji = 1, jpi
            iiki = imld(ji,jj)
            iikn = nmln(ji,jj)
            hmld (ji,jj) = gdepw_n(ji,jj,iiki  ) * ssmask(ji,jj)    ! Turbocline depth 
            hmlp (ji,jj) = gdepw_n(ji,jj,iikn  ) * ssmask(ji,jj)    ! Mixed layer depth
            hmlpt(ji,jj) = gdept_n(ji,jj,iikn-1) * ssmask(ji,jj)    ! depth of the last T-point inside the mixed layer
         END DO
      END DO
      ! no need to output in offline mode
      IF( .NOT.lk_offline ) THEN   
         IF ( iom_use("mldr10_1") ) THEN
            IF( ln_isfcav ) THEN
               CALL iom_put( "mldr10_1", hmlp - risfdep)   ! mixed layer thickness
            ELSE
               CALL iom_put( "mldr10_1", hmlp )            ! mixed layer depth
            END IF
         END IF
         IF ( iom_use("mldkz5") ) THEN
            IF( ln_isfcav ) THEN
               CALL iom_put( "mldkz5"  , hmld - risfdep )   ! turbocline thickness
            ELSE
               CALL iom_put( "mldkz5"  , hmld )             ! turbocline depth
            END IF
         END IF
      ENDIF
      
      ! Vertically-interpolated mixed-layer depth diagnostic
      CALL zdf_mxl_zint( kt )

      IF(ln_ctl)   CALL prt_ctl( tab2d_1=REAL(nmln,wp), clinfo1=' nmln : ', tab2d_2=hmlp, clinfo2=' hmlp : ', ovlap=1 )
      !
      CALL wrk_dealloc( jpi,jpj, imld )
      !
      IF( nn_timing == 1 )  CALL timing_stop('zdf_mxl')
      !
   END SUBROUTINE zdf_mxl

   SUBROUTINE zdf_mxl_zint_mld( sf ) 
      !!---------------------------------------------------------------------------------- 
      !!                    ***  ROUTINE zdf_mxl_zint_mld  *** 
      !                                                                        
      !   Calculate vertically-interpolated mixed layer depth diagnostic. 
      !            
      !   This routine can calculate the mixed layer depth diagnostic suggested by
      !   Kara et al, 2000, JGR, 105, 16803, but is more general and can calculate
      !   vertically-interpolated mixed-layer depth diagnostics with other parameter
      !   settings set in the namzdf_mldzint namelist.  
      ! 
      !   If mld_type=1 the mixed layer depth is calculated as the depth at which the  
      !   density has increased by an amount equivalent to a temperature difference of  
      !   0.8C at the surface. 
      ! 
      !   For other values of mld_type the mixed layer is calculated as the depth at  
      !   which the temperature differs by 0.8C from the surface temperature.  
      !                                                                        
      !   David Acreman, Daley Calvert                                      
      ! 
      !!----------------------------------------------------------------------------------- 

      TYPE(MXL_ZINT), INTENT(in)  :: sf

      ! Diagnostic criteria
      INTEGER   :: nn_mld_type   ! mixed layer type     
      REAL(wp)  :: rn_zref       ! depth of initial T_ref
      REAL(wp)  :: rn_dT_crit    ! Critical temp diff
      REAL(wp)  :: rn_iso_frac   ! Fraction of rn_dT_crit used

      ! Local variables
      REAL(wp), PARAMETER :: zepsilon = 1.e-30          ! local small value
      INTEGER, POINTER, DIMENSION(:,:) :: ikmt          ! number of active tracer levels 
      INTEGER, POINTER, DIMENSION(:,:) :: ik_ref        ! index of reference level 
      INTEGER, POINTER, DIMENSION(:,:) :: ik_iso        ! index of last uniform temp level 
      REAL, POINTER, DIMENSION(:,:,:)  :: zT            ! Temperature or density 
      REAL, POINTER, DIMENSION(:,:)    :: ppzdep        ! depth for use in calculating d(rho) 
      REAL, POINTER, DIMENSION(:,:)    :: zT_ref        ! reference temperature 
      REAL    :: zT_b                                   ! base temperature 
      REAL, POINTER, DIMENSION(:,:,:)  :: zdTdz         ! gradient of zT 
      REAL, POINTER, DIMENSION(:,:,:)  :: zmoddT        ! Absolute temperature difference 
      REAL    :: zdz                                    ! depth difference 
      REAL    :: zdT                                    ! temperature difference 
      REAL, POINTER, DIMENSION(:,:)    :: zdelta_T      ! difference critereon 
      REAL, POINTER, DIMENSION(:,:)    :: zRHO1, zRHO2  ! Densities 
      INTEGER :: ji, jj, jk                             ! loop counter 

      !!------------------------------------------------------------------------------------- 
      !  
      CALL wrk_alloc( jpi, jpj, ikmt, ik_ref, ik_iso) 
      CALL wrk_alloc( jpi, jpj, ppzdep, zT_ref, zdelta_T, zRHO1, zRHO2 ) 
      CALL wrk_alloc( jpi, jpj, jpk, zT, zdTdz, zmoddT ) 

      ! Unpack structure
      nn_mld_type = sf%mld_type
      rn_zref     = sf%zref
      rn_dT_crit  = sf%dT_crit
      rn_iso_frac = sf%iso_frac

      ! Set the mixed layer depth criterion at each grid point 
      IF( nn_mld_type == 0 ) THEN
         zdelta_T(:,:) = rn_dT_crit
         zT(:,:,:) = rhop(:,:,:)
      ELSE IF( nn_mld_type == 1 ) THEN
         ppzdep(:,:)=0.0 
         call eos ( tsn(:,:,1,:), ppzdep(:,:), zRHO1(:,:) ) 
! Use zT temporarily as a copy of tsn with rn_dT_crit added to SST 
! [assumes number of tracers less than number of vertical levels] 
         zT(:,:,1:jpts)=tsn(:,:,1,1:jpts) 
         zT(:,:,jp_tem)=zT(:,:,1)+rn_dT_crit 
         CALL eos( zT(:,:,1:jpts), ppzdep(:,:), zRHO2(:,:) ) 
         zdelta_T(:,:) = abs( zRHO1(:,:) - zRHO2(:,:) ) * rau0 
         ! RHO from eos (2d version) doesn't calculate north or east halo: 
         CALL lbc_lnk( zdelta_T, 'T', 1. ) 
         zT(:,:,:) = rhop(:,:,:) 
      ELSE 
         zdelta_T(:,:) = rn_dT_crit                      
         zT(:,:,:) = tsn(:,:,:,jp_tem)                           
      END IF 

      ! Calculate the gradient of zT and absolute difference for use later 
      DO jk = 1 ,jpk-2 
         zdTdz(:,:,jk)  =    ( zT(:,:,jk+1) - zT(:,:,jk) ) / e3w_n(:,:,jk+1) 
         zmoddT(:,:,jk) = abs( zT(:,:,jk+1) - zT(:,:,jk) ) 
      END DO 

      ! Find density/temperature at the reference level (Kara et al use 10m).          
      ! ik_ref is the index of the box centre immediately above or at the reference level 
      ! Find rn_zref in the array of model level depths and find the ref    
      ! density/temperature by linear interpolation.                                   
      DO jk = jpkm1, 2, -1 
         WHERE ( gdept_n(:,:,jk) > rn_zref ) 
           ik_ref(:,:) = jk - 1 
           zT_ref(:,:) = zT(:,:,jk-1) + zdTdz(:,:,jk-1) * ( rn_zref - gdept_n(:,:,jk-1) ) 
         END WHERE 
      END DO 

      ! If the first grid box centre is below the reference level then use the 
      ! top model level to get zT_ref 
      WHERE ( gdept_n(:,:,1) > rn_zref )  
         zT_ref = zT(:,:,1) 
         ik_ref = 1 
      END WHERE 

      ! The number of active tracer levels is 1 less than the number of active w levels 
      ikmt(:,:) = mbathy(:,:) - 1 

      ! Initialize / reset
      ll_found(:,:) = .false.

      IF ( rn_iso_frac - zepsilon > 0. ) THEN
         ! Search for a uniform density/temperature region where adjacent levels          
         ! differ by less than rn_iso_frac * deltaT.                                      
         ! ik_iso is the index of the last level in the uniform layer  
         ! ll_found indicates whether the mixed layer depth can be found by interpolation 
         ik_iso(:,:)   = ik_ref(:,:) 
         DO jj = 1, nlcj 
            DO ji = 1, nlci 
!CDIR NOVECTOR 
               DO jk = ik_ref(ji,jj), ikmt(ji,jj)-1 
                  IF ( zmoddT(ji,jj,jk) > ( rn_iso_frac * zdelta_T(ji,jj) ) ) THEN 
                     ik_iso(ji,jj)   = jk 
                     ll_found(ji,jj) = ( zmoddT(ji,jj,jk) > zdelta_T(ji,jj) ) 
                     EXIT 
                  END IF 
               END DO 
            END DO 
         END DO 

         ! Use linear interpolation to find depth of mixed layer base where possible 
         hmld_zint(:,:) = rn_zref 
         DO jj = 1, jpj 
            DO ji = 1, jpi 
               IF (ll_found(ji,jj) .and. tmask(ji,jj,1) == 1.0) THEN 
                  zdz =  abs( zdelta_T(ji,jj) / zdTdz(ji,jj,ik_iso(ji,jj)) ) 
                  hmld_zint(ji,jj) = gdept_n(ji,jj,ik_iso(ji,jj)) + zdz 
               END IF 
            END DO 
         END DO 
      END IF

      ! If ll_found = .false. then calculate MLD using difference of zdelta_T    
      ! from the reference density/temperature 
 
! Prevent this section from working on land points 
      WHERE ( tmask(:,:,1) /= 1.0 ) 
         ll_found = .true. 
      END WHERE 
 
      DO jk=1, jpk 
         ll_belowml(:,:,jk) = abs( zT(:,:,jk) - zT_ref(:,:) ) >= zdelta_T(:,:)  
      END DO 
 
! Set default value where interpolation cannot be used (ll_found=false)  
      DO jj = 1, jpj 
         DO ji = 1, jpi 
            IF ( .not. ll_found(ji,jj) )  hmld_zint(ji,jj) = gdept_n(ji,jj,ikmt(ji,jj)) 
         END DO 
      END DO 

      DO jj = 1, jpj 
         DO ji = 1, jpi 
!CDIR NOVECTOR 
            DO jk = ik_ref(ji,jj)+1, ikmt(ji,jj) 
               IF ( ll_found(ji,jj) ) EXIT 
               IF ( ll_belowml(ji,jj,jk) ) THEN                
                  zT_b = zT_ref(ji,jj) + zdelta_T(ji,jj) * SIGN(1.0, zdTdz(ji,jj,jk-1) ) 
                  zdT  = zT_b - zT(ji,jj,jk-1)                                      
                  zdz  = zdT / zdTdz(ji,jj,jk-1)                                       
                  hmld_zint(ji,jj) = gdept_n(ji,jj,jk-1) + zdz 
                  EXIT                                                   
               END IF 
            END DO 
         END DO 
      END DO 

      hmld_zint(:,:) = hmld_zint(:,:)*tmask(:,:,1) 
      !  
      CALL wrk_dealloc( jpi, jpj, ikmt, ik_ref, ik_iso) 
      CALL wrk_dealloc( jpi, jpj, ppzdep, zT_ref, zdelta_T, zRHO1, zRHO2 ) 
      CALL wrk_dealloc( jpi,jpj, jpk, zT, zdTdz, zmoddT ) 
      ! 
   END SUBROUTINE zdf_mxl_zint_mld

   SUBROUTINE zdf_mxl_zint_htc( kt )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE zdf_mxl_zint_htc  ***
      !! 
      !! ** Purpose :   
      !!
      !! ** Method  :   
      !!----------------------------------------------------------------------

      INTEGER, INTENT(in) ::   kt   ! ocean time-step index

      INTEGER :: ji, jj, jk
      INTEGER :: ikmax
      REAL(wp) :: zc, zcoef
      !
      INTEGER,  ALLOCATABLE, DIMENSION(:,:) ::   ilevel
      REAL(wp), ALLOCATABLE, DIMENSION(:,:) ::   zthick_0, zthick

      !!----------------------------------------------------------------------

      IF( .NOT. ALLOCATED(ilevel) ) THEN
         ALLOCATE( ilevel(jpi,jpj), zthick_0(jpi,jpj), &
         &         zthick(jpi,jpj), STAT=ji )
         IF( lk_mpp  )   CALL mpp_sum(ji)
         IF( ji /= 0 )   CALL ctl_stop( 'STOP', 'zdf_mxl_zint_htc : unable to allocate arrays' )
      ENDIF

      ! Find last whole model T level above the MLD
      ilevel(:,:)   = 0
      zthick_0(:,:) = 0._wp

      DO jk = 1, jpkm1  
         DO jj = 1, jpj
            DO ji = 1, jpi                    
               zthick_0(ji,jj) = zthick_0(ji,jj) + e3t_n(ji,jj,jk)
               IF( zthick_0(ji,jj) < hmld_zint(ji,jj) )   ilevel(ji,jj) = jk
            END DO
         END DO
         WRITE(numout,*) 'zthick_0(jk =',jk,') =',zthick_0(2,2)
         WRITE(numout,*) 'fsdepw(jk+1 =',jk+1,') =',gdepw_n(2,2,jk+1)
      END DO

      ! Surface boundary condition
      IF( lk_vvl ) THEN   ;   zthick(:,:) = 0._wp       ;   htc_mld(:,:) = 0._wp                                   
      ELSE                ;   zthick(:,:) = sshn(:,:)   ;   htc_mld(:,:) = tsn(:,:,1,jp_tem) * sshn(:,:) * tmask(:,:,1)   
      ENDIF

      ! Deepest whole T level above the MLD
      ikmax = MIN( MAXVAL( ilevel(:,:) ), jpkm1 )

      ! Integration down to last whole model T level
      DO jk = 1, ikmax
         DO jj = 1, jpj
            DO ji = 1, jpi
               zc = e3t_n(ji,jj,jk) * REAL( MIN( MAX( 0, ilevel(ji,jj) - jk + 1 ) , 1  )  )    ! 0 below ilevel
               zthick(ji,jj) = zthick(ji,jj) + zc
               htc_mld(ji,jj) = htc_mld(ji,jj) + zc * tsn(ji,jj,jk,jp_tem) * tmask(ji,jj,jk)
            END DO
         END DO
      END DO

      ! Subsequent partial T level
      zthick(:,:) = hmld_zint(:,:) - zthick(:,:)   !   remaining thickness to reach MLD

      DO jj = 1, jpj
         DO ji = 1, jpi
            htc_mld(ji,jj) = htc_mld(ji,jj) + tsn(ji,jj,ilevel(ji,jj)+1,jp_tem)  & 
      &                      * MIN( e3t_n(ji,jj,ilevel(ji,jj)+1), zthick(ji,jj) ) * tmask(ji,jj,ilevel(ji,jj)+1)
         END DO
      END DO

      WRITE(numout,*) 'htc_mld(after) =',htc_mld(2,2)

      ! Convert to heat content
      zcoef = rau0 * rcp
      htc_mld(:,:) = zcoef * htc_mld(:,:)

   END SUBROUTINE zdf_mxl_zint_htc

   SUBROUTINE zdf_mxl_zint( kt )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE zdf_mxl_zint  ***
      !! 
      !! ** Purpose :   
      !!
      !! ** Method  :   
      !!----------------------------------------------------------------------

      INTEGER, INTENT(in) ::   kt   ! ocean time-step index

      INTEGER :: ios
      INTEGER :: jn

      INTEGER :: nn_mld_diag = 0    ! number of diagnostics

      CHARACTER(len=1) :: cmld

      TYPE(MXL_ZINT) :: sn_mld1, sn_mld2, sn_mld3, sn_mld4, sn_mld5
      TYPE(MXL_ZINT), SAVE, DIMENSION(5) ::   mld_diags

      NAMELIST/namzdf_mldzint/ nn_mld_diag, sn_mld1, sn_mld2, sn_mld3, sn_mld4, sn_mld5

      !!----------------------------------------------------------------------
      
      IF( kt == nit000 ) THEN
         REWIND( numnam_ref )              ! Namelist namzdf_mldzint in reference namelist 
         READ  ( numnam_ref, namzdf_mldzint, IOSTAT = ios, ERR = 901)
901      IF( ios /= 0 ) CALL ctl_nam ( ios , 'namzdf_mldzint in reference namelist', lwp )

         REWIND( numnam_cfg )              ! Namelist namzdf_mldzint in configuration namelist 
         READ  ( numnam_cfg, namzdf_mldzint, IOSTAT = ios, ERR = 902 )
902      IF( ios /= 0 ) CALL ctl_nam ( ios , 'namzdf_mldzint in configuration namelist', lwp )
         IF(lwm) WRITE ( numond, namzdf_mldzint )

         IF( nn_mld_diag > 5 )   CALL ctl_stop( 'STOP', 'zdf_mxl_ini: Specify no more than 5 MLD definitions' )

         mld_diags(1) = sn_mld1
         mld_diags(2) = sn_mld2
         mld_diags(3) = sn_mld3
         mld_diags(4) = sn_mld4
         mld_diags(5) = sn_mld5

         IF( lwp .AND. (nn_mld_diag > 0) ) THEN
            WRITE(numout,*) '=============== Vertically-interpolated mixed layer ================'
            WRITE(numout,*) '(Diagnostic number, nn_mld_type, rn_zref, rn_dT_crit, rn_iso_frac)'
            DO jn = 1, nn_mld_diag
               WRITE(numout,*) 'MLD criterion',jn,':'
               WRITE(numout,*) '    nn_mld_type =', mld_diags(jn)%mld_type
               WRITE(numout,*) '    rn_zref ='    , mld_diags(jn)%zref
               WRITE(numout,*) '    rn_dT_crit =' , mld_diags(jn)%dT_crit
               WRITE(numout,*) '    rn_iso_frac =', mld_diags(jn)%iso_frac
            END DO
            WRITE(numout,*) '===================================================================='
         ENDIF
      ENDIF

      IF( nn_mld_diag > 0 ) THEN
         DO jn = 1, nn_mld_diag
            WRITE(cmld,'(I1)') jn
            IF( iom_use( "mldzint_"//cmld ) .OR. iom_use( "mldhtc_"//cmld ) ) THEN
               CALL zdf_mxl_zint_mld( mld_diags(jn) )

               IF( iom_use( "mldzint_"//cmld ) ) THEN
                  CALL iom_put( "mldzint_"//cmld, hmld_zint(:,:) )
               ENDIF

               IF( iom_use( "mldhtc_"//cmld ) )  THEN
                  CALL zdf_mxl_zint_htc( kt )
                  CALL iom_put( "mldhtc_"//cmld , htc_mld(:,:)   )
               ENDIF
            ENDIF
         END DO
      ENDIF

   END SUBROUTINE zdf_mxl_zint

   !!======================================================================
END MODULE zdfmxl
