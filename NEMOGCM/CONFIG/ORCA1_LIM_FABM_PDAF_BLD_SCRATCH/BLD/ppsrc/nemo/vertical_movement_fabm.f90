












MODULE vertical_movement_fabm
   !!======================================================================
   !!                         ***  MODULE vertical_movement_fabm  ***
   !! TOP :   Module for the vertical movement of the FABM tracers
   !!======================================================================

   !!----------------------------------------------------------------------
   !!   'key_fabm'                                               FABM tracers
   !!----------------------------------------------------------------------
   !! compute_vertical_movement : compute vertical movement of FABM fields
   !!----------------------------------------------------------------------
   USE par_trc
   USE oce_trc
   USE trc
   USE par_fabm
   USE dom_oce

   IMPLICIT NONE

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

   PRIVATE

   PUBLIC compute_vertical_movement

   ! Work arrays for vertical advection (residual movement/sinking/floating)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, TARGET, DIMENSION(:,:,:) :: w_ct

   CONTAINS

   SUBROUTINE compute_vertical_movement( kt, method )
      !!----------------------------------------------------------------------
      !!                     ***  compute_vertical_movement  ***
      !!
      !! ** Purpose : compute vertical movement of FABM tracers through the water
      !!              (sinking/floating/active movement)
      !!
      !! ** Method  : Retrieves additional vertical velocity field and applies
      !!              advection scheme.
      !!----------------------------------------------------------------------
      !
      INTEGER, INTENT(in) ::   kt     ! ocean time-step index
      INTEGER, INTENT(in) ::   method ! advection method (1: 1st order upstream, 3: 3rd order TVD with QUICKEST limiter)

      INTEGER :: ji,jj,jk,jn,k_floor
      REAL(wp) :: zwgt_if(1:jpkm1-1), dc(1:jpkm1), w_if(1:jpkm1-1), z2dt, h(1:jpkm1)


      IF( neuler == 0 .AND. kt == nittrc000 ) THEN
         z2dt = rdt                  ! set time step size (Euler)
      ELSE
         z2dt = 2._wp * rdt          ! set time step size (Leapfrog)
      ENDIF

      ! Compute interior vertical velocities and include them in source array.
      DO jj=2,jpjm1 ! j-loop
         ! Get vertical velocities at layer centres (entire i-k slice).
         DO jk=1,jpkm1
            CALL model%get_vertical_movement(2,jpim1,jj,jk,w_ct(:,jk,:))
         END DO
         DO ji=2,jpim1 ! i-loop
            ! Only process this horizontal point (ji,jj) if number of layers exceeds 1
            k_floor = mbkt(ji,jj)
            IF (k_floor > 1) THEN
               ! Linearly interpolate to velocities at the interfaces between layers
               ! Note:
               !    - interface k sits between cell centre k and k+1 (k=0 for surface)
               !    - k [1,jpkm1] increases downwards
               !    - upward velocity is positive, downward velocity is negative
               h(1:k_floor) = e3t_n(ji,jj,1:k_floor)
               zwgt_if(1:k_floor-1) = h(2:k_floor) / (h(1:k_floor-1) + h(2:k_floor))

               ! Advect:
               DO jn=1,jp_fabm ! State loop
                  IF (ALL(w_ct(ji,1:k_floor,jn) == 0._wp)) CYCLE

                  ! Compute velocities at interfaces
                  w_if(1:k_floor-1) = zwgt_if(1:k_floor-1) * w_ct(ji,1:k_floor-1,jn) + (1._wp - zwgt_if(1:k_floor-1)) * w_ct(ji,2:k_floor,jn)

                  ! Compute change (per volume) due to vertical movement per layer
                  IF (method == 1) THEN
                     CALL advect_1(k_floor, trn(ji,jj,1:k_floor,jp_fabm_m1+jn), w_if(1:k_floor-1), h(1:k_floor), z2dt, dc(1:k_floor))
                  ELSE
                     CALL advect_3(k_floor, trb(ji,jj,1:k_floor,jp_fabm_m1+jn), w_if(1:k_floor-1), h(1:k_floor), z2dt, dc(1:k_floor))
                  END IF

                  ! Incorporate change due to vertical movement in sources-sinks
                  tra(ji,jj,1:k_floor,jp_fabm_m1+jn) = tra(ji,jj,1:k_floor,jp_fabm_m1+jn) + dc(1:k_floor)

               END DO ! State loop
            END IF ! Level check
         END DO ! i-loop
      END DO ! j-loop


   END SUBROUTINE compute_vertical_movement

   SUBROUTINE advect_1(nk, c, w, h, dt, trend)
      INTEGER,  INTENT(IN)  :: nk
      REAL(wp), INTENT(IN)  :: c(1:nk)
      REAL(wp), INTENT(IN)  :: w(1:nk-1)
      REAL(wp), INTENT(IN)  :: h(1:nk)
      REAL(wp), INTENT(IN)  :: dt
      REAL(wp), INTENT(OUT) :: trend(1:nk)

      REAL(wp) :: flux(0:nk)
      INTEGER  :: jk
      ! Compute fluxes (per surface area) over at interfaces (remember: positive for upwards)
      flux(0) = 0._wp
      DO jk=1,nk-1 ! k-loop
         IF (w(jk) > 0) THEN
            ! Upward movement (source layer is jk+1)
            flux(jk) = min(w(jk), h(jk+1)/dt) * c(jk+1)
         ELSE
            ! Downward movement (source layer is jk)
            flux(jk) = max(w(jk), -h(jk)/dt) * c(jk)
         END IF
      END DO
      flux(nk) = 0._wp
      trend = (flux(1:nk) - flux(0:nk-1)) / h
   END SUBROUTINE

   SUBROUTINE advect_3(nk, c_old, w, h, dt, trend)
      INTEGER,  INTENT(IN)  :: nk
      REAL(wp), INTENT(IN)  :: c_old(1:nk)
      REAL(wp), INTENT(IN)  :: w(1:nk-1)
      REAL(wp), INTENT(IN)  :: h(1:nk)
      REAL(wp), INTENT(IN)  :: dt
      REAL(wp), INTENT(OUT) :: trend(1:nk)

      INTEGER, PARAMETER :: n_itermax=100
      REAL(wp) :: cmax_no
      REAL(wp) :: cfl(1:nk-1)
      INTEGER  :: n_iter, n_count, jk
      REAL(wp) :: c(1:nk)
      REAL(wp) :: tr_u(1:nk-1)
      REAL(wp) :: tr_c(1:nk-1)
      REAL(wp) :: tr_d(1:nk-1)
      REAL(wp) :: delta_tr_u(1:nk-1)
      REAL(wp) :: delta_tr(1:nk-1)
      REAL(wp) :: ratio(1:nk-1)
      REAL(wp) :: x_fac(1:nk-1)
      REAL(wp) :: phi_lim(1:nk-1)
      REAL(wp) :: limiter(1:nk-1)
      REAL(wp) :: flux_if(1:nk-1)

      c(:) = c_old(:)

      ! get maximum Courant number:
      cfl = ABS(w) * dt / (0.5_wp * (h(2:nk) + h(1:nk-1)))
      cmax_no = MAXVAL(cfl)

      ! number of iterations:
      n_iter = MIN(n_itermax, INT(cmax_no) + 1)
      IF (ln_ctl.AND.(n_iter .gt. 1)) THEN
         WRITE(numout,*) 'compute_vertical_movement::advect_3():'
         WRITE(numout,*) '   Maximum Courant number is ',cmax_no,'.'
         WRITE(numout,*) '   ',n_iter,' iterations used for vertical advection.'
      ENDIF

      ! effective Courant number:
      cfl = cfl/n_iter

      DO n_count=1,n_iter ! Iterative loop
         ! Determine tracer concentration at 1.5 upstream (tr_u), 0.5 upstream (tr_c), 0.5 downstream (tr_d) from interface
         IF (nk.gt.2) THEN
            ! More than 2 vertical wet points
            IF (nk.gt.3) THEN
               WHERE (w(2:nk-2).ge.0._wp)
                  !upward movement
                  tr_u(2:nk-2)=c(4:nk)
                  tr_c(2:nk-2)=c(3:nk-1)
                  tr_d(2:nk-2)=c(2:nk-2)
               ELSEWHERE
                  ! downward movement
                  tr_u(2:nk-2)=c(1:nk-3)
                  tr_c(2:nk-2)=c(2:nk-2)
                  tr_d(2:nk-2)=c(3:nk-1)
               ENDWHERE
            ENDIF

            ! Interface between surface layer and the next
            IF (w(1).ge.0._wp) THEN
               ! upward movement
               tr_u(1)=c(3)
               tr_c(1)=c(2)
               tr_d(1)=c(1)
            ELSE
               ! downward movement
               tr_u(1)=c(1)
               tr_c(1)=c(1)
               tr_d(1)=c(2)
            ENDIF

            ! Interface between bottom layer and the previous
            IF (w(nk-1).ge.0._wp) THEN
               ! upward movement
               tr_u(nk-1)=c(nk)
               tr_c(nk-1)=c(nk)
               tr_d(nk-1)=c(nk-1)
            ELSE
               ! downward movement
               tr_u(nk-1)=c(nk-2)
               tr_c(nk-1)=c(nk-1)
               tr_d(nk-1)=c(nk)
            ENDIF
         ELSE
            ! only 2 vertical wet points, i.e. only 1 interface
            IF (w(1).ge.0._wp) THEN
               ! upward movement
               tr_u(1)=c(2)
               tr_c(1)=c(2)
               tr_d(1)=c(1)
            ELSE
               ! downward movement
               tr_u(1)=c(1)
               tr_c(1)=c(1)
               tr_d(1)=c(2)
            ENDIF
         ENDIF

         delta_tr_u = tr_c - tr_u
         delta_tr = tr_d - tr_c
         WHERE (delta_tr * delta_tr_u > 0._wp)
            ! Monotonic function over tr_u, tr_c, r_d

            ! Compute slope ratio
            ratio = delta_tr_u / delta_tr

            ! QUICKEST flux limiter
            x_fac = (1._wp - 2._wp * cfl) / 6._wp
            phi_lim = (0.5_wp + x_fac) + (0.5_wp - x_fac) * ratio
            limiter = MIN(phi_lim, 2._wp / (1._wp - cfl), 2._wp * ratio / (cfl + 1.e-10_wp))

            ! Compute limited flux
            flux_if = w * (tr_c + 0.5_wp * limiter * (1._wp - cfl) * delta_tr)
         ELSEWHERE
            ! Non-monotonic, use 1st order upstream
            flux_if = w * tr_c
         ENDWHERE

         ! Compute pseudo update for trend aggregation:
         c(1:nk-1) = c(1:nk-1) + dt / real(n_iter, wp) / h(1:nk-1) * flux_if
         c(2:nk)   = c(2:nk)   - dt / real(n_iter, wp) / h(2:nk)   * flux_if

      ENDDO ! Iterative loop

      ! Estimate rate of change from pseudo state updates (source splitting):
      trend = (c - c_old) / dt
   END SUBROUTINE

END MODULE
