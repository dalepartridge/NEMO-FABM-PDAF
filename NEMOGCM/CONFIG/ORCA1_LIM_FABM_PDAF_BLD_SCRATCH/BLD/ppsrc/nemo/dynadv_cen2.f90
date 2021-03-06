












MODULE dynadv_cen2
   !!======================================================================
   !!                       ***  MODULE  dynadv  ***
   !! Ocean dynamics: Update the momentum trend with the flux form advection
   !!                 using a 2nd order centred scheme
   !!======================================================================
   !! History :  2.0  ! 2006-08  (G. Madec, S. Theetten)  Original code
   !!            3.2  ! 2009-07  (R. Benshila)  Suppression of rigid-lid option
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   dyn_adv_cen2       : flux form momentum advection (ln_dynadv_cen2=T)
   !!                        trends using a 2nd order centred scheme  
   !!----------------------------------------------------------------------
   USE oce            ! ocean dynamics and tracers
   USE dom_oce        ! ocean space and time domain
   USE trd_oce        ! trends: ocean variables
   USE trddyn         ! trend manager: dynamics
   !
   USE in_out_manager ! I/O manager
   USE lib_mpp        ! MPP library
   USE prtctl         ! Print control
   USE wrk_nemo       ! Memory Allocation
   USE timing         ! Timing

   IMPLICIT NONE
   PRIVATE

   PUBLIC   dyn_adv_cen2   ! routine called by step.F90

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
   !! NEMO/OPA 4.0 , NEMO Consortium (2011)
   !! $Id$
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE dyn_adv_cen2( kt )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE dyn_adv_cen2  ***
      !!
      !! ** Purpose :   Compute the now momentum advection trend in flux form
      !!              and the general trend of the momentum equation.
      !!
      !! ** Method  :   Trend evaluated using now fields (centered in time) 
      !!
      !! ** Action  :   (ua,va) updated with the now vorticity term trend
      !!----------------------------------------------------------------------
      INTEGER, INTENT( in ) ::   kt   ! ocean time-step index
      !
      INTEGER  ::   ji, jj, jk   ! dummy loop indices
      REAL(wp) ::   zbu, zbv     ! local scalars
      REAL(wp), POINTER, DIMENSION(:,:,:) ::  zfu_t, zfv_t, zfu_f, zfv_f, zfu_uw, zfv_vw, zfw
      REAL(wp), POINTER, DIMENSION(:,:,:) ::  zfu, zfv
      !!----------------------------------------------------------------------
      !
      IF( nn_timing == 1 )  CALL timing_start('dyn_adv_cen2')
      !
      CALL wrk_alloc( jpi, jpj, jpk, zfu_t, zfv_t, zfu_f, zfv_f, zfu_uw, zfv_vw, zfu, zfv, zfw )
      !
      IF( kt == nit000 .AND. lwp ) THEN
         WRITE(numout,*)
         WRITE(numout,*) 'dyn_adv_cen2 : 2nd order flux form momentum advection'
         WRITE(numout,*) '~~~~~~~~~~~~'
      ENDIF
      !
      IF( l_trddyn ) THEN           ! Save ua and va trends
         zfu_uw(:,:,:) = ua(:,:,:)
         zfv_vw(:,:,:) = va(:,:,:)
      ENDIF

      !                                      ! ====================== !
      !                                      !  Horizontal advection  !
      DO jk = 1, jpkm1                       ! ====================== !
         !                                         ! horizontal volume fluxes
         zfu(:,:,jk) = 0.25 * e2u(:,:) * e3u_n(:,:,jk) * un(:,:,jk)
         zfv(:,:,jk) = 0.25 * e1v(:,:) * e3v_n(:,:,jk) * vn(:,:,jk)
         !
         DO jj = 1, jpjm1                          ! horizontal momentum fluxes at T- and F-point
            DO ji = 1, jpim1   ! vector opt.
               zfu_t(ji+1,jj  ,jk) = ( zfu(ji,jj,jk) + zfu(ji+1,jj  ,jk) ) * ( un(ji,jj,jk) + un(ji+1,jj  ,jk) )
               zfv_f(ji  ,jj  ,jk) = ( zfv(ji,jj,jk) + zfv(ji+1,jj  ,jk) ) * ( un(ji,jj,jk) + un(ji  ,jj+1,jk) )
               zfu_f(ji  ,jj  ,jk) = ( zfu(ji,jj,jk) + zfu(ji  ,jj+1,jk) ) * ( vn(ji,jj,jk) + vn(ji+1,jj  ,jk) )
               zfv_t(ji  ,jj+1,jk) = ( zfv(ji,jj,jk) + zfv(ji  ,jj+1,jk) ) * ( vn(ji,jj,jk) + vn(ji  ,jj+1,jk) )
            END DO
         END DO
         DO jj = 2, jpjm1                          ! divergence of horizontal momentum fluxes
            DO ji = 2, jpim1   ! vector opt.
               zbu = e1u(ji,jj) * e2u(ji,jj) * e3u_n(ji,jj,jk)
               zbv = e1v(ji,jj) * e2v(ji,jj) * e3v_n(ji,jj,jk)
               !
               ua(ji,jj,jk) = ua(ji,jj,jk) - (  zfu_t(ji+1,jj  ,jk) - zfu_t(ji  ,jj  ,jk)    &
                  &                           + zfv_f(ji  ,jj  ,jk) - zfv_f(ji  ,jj-1,jk)  ) / zbu
               va(ji,jj,jk) = va(ji,jj,jk) - (  zfu_f(ji  ,jj  ,jk) - zfu_f(ji-1,jj  ,jk)    &
                  &                           + zfv_t(ji  ,jj+1,jk) - zfv_t(ji  ,jj  ,jk)  ) / zbv
            END DO
         END DO
      END DO
      !
      IF( l_trddyn ) THEN                          ! save the horizontal advection trend for diagnostic
         zfu_uw(:,:,:) = ua(:,:,:) - zfu_uw(:,:,:)
         zfv_vw(:,:,:) = va(:,:,:) - zfv_vw(:,:,:)
         CALL trd_dyn( zfu_uw, zfv_vw, jpdyn_keg, kt )
         zfu_t(:,:,:) = ua(:,:,:)
         zfv_t(:,:,:) = va(:,:,:)
      ENDIF
      !

      !                                      ! ==================== !
      !                                      !  Vertical advection  !
      DO jk = 1, jpkm1                       ! ==================== !
         !                                         ! Vertical volume fluxes?
         zfw(:,:,jk) = 0.25 * e1t(:,:) * e2t(:,:) * wn(:,:,jk)
         !
         IF( jk == 1 ) THEN                        ! surface/bottom advective fluxes                   
            zfu_uw(:,:,jpk) = 0.e0                      ! Bottom  value : flux set to zero
            zfv_vw(:,:,jpk) = 0.e0
            !                                           ! Surface value :
            IF( lk_vvl ) THEN                                ! variable volume : flux set to zero
               zfu_uw(:,:, 1 ) = 0.e0    
               zfv_vw(:,:, 1 ) = 0.e0
            ELSE                                             ! constant volume : advection through the surface
               DO jj = 2, jpjm1
                  DO ji = 2, jpim1
                     zfu_uw(ji,jj, 1 ) = 2.e0 * ( zfw(ji,jj,1) + zfw(ji+1,jj  ,1) ) * un(ji,jj,1)
                     zfv_vw(ji,jj, 1 ) = 2.e0 * ( zfw(ji,jj,1) + zfw(ji  ,jj+1,1) ) * vn(ji,jj,1)
                  END DO
               END DO
            ENDIF
         ELSE                                      ! interior fluxes
            DO jj = 2, jpjm1
               DO ji = 2, jpim1   ! vector opt.
                  zfu_uw(ji,jj,jk) = ( zfw(ji,jj,jk)+ zfw(ji+1,jj  ,jk) ) * ( un(ji,jj,jk) + un(ji,jj,jk-1) )
                  zfv_vw(ji,jj,jk) = ( zfw(ji,jj,jk)+ zfw(ji  ,jj+1,jk) ) * ( vn(ji,jj,jk) + vn(ji,jj,jk-1) )
               END DO
            END DO
         ENDIF
      END DO
      DO jk = 1, jpkm1                             ! divergence of vertical momentum flux divergence
         DO jj = 2, jpjm1 
            DO ji = 2, jpim1   ! vector opt.
               ua(ji,jj,jk) =  ua(ji,jj,jk) - ( zfu_uw(ji,jj,jk) - zfu_uw(ji,jj,jk+1) )    &
                  &  / ( e1u(ji,jj) * e2u(ji,jj) * e3u_n(ji,jj,jk) )
               va(ji,jj,jk) =  va(ji,jj,jk) - ( zfv_vw(ji,jj,jk) - zfv_vw(ji,jj,jk+1) )    &
                  &  / ( e1v(ji,jj) * e2v(ji,jj) * e3v_n(ji,jj,jk) )
            END DO
         END DO
      END DO
      !
      IF( l_trddyn ) THEN                          ! save the vertical advection trend for diagnostic
         zfu_t(:,:,:) = ua(:,:,:) - zfu_t(:,:,:)
         zfv_t(:,:,:) = va(:,:,:) - zfv_t(:,:,:)
         CALL trd_dyn( zfu_t, zfv_t, jpdyn_zad, kt )
      ENDIF
      !                                            ! Control print
      IF(ln_ctl)   CALL prt_ctl( tab3d_1=ua, clinfo1=' cen2 adv - Ua: ', mask1=umask,   &
         &                       tab3d_2=va, clinfo2=           ' Va: ', mask2=vmask, clinfo3='dyn' )
      !
      CALL wrk_dealloc( jpi, jpj, jpk, zfu_t, zfv_t, zfu_f, zfv_f, zfu_uw, zfv_vw, zfu, zfv, zfw )
      !
      IF( nn_timing == 1 )  CALL timing_stop('dyn_adv_cen2')
      !
   END SUBROUTINE dyn_adv_cen2

   !!==============================================================================
END MODULE dynadv_cen2
