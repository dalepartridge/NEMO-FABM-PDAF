












MODULE dynldf_bilap
   !!======================================================================
   !!                     ***  MODULE  dynldf_bilap  ***
   !! Ocean dynamics:  lateral viscosity trend
   !!======================================================================
   !! History :  OPA  ! 1990-09  (G. Madec)  Original code
   !!            4.0  ! 1993-03  (M. Guyon)  symetrical conditions (M. Guyon)
   !!            6.0  ! 1996-01  (G. Madec)  statement function for e3
   !!            8.0  ! 1997-07  (G. Madec)  lbc calls
   !!   NEMO     1.0  ! 2002-08  (G. Madec)  F90: Free form and module
   !!            2.0  ! 2004-08  (C. Talandier) New trends organization
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   dyn_ldf_bilap : update the momentum trend with the lateral diffusion
   !!                   using an iso-level bilaplacian operator
   !!----------------------------------------------------------------------
   USE oce             ! ocean dynamics and tracers
   USE dom_oce         ! ocean space and time domain
   USE ldfdyn_oce      ! ocean dynamics: lateral physics
   !
   USE in_out_manager  ! I/O manager
   USE lbclnk          ! ocean lateral boundary conditions (or mpp link)
   USE wrk_nemo        ! Memory Allocation
   USE timing          ! Timing

   IMPLICIT NONE
   PRIVATE

   PUBLIC   dyn_ldf_bilap   ! called by step.F90

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
   !!                    ***  ldfdyn_substitute.h90  ***
   !!----------------------------------------------------------------------
   !! ** purpose :   substitute fsahm., the lateral eddy viscosity coeff. 
   !!      with a constant, or 1D, or 2D or 3D array, using CPP macro.
   !!----------------------------------------------------------------------
   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.3 , NEMO Consortium (2010)
   !! $Id$
   !! Software governed by the CeCILL licence (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
   !!
   !! fsahmt, fsahmf - used for laplaian operator only
   !! fsahmu, fsahmv - used for bilaplacian operator only
   !!
!   ' key_dynldf_c3d' :                  3D coefficient
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

   SUBROUTINE dyn_ldf_bilap( kt )
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE dyn_ldf_bilap  ***
      !!
      !! ** Purpose :   Compute the before trend of the lateral momentum
      !!      diffusion and add it to the general trend of momentum equation.
      !!
      !! ** Method  :   The before horizontal momentum diffusion trend is a 
      !!      bi-harmonic operator (bilaplacian type) which separates the
      !!      divergent and rotational parts of the flow.
      !!      Its horizontal components are computed as follow:
      !!      laplacian:
      !!          zlu = 1/e1u di[ hdivb ] - 1/(e2u*e3u) dj-1[ e3f rotb ]
      !!          zlv = 1/e2v dj[ hdivb ] + 1/(e1v*e3v) di-1[ e3f rotb ]
      !!      third derivative:
      !!       * multiply by the eddy viscosity coef. at u-, v-point, resp.
      !!          zlu = ahmu * zlu
      !!          zlv = ahmv * zlv
      !!       * curl and divergence of the laplacian
      !!          zuf = 1/(e1f*e2f) ( di[e2v zlv] - dj[e1u zlu] )
      !!          zut = 1/(e1t*e2t*e3t) ( di[e2u*e3u zlu] + dj[e1v*e3v zlv] )
      !!      bilaplacian:
      !!              diffu = 1/e1u di[ zut ] - 1/(e2u*e3u) dj-1[ e3f zuf ]
      !!              diffv = 1/e2v dj[ zut ] + 1/(e1v*e3v) di-1[ e3f zuf ]
      !!      If ln_sco=F and ln_zps=F, the vertical scale factors in the
      !!      rotational part of the diffusion are simplified
      !!      Add this before trend to the general trend (ua,va):
      !!            (ua,va) = (ua,va) + (diffu,diffv)
      !!
      !! ** Action : - Update (ua,va) with the before iso-level biharmonic
      !!               mixing trend.
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt   ! ocean time-step index
      !
      INTEGER  ::   ji, jj, jk                  ! dummy loop indices
      REAL(wp) ::   zua, zva, zbt, ze2u, ze2v   ! temporary scalar
      REAL(wp), POINTER, DIMENSION(:,:  ) :: zcu, zcv
      REAL(wp), POINTER, DIMENSION(:,:,:) :: zuf, zut, zlu, zlv
      !!----------------------------------------------------------------------
      !
      IF( nn_timing == 1 )  CALL timing_start('dyn_ldf_bilap')
      !
      CALL wrk_alloc( jpi, jpj,      zcu, zcv           )
      CALL wrk_alloc( jpi, jpj, jpk, zuf, zut, zlu, zlv ) 
      !
      IF( kt == nit000 .AND. lwp ) THEN
         WRITE(numout,*)
         WRITE(numout,*) 'dyn_ldf_bilap : iso-level bilaplacian operator'
         WRITE(numout,*) '~~~~~~~~~~~~~'
      ENDIF

!!bug gm this should be enough
!!$      zuf(:,:,jpk) = 0.e0
!!$      zut(:,:,jpk) = 0.e0
!!$      zlu(:,:,jpk) = 0.e0
!!$      zlv(:,:,jpk) = 0.e0
      zuf(:,:,:) = 0._wp
      zut(:,:,:) = 0._wp
      zlu(:,:,:) = 0._wp
      zlv(:,:,:) = 0._wp

      !                                                ! ===============
      DO jk = 1, jpkm1                                 ! Horizontal slab
         !                                             ! ===============
         ! Laplacian
         ! ---------

         IF( ln_sco .OR. ln_zps ) THEN   ! s-coordinate or z-coordinate with partial steps
            zuf(:,:,jk) = rotb(:,:,jk) * e3f_n(:,:,jk)
            DO jj = 2, jpjm1
               DO ji = 2, jpim1   ! vector opt.
                  zlu(ji,jj,jk) = - ( zuf(ji,jj,jk) - zuf(ji,jj-1,jk) ) / ( e2u(ji,jj) * e3u_n(ji,jj,jk) )   &
                     &         + ( hdivb(ji+1,jj,jk) - hdivb(ji,jj,jk) ) / e1u(ji,jj)
   
                  zlv(ji,jj,jk) = + ( zuf(ji,jj,jk) - zuf(ji-1,jj,jk) ) / ( e1v(ji,jj) * e3v_n(ji,jj,jk) )   &
                     &         + ( hdivb(ji,jj+1,jk) - hdivb(ji,jj,jk) ) / e2v(ji,jj)
               END DO
            END DO
         ELSE                            ! z-coordinate - full step
            DO jj = 2, jpjm1
               DO ji = 2, jpim1   ! vector opt.
                  zlu(ji,jj,jk) = - ( rotb (ji  ,jj,jk) - rotb (ji,jj-1,jk) ) / e2u(ji,jj)   &
                     &         + ( hdivb(ji+1,jj,jk) - hdivb(ji,jj  ,jk) ) / e1u(ji,jj)
   
                  zlv(ji,jj,jk) = + ( rotb (ji,jj  ,jk) - rotb (ji-1,jj,jk) ) / e1v(ji,jj)   &
                     &         + ( hdivb(ji,jj+1,jk) - hdivb(ji  ,jj,jk) ) / e2v(ji,jj)
               END DO  
            END DO  
         ENDIF
      END DO
      CALL lbc_lnk( zlu, 'U', -1. )   ;   CALL lbc_lnk( zlv, 'V', -1. )   ! Boundary conditions

         
      DO jk = 1, jpkm1
   
         ! Third derivative
         ! ----------------
         
         ! Multiply by the eddy viscosity coef. (at u- and v-points)
         zlu(:,:,jk) = zlu(:,:,jk) * ( ahm3(:,:,jk) * (1-nkahm_smag) + nkahm_smag)

         zlv(:,:,jk) = zlv(:,:,jk) * ( ahm4(:,:,jk) * (1-nkahm_smag) + nkahm_smag)
         
         ! Contravariant "laplacian"
         zcu(:,:) = e1u(:,:) * zlu(:,:,jk)
         zcv(:,:) = e2v(:,:) * zlv(:,:,jk)
         
         ! Laplacian curl ( * e3f if s-coordinates or z-coordinate with partial steps)
         DO jj = 1, jpjm1
            DO ji = 1, jpim1   ! vector opt.
               zuf(ji,jj,jk) = fmask(ji,jj,jk) * (  zcv(ji+1,jj  ) - zcv(ji,jj)      &
                  &                            - zcu(ji  ,jj+1) + zcu(ji,jj)  )   &
                  &       * e3f_n(ji,jj,jk) / ( e1f(ji,jj)*e2f(ji,jj) )
            END DO  
         END DO  

         ! Laplacian Horizontal fluxes
         DO jj = 1, jpjm1
            DO ji = 1, jpim1   ! vector opt.
               zlu(ji,jj,jk) = e2u(ji,jj) * e3u_n(ji,jj,jk) * zlu(ji,jj,jk)
               zlv(ji,jj,jk) = e1v(ji,jj) * e3v_n(ji,jj,jk) * zlv(ji,jj,jk)
            END DO
         END DO

         ! Laplacian divergence
         DO jj = 2, jpj
            DO ji = 2, jpi   ! vector opt.
               zbt = e1t(ji,jj) * e2t(ji,jj) * e3t_n(ji,jj,jk)
               zut(ji,jj,jk) = (  zlu(ji,jj,jk) - zlu(ji-1,jj  ,jk)   &
                  &             + zlv(ji,jj,jk) - zlv(ji  ,jj-1,jk) ) / zbt
            END DO
         END DO
      END DO


      ! boundary conditions on the laplacian curl and div (zuf,zut)
!!bug gm no need to do this 2 following lbc...
      CALL lbc_lnk( zuf, 'F', 1. )
      CALL lbc_lnk( zut, 'T', 1. )

      DO jk = 1, jpkm1      
   
         ! Bilaplacian
         ! -----------

         DO jj = 2, jpjm1
            DO ji = 2, jpim1   ! vector opt.
               ze2u = e2u(ji,jj) * e3u_n(ji,jj,jk)
               ze2v = e1v(ji,jj) * e3v_n(ji,jj,jk)
               ! horizontal biharmonic diffusive trends
               zua = - ( zuf(ji  ,jj,jk) - zuf(ji,jj-1,jk) ) / ze2u   &
                  &  + ( zut(ji+1,jj,jk) - zut(ji,jj  ,jk) ) / e1u(ji,jj)

               zva = + ( zuf(ji,jj  ,jk) - zuf(ji-1,jj,jk) ) / ze2v   &
                  &  + ( zut(ji,jj+1,jk) - zut(ji  ,jj,jk) ) / e2v(ji,jj)
               ! add it to the general momentum trends
               ua(ji,jj,jk) = ua(ji,jj,jk) + zua * ( ahm3(ji,jj,jk)*nkahm_smag +(1 -nkahm_smag ))
               va(ji,jj,jk) = va(ji,jj,jk) + zva * ( ahm4(ji,jj,jk)*nkahm_smag +(1 -nkahm_smag ))
            END DO
         END DO

         !                                             ! ===============
      END DO                                           !   End of slab
      !                                                ! ===============
      CALL wrk_dealloc( jpi, jpj,      zcu, zcv           )
      CALL wrk_dealloc( jpi, jpj, jpk, zuf, zut, zlu, zlv ) 
      !
      IF( nn_timing == 1 )  CALL timing_stop('dyn_ldf_bilap')
      !
   END SUBROUTINE dyn_ldf_bilap

   !!======================================================================
END MODULE dynldf_bilap
