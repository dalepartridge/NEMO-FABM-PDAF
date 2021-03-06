












MODULE trcnxt
   !!======================================================================
   !!                       ***  MODULE  trcnxt  ***
   !! Ocean passive tracers:  time stepping on passives tracers
   !!======================================================================
   !! History :  7.0  !  1991-11  (G. Madec)  Original code
   !!                 !  1993-03  (M. Guyon)  symetrical conditions
   !!                 !  1995-02  (M. Levy)   passive tracers
   !!                 !  1996-02  (G. Madec & M. Imbard)  opa release 8.0
   !!            8.0  !  1996-04  (A. Weaver)  Euler forward step
   !!            8.2  !  1999-02  (G. Madec, N. Grima)  semi-implicit pressure grad.
   !!  NEMO      1.0  !  2002-08  (G. Madec)  F90: Free form and module
   !!                 !  2002-08  (G. Madec)  F90: Free form and module
   !!                 !  2002-11  (C. Talandier, A-M Treguier) Open boundaries
   !!                 !  2004-03  (C. Ethe) passive tracers
   !!                 !  2007-02  (C. Deltel) Diagnose ML trends for passive tracers
   !!            2.0  !  2006-02  (L. Debreu, C. Mazauric) Agrif implementation
   !!            3.0  !  2008-06  (G. Madec)  time stepping always done in trazdf
   !!            3.1  !  2009-02  (G. Madec, R. Benshila)  re-introduce the vvl option
   !!            3.3  !  2010-06  (C. Ethe, G. Madec) Merge TRA-TRC
   !!----------------------------------------------------------------------
   !!----------------------------------------------------------------------
   !!   'key_top'                                                TOP models
   !!----------------------------------------------------------------------
   !!   trc_nxt     : time stepping on passive tracers
   !!----------------------------------------------------------------------
   USE oce_trc         ! ocean dynamics and tracers variables
   USE domvvl          ! variable volume  
   USE trc             ! ocean passive tracers variables
   USE lbclnk          ! ocean lateral boundary conditions (or mpp link)
   USE prtctl_trc      ! Print control for debbuging
   USE trcnam_trp      ! passive tracers transport namelist variables
   USE trd_oce
   USE trdtra
   USE tranxt

   IMPLICIT NONE
   PRIVATE

   PUBLIC   trc_nxt          ! routine called by step.F90
   PUBLIC   trc_nxt_alloc    ! routine called by nemogcm.F90

   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:) ::   r2dt

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
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   INTEGER FUNCTION trc_nxt_alloc()
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE trc_nxt_alloc  ***
      !!----------------------------------------------------------------------
      ALLOCATE( r2dt(jpk), STAT=trc_nxt_alloc )
      !
      IF( trc_nxt_alloc /= 0 )   CALL ctl_warn('trc_nxt_alloc : failed to allocate array')
      !
   END FUNCTION trc_nxt_alloc


   SUBROUTINE trc_nxt( kt )
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE trcnxt  ***
      !!
      !! ** Purpose :   Compute the passive tracers fields at the 
      !!      next time-step from their temporal trends and swap the fields.
      !! 
      !! ** Method  :   Apply lateral boundary conditions on (ua,va) through 
      !!      call to lbc_lnk routine
      !!   default:
      !!      arrays swap
      !!         (trn) = (tra) ; (tra) = (0,0)
      !!         (trb) = (trn) 
      !!
      !!   For Arakawa or TVD Scheme : 
      !!      A Asselin time filter applied on now tracers (trn) to avoid
      !!      the divergence of two consecutive time-steps and tr arrays
      !!      to prepare the next time_step:
      !!         (trb) = (trn) + atfp [ (trb) + (tra) - 2 (trn) ]
      !!         (trn) = (tra) ; (tra) = (0,0)
      !!
      !!
      !! ** Action  : - update trb, trn
      !!----------------------------------------------------------------------
      INTEGER, INTENT( in ) ::   kt     ! ocean time-step index
      !
      INTEGER  ::   jk, jn   ! dummy loop indices
      REAL(wp) ::   zfact            ! temporary scalar
      CHARACTER (len=22) :: charout
      REAL(wp), POINTER, DIMENSION(:,:,:,:) ::  ztrdt 
      !!----------------------------------------------------------------------
      !
      IF( nn_timing == 1 )  CALL timing_start('trc_nxt')
      !
      IF( kt == nittrc000 .AND. lwp ) THEN
         WRITE(numout,*)
         WRITE(numout,*) 'trc_nxt : time stepping on passive tracers'
      ENDIF

      ! Update after tracer on domain lateral boundaries
      DO jn = 1, jptra
         CALL lbc_lnk( tra(:,:,:,jn), 'T', 1. )   
      END DO




      ! set time step size (Euler/Leapfrog)
      IF( neuler == 0 .AND. kt ==  nittrc000 ) THEN  ;  r2dt(:) =     rdttrc(:)   ! at nittrc000             (Euler)
      ELSEIF( kt <= nittrc000 + nn_dttrc )     THEN  ;  r2dt(:) = 2.* rdttrc(:)   ! at nit000 or nit000+1 (Leapfrog)
      ENDIF

      ! trends computation initialisation
      IF( l_trdtrc )  THEN
         CALL wrk_alloc( jpi, jpj, jpk, jptra, ztrdt )  !* store now fields before applying the Asselin filter
         ztrdt(:,:,jpk,:) = 0._wp
         IF( ln_trcldf_iso ) THEN                       ! diagnose the "pure" Kz diffusive trend 
            DO jn = 1, jptra
               CALL trd_tra( kt, 'TRC', jn, jptra_zdfp, ztrdt(:,:,:,jn) )
            ENDDO
         ENDIF
         ! total trend for the non-time-filtered variables.
         ! G Nurser 23 Mar 2017. Recalculate trend as Delta(e3t*T)/e3tn; e3tn
         ! cancel from tsn terms
         IF( lk_vvl ) THEN
            DO jn = 1, jptra
               DO jk = 1, jpkm1
                  zfact = 1.0 / rdttrc(jk)
                  ztrdt(:,:,jk,jn) = ( tra(:,:,jk,jn)*e3t_a(:,:,jk) / e3t_n(:,:,jk) - &
                                       trn(:,:,jk,jn) ) * zfact
               END DO
            END DO
         ELSE
            DO jn = 1, jptra
               DO jk = 1, jpkm1
                  zfact = 1.0 / rdttrc(jk)
                  ztrdt(:,:,jk,jn) = ( tra(:,:,jk,jn) - trn(:,:,jk,jn) ) * zfact
               END DO
            END DO
         END IF
         DO jn = 1, jptra
            CALL trd_tra( kt, 'TRC', jn, jptra_tot, ztrdt(:,:,:,jn) )
         ENDDO
         IF( .NOT.lk_vvl )  THEN
            ! Store now fields before applying the Asselin filter 
            ! in order to calculate Asselin filter trend later.
            ztrdt(:,:,:,:)  = trn(:,:,:,:)
         ENDIF
      ENDIF
      ! Leap-Frog + Asselin filter time stepping
      IF( neuler == 0 .AND. kt == nittrc000 ) THEN        ! Euler time-stepping at first time-step
         !                                                ! (only swap)
         DO jn = 1, jptra
            DO jk = 1, jpkm1
               trn(:,:,jk,jn) = tra(:,:,jk,jn)
            END DO
         END DO
         IF (l_trdtrc.AND.lk_vvl) THEN      ! Zero Asselin filter contribution
                                            ! must be explicitly written out since for vvl
                                            ! Asselin filter is output by
                                            ! tra_nxt_vvl that is not called on
                                            ! this time step
            ztrdt(:,:,:,:) = 0._wp
            DO jn = 1, jptra
               CALL trd_tra( kt, 'TRC', jn, jptra_atf, ztrdt(:,:,:,jn) )
            ENDDO
         END IF

         !                                              
      ELSE
         ! Leap-Frog + Asselin filter time stepping
         IF( lk_vvl ) THEN   ;   CALL tra_nxt_vvl( kt, nittrc000, rdttrc, 'TRC', trb, trn, tra,      &
           &                                                                sbc_trc, sbc_trc_b, jptra )      ! variable volume level (vvl) 
         ELSE                ;   CALL tra_nxt_fix( kt, nittrc000,         'TRC', trb, trn, tra, jptra )      ! fixed    volume level 
         ENDIF
      ENDIF

      ! trends computation
      IF( l_trdtrc.AND..NOT.lk_vvl) THEN                                      ! trends
         DO jn = 1, jptra
            DO jk = 1, jpkm1
               zfact = 1.e0 / r2dt(jk)  
               ztrdt(:,:,jk,jn) = ( trb(:,:,jk,jn) - ztrdt(:,:,jk,jn) ) * zfact 
            END DO
            CALL trd_tra( kt, 'TRC', jn, jptra_atf, ztrdt(:,:,:,jn) )
         END DO
      END IF
      !
      IF( l_trdtrc)  CALL wrk_dealloc( jpi, jpj, jpk, jptra, ztrdt ) 
      !
      IF(ln_ctl)   THEN  ! print mean trends (used for debugging)
         WRITE(charout, FMT="('nxt')")
         CALL prt_ctl_trc_info(charout)
         CALL prt_ctl_trc(tab4d=trn, mask=tmask, clinfo=ctrcnm)
      ENDIF
      !
      IF( nn_timing == 1 )  CALL timing_stop('trc_nxt')
      !
   END SUBROUTINE trc_nxt

   !!======================================================================
END MODULE trcnxt
