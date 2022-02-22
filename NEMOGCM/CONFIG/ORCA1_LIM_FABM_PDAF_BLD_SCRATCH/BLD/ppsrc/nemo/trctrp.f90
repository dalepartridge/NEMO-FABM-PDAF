












MODULE trctrp
   !!======================================================================
   !!                       ***  MODULE trctrp  ***
   !! Ocean Physics    : manage the passive tracer transport
   !!======================================================================
   !! History :   1.0  !  2004-03 (C. Ethe) Original code
   !!             3.3  !  2010-07 (C. Ethe) Merge TRA-TRC
   !!----------------------------------------------------------------------
   !!----------------------------------------------------------------------
   !!   'key_top'                                                TOP models
   !!----------------------------------------------------------------------
   !!   trc_trp        : passive tracer transport
   !!----------------------------------------------------------------------
   USE oce_trc         ! ocean dynamics and active tracers variables
   USE trc             ! ocean passive tracers variables 
   USE trcnam_trp      ! passive tracers transport namelist variables
   USE trabbl          ! bottom boundary layer               (trc_bbl routine)
   USE trcbbl          ! bottom boundary layer               (trc_bbl routine)
   USE zdfkpp          ! KPP non-local tracer fluxes         (trc_kpp routine)
   USE trcdmp          ! internal damping                    (trc_dmp routine)
   USE trcldf          ! lateral mixing                      (trc_ldf routine)
   USE trcadv          ! advection                           (trc_adv routine)
   USE trczdf          ! vertical diffusion                  (trc_zdf routine)
   USE trcnxt          ! time-stepping                       (trc_nxt routine)
   USE trcrad          ! positivity                          (trc_rad routine)
   USE trcsbc          ! surface boundary condition          (trc_sbc routine)
   USE zpshde          ! partial step: hor. derivative       (zps_hde routine)



   IMPLICIT NONE
   PRIVATE

   PUBLIC   trc_trp    ! called by trc_stp

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
   !!----------------------------------------------------------------------
   !! NEMO/TOP 3.3 , NEMO Consortium (2010)
   !! $Id$
   !! Software governed by the CeCILL licence (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------

CONTAINS

   SUBROUTINE trc_trp( kstp )
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE trc_trp  ***
      !!                      
      !! ** Purpose :   Management of passive tracers transport
      !! 
      !! ** Method  : - Compute the passive tracers trends 
      !!              - Update the passive tracers
      !!----------------------------------------------------------------------
      INTEGER, INTENT( in ) ::  kstp  ! ocean time-step index
      !! ---------------------------------------------------------------------
      !
      IF( nn_timing == 1 )   CALL timing_start('trc_trp')
      !
      IF( .NOT. lk_c1d ) THEN
         !
                                CALL trc_sbc( kstp )            ! surface boundary condition
         IF( lk_trabbl )        CALL trc_bbl( kstp )            ! advective (and/or diffusive) bottom boundary layer scheme
         IF( ln_trcdmp )        CALL trc_dmp( kstp )            ! internal damping trends
                                CALL trc_adv( kstp )            ! horizontal & vertical advection 
         IF( ln_zps ) THEN
           IF( ln_isfcav ) THEN ; CALL zps_hde_isf( kstp, jptra, trb, pgtu=gtru, pgtv=gtrv, pgtui=gtrui, pgtvi=gtrvi )  ! both top & bottom
           ELSE                 ; CALL zps_hde    ( kstp, jptra, trb, gtru, gtrv )                                      !  only bottom
           ENDIF
         ENDIF
                                CALL trc_ldf( kstp )            ! lateral mixing
         IF( .NOT. lk_offline .AND. lk_zdfkpp )    &
            &                   CALL trc_kpp( kstp )            ! KPP non-local tracer fluxes
                                CALL trc_zdf( kstp )            ! vertical mixing and after tracer fields
                                CALL trc_nxt( kstp )            ! tracer fields at next time step     
         IF( ln_trcrad )        CALL trc_rad( kstp )            ! Correct artificial negative concentrations
         IF( ln_trcdmp_clo )    CALL trc_dmp_clo( kstp )        ! internal damping trends on closed seas only


      ELSE                                               ! 1D vertical configuration
                                CALL trc_sbc( kstp )            ! surface boundary condition
         IF( .NOT. lk_offline .AND. lk_zdfkpp )    &
            &                   CALL trc_kpp( kstp )            ! KPP non-local tracer fluxes
                                CALL trc_zdf( kstp )            ! vertical mixing and after tracer fields
                                CALL trc_nxt( kstp )            ! tracer fields at next time step     
          IF( ln_trcrad )       CALL trc_rad( kstp )            ! Correct artificial negative concentrations
         !
      END IF
      !
      IF( nn_timing == 1 )   CALL timing_stop('trc_trp')
      !
9400  FORMAT(a25,i4,D23.16)
      !
   END SUBROUTINE trc_trp

   
   !!======================================================================
END MODULE trctrp
