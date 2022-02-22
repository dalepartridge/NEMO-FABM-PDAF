












MODULE trcbbl
  !!======================================================================
   !!                       ***  MODULE  trcbbl  ***
   !! Ocean passive tracers physics :  advective and/or diffusive bottom boundary 
   !!                                  layer scheme
   !!======================================================================
   !!==============================================================================
   !! History :  OPA  !  1996-06  (L. Mortier)  Original code
   !!            8.0  !  1997-11  (G. Madec)    Optimization
   !!   NEMO     1.0  !  2002-08  (G. Madec)  free form + modules
   !!             -   !  2004-01  (A. de Miranda, G. Madec, J.M. Molines ) add advective bbl
   !!            3.3  !  2009-11  (G. Madec)  merge trabbl and trabbl_adv + style + optimization 
   !!             -   !  2010-04  (G. Madec)  Campin & Goosse advective bbl 
   !!             -   !  2010-06  (C. Ethe, G. Madec)  merge TRA-TRC
   !!----------------------------------------------------------------------
   !!----------------------------------------------------------------------
   !!   'key_trabbl                      diffusive or/and adevective bottom boundary layer
   !!----------------------------------------------------------------------
   !!    trc_bbl       : update the tracer trends due to the bottom boundary layer (advective and/or diffusive)
   !!----------------------------------------------------------------------
   USE oce_trc             ! ocean dynamics and active tracers variables
   USE trc                 ! ocean passive tracers variables
   USE trcnam_trp      ! passive tracers transport namelist variables
   USE trabbl              ! 
   USE prtctl_trc          ! Print control for debbuging
   USE trd_oce
   USE trdtra

   PUBLIC   trc_bbl       !  routine called by step.F90


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


   SUBROUTINE trc_bbl( kt )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE bbl  ***
      !!                   
      !! ** Purpose :   Compute the before tracer (t & s) trend associated 
      !!     with the bottom boundary layer and add it to the general trend
      !!     of tracer equations.
      !!
      !!----------------------------------------------------------------------  
      IMPLICIT NONE
      INTEGER, INTENT( in ) ::   kt   ! ocean time-step 
      CHARACTER (len=22) :: charout
      REAL(wp), ALLOCATABLE, DIMENSION(:,:,:,:) ::   ztrtrd
      INTEGER :: jn                   ! Local loop index
      !!----------------------------------------------------------------------
      !
      IF( nn_timing == 1 )  CALL timing_start('trc_bbl')
      !
      IF( .NOT. lk_offline .AND. nn_dttrc == 1 ) THEN
         CALL bbl( kt, nittrc000, 'TRC' )      ! Online coupling with dynamics  : Computation of bbl coef and bbl transport
         l_bbl = .FALSE.                       ! Offline coupling with dynamics : Read bbl coef and bbl transport from input files
      ENDIF

      IF( l_trdtrc )  THEN
         ALLOCATE(ztrtrd( 1:jpi, 1:jpj, 1:jpk, 1:jptra )) ! temporary save of trends
         ztrtrd(:,:,:,:)  = tra(:,:,:,:)
      ENDIF

      !* Diffusive bbl :
      IF( nn_bbl_ldf == 1 ) THEN
         !
         CALL tra_bbl_dif( trb, tra, jptra )  
         IF( ln_ctl )   THEN
            WRITE(charout, FMT="(' bbl_dif')")  ;  CALL prt_ctl_trc_info(charout)
            CALL prt_ctl_trc( tab4d=tra, mask=tmask, clinfo=ctrcnm, clinfo2='trd' )
         ENDIF
         !
      END IF

      !* Advective bbl : bbl upstream advective trends added to the tracer trends
      IF( nn_bbl_adv /= 0 ) THEN
         !
         CALL tra_bbl_adv( trb, tra, jptra )  
         IF( ln_ctl )   THEN
            WRITE(charout, FMT="(' bbl_adv')")  ;  CALL prt_ctl_trc_info(charout)
            CALL prt_ctl_trc( tab4d=tra, mask=tmask, clinfo=ctrcnm, clinfo2='trd' )
         ENDIF
         !
      END IF

      IF( l_trdtrc )   THEN                      ! save the horizontal diffusive trends for further diagnostics
        DO jn = 1, jptra
           ztrtrd(:,:,:,jn) = tra(:,:,:,jn) - ztrtrd(:,:,:,jn)
           CALL trd_tra( kt, 'TRC', jn, jptra_bbl, ztrtrd(:,:,:,jn) )
        END DO
        DEALLOCATE(ztrtrd ) ! temporary save of trends
      ENDIF
      !
      IF( nn_timing == 1 ) CALL timing_stop('trc_bbl')
      !
   END SUBROUTINE trc_bbl


   !!======================================================================
END MODULE trcbbl
