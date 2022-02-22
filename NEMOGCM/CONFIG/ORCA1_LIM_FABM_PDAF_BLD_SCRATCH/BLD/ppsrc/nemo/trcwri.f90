












MODULE trcwri
   !!======================================================================
   !!                       *** MODULE trcwri ***
   !!    TOP :   Output of passive tracers
   !!======================================================================
   !! History :   1.0  !  2009-05 (C. Ethe)  Original code
   !!              -   !  2014-06 (A. Yool, J. Palmieri) adding MEDUSA-2
   !!----------------------------------------------------------------------
   !!----------------------------------------------------------------------
   !!   'key_top'                                           TOP models
   !!----------------------------------------------------------------------
   !! trc_wri_trc   :  outputs of concentration fields
   !!----------------------------------------------------------------------
   USE dom_oce     ! ocean space and time domain variables
   USE oce_trc     ! shared variables between ocean and passive tracers
   USE trc         ! passive tracers common variables 
   USE iom         ! I/O manager
   USE dianam      ! Output file name
   USE trcwri_pisces
   USE trcwri_cfc
   USE trcwri_c14b
   USE trcwri_age
   USE trcwri_my_trc
   USE trcwri_idtra
   USE trcwri_medusa
   ! +++>>> FABM
   USE trcwri_fabm
   ! FABM <<<+++

   IMPLICIT NONE
   PRIVATE

   PUBLIC trc_wri      

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

   SUBROUTINE trc_wri( kt )
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE trc_wri  ***
      !! 
      !! ** Purpose :   output passive tracers fields and dynamical trends
      !!---------------------------------------------------------------------
      INTEGER, INTENT( in )     :: kt
      !
      INTEGER                   :: jn
      CHARACTER (len=20)        :: cltra
      CHARACTER (len=40)        :: clhstnam
      INTEGER ::   inum = 11            ! temporary logical unit
      !!---------------------------------------------------------------------
      !
      IF( nn_timing == 1 )  CALL timing_start('trc_wri')
      !
      IF( lk_offline .AND. kt == nittrc000 .AND. lwp ) THEN    ! WRITE root name in date.file for use by postpro
         CALL dia_nam( clhstnam, nn_writetrc,' ' )
         CALL ctl_opn( inum, 'date.file', 'REPLACE', 'FORMATTED', 'SEQUENTIAL', -1, numout, lwp, narea )
         WRITE(inum,*) clhstnam
         CLOSE(inum)
      ENDIF
      ! write the tracer concentrations in the file
      ! ---------------------------------------
      IF( lk_pisces  )   CALL trc_wri_pisces     ! PISCES 
      IF( lk_cfc     )   CALL trc_wri_cfc        ! surface fluxes of CFC
      IF( lk_c14b    )   CALL trc_wri_c14b       ! surface fluxes of C14
      IF( lk_age     )   CALL trc_wri_age        ! AGE tracer
      IF( lk_my_trc  )   CALL trc_wri_my_trc     ! MY_TRC  tracers
      IF( lk_idtra   )   CALL trc_wri_idtra       ! Idealize tracers
      IF( lk_medusa  )   CALL trc_wri_medusa     ! MESDUSA
      !
     ! +++>>>FABM
      IF( lk_fabm  )   CALL trc_wri_fabm (kt)     ! FABM  tracers
      ! FABM <<<+++

      IF( nn_timing == 1 )  CALL timing_stop('trc_wri')
      !
   END SUBROUTINE trc_wri


   !!----------------------------------------------------------------------
   !! NEMO/TOP 3.3 , NEMO Consortium (2010)
   !! $Id$
   !! Software governed by the CeCILL licence (NEMOGCM/NEMO_CeCILL.txt)
   !!======================================================================
END MODULE trcwri
