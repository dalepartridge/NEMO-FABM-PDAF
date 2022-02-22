












MODULE trcstat
   !!======================================================================
   !!                         ***  MODULE trcrst  ***
   !! TOP :   Manage the passive tracer restart
   !!======================================================================
   !! History :    -   !  1991-03  ()  original code
   !!             1.0  !  2005-03 (O. Aumont, A. El Moussaoui) F90
   !!              -   !  2005-10 (C. Ethe) print control
   !!             2.0  !  2005-10 (C. Ethe, G. Madec) revised architecture
   !!----------------------------------------------------------------------
   !!----------------------------------------------------------------------
   !!   'key_top'                                                TOP models
   !!----------------------------------------------------------------------
   !!----------------------------------------------------------------------
   !!   trc_rst :   Restart for passive tracer
   !!----------------------------------------------------------------------
   !!----------------------------------------------------------------------
   !!   'key_top'                                                TOP models
   !!----------------------------------------------------------------------
   !!   trc_rst_opn    : open  restart file
   !!   trc_rst_read   : read  restart file
   !!   trc_rst_wri    : write restart file
   !!----------------------------------------------------------------------
   USE trc,               ONLY: tra, ctrcnm
   USE par_kind,          ONLY: wp
   USE in_out_manager,    ONLY: lwp, numout
   USE par_oce,           ONLY: jpi, jpj
   USE par_trc,           ONLY: jptra
   USE dom_oce,           ONLY: e3t_0, gdepw_0, tmask, e1e2t
   USE dom_oce,           ONLY: e3t_a, e3t_n, gdepw_n
   !* MPP library                         
   USE lib_mpp
   !* Fortran utilities                         
   USE lib_fortran

   IMPLICIT NONE
   PRIVATE

   PUBLIC   trc_rst_dia_stat
   PUBLIC   trc_rst_tra_stat

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
   
   SUBROUTINE trc_rst_tra_stat
      !!----------------------------------------------------------------------
      !!                    ***  trc_rst_tra_stat  ***
      !!
      !! ** purpose  :   Compute tracers statistics - check where crazy values appears
      !!----------------------------------------------------------------------
      INTEGER  :: jk, jn
      REAL(wp) :: ztraf, zmin, zmax, zmean, zdrift, areasf
      REAL(wp), DIMENSION(jpi,jpj) :: zvol
      !!----------------------------------------------------------------------

      IF( lwp ) THEN
         WRITE(numout,*)
         WRITE(numout,*) '           ----SURFACE TRA STAT----             '
         WRITE(numout,*)
      ENDIF
      !
      zvol(:,:) = e1e2t(:,:) * e3t_a(:,:,1) * tmask(:,:,1)
      areasf = glob_sum(zvol(:,:))
      DO jn = 1, jptra
         ztraf = glob_sum( tra(:,:,1,jn) * zvol(:,:) )
         zmin  = MINVAL( tra(:,:,1,jn), mask= ((tmask(:,:,1).NE.0.)) )
         zmax  = MAXVAL( tra(:,:,1,jn), mask= ((tmask(:,:,1).NE.0.)) )
         IF( lk_mpp ) THEN
            CALL mpp_min( zmin )      ! min over the global domain
            CALL mpp_max( zmax )      ! max over the global domain
         END IF
         zmean  = ztraf / areasf
         IF(lwp) WRITE(numout,9001) jn, TRIM( ctrcnm(jn) ), zmean, zmin, zmax
      END DO
      IF(lwp) WRITE(numout,*)
9001  FORMAT(' tracer nb :',i2,'    name :',a10,'    mean :',e18.10,'    min :',e18.10, &
      &      '    max :',e18.10)
      !
   END SUBROUTINE trc_rst_tra_stat



   SUBROUTINE trc_rst_dia_stat( dgtr, names)
      !!----------------------------------------------------------------------
      !!                    ***  trc_rst_dia_stat  ***
      !!
      !! ** purpose  :   Compute tracers statistics
      !!----------------------------------------------------------------------
      REAL(wp), DIMENSION(jpi,jpj) , INTENT(in) ::   dgtr      ! 2D diag var
      CHARACTER(len=*)             , INTENT(in) ::   names     ! 2D diag name
      !!---------------------------------------------------------------------
      INTEGER  :: jk, jn
      CHARACTER (LEN=18) :: text_zmean
      REAL(wp) :: ztraf, zmin, zmax, zmean, areasf
      REAL(wp), DIMENSION(jpi,jpj) :: zvol
      !!----------------------------------------------------------------------

      IF( lwp )  WRITE(numout,*) 'STAT- ', names
      
      ! fse3t_a will be undefined at the start of a run, but this routine
      ! may be called at any stage! Hence we MUST make sure it is 
      ! initialised to zero when allocated to enable us to test for 
      ! zero content here and avoid potentially dangerous and non-portable 
      ! operations (e.g. divide by zero, global sums of junk values etc.)   
      zvol(:,:) = e1e2t(:,:) * e3t_a(:,:,1) * tmask(:,:,1)
      ztraf = glob_sum( dgtr(:,:) * zvol(:,:) )
      !! areasf = glob_sum(e1e2t(:,:) * tmask(:,:,1) )
      areasf = glob_sum(zvol(:,:))
      zmin  = MINVAL( dgtr(:,:), mask= ((tmask(:,:,1).NE.0.)) )
      zmax  = MAXVAL( dgtr(:,:), mask= ((tmask(:,:,1).NE.0.)) )
      IF( lk_mpp ) THEN
         CALL mpp_min( zmin )      ! min over the global domain
         CALL mpp_max( zmax )      ! max over the global domain
      END IF

      text_zmean = "N/A"
      ! Avoid divide by zero. areasf must be positive.
      IF  (areasf > 0.0) THEN 
         zmean = ztraf / areasf
         WRITE(text_zmean,'(e18.10)') zmean
      ENDIF

      IF(lwp) WRITE(numout,9002) TRIM( names ), text_zmean, zmin, zmax

  9002  FORMAT(' tracer name :',A,'    mean :',A,'    min :',e18.10, &
      &      '    max :',e18.10 )
      !
   END SUBROUTINE trc_rst_dia_stat



   !!----------------------------------------------------------------------
   !! NEMO/TOP 3.3 , NEMO Consortium (2010)
   !! $Id$
   !! Software governed by the CeCILL licence (NEMOGCM/NEMO_CeCILL.txt)
   !!======================================================================
END MODULE trcstat
