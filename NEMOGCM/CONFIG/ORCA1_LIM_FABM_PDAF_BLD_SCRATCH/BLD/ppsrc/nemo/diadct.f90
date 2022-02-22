












MODULE diadct
  !!=====================================================================
  !!                       ***  MODULE  diadct  ***
  !! Ocean diagnostics: Compute the transport trough a sec.
  !!===============================================================
  !! History : 
  !!
  !!         original  : 02/99 (Y Drillet)
  !!         addition  : 10/01 (Y Drillet, R Bourdalle Badie)
  !!                   : 10/05 (M Laborie) F90
  !!         addition  : 04/07 (G Garric) Ice sections
  !!         bugfix    : 04/07 (C Bricaud) test on sec%nb_point
  !!                                      initialisation of ztransp1,ztransp2,...
  !!         nemo_v_3_4: 09/2011 (C Bricaud)
  !!
  !!
  !!----------------------------------------------------------------------
   !!----------------------------------------------------------------------
   !!   Default option :                                       Dummy module
   !!----------------------------------------------------------------------
   LOGICAL, PUBLIC, PARAMETER ::   lk_diadct = .FALSE.    !: diamht flag
   PUBLIC 
   !! $Id$
CONTAINS

   SUBROUTINE dia_dct_init          ! Dummy routine
   IMPLICIT NONE
      WRITE(*,*) 'dia_dct_init: You should not have seen this print! error?'
   END SUBROUTINE dia_dct_init

   SUBROUTINE dia_dct( kt )         ! Dummy routine
   IMPLICIT NONE
      INTEGER, INTENT( in ) :: kt   ! ocean time-step index
      WRITE(*,*) 'dia_dct: You should not have seen this print! error?', kt
   END SUBROUTINE dia_dct

END MODULE diadct
