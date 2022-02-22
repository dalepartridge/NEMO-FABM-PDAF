












MODULE sedwri
   !!======================================================================
   !! MODULE sedwri  :   Dummy module
   !!======================================================================
   !! $Id$
CONTAINS
   SUBROUTINE sed_wri( kt )         ! Empty routine
      INTEGER, INTENT(in) :: kt
      WRITE(*,*) 'sed_adv: You should not have seen this print! error?', kt
   END SUBROUTINE sed_wri

   !!======================================================================

END MODULE sedwri
