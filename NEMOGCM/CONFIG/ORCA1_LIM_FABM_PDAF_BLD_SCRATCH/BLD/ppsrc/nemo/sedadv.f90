












MODULE sedadv
   !!======================================================================
   !! MODULE sedbtb  :   Dummy module 
   !!======================================================================
   !! $Id$
CONTAINS
   SUBROUTINE sed_adv( kt )         ! Empty routine
      INTEGER, INTENT(in) :: kt
      WRITE(*,*) 'sed_adv: You should not have seen this print! error?', kt
   END SUBROUTINE sed_adv

   !!======================================================================

END MODULE sedadv
