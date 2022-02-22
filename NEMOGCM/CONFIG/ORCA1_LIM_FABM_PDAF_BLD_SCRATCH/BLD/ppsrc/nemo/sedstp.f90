












MODULE sedstp
   !!======================================================================
   !! MODULE sedstp  :   Dummy module 
   !!======================================================================
   !! $Id$
CONTAINS
   SUBROUTINE sed_stp( kt )         ! Empty routine
      INTEGER, INTENT(in) :: kt
      WRITE(*,*) 'sed_stp: You should not have seen this print! error?', kt
   END SUBROUTINE sed_stp
END MODULE sedstp
