












MODULE sedmbc
   !!======================================================================
   !! MODULE sedmbc :   Dummy module 
   !!======================================================================
   !! $Id$
CONTAINS
   SUBROUTINE sed_mbc( kt )         ! Empty routine
      INTEGER, INTENT(in) :: kt
      WRITE(*,*) 'sed_mbc: You should not have seen this print! error?', kt
   END SUBROUTINE sed_mbc
END MODULE sedmbc
