












MODULE sedrst
   !!======================================================================
   !! MODULE sedrst :   Dummy module 
   !!======================================================================
   !! $Id$
CONTAINS
   SUBROUTINE sed_rst_read                      ! Empty routines
   END SUBROUTINE sed_rst_read
   SUBROUTINE sed_rst_wri( kt )
      INTEGER, INTENT ( in ) :: kt
      WRITE(*,*) 'sed_rst_wri: You should not have seen this print! error?', kt
   END SUBROUTINE sed_rst_wri   

END MODULE sedrst
