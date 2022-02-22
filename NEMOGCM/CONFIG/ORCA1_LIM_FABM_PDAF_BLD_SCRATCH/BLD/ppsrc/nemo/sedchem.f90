












MODULE sedchem

   !!======================================================================
   !! MODULE sedchem  :   Dummy module 
   !!======================================================================
   !! $Id$
CONTAINS
   SUBROUTINE sed_chem( kt )         ! Empty routine
      INTEGER, INTENT(in) :: kt
      WRITE(*,*) 'trc_stp: You should not have seen this print! error?', kt
   END SUBROUTINE sed_chem

   !!======================================================================


END MODULE sedchem
