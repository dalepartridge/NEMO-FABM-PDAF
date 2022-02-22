












MODULE sedmodel
   !!======================================================================
   !! MODULE sedmodel  :   Dummy module 
   !!======================================================================
   LOGICAL, PUBLIC, PARAMETER ::   lk_sed = .FALSE.     !: sediment flag
   !! $Id$
CONTAINS
   SUBROUTINE sed_model( kt )         ! Empty routine
      INTEGER, INTENT(in) :: kt
      WRITE(*,*) 'sed_stp: You should not have seen this print! error?', kt
   END SUBROUTINE sed_model

END MODULE sedmodel
