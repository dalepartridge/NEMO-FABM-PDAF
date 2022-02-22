












MODULE gastransfer
   !!======================================================================
   !!                         ***  MODULE trcdms_medusa  ***
   !! TOP :   MEDUSA
   !!======================================================================
   !! History :
   !!  -   !  2015-06  (A. Yool)             added for UKESM1 project
   !!----------------------------------------------------------------------
   !!======================================================================
   !!  Dummy module :                                   No MEDUSA bio-model
   !!======================================================================

CONTAINS

   SUBROUTINE gas_transfer(wind, N, eqn, kw660)
      USE par_kind

      REAL(wp), INTENT( in )    :: wind
      REAL(wp), INTENT( in )    :: kw660
      INTEGER, INTENT(in) :: N, eqn

      WRITE(*,*) 'gas_transfer: You should not have seen this print! error?', kt

   END SUBROUTINE gas_transfer

   !!======================================================================
END MODULE gastransfer
