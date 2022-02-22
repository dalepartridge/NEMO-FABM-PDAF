












MODULE mocsy_wrapper
   !!======================================================================
   !!                         ***  MODULE mocsy_wrapper  ***
   !! TOP :   MEDUSA
   !!======================================================================
   !! History :
   !!  -   !  2015-06  (A. Yool)             added for UKESM project
   !!  -   !  2017-04  (A. Yool)             alter optK1K2 to 'w14' option
   !!----------------------------------------------------------------------
   !!======================================================================
   !!  Dummy module :                         No MOCSY carbonate chemistry
   !!======================================================================

CONTAINS

   SUBROUTINE mocsy_interface( kt )                   ! Empty routine

      INTEGER, INTENT( in ) ::   kt

      WRITE(*,*) 'mocsy_interface: You should not have seen this print! error?', kt

   END SUBROUTINE mocsy_interface

   !!======================================================================

END MODULE mocsy_wrapper
