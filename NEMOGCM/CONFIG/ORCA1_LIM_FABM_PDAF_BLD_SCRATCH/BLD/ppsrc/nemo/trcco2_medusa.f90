












MODULE trcco2_medusa
   !!======================================================================
   !!                         ***  MODULE trcco2_medusa  ***
   !! TOP :   MEDUSA
   !!======================================================================
   !! History :
   !!  -   !  2010-12  (A. Yool)             added for ROAM project
   !!----------------------------------------------------------------------
   !!======================================================================
   !!  Dummy module :                         No MEDUSA carbonate chemistry
   !!======================================================================

CONTAINS

   SUBROUTINE trc_co2_medusa( kt )                   ! Empty routine

      INTEGER, INTENT( in ) ::   kt

      WRITE(*,*) 'trc_co2_medusa: You should not have seen this print! error?', kt

   END SUBROUTINE trc_co2_medusa

   !!======================================================================
END MODULE  trcco2_medusa
