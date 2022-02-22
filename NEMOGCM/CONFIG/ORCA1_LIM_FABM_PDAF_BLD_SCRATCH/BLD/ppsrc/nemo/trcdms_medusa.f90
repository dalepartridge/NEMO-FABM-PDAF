












MODULE trcdms_medusa
   !!======================================================================
   !!                         ***  MODULE trcdms_medusa  ***
   !! TOP :   MEDUSA
   !!======================================================================
   !! History :
   !!  -   !  2014-08  (J. Palmieri - A. Yool)    added for UKESM1 project
   !!  -   !  2017-05  (A. Yool)                  add extra Anderson scheme
   !!----------------------------------------------------------------------
   !!======================================================================
   !!  Dummy module :                                   No MEDUSA bio-model
   !!======================================================================

CONTAINS

!=======================================================================
!
   SUBROUTINE trc_dms_medusa( kt )                                        !! EMPTY Routine
!      
!
      INTEGER, INTENT( in ) ::   kt
!

      WRITE(*,*) 'trc_dms_medusa: You should not have seen this print! error?'

   END SUBROUTINE trc_dms_medusa

   !!======================================================================
END MODULE  trcdms_medusa



