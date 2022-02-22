












MODULE trcsms_medusa
   !!======================================================================
   !!                         ***  MODULE trcsms_medusa  ***
   !! TOP :   Main module of the MEDUSA tracers
   !!======================================================================
   !! History :   2.0  !  2007-12  (C. Ethe, G. Madec) Original code
   !!              -   !  2008-08  (K. Popova) adaptation for MEDUSA
   !!              -   !  2008-11  (A. Yool) continuing adaptation for MEDUSA
   !!              -   !  2010-03  (A. Yool) updated for branch inclusion
   !!              -   !  2017-08  (A. Yool) amend for slow detritus bug
   !!----------------------------------------------------------------------
   !!----------------------------------------------------------------------
   !!   Dummy module                                        No MEDUSA model
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE trc_sms_medusa( kt )             ! Empty routine
      INTEGER, INTENT( in ) ::   kt
      WRITE(*,*) 'trc_sms_medusa: You should not have seen this print! error?', kt
   END SUBROUTINE trc_sms_medusa

   !!======================================================================
END MODULE trcsms_medusa

