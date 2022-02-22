












MODULE trcsms_idtra
   !!======================================================================
   !!                      ***  MODULE trcsms_idtra  ***
   !! TOP : TRI main model
   !!======================================================================
   !! History :    -   !  1999-10  (JC. Dutay)  original code
   !!             1.0  !  2004-03 (C. Ethe) free form + modularity
   !!             2.0  !  2007-12  (C. Ethe, G. Madec)  reorganisation
   !!----------------------------------------------------------------------
   !!----------------------------------------------------------------------
   !!   Dummy module                                         No TRI tracers
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE trc_sms_idtra( kt )       ! Empty routine
      WRITE(*,*) 'trc_sms_idtra: You should not have seen this print! error?', kt
   END SUBROUTINE trc_sms_idtra

   !!======================================================================
END MODULE trcsms_idtra




