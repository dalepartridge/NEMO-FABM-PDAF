












MODULE trcsed_medusa
   !!======================================================================
   !!                         ***  MODULE trcsed_medusa  ***
   !! TOP :   MEDUSA Compute loss of organic matter in the sediments
   !!======================================================================
   !! History :    -   !  1995-06 (M. Levy)  original code
   !!              -   !  2000-12 (E. Kestenare)  clean up
   !!             2.0  !  2007-12  (C. Deltel, G. Madec)  F90 + simplifications
   !!              -   !  2008-08  (K. Popova) adaptation for MEDUSA
   !!              -   !  2008-11  (A. Yool) continuing adaptation for MEDUSA
   !!              -   !  2010-03  (A. Yool) updated for branch inclusion
   !!              -   !  2011-04  (A. Yool) updated for ROAM project
   !!----------------------------------------------------------------------
   !!======================================================================
   !!  Dummy module :                                   No MEDUSA bio-model
   !!======================================================================
CONTAINS
   SUBROUTINE trc_sed_medusa( kt )                   ! Empty routine
      INTEGER, INTENT( in ) ::   kt
      WRITE(*,*) 'trc_sed_medusa: You should not have seen this print! error?', kt
   END SUBROUTINE trc_sed_medusa

   !!======================================================================
END MODULE  trcsed_medusa

