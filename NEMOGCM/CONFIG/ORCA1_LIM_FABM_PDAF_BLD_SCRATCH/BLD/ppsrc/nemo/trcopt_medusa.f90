












MODULE trcopt_medusa
   !!======================================================================
   !!                         ***  MODULE trcopt_medusa  ***
   !! TOP :   MEDUSA Compute the light availability in the water column
   !!======================================================================
   !! History :    -   !  1995-05  (M. Levy) Original code
   !!              -   !  1999-09  (J.-M. Andre, M. Levy) 
   !!              -   !  1999-11  (C. Menkes, M.-A. Foujols) itabe initial
   !!              -   !  2000-02  (M.A. Foujols) change x**y par exp(y*log(x))
   !!             2.0  !  2007-12  (C. Deltel, G. Madec)  F90
   !!              -   !  2008-08  (K. Popova) adaptation for MEDUSA
   !!              -   !  2008-11  (A. Yool) continuing adaptation for MEDUSA
   !!              -   !  2010-03  (A. Yool) updated for branch inclusion
   !!----------------------------------------------------------------------
   !!======================================================================
   !!  Dummy module :                                   No MEDUSA bio-model
   !!======================================================================
CONTAINS
   SUBROUTINE trc_opt_medusa( kt )                   ! Empty routine
      INTEGER, INTENT( in ) ::   kt
      WRITE(*,*) 'trc_opt_medusa: You should not have seen this print! error?', kt
   END SUBROUTINE trc_opt_medusa

   !!======================================================================
END MODULE  trcopt_medusa
