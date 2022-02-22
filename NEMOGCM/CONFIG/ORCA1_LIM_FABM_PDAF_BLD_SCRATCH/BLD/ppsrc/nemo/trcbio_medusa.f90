












MODULE trcbio_medusa
   !!======================================================================
   !!                         ***  MODULE trcbio  ***
   !! TOP :   MEDUSA
   !!======================================================================
   !! History :
   !!  -   !  1999-07  (M. Levy)              original code
   !!  -   !  2000-12  (E. Kestenare)         assign parameters to name 
   !!                                         individual tracers
   !!  -   !  2001-03  (M. Levy)              LNO3 + dia2d 
   !! 2.0  !  2007-12  (C. Deltel, G. Madec)  F90
   !!  -   !  2008-08  (K. Popova)            adaptation for MEDUSA
   !!  -   !  2008-11  (A. Yool)              continuing adaptation for MEDUSA
   !!  -   !  2010-03  (A. Yool)              updated for branch inclusion
   !!  -   !  2011-08  (A. Yool)              updated for ROAM (see below)
   !!  -   !  2013-03  (A. Yool)              updated for iMARNET
   !!  -   !  2013-05  (A. Yool)              updated for v3.5
   !!  -   !  2014-08  (A. Yool, J. Palm)     Add DMS module for UKESM1 model
   !!  -   !  2015-06  (A. Yool)              Update to include MOCSY
   !!  -   !  2015-07  (A. Yool)              Update for rolling averages
   !!  -   !  2015-10  (J. Palm)              Update for diag outputs through 
   !!                                         iom_use  
   !!  -   !  2016-11  (A. Yool)              Updated diags for CMIP6
   !!  -   !  2017-05  (A. Yool)              Added extra DMS calculation
   !!  -   !  2017-11  (J. Palm, A. Yool)     Diagnose tracer excursions
   !!----------------------------------------------------------------------
   !!
   !!
   !!
   !!=====================================================================
   !!  Dummy module :                                   No MEDUSA bio-model
   !!=====================================================================
CONTAINS
   SUBROUTINE trc_bio_medusa( kt )                   ! Empty routine
      IMPLICIT NONE
      INTEGER, INTENT( in ) ::   kt
      WRITE(*,*) 'trc_bio_medusa: You should not have seen this print! error?', kt
   END SUBROUTINE trc_bio_medusa

   !!=====================================================================
END MODULE  trcbio_medusa
