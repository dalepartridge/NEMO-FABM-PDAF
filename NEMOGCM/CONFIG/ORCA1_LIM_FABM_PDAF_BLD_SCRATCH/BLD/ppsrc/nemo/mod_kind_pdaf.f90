












MODULE mod_kind_pdaf

! This module defines the kind of real and length of character strings
! for the PDAF call-back routines and interfaces. It is based on the NEMO
! module `par_kind.F90`.

   IMPLICIT NONE
   SAVE

   INTEGER, PARAMETER :: pdp = SELECTED_REAL_KIND(12, 307)
   INTEGER, PARAMETER :: pwp = pdp
   INTEGER, PARAMETER :: lc = 256

END MODULE mod_kind_pdaf
