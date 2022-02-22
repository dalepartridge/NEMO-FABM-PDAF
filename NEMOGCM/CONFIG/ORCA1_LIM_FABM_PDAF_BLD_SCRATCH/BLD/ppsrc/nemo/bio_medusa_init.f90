












MODULE bio_medusa_init_mod
   !!======================================================================
   !!                         ***  MODULE bio_medusa_init  ***
   !! Initialisation for TRC_BIO_MEDUSA
   !!======================================================================
   !! History :
   !!   -   ! 2017-04 (M. Stringer)        Code taken from trcbio_medusa.F90
   !!   -   ! 2017-08 (A. Yool)            Add slow-sinking detrius variables
   !!----------------------------------------------------------------------
   !!======================================================================
   !!  Dummy module :                                   No MEDUSA bio-model
   !!======================================================================
CONTAINS
   SUBROUTINE bio_medusa_init( )                   ! Empty routine
      IMPLICIT NONE
      WRITE(*,*) 'bio_medusa_init: You should not have seen this print! error?'
   END SUBROUTINE bio_medusa_init

   !!======================================================================
END MODULE bio_medusa_init_mod
