












MODULE bio_medusa_update_mod
   !!======================================================================
   !!                         ***  MODULE bio_medusa_update_mod  ***
   !! Update tracer fields
   !!======================================================================
   !! History :
   !!   -   ! 2017-04 (M. Stringer)        Code taken from trcbio_medusa.F90
   !!   -   ! 2017-08 (A. Yool)            Amend slow-detritus bug
   !!   -   ! 2017-08 (A. Yool)            Reformatting for clarity
   !!----------------------------------------------------------------------
   !!======================================================================
   !!  Dummy module :                                   No MEDUSA bio-model
   !!======================================================================
CONTAINS
   SUBROUTINE bio_medusa_update( )                    ! Empty routine
      WRITE(*,*) 'bio_medusa_update: You should not have seen this print! error?'
   END SUBROUTINE bio_medusa_update

   !!======================================================================
END MODULE bio_medusa_update_mod
