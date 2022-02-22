












MODULE air_sea_mod
   !!======================================================================
   !!                         ***  MODULE air_sea_mod  ***
   !! Calculate the carbon chemistry for the whole ocean
   !!======================================================================
   !! History :
   !!   -   ! 2017-04 (M. Stringer)        Code taken from trcbio_medusa.F90
   !!   -   ! 2017-08 (A. Yool)            Add air-sea flux kill switch
   !!----------------------------------------------------------------------
   !!======================================================================
   !!  Dummy module :                                   No MEDUSA bio-model
   !!======================================================================
CONTAINS
   SUBROUTINE air_sea( )                    ! Empty routine
      WRITE(*,*) 'air_sea: You should not have seen this print! error?'
   END SUBROUTINE air_sea

   !!======================================================================
END MODULE air_sea_mod
