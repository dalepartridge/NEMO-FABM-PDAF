












MODULE trcoxy_medusa
   !!======================================================================
   !!                         ***  MODULE trcoxy_medusa  ***
   !! TOP :   MEDUSA
   !!======================================================================
   !! History :
   !!  -   !  2011-07  (A. Yool)             added for ROAM project
   !!----------------------------------------------------------------------
   !!======================================================================
   !!  Dummy module :                                   No MEDUSA bio-model
   !!======================================================================

CONTAINS

   SUBROUTINE trc_oxy_medusa( pt, ps, kw660, pp0, o2,  &  !! inputs
      o2flux, o2sat )                                     !! outputs
      USE par_kind

      REAL(wp), INTENT( in )    :: pt
      REAL(wp), INTENT( in )    :: ps
      REAL(wp), INTENT( in )    :: kw660
      REAL(wp), INTENT( in )    :: pp0
      REAL(wp), INTENT( in )    :: o2
      REAL(wp), INTENT( inout ) :: o2flux, o2sat

      WRITE(*,*) 'trc_oxy_medusa: You should not have seen this print! error?', kt

   END SUBROUTINE trc_oxy_medusa

   !!======================================================================
END MODULE  trcoxy_medusa
  
