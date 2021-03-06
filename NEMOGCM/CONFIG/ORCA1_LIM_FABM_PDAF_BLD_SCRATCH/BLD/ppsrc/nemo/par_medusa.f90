












MODULE par_medusa
   !!======================================================================
   !!                        ***  par_medusa  ***
   !! TOP :   set the MEDUSA parameters
   !!======================================================================
   !! History :   2.0  !  2007-12  (C. Ethe, G. Madec)  revised architecture
   !!              -   !  2008-08  (K. Popova) adaptation for MEDUSA
   !!              -   !  2008-11  (A. Yool) continuing adaptation for MEDUSA
   !!              -   !  2010-03  (A. Yool) updated for branch inclusion
   !!              -   !  2011-04  (A. Yool) updated for ROAM project
   !!		   -   !  2013-03  (A. Yool) updated for v3.5 NEMO
   !!----------------------------------------------------------------------
   !! NEMO/TOP 2.0 , LOCEAN-IPSL (2007) 
   !! $Id$
   !! Software governed by the CeCILL licence (modipsl/doc/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
   USE par_pisces , ONLY : jp_pisces       !: number of tracers in PISCES
   USE par_pisces , ONLY : jp_pisces_2d    !: number of 2D diag in PISCES
   USE par_pisces , ONLY : jp_pisces_3d    !: number of 3D diag in PISCES
   USE par_pisces , ONLY : jp_pisces_trd   !: number of biological diag in PISCES

   IMPLICIT NONE

   INTEGER, PARAMETER ::   jp_lm      =  jp_pisces      !: 
   INTEGER, PARAMETER ::   jp_lm_2d   =  jp_pisces_2d   !:
   INTEGER, PARAMETER ::   jp_lm_3d   =  jp_pisces_3d   !:
   INTEGER, PARAMETER ::   jp_lm_trd  =  jp_pisces_trd  !:

   !!---------------------------------------------------------------------
   !!   Default                           No user defined tracers (MEDUSA)
   !!---------------------------------------------------------------------
   LOGICAL, PUBLIC, PARAMETER ::   lk_medusa     = .FALSE.  !: MEDUSA flag 
   INTEGER, PUBLIC, PARAMETER ::   jp_medusa     =  0       !: No MEDUSA tracers
   INTEGER, PUBLIC, PARAMETER ::   jp_medusa_2d  =  0       !: No MEDUSA additional 2d output arrays 
   INTEGER, PUBLIC, PARAMETER ::   jp_medusa_3d  =  0       !: No MEDUSA additional 3d output arrays 
   INTEGER, PUBLIC, PARAMETER ::   jp_medusa_trd =  0       !: number of sms trends for MEDUSA

   ! Starting/ending PISCES do-loop indices (N.B. no PISCES : jpl_pcs < jpf_pcs the do-loop are never done)
   INTEGER, PUBLIC, PARAMETER ::   jp_msa0     = jp_lm     + 1              !: First index of MEDUSA passive tracers
   INTEGER, PUBLIC, PARAMETER ::   jp_msa1     = jp_lm     + jp_medusa      !: Last  index of MEDUSA passive tracers
   INTEGER, PUBLIC, PARAMETER ::   jp_msa0_2d  = jp_lm_2d  + 1              !: First index of MEDUSA passive tracers
   INTEGER, PUBLIC, PARAMETER ::   jp_msa1_2d  = jp_lm_2d  + jp_medusa_2d   !: Last  index of MEDUSA passive tracers
   INTEGER, PUBLIC, PARAMETER ::   jp_msa0_3d  = jp_lm_3d  + 1              !: First index of MEDUSA passive tracers
   INTEGER, PUBLIC, PARAMETER ::   jp_msa1_3d  = jp_lm_3d  + jp_medusa_3d   !: Last  index of MEDUSA passive tracers
   INTEGER, PUBLIC, PARAMETER ::   jp_msa0_trd = jp_lm_trd + 1              !: First index of MEDUSA passive tracers
   INTEGER, PUBLIC, PARAMETER ::   jp_msa1_trd = jp_lm_trd + jp_medusa_trd  !: Last  index of MEDUSA passive tracers

   !!======================================================================
END MODULE par_medusa
