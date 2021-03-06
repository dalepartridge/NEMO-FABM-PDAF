












!> \file mocsy_singledouble.f90
!! \BRIEF 
!> Module that defines single and double precision - used by all other modules
MODULE mocsy_singledouble
! INTEGER, PARAMETER :: r4 = SELECTED_REAL_KIND(6)
! INTEGER, PARAMETER :: r8 = SELECTED_REAL_KIND(12)
  INTEGER, PARAMETER :: r4 = KIND(1.0)
  INTEGER, PARAMETER :: r8 = KIND(1.0d0)
! INTEGER, PARAMETER :: wp = KIND(1.0d0)
  INTEGER, PARAMETER :: wp = SELECTED_REAL_KIND(12,307)
END MODULE mocsy_singledouble
