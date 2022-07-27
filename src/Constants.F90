module Constants_Mod

  use iso_fortran_env, only: output_unit

  implicit none

  !! Precision Definitions
  integer, parameter        :: sp = Kind(0.0) !! single precision
  integer, parameter        :: dp = Kind(0.0d0) !! double precision
  integer, parameter        :: qp = selected_real_Kind(30, 8) !! quad precision

  !! Constant Values
  real(kind=dp), parameter :: PI = 4.D0*DATAN(1.0D0)
  real(kind=dp), parameter :: dp_EPSILON = 1.0d-12
  real(kind=dp), parameter :: SMALL_NUMBER = 10.0d-8
  real(kind=dp), parameter :: LARGE_NUMBER = 10.0d8

  !! characters
  character, parameter  :: COMMENT_CHAR = '!'
  character(len=2), parameter  :: tab = "  "
  character, parameter  :: space = " "

end module Constants_Mod
