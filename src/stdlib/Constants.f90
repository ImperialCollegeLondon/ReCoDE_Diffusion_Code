module Constants_Mod

  Implicit None

  !! Precision Definitions
  Integer, Parameter        :: sp=Kind(0.0)                 !! single precision
  Integer, Parameter        :: dp=Kind(0.0d0)               !! double precision
  Integer, Parameter        :: qp=selected_Real_Kind(30,8)  !! quad precision

  !! Constant Values
  Real(Kind=dp),  Parameter :: PI=4.D0*DATAN(1.0D0)
  Real(Kind=dp),  Parameter :: dp_EPSILON = 1.0d-12
  Real(Kind=dp),  Parameter :: SMALL_NUMBER = 10.0d-8
  Real(Kind=dp),  Parameter :: LARGE_NUMBER = 10.0d8

  !! Characters
  Character, Parameter  :: COMMENT_CHAR = '!'
  Character(len=2), Parameter  :: tab = "  "
  Character, Parameter  :: space = " "

end module
