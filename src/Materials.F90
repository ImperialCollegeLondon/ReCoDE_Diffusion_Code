module Materials_Mod

  use Constants_Mod
  !!Stores standard material data and material data explicitly set via an input file

  implicit none

  type, public :: t_material
    private
    real(kind=dp) :: Sig_a, S
    character(len=20) :: Name
  contains
    !!Procedures which handle the storing, calculation and retrieval of material data
    procedure, public :: SetName => SetMaterialName
    procedure, public :: GetName => GetMaterialName
    procedure, public :: SetProps => SetMaterialProperties
    procedure, public :: GetSig_a
    procedure, public :: GetS
  end type
contains

  subroutine SetMaterialName(this, name)
    !!Set the name of the material
    class(t_material) :: this
    character(len=20) :: name
    this%name = name
  end subroutine SetMaterialName

  function GetMaterialName(this) Result(Res)
    !!Get the name of the material
    class(t_material) :: this
    character(len=20) :: Res
    Res = this%name
  end function GetMaterialName

  subroutine SetMaterialProperties(this, Sig_a, S)
    !!Set the properties of the material
    class(t_material) :: this
    real(kind=dp) :: Sig_a, S
    this%Sig_a = Sig_a
    this%S = S
  end subroutine SetMaterialProperties

  function GetSig_a(this) Result(Res)
    !!Get absorption cross section of material
    class(t_material) :: this
    real(kind=dp) :: Res
    Res = this%Sig_a
  end function GetSig_a

  function GetS(this) Result(Res)
    !!Get source of material
    class(t_material) :: this
    real(kind=dp) :: Res
    Res = this%S
  end function GetS

end module
