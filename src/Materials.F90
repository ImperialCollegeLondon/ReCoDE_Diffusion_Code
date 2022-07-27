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
    class(t_material) :: this
    character(len=20) :: name
        !!Set the name of the material
    this%name = name
  end subroutine SetMaterialName

  function GetMaterialName(this) Result(Res)
    class(t_material) :: this
    character(len=20) :: Res
        !!Get the name of the material
    Res = this%name
  end function GetMaterialName

  subroutine SetMaterialProperties(this, Sig_a, S)
    class(t_material) :: this
    real(kind=dp) :: Sig_a, S
        !!Set the properties of the material
    this%Sig_a = Sig_a
    this%S = S
  end subroutine SetMaterialProperties

  function GetSig_a(this) Result(Res)
    class(t_material) :: this
    real(kind=dp) :: Res
        !!Get absorption cross section of material
    Res = this%Sig_a
  end function GetSig_a

  function GetS(this) Result(Res)
    class(t_material) :: this
    real(kind=dp) :: Res
        !!Get source of material
    Res = this%S
  end function GetS

end module
