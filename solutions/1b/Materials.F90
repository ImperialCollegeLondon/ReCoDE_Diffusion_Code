Module Materials_Mod

  use Constants_Mod
    !!Stores standard material data and material data explicitly set via an input file

  Implicit None

  type, public :: t_material
    private
    Real(kind=dp) :: Sig_a, S
    Character(len=20) :: Name
  contains
    !!Procedures which handle the storing, calculation and retrieval of material data
    procedure, public :: SetName => SetMaterialName
    procedure, public :: GetName => GetMaterialName
    procedure, public :: SetProps => SetMaterialProperties
    procedure, public :: GetSig_a
    procedure, public :: GetS
        !! $$ Exercise 1b
    procedure, public :: PrintMaterial
  end type
contains

  Subroutine SetMaterialName(this, name)
    class(t_material) :: this
    Character(len=20) :: name
        !!Set the name of the material
    this%name = name
  End Subroutine SetMaterialName

  Function GetMaterialName(this) Result(Res)
    class(t_material) :: this
    Character(len=20) :: Res
        !!Get the name of the material
    Res = this%name
  End Function GetMaterialName

  Subroutine SetMaterialProperties(this, Sig_a, S)
    class(t_material) :: this
    Real(Kind=dp) :: Sig_a, S
        !!Set the properties of the material
    this%Sig_a = Sig_a
    this%S = S
  End Subroutine SetMaterialProperties

  Function GetSig_a(this) Result(Res)
    class(t_material) :: this
    Real(Kind=dp) :: Res
        !!Get absorption cross section of material
    Res = this%Sig_a
  End Function GetSig_a

  Function GetS(this) Result(Res)
    class(t_material) :: this
    Real(Kind=dp) :: Res
        !!Get source of material
    Res = this%S
  End Function GetS

    !! $$ Exercise 1b
  Subroutine PrintMaterial(this)
    class(t_material) :: this
        !!Print the material data
    Write(output_unit, *) "=========="
    Write(output_unit, *) "Material Name: ", this%name
    Write(output_unit, '(g0)', advance='no') " Absorption: "
    Write(output_unit, '(E14.6)') this%Sig_a
    Write(output_unit, '(g0)', advance='no') " Source:"
    Write(output_unit, '(E14.6)') this%S
    Write(output_unit, *) "=========="
  End Subroutine PrintMaterial

End Module
