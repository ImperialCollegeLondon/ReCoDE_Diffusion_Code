Module Materials_Mod

    use Constants_Mod
    !!Stores standard material data and material data explicitly set via an input file

  type, public :: t_material
        private
        !!Currently only storing the absorption and source terms
        !!For a multigroup code these would be allocatable arrays
        !!There would also be an associated allocatable scattering matrix
        Real(kind=dp) :: Sig_a, S
        Character(len=20) :: Name
    contains
    !!Procedures which handle the storing, calculation and retrieval of material data
        procedure, public :: SetName => SetMaterialName
        procedure, public :: GetName => GetMaterialName
        procedure, public :: SetProps => SetMaterialProperties
        procedure, public :: GetSig_a
        procedure, public :: GetS
  end type
contains


    Subroutine SetMaterialName(this,name)
        Implicit None
        class(t_material) :: this
        Character(len=20) :: name
        !!Set the name of the material
        this%name = name
    End Subroutine SetMaterialName


    Function GetMaterialName(this) Result(Res)
        Implicit None
        class(t_material) :: this
        Character(len=20) :: Res
        !!Get the name of the material (generally for debugging purposes)
        Res = this%name
    End Function GetMaterialName


    Subroutine SetMaterialProperties(this,Sig_a,S)
        Implicit None
        class(t_material) :: this
        Real(Kind=dp) :: Sig_a, S
        !!Set the properties of the material
        this%Sig_a = Sig_a
        this%S = S
    End Subroutine SetMaterialProperties


    Function GetSig_a(this) Result(Res)
        Implicit None
        class(t_material) :: this
        Real(Kind=dp) :: Res
        !!Get absorption cross section of material
        Res = this%Sig_a
    End Function GetSig_a


    Function GetS(this) Result(Res)
        Implicit None
        class(t_material) :: this
        Real(Kind=dp) :: Res
        !!Get source of material
        Res = this%S
    End Function GetS

End Module
