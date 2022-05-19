Module Problem_Mod

  use Constants_Mod
  use Materials_Mod
  !!Reads through an input deck and passes the required data to external routines

  type, public :: t_Problem
      Integer :: N_Regions
      Integer, allocatable, dimension(:) :: Nodes
      Real(kind=dp), allocatable, dimension(:) :: Boundary_Pos 
  contains
  !!Procedures which handle the storing, calculation and retrieval of material data
      procedure, public :: ReadInput
      procedure, public :: GetN_Regions
      procedure, public :: GetNodes 
      procedure, public :: GetBoundary_Pos
  end type

contains

Subroutine ReadInput(this,Material)
!!Read the Input File
  Implicit None
  class(t_Problem) :: this
  type(t_material), allocatable, dimension(:) :: Material
  Integer :: ii, jj, ID
  Integer, parameter :: InputFile = 101
  Real(kind=dp) :: Initial_Val
  Character(len=1) :: Char_Read
  Character(len=32) :: String_Read


  !!Open onput file containing problem specification
  Open(InputFile, File='input.in', Status='Old')

  !!Read in the number of Regions
  String_Read = ''
  Do While (String_Read .NE. 'Regions:')
    Read(InputFile,*) String_Read
  EndDo
  Read(InputFile,*) this%N_Regions

  !!Allocate the relevant arrays
  Allocate(this%Boundary_Pos(this%N_Regions+1))
  Allocate(this%Nodes(this%N_Regions))
  Allocate(Material(this%N_Regions))

  !!Read in the boundary positions
  String_Read = ''
  Do While (String_Read .NE. 'Boundaries:')
    Read(InputFile,*) String_Read
  EndDo
  Do ii = 1, this%N_Regions+1
    Read(InputFile,*) this%Boundary_Pos(ii)
  EndDo

  !!Read in the number of nodes in each region
  String_Read = ''
  Do While (String_Read .NE. 'Nodes:')
    Read(InputFile,*) String_Read
  EndDo
  Do ii = 1, this%N_Regions
    Read(InputFile,*) this%Nodes(ii)
  EndDo

  !!Read in the materials and set the data
  String_Read = ''
  Do While (String_Read .NE. 'Materials:')
    Read(InputFile,*) String_Read
  EndDo
  Do ii = 1, this%N_Regions
    Read(InputFile,*) String_Read
    Call Material(ii)%SetName(String_Read)
    If (String_Read == 'Fuel') Then 
      Call Material(ii)%SetProps(1._dp,1._dp)
    ElseIf (String_Read == 'Water') Then 
      Call Material(ii)%SetProps(1._dp,1._dp)
    ElseIf  (String_Read == 'Steel') Then 
      Call Material(ii)%SetProps(1._dp,1._dp)
    Else 
      Write(*,*) "ERROR: Unrecognised Material"
    EndIf
  EndDo

  Close(InputFile)

End Subroutine ReadInput


Function GetN_Regions(this) Result(Res)
    Implicit None
    class(t_problem) :: this
    Integer :: Res
    !!Get the name of the material (generally for debugging purposes)
    Res = this%N_Regions
End Function GetN_Regions


Function GetNodes(this) Result(Res)
    Implicit None
    class(t_problem) :: this
    Integer, dimension(this%N_Regions) :: Res
    !!Get the name of the material (generally for debugging purposes)
    Res = this%Nodes
End Function GetNodes


Function GetBoundary_Pos(this) Result(Res)
    Implicit None
    class(t_problem) :: this
    Real(kind=dp), dimension(this%N_Regions+1) :: Res
    !!Get the name of the material (generally for debugging purposes)
    Res = this%Boundary_Pos
End Function GetBoundary_Pos

End Module
