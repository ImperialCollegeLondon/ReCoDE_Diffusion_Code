Module Problem_Mod

  use Constants_Mod
  use Materials_Mod
  !!Reads through an input deck and passes the required data to external routines

  type, public :: t_Problem
      Integer :: N_Regions, N_Nodes
      Integer, allocatable, dimension(:) :: Nodes
      Real(kind=dp), allocatable, dimension(:) :: Boundary_Pos 
  contains
  !!Procedures which handle the storing, calculation and retrieval of material data
      procedure, public :: ReadInput
      procedure, public :: GetN_Regions
      procedure, public :: GetNodes 
      procedure, public :: GetN_Nodes
      procedure, public :: GetBoundary_Pos
      procedure, public :: DestroyProblem
  end type

contains

Subroutine ReadInput(this,Material)
!!Read the Input File
  Implicit None
  class(t_Problem) :: this
  type(t_material), allocatable, dimension(:) :: Material
  Integer :: ii
  Integer, parameter :: InputFile = 101
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
  this%N_Nodes = 0
  Do ii = 1, this%N_Regions
    Read(InputFile,*) this%Nodes(ii)
    this%N_Nodes = this%N_Nodes + this%Nodes(ii)
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
      Call Material(ii)%SetProps(1._dp,10._dp)
    ElseIf (String_Read == 'Water') Then 
      Call Material(ii)%SetProps(3._dp,4._dp)
    ElseIf  (String_Read == 'Steel') Then 
      Call Material(ii)%SetProps(5._dp,1._dp)
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
    !!Get the number of regions in the problem
    Res = this%N_Regions
End Function GetN_Regions


Function GetNodes(this) Result(Res)
    Implicit None
    class(t_problem) :: this
    Integer, dimension(this%N_Regions) :: Res
    !!Get an array containing the number of nodes in each region
    Res = this%Nodes
End Function GetNodes


Function GetN_Nodes(this) Result(Res)
    Implicit None
    class(t_problem) :: this
    Integer :: Res
    !!Get the total number of nodes in the problem
    Res = this%N_Nodes
End Function GetN_Nodes


Function GetBoundary_Pos(this) Result(Res)
    Implicit None
    class(t_problem) :: this
    Real(kind=dp), dimension(this%N_Regions+1) :: Res
    !!Get the positions of the boundaries in the problem
    Res = this%Boundary_Pos
End Function GetBoundary_Pos


Subroutine DestroyProblem(this,Material)
  !!Destroy the data stored in the problem class
  Implicit None
  class(t_Problem) :: this
  type(t_material), allocatable, dimension(:) :: Material

  !!Deallocate the relevant arrays
  Deallocate(this%Boundary_Pos,this%Nodes)
  Deallocate(Material)
End Subroutine DestroyProblem


End Module
