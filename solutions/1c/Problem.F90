module Problem_Mod

  use Constants_Mod
  use Materials_Mod
  !!Reads through an input deck and passes the required data to external routines

  implicit none
  private

  type, public :: t_Problem
    integer :: N_Regions, N_Nodes
    integer, allocatable, dimension(:) :: Nodes
    integer, dimension(2) :: Boundary_Conditions
    real(kind=dp), allocatable, dimension(:) :: Boundary_Pos
  contains
  !!Procedures which handle the storing, calculation and retrieval of material data
    procedure, public :: ReadInput
    procedure, public :: GetN_Regions
    procedure, public :: GetNodes
    procedure, public :: GetN_Nodes
    procedure, public :: GetBoundary_Pos
    procedure, public :: GetBoundary_Conditions
    procedure, public :: DestroyProblem
  end type

contains

  subroutine ReadInput(this, Material)
    !!Read the Input File
    class(t_Problem) :: this
    type(t_material), allocatable, dimension(:) :: Material
    integer :: ii
    integer, parameter :: InputFile = 101
    character(len=32) :: String_Read

    !!Open input file containing problem specification
    open(InputFile, File='input.in', Status='Old')

    !!Read in the number of Regions
    String_Read = ''
    do while (String_Read /= 'Regions:')
      read(InputFile, *) String_Read
    end do
    read(InputFile, *) this%N_Regions

    !!Allocate the relevant arrays
    allocate(this%Boundary_Pos(this%N_Regions + 1))
    allocate(this%Nodes(this%N_Regions))
    allocate(Material(this%N_Regions))

    !!Read in the boundary positions
    String_Read = ''
    do while (String_Read /= 'Boundaries:')
      read(InputFile, *) String_Read
    end do
    do ii = 1, this%N_Regions + 1
      read(InputFile, *) this%Boundary_Pos(ii)
    end do

    !!Read in the number of nodes in each region
    String_Read = ''
    do while (String_Read /= 'Nodes:')
      read(InputFile, *) String_Read
    end do
    this%N_Nodes = 0
    do ii = 1, this%N_Regions
      read(InputFile, *) this%Nodes(ii)
      this%N_Nodes = this%N_Nodes + this%Nodes(ii)
    end do
    this%N_Nodes = this%N_Nodes - (this%N_Regions - 1)

    !!Read in the materials and set the data
    String_Read = ''
    do while (String_Read /= 'Materials:')
      read(InputFile, *) String_Read
    end do
    do ii = 1, this%N_Regions
      read(InputFile, *) String_Read
      call Material(ii)%SetName(String_Read)
      if (String_Read == 'Fuel') then
        call Material(ii)%SetProps(1._dp, 6._dp)
      else if (String_Read == 'Water') then
        call Material(ii)%SetProps(2._dp, 0.1_dp)
      else if (String_Read == 'Steel') then
        call Material(ii)%SetProps(5._dp, 0.1_dp)
      !! $$ Exercise 1c
      else if (String_Read == 'Iron') then
        call Material(ii)%SetProps(4._dp, 0.1_dp)
      else
        write(output_unit, *) "ERROR: Unrecognised Material"
      end if
    end do

    !!Read in the boundary conditions of the problem
    String_Read = ''
    do while (String_Read /= 'Boundary_Conditions:')
      read(InputFile, *) String_Read
    end do
    do ii = 1, 2
      read(InputFile, *) String_Read
      if (String_Read == 'Zero') then
        this%Boundary_Conditions(ii) = 0
      else if (String_Read == 'Reflective') then
        this%Boundary_Conditions(ii) = 1
      else
        write(output_unit, *) "ERROR: Unrecognised Boundary Condition"
      end if
    end do

    close(InputFile)

  end subroutine ReadInput

  function GetN_Regions(this) Result(Res)
    !!Get the number of regions in the problem
    class(t_problem) :: this
    integer :: Res
    Res = this%N_Regions
  end function GetN_Regions

  function GetNodes(this) Result(Res)
    !!Get an array containing the number of nodes in each region
    class(t_problem) :: this
    integer, dimension(this%N_Regions) :: Res
    Res = this%Nodes
  end function GetNodes

  function GetN_Nodes(this) Result(Res)
    !!Get the total number of nodes in the problem
    class(t_problem) :: this
    integer :: Res
    Res = this%N_Nodes
  end function GetN_Nodes

  function GetBoundary_Pos(this) Result(Res)
    !!Get the positions of the boundaries in the problem
    class(t_problem) :: this
    real(kind=dp), dimension(this%N_Regions + 1) :: Res
    Res = this%Boundary_Pos
  end function GetBoundary_Pos

  function GetBoundary_Conditions(this) Result(Res)
    !!Get the boundary condition of the problem
    class(t_problem) :: this
    integer, dimension(2) :: Res
    Res = this%Boundary_Conditions
  end function GetBoundary_Conditions

  subroutine DestroyProblem(this, Material)
    !!Destroy the data stored in the problem class
    class(t_Problem) :: this
    type(t_material), allocatable, dimension(:) :: Material

    !!Deallocate the relevant arrays
    deallocate(this%Boundary_Pos, this%Nodes)
    deallocate(Material)
  end subroutine DestroyProblem

end module Problem_Mod
