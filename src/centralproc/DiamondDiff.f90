Module DiamondDiff_Mod

    use Constants_Mod
    use StdLib_Mod
    use Materials_Mod
    use Problem_Mod
    use PETSc_Init_Mod
    use PETSc_Vec_Mod
    use PETSc_Mat_Mod
    use Solver_Mod
    use PETScSolver_Mod
    use Output_Mod

    !!Discretises the problem and sets up the system of equations which can then be solved
    type, public :: t_DDiff
        Integer :: N_Regions
    contains
        procedure, public :: CreatePETSc
        procedure, public :: SolveProblem
        procedure, public :: DestroyPETSc
    end type
contains

Subroutine CreatePETSc(this,Problem,pMatA,pVecb,pVecx)
    !!Set up PETSc
    Implicit None
    class(t_DDiff) :: this
    type(t_Problem) :: Problem
    type(pMat) :: pMatA
    type(pVec) :: pVecb, pVecx
    Integer :: ii, jj, N_Regions, N_Nodes
    Integer, allocatable, dimension(:) :: RegionNodes
    Real(kind=dp), allocatable, dimension(:) :: Boundary_Pos

    !!Initialize PETSc
    call PETSc_Init()

    !!Extract relevant data from the Problem specification
    N_Nodes = Problem%GetN_Nodes()

    !!Generate the required PETSc Matrices and Vectors for the problem
    call pMatA%Create(N_Nodes,N_Nodes,5)
    call pVecb%Create(N_Nodes)
    call pVecx%Create(N_Nodes)

End Subroutine CreatePETSc


Subroutine SolveProblem(this,Material,Problem,pMatA,pVecb,pVecx)
    !!Generate the system of equations to be fed into the solver
    Implicit None
    class(t_DDiff) :: this
    type(t_material), dimension(:) :: Material 
    type(t_Problem) :: Problem
    type(pMat) :: pMatA
    type(pVec) :: pVecb, pVecx
    type(t_PETScSolver) :: PETScSolver
    Integer :: ii, jj, N_Regions, N_Nodes, NodeID
    Integer, allocatable, dimension(:) :: RegionNodes
    Real(kind=dp) :: Delta
    Real(kind=dp), allocatable, dimension(:) :: Boundary_Pos, Sig_a, Source


    !!Extract relevant data from the Problem specification
    N_Regions = Problem%GetN_Regions()
    Allocate(RegionNodes(N_Regions),Boundary_Pos(N_Regions))
    Boundary_Pos = Problem%GetBoundary_Pos()
    RegionNodes = Problem%GetNodes()
    N_Nodes = Problem%GetN_Nodes()

    !!Extract relevant material data from the materials class
    Allocate(Sig_a(N_Regions),Source(N_Regions))
    Do ii = 1, N_Regions
        Sig_a(ii) = Material(ii)%GetSig_a()
        Source(ii) = Material(ii)%GetS()
    EndDo

    !!Fill PETSc Matrices and Vectors with problem information
    !!Loop over each region within the geometry
    NodeID = 0
    Do ii = 1, N_Regions
        !!Loop over each node within the region
        Do jj = 1, RegionNodes(ii)
            !!Calculate the delta value between each node in the region
            Delta = (Boundary_Pos(ii+1)-Boundary_Pos(ii))/Real(RegionNodes(ii),dp)
            !!Use Delta and material properties to fill the matrix
            NodeID = NodeID + 1
            call pMatA%AddVal(NodeID,NodeID,Delta*Sig_a(ii))
            call pVecb%Add(NodeID,Delta*Source(ii))
        EndDo
    EndDo

    !!Solve the problem
    call pMatA%Assemble()
    call pVecb%Assemble()
    call PETScSolver%Solve(pMatA,pVecb,pVecx)

    Deallocate(RegionNodes,Boundary_Pos)
    Deallocate(Sig_a,Source)
End Subroutine SolveProblem


Subroutine DestroyPETSc(this,pMatA,pVecb,pVecx)
    !!Dismantle PETSc
    Implicit None
    class(t_DDiff) :: this
    type(pMat) :: pMatA
    type(pVec) :: pVecb, pVecx
    Integer :: ii, jj, N_Regions, N_Nodes
    Integer, allocatable, dimension(:) :: RegionNodes
    Real(kind=dp), allocatable, dimension(:) :: Boundary_Pos

    !!Initialize PETSc
    call PETSc_Init()

    !!Destroy the PETSc Matrices and Vectors
    call pMatA%Destroy()
    call pVecb%Destroy()
    call pVecx%Destroy()

End Subroutine DestroyPETSc


End Module
