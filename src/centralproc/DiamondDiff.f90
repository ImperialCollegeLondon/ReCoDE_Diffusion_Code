Module DiamondDiff_Mod

    use Constants_Mod
    use StdLib_Mod
    use Materials_Mod
    use Problem_Mod
# ifdef PETSC
    use PETSc_Init_Mod
    use PETSc_Vec_Mod
    use PETSc_Mat_Mod
    use PETScSolver_Mod
# endif
# ifndef PETSC
    use CRS_Mod
    use Solver_Mod
# endif
    use Output_Mod

    Implicit None

    !!Discretises the problem and sets up the system of equations which can then be solved
    type, public :: t_DDiff
        Integer :: N_Regions
        Real(kind=dp), allocatable, dimension(:) :: Vecb
# ifndef PETSC
        type(t_CRS) :: CRS
# endif
# ifdef PETSC
        type(pMat) :: pMatA
        type(pVec) :: pVecb, pVecx
# endif
    contains
        procedure, public :: Create
        procedure, public :: Solve
        procedure, public :: Destroy
    end type
contains

Subroutine Create(this,Problem)
    !!Set up Problem
    class(t_DDiff) :: this
    type(t_Problem) :: Problem
    Integer :: N_Nodes


    !!Extract relevant data from the Problem specification
    N_Nodes = Problem%GetN_Nodes()
    Allocate(this%Vecb(N_Nodes))

# ifndef PETSC 
    call this%CRS%construct(N_Nodes)
# endif

# ifdef PETSC 
    !!Initialize PETSc
    call PETSc_Init()

    !!Generate the required PETSc Matrices and Vectors for the problem
    call this%pMatA%Create(N_Nodes,N_Nodes,5)
    call this%pVecb%Create(N_Nodes)
    call this%pVecx%Create(N_Nodes)
# endif

End Subroutine Create


Subroutine Solve(this,Material,Problem,Vecx)
    !!Generate the system of equations to be fed into the solver
    class(t_DDiff) :: this
    type(t_material), dimension(:) :: Material 
    type(t_Problem) :: Problem
# ifndef PETSC
    type(t_Solver) :: Solver
# endif 
# ifdef PETSC
    type(t_PETScSolver) :: PETScSolver
# endif
    Integer :: ii, jj, N_Regions, N_Nodes, NodeID
    Integer, allocatable, dimension(:) :: RegionNodes
    Real(kind=dp) :: Delta, Delta_prev
    Real(kind=dp), allocatable, dimension(:) :: Boundary_Pos, Sig_a, Source, Vecx


    !!Extract relevant data from the Problem specification
    N_Regions = Problem%GetN_Regions()
    Allocate(RegionNodes(N_Regions),Boundary_Pos(N_Regions))
    Boundary_Pos = Problem%GetBoundary_Pos()
    RegionNodes = Problem%GetNodes()
    N_Nodes = Problem%GetN_Nodes()
    Allocate(Vecx(N_Nodes))

    !!Extract relevant material data from the materials class
    Allocate(Sig_a(N_Regions),Source(N_Regions))
    Do ii = 1, N_Regions
        Sig_a(ii) = Material(ii)%GetSig_a()
        Source(ii) = Material(ii)%GetS()
    EndDo


    !!Non-PETSc Implementation
# ifndef PETSC 
    !!Fill CRS Matrices with problem information
    !!Loop over each region within the geometry
    NodeID = 0
    Do ii = 1, N_Regions
        !!Loop over each node within the region
        !!Calculate the delta value between each node in the region
        Delta = (Boundary_Pos(ii+1)-Boundary_Pos(ii))/Real(RegionNodes(ii)-1,dp)
        !!Initial Node
        NodeID = NodeID + 1
        If (ii /= 1) Then
            Delta_prev = (Boundary_Pos(ii)-Boundary_Pos(ii-1))/Real(RegionNodes(ii-1)-1,dp)
            call this%CRS%insert(NodeID,NodeID,(0.5_dp*((Delta*Sig_a(ii))+(Delta_prev*Sig_a(ii-1)))))
            this%Vecb(NodeID) = (0.5_dp*((Delta*Source(ii))+(Delta_prev*Source(ii-1))))
        Else 
            call this%CRS%insert(NodeID,NodeID,Delta*Sig_a(ii))
            this%Vecb(NodeID) = Delta*Source(ii)
        EndIf
        !!Loop up to boundary
        Do jj = 1, RegionNodes(ii)-2
            !!Use Delta and material properties to fill the matrix and vector
            NodeID = NodeID + 1
            call this%CRS%insert(NodeID,NodeID,Delta*Sig_a(ii))
            this%Vecb(NodeID) = Delta*Source(ii)
        EndDo
    EndDo
    !!Final Node
    NodeID = NodeID + 1
    call this%CRS%insert(NodeID,NodeID,Delta*Sig_a(N_Regions))
    this%Vecb(NodeID) = Delta*Source(N_Regions)

    !!Solve the problem
    call Solver%solve(this%CRS,this%Vecb,N_Nodes,Vecx)
# endif


    !!PETSc Implementation
# ifdef PETSC 
    !!Fill PETSc Matrices and Vectors with problem information
    !!Loop over each region within the geometry
    NodeID = 0
    Do ii = 1, N_Regions
        !!Loop over each node within the region
        !!Calculate the delta value between each node in the region
        Delta = (Boundary_Pos(ii+1)-Boundary_Pos(ii))/Real(RegionNodes(ii)-1,dp)
        !!Initial Node
        NodeID = NodeID + 1
        If (ii /= 1) Then
            Delta_prev = (Boundary_Pos(ii)-Boundary_Pos(ii-1))/Real(RegionNodes(ii-1)-1,dp)
            call this%pMatA%InsertVal(NodeID,NodeID,(0.5_dp*((Delta*Sig_a(ii))+(Delta_prev*Sig_a(ii-1)))))
            this%Vecb(NodeID) = (0.5_dp*((Delta*Source(ii))+(Delta_prev*Source(ii-1))))
        Else 
            call this%pMatA%InsertVal(NodeID,NodeID,Delta*Sig_a(ii))
            this%Vecb(NodeID) = Delta*Source(ii)
        EndIf
        !!Loop up to boundary
        Do jj = 1, RegionNodes(ii)-2
            !!Use Delta and material properties to fill the matrix and vector
            NodeID = NodeID + 1
            call this%pMatA%InsertVal(NodeID,NodeID,Delta*Sig_a(ii))
            this%Vecb(NodeID) = Delta*Source(ii)
        EndDo
    EndDo
    !!Final Node
    NodeID = NodeID + 1
    call this%pMatA%InsertVal(NodeID,NodeID,Delta*Sig_a(N_Regions))
    this%Vecb(NodeID) = Delta*Source(N_Regions)
    call this%pVecb%ConvTo(this%Vecb)

    !!Solve the problem
    call this%pMatA%Assemble()
    call this%pVecb%Assemble()
    call PETScSolver%Solve(this%pMatA,this%pVecb,this%pVecx)
    call this%pVecx%ConvFrom(Vecx)

# endif

    Deallocate(RegionNodes,Boundary_Pos)
    Deallocate(Sig_a,Source)
End Subroutine Solve


Subroutine Destroy(this,Flux)
    !!Dismantle Problem
    class(t_DDiff) :: this
    Real(kind=dp), allocatable, dimension(:) :: Flux

    If(Allocated(Flux)) Deallocate(Flux)
    If(Allocated(this%Vecb)) Deallocate(this%Vecb)

# ifndef PETSC 
    call this%CRS%destroy()
    
# endif

# ifdef PETSC 
    !!Destroy the PETSc Matrices and Vectors
    call this%pMatA%Destroy()
    call this%pVecb%Destroy()
    call this%pVecx%Destroy()
# endif

End Subroutine Destroy

End Module
