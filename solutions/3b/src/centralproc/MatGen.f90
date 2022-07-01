Module MatGen_Mod

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
    use CDS_Mod
    use Solver_Mod
# endif
    use Output_Mod

    Implicit None

    !!Discretises the problem and sets up the system of equations which can then be solved
    type, public :: t_MatGen
        Integer :: N_Regions
        Real(kind=dp), allocatable, dimension(:) :: Vecb
# ifndef PETSC
        class(t_matrix_base), pointer :: matrix
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
    class(t_MatGen) :: this
    type(t_Problem) :: Problem
    Integer :: N_Nodes


    !!Extract relevant data from the Problem specification
    N_Nodes = Problem%GetN_Nodes()
    Allocate(this%Vecb(N_Nodes))

# ifndef PETSC 
    !! Allocate the matrix to be a CRS matrix
    allocate(t_cds :: this%matrix)
    !! Construct the matrix
    call this%matrix%construct(N_Nodes, N_Nodes)
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
    class(t_MatGen) :: this
    type(t_material), dimension(:) :: Material 
    type(t_Problem) :: Problem
    Integer :: ii, jj, N_Regions, N_Nodes, NodeID
    Integer, allocatable, dimension(:) :: RegionNodes
    Integer, dimension(2) :: Boundary_Conditions
    Real(kind=dp) :: D_Value, Dm1_Value, Dp1_Value, Delta_Value, Sig_a_Value, Source_Value, a, b, c
    Real(kind=dp), allocatable, dimension(:) :: Boundary_Pos, Delta, Sig_a, Source, Vecx


    !!Extract relevant data from the Problem specification
    N_Regions = Problem%GetN_Regions()
    Boundary_Pos = Problem%GetBoundary_Pos()
    RegionNodes = Problem%GetNodes()
    N_Nodes = Problem%GetN_Nodes()
    Boundary_Conditions = Problem%GetBoundary_Conditions()
    Allocate(Delta(N_Regions),Vecx(N_Nodes))

    !!Calculate Delta throughout problem
    Do ii = 1, N_Regions
        Delta(ii) = (Boundary_Pos(ii+1)-Boundary_Pos(ii))/Real(RegionNodes(ii)-1,dp)
    EndDo

    !!Extract relevant material data from the materials class
    Allocate(Sig_a(N_Regions),Source(N_Regions))
    Do ii = 1, N_Regions
        Sig_a(ii) = Material(ii)%GetSig_a()
        Source(ii) = Material(ii)%GetS()
    EndDo

    !!Non-PETSc Implementation
# ifndef PETSC 
    !!Fill matrix with problem information
    !!Loop over each region within the geometry
    NodeID = 0
    Do ii = 1, N_Regions
        !!Loop over each node within the region (excluding last node)
        Do jj = 1, RegionNodes(ii)-1
            NodeID = NodeID + 1
            If ((jj == 1) .AND. (ii /= 1)) Then 
                !!First node in region (excluding first in problem)
                Sig_a_Value = .5_dp*(Sig_a(ii) + Sig_a(ii-1))
                Source_Value = .5_dp*(Source(ii) + Source(ii-1))
                Delta_Value = .5_dp*(Delta(ii) + Delta(ii-1))
                Dm1_Value = 1._dp/(3._dp*Sig_a(ii-1))
                D_Value = 1._dp/(3._dp*(.5_dp*(Sig_a(ii) + Sig_a(ii-1))))
                Dp1_Value = 1._dp/(3._dp*Sig_a(ii))
            Else
                !!General node
                Sig_a_Value = Sig_a(ii)
                Source_Value = Source(ii)
                Delta_Value = Delta(ii)
                Dm1_Value = 1._dp/(3._dp*Sig_a(ii))
                D_Value = 1._dp/(3._dp*Sig_a(ii))
                Dp1_Value = 1._dp/(3._dp*Sig_a(ii))
            EndIf
            a = -(.5_dp)*((D_Value+Dm1_Value)/(Delta_Value**2))
            b = Sig_a_Value + (.5_dp*((Dp1_Value+(2._dp*D_Value)+Dm1_Value)/(Delta_Value**2)))
            c = -(.5_dp)*((Dp1_Value+D_Value)/(Delta_Value**2))
            If (NodeID /= 1) Then
                call this%matrix%set(NodeID,NodeID-1,a)
            EndIf
            call this%matrix%set(NodeID,NodeID,b)
            call this%matrix%set(NodeID,NodeID+1,c)
            this%Vecb(NodeID) = Source_Value

        EndDo 
    EndDo
    !!Final Node
    NodeID = NodeID + 1
    Sig_a_Value = Sig_a(N_Regions)
    Source_Value = Source(N_Regions)
    Delta_Value = Delta(N_Regions)
    Dm1_Value = 1._dp/(3._dp*Sig_a(N_Regions))
    D_Value = 1._dp/(3._dp*Sig_a(N_Regions))
    Dp1_Value = 1._dp/(3._dp*Sig_a(N_Regions))

    a = -(.5_dp)*((D_Value+Dm1_Value)/(Delta_Value**2))
    b = Sig_a_Value + (.5_dp*((Dp1_Value+(2._dp*D_Value)+Dm1_Value)/(Delta_Value**2)))
    call this%matrix%set(NodeID,NodeID-1,a)
    call this%matrix%set(NodeID,NodeID,b)
    this%Vecb(NodeID) = Source_Value

    !!Boundary Conditions
    If (Boundary_Conditions(1) == 0) Then
        call this%matrix%set(1,1,(1E2_dp*this%matrix%get(1,1)))
    ElseIf (Boundary_Conditions(1) == 1) Then 
        call this%matrix%set(1,2,(2._dp*this%matrix%get(1,2)))
    EndIf
    If (Boundary_Conditions(2) == 0) Then
        call this%matrix%set(NodeID,NodeID,(1E2_dp*this%matrix%get(NodeID,NodeID)))
    ElseIf (Boundary_Conditions(2) == 1) Then 
        call this%matrix%set(NodeID,NodeID-1,(2._dp*this%matrix%get(NodeID,NodeID-1)))
    EndIf

    !!Solve the problem
    call ThomAlg_Solve(this%matrix,this%Vecb,N_Nodes,Vecx)
    !!CG Alg Version
    ! call CG_Solve(this%matrix,this%Vecb,N_Nodes,Vecx)
# endif


    !PETSc Implementation
# ifdef PETSC  
    !!Fill PETSc Matrices and Vectors with problem information
    !!Loop over each region within the geometry
    NodeID = 0
    Do ii = 1, N_Regions
        !!Loop over each node within the region (excluding last node)
        Do jj = 1, RegionNodes(ii)-1
            NodeID = NodeID + 1
            If ((jj == 1) .AND. (ii /= 1)) Then 
                !!First node in region (excluding first in problem)
                Sig_a_Value = .5_dp*(Sig_a(ii) + Sig_a(ii-1))
                Source_Value = .5_dp*(Source(ii) + Source(ii-1))
                Delta_Value = .5_dp*(Delta(ii) + Delta(ii-1))
                Dm1_Value = 1._dp/(3._dp*Sig_a(ii-1))
                D_Value = 1._dp/(3._dp*(.5_dp*(Sig_a(ii) + Sig_a(ii-1))))
                Dp1_Value = 1._dp/(3._dp*Sig_a(ii))
            Else
                !!General node
                Sig_a_Value = Sig_a(ii)
                Source_Value = Source(ii)
                Delta_Value = Delta(ii)
                Dm1_Value = 1._dp/(3._dp*Sig_a(ii))
                D_Value = 1._dp/(3._dp*Sig_a(ii))
                Dp1_Value = 1._dp/(3._dp*Sig_a(ii))
            EndIf

            a = -(.5_dp)*((D_Value+Dm1_Value)/(Delta_Value**2))
            b = Sig_a_Value + (.5_dp*((Dp1_Value+(2._dp*D_Value)+Dm1_Value)/(Delta_Value**2)))
            c = -(.5_dp)*((Dp1_Value+D_Value)/(Delta_Value**2))
            
            If (NodeID /= 1) Then
                call this%pMatA%InsertVal(NodeID,NodeID-1,a)
            EndIf
            call this%pMatA%InsertVal(NodeID,NodeID,b)
            call this%pMatA%InsertVal(NodeID,NodeID+1,c)
            this%Vecb(NodeID) = Source_Value
        EndDo 
    EndDo
    !!Final Node
    NodeID = NodeID + 1
    Sig_a_Value = Sig_a(N_Regions)
    Source_Value = Source(N_Regions)
    Delta_Value = Delta(N_Regions)
    Dm1_Value = 1._dp/(3._dp*Sig_a(N_Regions))
    D_Value = 1._dp/(3._dp*Sig_a(N_Regions))
    Dp1_Value = 1._dp/(3._dp*Sig_a(N_Regions))

    a = (-.5_dp)*((D_Value+Dm1_Value)/(Delta_Value**2))
    b = Sig_a_Value + (.5_dp*((Dp1_Value+(2._dp*D_Value)+Dm1_Value)/(Delta_Value**2)))
    call this%pMatA%InsertVal(NodeID,NodeID-1,a)
    call this%pMatA%InsertVal(NodeID,NodeID,b)
    this%Vecb(NodeID) = Source_Value

    !!Boundary Conditions
    If (Boundary_Conditions(1) == 0) Then
        Dm1_Value = 1._dp/(3._dp*Sig_a(1))
        D_Value = 1._dp/(3._dp*Sig_a(1))
        Dp1_Value = 1._dp/(3._dp*Sig_a(1))
        b = Sig_a(1) + (.5_dp*((Dp1_Value+(2._dp*D_Value)+Dm1_Value)/(Delta(1))**2))
        call this%pMatA%InsertVal(1,1,1E3_dp*b)
    ElseIf (Boundary_Conditions(1) == 1) Then 
        D_Value = 1._dp/(3._dp*Sig_a(1))
        a = (-.5_dp)*((2._dp*D_Value)/(Delta(1)**2))
        call this%pMatA%InsertVal(1,2,2._dp*a)
        
    EndIf
    If (Boundary_Conditions(2) == 0) Then
        Dm1_Value = 1._dp/(3._dp*Sig_a(N_Regions))
        D_Value = 1._dp/(3._dp*Sig_a(N_Regions))
        Dp1_Value = 1._dp/(3._dp*Sig_a(N_Regions))
        b = Sig_a(N_Regions) + (.5_dp*((Dp1_Value+(2._dp*D_Value)+Dm1_Value)/(Delta(N_Regions))**2))
        call this%pMatA%InsertVal(NodeID,NodeID,1E3_dp*b)
    ElseIf (Boundary_Conditions(2) == 1) Then 
        D_Value = 1._dp/(3._dp*Sig_a(N_Regions))
        a = (-.5_dp)*((2._dp*D_Value)/(Delta(N_Regions)**2))
        call this%pMatA%InsertVal(NodeID,NodeID-1,2._dp*a)
    EndIf
    
    !!Assemble PETSc Matrix and Vector
    call this%pVecb%ConvTo(this%Vecb)
    call this%pMatA%Assemble()
    call this%pVecb%Assemble()

    !!Solve the problem
    call PETSc_Solve(this%pMatA,this%pVecb,this%pVecx)
    call this%pVecx%ConvFrom(Vecx)
# endif


End Subroutine Solve


Subroutine Destroy(this,Flux)
    !!Dismantle Problem
    class(t_MatGen) :: this
    Real(kind=dp), allocatable, dimension(:) :: Flux

    If(Allocated(Flux)) Deallocate(Flux)
    If(Allocated(this%Vecb)) Deallocate(this%Vecb)

# ifndef PETSC 
    call this%matrix%destroy()
    
# endif

# ifdef PETSC 
    !!Destroy the PETSc Matrices and Vectors
    call this%pMatA%Destroy()
    call this%pVecb%Destroy()
    call this%pVecx%Destroy()
# endif

End Subroutine Destroy

End Module
