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
    use Solver_Mod
# endif
    use Output_Mod

    Implicit None

    !!Discretises the problem and sets up the system of equations which can then be solved
    type, public :: t_MatGen
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
    class(t_MatGen) :: this
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
    class(t_MatGen) :: this
    type(t_material), dimension(:) :: Material 
    type(t_Problem) :: Problem
# ifndef PETSC
    type(t_Solver) :: Solver
# endif 
# ifdef PETSC
    type(t_PETScSolver) :: PETScSolver
# endif
    Integer :: ii, jj, kk, N_Regions, N_Nodes, NodeID
    Integer, allocatable, dimension(:) :: RegionNodes
    Real(kind=dp) :: D_Value, Dm1_Value, Dp1_Value, Delta_Value, Sig_a_Value, Source_Value, a, b, c
    Real(kind=dp) :: Delta_old, Delta_prev
    Real(kind=dp), allocatable, dimension(:) :: Boundary_Pos, Delta, Sig_a, Source, Vecx


    !!Extract relevant data from the Problem specification
    N_Regions = Problem%GetN_Regions()
    Boundary_Pos = Problem%GetBoundary_Pos()
    RegionNodes = Problem%GetNodes()
    N_Nodes = Problem%GetN_Nodes()
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

    ! Write(*,*) "Boundary_Pos", Boundary_Pos
    ! Write(*,*) "RegionNodes", RegionNodes
    ! Write(*,*) "Delta", Delta
    ! Write(*,*) "Sig_a", Sig_a
    ! Write(*,*) "Source", Source


    !!Non-PETSc Implementation
# ifndef PETSC 
    !!Fill CRS Matrices with problem information
    !!Loop over each region within the geometry
    NodeID = 0
    Do ii = 1, N_Regions
        !!Loop over each node within the region (excluding last node)
        Do jj = 1, RegionNodes(ii)-1
            NodeID = NodeID + 1
            ! Write(*,*) "---"
            ! Write(*,*) "NodeID:", NodeID
            If ((jj == 1) .AND. (ii /= 1)) Then 
                !!First node in region (excluding first in problem)
                
                Sig_a_Value = .5_dp*(Sig_a(ii) + Sig_a(ii-1))
                Source_Value = .5_dp*(Source(ii) + Source(ii-1))
                Delta_Value = .5_dp*(Delta(ii) + Delta(ii-1))
                Dm1_Value = 1._dp/(3._dp*Sig_a(ii-1))
                D_Value = 1._dp/(3._dp*(.5_dp*(Sig_a(ii) + Sig_a(ii-1))))
                Dp1_Value = 1._dp/(3._dp*Sig_a(ii))

                ! Write(*,*) "First"
                ! Write(*,*) "Sig_a", Sig_a_Value
                ! Write(*,*) "Source", Source_Value
                ! Write(*,*) "Delta", Delta_Value
                ! Write(*,*) "D", Dm1_Value, D_Value, Dp1_Value
                ! Write(*,*) 
            Else
                !!General node
                
                Sig_a_Value = Sig_a(ii)
                Source_Value = Source(ii)
                Delta_Value = Delta(ii)
                Dm1_Value = 1._dp/(3._dp*Sig_a(ii))
                D_Value = 1._dp/(3._dp*Sig_a(ii))
                Dp1_Value = 1._dp/(3._dp*Sig_a(ii))

                ! Write(*,*) "General"
                ! Write(*,*) "Sig_a", Sig_a_Value
                ! Write(*,*) "Source", Source_Value
                ! Write(*,*) "Delta", Delta_Value
                ! Write(*,*) "D", Dm1_Value, D_Value, Dp1_Value
                ! Write(*,*) 
            EndIf

            a = -(.5_dp)*((D_Value+Dm1_Value)/(Delta_Value**2))
            b = Sig_a_Value + (.5_dp*((Dp1_Value+(2._dp*D_Value)+Dm1_Value)/(Delta_Value**2)))
            c = -(.5_dp)*((Dp1_Value+D_Value)/(Delta_Value**2))
            
            ! Write(*,*) "---"
            ! Write(*,*) "NodeID:", NodeID
            If (NodeID /= 1) Then
                call this%CRS%insert(NodeID,NodeID-1,a)
                ! Write(*,*) "A->", a, NodeID-1, NodeID, this%CRS%get(NodeID-1,NodeID)
                ! Write(*,*) this%CRS%get(2,3)
            EndIf
            call this%CRS%insert(NodeID,NodeID,b)
            ! Write(*,*) "B->", b, NodeID, NodeID
            call this%CRS%insert(NodeID,NodeID+1,c)
            ! Write(*,*) "C->", c, NodeID+1, NodeID
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

    ! Write(*,*) "---"
    ! Write(*,*) "NodeID:", NodeID
    ! Write(*,*) "A->", a, NodeID-1, NodeID
    ! Write(*,*) "B->", b, NodeID, NodeID
    call this%CRS%insert(NodeID,NodeID-1,a)
    call this%CRS%insert(NodeID,NodeID,b)
    this%Vecb(NodeID) = Source_Value

    !!Reflective modification
    call this%CRS%insert(1,2,(2._dp*this%CRS%get(1,2)))
    call this%CRS%insert(NodeID,NodeID-1,(2._dp*this%CRS%get(NodeID,NodeID-1)))

    !!Zero modification
    ! call this%CRS%insert(2,1,(1E2_dp*this%CRS%get(2,1)))
    ! call this%CRS%insert(NodeID-1,NodeID,(1E2_dp*this%CRS%get(NodeID-1,NodeID)))

    !!Prints matrix for debugging purposes
    ! kk = this%CRS%get_N_rows()
    ! Do ii = 1, kk
    !     Write(*,*) "-"
    !     Do jj = 1, kk
    !         Write(*,*) "Pos", ii, jj, "Val:", this%CRS%get(ii,jj)
    !     EndDo
    ! EndDo
    ! Write(*,*) "---b---"
    ! Write(*,*) this%Vecb

    !!Solve the problem
    call Solver%solve(this%CRS,this%Vecb,N_Nodes,Vecx)
    ! Write(*,*) "---x---"
    ! Write(*,*) Vecx
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
            ! Write(*,*) "---"
            ! Write(*,*) "NodeID:", NodeID, ii, jj
            If ((jj == 1) .AND. (ii /= 1)) Then 
                !!First node in region (excluding first in problem)
                
                Sig_a_Value = .5_dp*(Sig_a(ii) + Sig_a(ii-1))
                Source_Value = .5_dp*(Source(ii) + Source(ii-1))
                Delta_Value = .5_dp*(Delta(ii) + Delta(ii-1))
                Dm1_Value = 1._dp/(3._dp*Sig_a(ii-1))
                D_Value = 1._dp/(3._dp*(.5_dp*(Sig_a(ii) + Sig_a(ii-1))))
                Dp1_Value = 1._dp/(3._dp*Sig_a(ii))

                ! Write(*,*) "First"
                ! Write(*,*) "Sig_a", Sig_a_Value
                ! Write(*,*) "Source", Source_Value
                ! Write(*,*) "Delta", Delta_Value
                ! Write(*,*) "D", Dm1_Value, D_Value, Dp1_Value
                ! Write(*,*) 
            Else
                !!General node
                
                Sig_a_Value = Sig_a(ii)
                Source_Value = Source(ii)
                Delta_Value = Delta(ii)
                Dm1_Value = 1._dp/(3._dp*Sig_a(ii))
                D_Value = 1._dp/(3._dp*Sig_a(ii))
                Dp1_Value = 1._dp/(3._dp*Sig_a(ii))

                ! Write(*,*) "General"
                ! Write(*,*) "Sig_a", Sig_a_Value
                ! Write(*,*) "Source", Source_Value
                ! Write(*,*) "Delta", Delta_Value
                ! Write(*,*) "D", Dm1_Value, D_Value, Dp1_Value
                ! Write(*,*) 
            EndIf

            a = -(.5_dp)*((D_Value+Dm1_Value)/(Delta_Value**2))
            b = Sig_a_Value + (.5_dp*((Dp1_Value+(2._dp*D_Value)+Dm1_Value)/(Delta_Value**2)))
            c = -(.5_dp)*((Dp1_Value+D_Value)/(Delta_Value**2))
            
            ! Write(*,*) "---"
            ! Write(*,*) "NodeID:", NodeID
            If (NodeID /= 1) Then
                call this%pMatA%InsertVal(NodeID,NodeID-1,a)
                ! Write(*,*) "A->", a, NodeID-1, NodeID, this%CRS%get(NodeID-1,NodeID)
                ! Write(*,*) this%CRS%get(2,3)
            EndIf
            call this%pMatA%InsertVal(NodeID,NodeID,b)
            ! Write(*,*) "B->", b, NodeID, NodeID
            call this%pMatA%InsertVal(NodeID,NodeID+1,c)
            ! Write(*,*) "C->", c, NodeID+1, NodeID
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

    ! Write(*,*) "---"
    ! Write(*,*) "NodeID:", NodeID
    ! Write(*,*) "A->", a, NodeID-1, NodeID
    ! Write(*,*) "B->", b, NodeID, NodeID
    call this%pMatA%InsertVal(NodeID,NodeID-1,a)
    call this%pMatA%InsertVal(NodeID,NodeID,b)
    this%Vecb(NodeID) = Source_Value

    !!Reflective Boundary Conditions
    call this%pMatA%SwitchAssemble()
    !!Initial Boundary
    D_Value = 1._dp/(3._dp*Sig_a(1))
    Delta_Value = Delta(1)
    a = (-.5_dp)*((2._dp*D_Value)/(Delta_Value**2))
    call this%pMatA%AddVal(1,2,a)
    !!Final Boundary
    D_Value = 1._dp/(3._dp*Sig_a(N_Regions))
    Delta_Value = Delta(N_Regions)
    a = (-.5_dp)*((2._dp*D_Value)/(Delta_Value**2))
    call this%pMatA%AddVal(NodeID,NodeID-1,a)


    call this%pVecb%ConvTo(this%Vecb)
    call this%pMatA%Assemble()
    call this%pVecb%Assemble()

    !!Prints matrix for debugging purposes
    ! call this%pMatA%MatView()
    ! call this%pVecb%VecView()

    !!Solve the problem
    
    call PETScSolver%Solve(this%pMatA,this%pVecb,this%pVecx)
    call this%pVecx%ConvFrom(Vecx)
    call this%pVecx%VecView()
# endif


!     !!PETSc Implementation
! # ifdef PETSC 
!     !!Fill PETSc Matrices and Vectors with problem information
!     !!Loop over each region within the geometry
!     NodeID = 0
!     Do ii = 1, N_Regions
!         !!Loop over each node within the region
!         !!Calculate the delta value between each node in the region
!         Delta_old = (Boundary_Pos(ii+1)-Boundary_Pos(ii))/Real(RegionNodes(ii)-1,dp)

!         Do jj = 1, RegionNodes(ii)-1
!         NodeID = NodeID + 1
!             ! Write(*,*) "---"
!             ! Write(*,*) "NodeID:", NodeID
!             If ((jj == 1) .AND. (ii /= 1)) Then 
!                 !!First node in region (excluding first in problem)
                
!                 Sig_a_Value = .5_dp*(Sig_a(ii) + Sig_a(ii-1))
!                 Source_Value = .5_dp*(Source(ii) + Source(ii-1))
!                 Delta_Value = .5_dp*(Delta(ii) + Delta(ii-1))
!                 Dm1_Value = 1._dp/(3._dp*Sig_a(ii-1))
!                 D_Value = 1._dp/(3._dp*(.5_dp*(Sig_a(ii) + Sig_a(ii-1))))
!                 Dp1_Value = 1._dp/(3._dp*Sig_a(ii))

!                 ! Write(*,*) "First"
!                 ! Write(*,*) "Sig_a", Sig_a_Value
!                 ! Write(*,*) "Source", Source_Value
!                 ! Write(*,*) "Delta", Delta_Value
!                 ! Write(*,*) "D", Dm1_Value, D_Value, Dp1_Value
!                 ! Write(*,*) 
!             Else
!                 !!General node
                
!                 Sig_a_Value = Sig_a(ii)
!                 Source_Value = Source(ii)
!                 Delta_Value = Delta(ii)
!                 Dm1_Value = 1._dp/(3._dp*Sig_a(ii))
!                 D_Value = 1._dp/(3._dp*Sig_a(ii))
!                 Dp1_Value = 1._dp/(3._dp*Sig_a(ii))

!                 ! Write(*,*) "General"
!                 ! Write(*,*) "Sig_a", Sig_a_Value
!                 ! Write(*,*) "Source", Source_Value
!                 ! Write(*,*) "Delta", Delta_Value
!                 ! Write(*,*) "D", Dm1_Value, D_Value, Dp1_Value
!                 ! Write(*,*) 
!             EndIf

!             a = -(.5_dp)*((D_Value+Dm1_Value)/(Delta_Value**2))
!             b = Sig_a_Value + (.5_dp*((Dp1_Value+(2._dp*D_Value)+Dm1_Value)/(Delta_Value**2)))
!             c = -(.5_dp)*((Dp1_Value+D_Value)/(Delta_Value**2))
!             If (NodeID /= 1) Then
!                 call this%CRS%insert(NodeID-1,NodeID,a)
!                 ! Write(*,*) "A->", a, NodeID-1, NodeID, this%CRS%get(NodeID-1,NodeID)
!                 ! Write(*,*) this%CRS%get(2,3)
!             EndIf
!             call this%CRS%insert(NodeID,NodeID,b)
!             ! Write(*,*) "B->", b, NodeID, NodeID
!             call this%CRS%insert(NodeID+1,NodeID,c)
!             ! Write(*,*) "C->", c, NodeID+1, NodeID
!             this%Vecb(NodeID) = Source_Value
!         EndDo
!     EndDo
!     !!Final Node
!     NodeID = NodeID + 1
!     call this%pMatA%InsertVal(NodeID,NodeID,Delta_old*Sig_a(N_Regions))
!     this%Vecb(NodeID) = Delta_old*Source(N_Regions)
!     call this%pVecb%ConvTo(this%Vecb)

!     !!Solve the problem
!     call this%pMatA%Assemble()
!     call this%pVecb%Assemble()
!     call PETScSolver%Solve(this%pMatA,this%pVecb,this%pVecx)
!     call this%pVecx%ConvFrom(Vecx)

! # endif

!     !!PETSc Implementation
! # ifdef PETSC 
!     !!Fill PETSc Matrices and Vectors with problem information
!     !!Loop over each region within the geometry
!     NodeID = 0
!     Do ii = 1, N_Regions
!         !!Loop over each node within the region
!         !!Calculate the delta value between each node in the region
!         Delta_old = (Boundary_Pos(ii+1)-Boundary_Pos(ii))/Real(RegionNodes(ii)-1,dp)
!         !!Initial Node
!         NodeID = NodeID + 1
!         If (ii /= 1) Then
!             Delta_prev = (Boundary_Pos(ii)-Boundary_Pos(ii-1))/Real(RegionNodes(ii-1)-1,dp)
!             call this%pMatA%InsertVal(NodeID,NodeID,(0.5_dp*((Delta_old*Sig_a(ii))+(Delta_prev*Sig_a(ii-1)))))
!             this%Vecb(NodeID) = (0.5_dp*((Delta_old*Source(ii))+(Delta_prev*Source(ii-1))))
!         Else 
!             call this%pMatA%InsertVal(NodeID,NodeID,Delta_old*Sig_a(ii))
!             this%Vecb(NodeID) = Delta_old*Source(ii)
!         EndIf
!         !!Loop up to boundary
!         Do jj = 1, RegionNodes(ii)-2
!             !!Use Delta and material properties to fill the matrix and vector
!             NodeID = NodeID + 1
!             call this%pMatA%InsertVal(NodeID,NodeID,Delta_old*Sig_a(ii))
!             this%Vecb(NodeID) = Delta_old*Source(ii)
!         EndDo
!     EndDo
!     !!Final Node
!     NodeID = NodeID + 1
!     call this%pMatA%InsertVal(NodeID,NodeID,Delta_old*Sig_a(N_Regions))
!     this%Vecb(NodeID) = Delta_old*Source(N_Regions)
!     call this%pVecb%ConvTo(this%Vecb)

!     !!Solve the problem
!     call this%pMatA%Assemble()
!     call this%pVecb%Assemble()
!     call PETScSolver%Solve(this%pMatA,this%pVecb,this%pVecx)
!     call this%pVecx%ConvFrom(Vecx)

! # endif

End Subroutine Solve


Subroutine Destroy(this,Flux)
    !!Dismantle Problem
    class(t_MatGen) :: this
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
