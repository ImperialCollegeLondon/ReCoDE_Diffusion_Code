Module PETScSolver_Mod

    use Constants_Mod
    use PETSc_Init_Mod
    use PETSc_Vec_Mod
    use PETSc_Mat_Mod
    use PETSc_Ksp_Mod
    !!Solves the system of equations with PETSc to generate a result which can be fed into the output module

    Implicit None
    Private

    type, public :: t_PETScSolver
    contains
        procedure, public :: Solve
    end type
contains

Subroutine Solve(this,pMatA,pVecb,pVecx)
    class(t_PETScSolver) :: this
    type(pMat) :: pMatA
    type(pVec) :: pVecb, pVecx
    type(pSol) :: pSolve
    
    call pSolve%Create(pMatA,'N',1E-5_dp,1E-5_dp,100000)
    call pSolve%Solve(pVecb,pVecx)
    call pVecx%Assemble()

    call pSolve%Destroy()

End Subroutine Solve


End Module
