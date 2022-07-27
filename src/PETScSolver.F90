module PETScSolver_Mod

#ifdef PETSC
  use Constants_Mod
  use PETSc_Init_Mod
  use PETSc_Vec_Mod
  use PETSc_Mat_Mod
  use PETSc_Ksp_Mod
    !!Solves the system of equations with PETSc to generate a result which can be fed into the output module

  implicit none
  Private
  public :: PETSc_Solve

  type, public :: t_PETScSolver

  end type
contains

  subroutine PETSc_Solve(pMatA, pVecb, pVecx)
    type(pMat) :: pMatA
    type(pVec) :: pVecb, pVecx
    type(pSol) :: pSolve

    call pSolve%Create(pMatA, 'N', 1E-5_dp, 1E-5_dp, 100000)
    call pSolve%Solve(pVecb, pVecx)
    call pVecx%Assemble()

    call pSolve%Destroy()

  end subroutine PETSc_Solve
#endif

end module PETScSolver_Mod
