module PETSc_Ksp_Mod

#ifdef PETSC
#include <petsc/finclude/petscksp.h>
#include <petsc/finclude/petscsys.h>
  use petscSys
  use petscKsp
  use Constants_Mod
  use PETSc_Mat_Mod
  use PETSc_Vec_Mod
  
  implicit none
  private
  public :: pSol

  type :: pSol
    type(tksp) :: ksp
    integer :: Max_Its
    real(kind=dp) :: Rel_Tol, Abs_Tol
  contains
    procedure :: Create => Create_PETSc_Ksp
    procedure :: Destroy => Destroy_PETSc_Ksp
    procedure :: Analysis => Analysis_PETSc_Ksp
    procedure :: Solve => Solve_PETSc_Ksp
  end type

contains

  subroutine Create_PETSc_Ksp(This, A, PC_type, Rel_Tol, Abs_Tol, Max_Its)
    class(pSol) :: This
    class(pMat) :: A
    PC Precon
    PetscErrorCode :: ierr
    real(kind=dp) :: Rel_Tol, Abs_Tol
    integer :: Max_Its
    character :: PC_type

    This%Rel_Tol = Rel_Tol
    This%Abs_Tol = Abs_Tol
    This%Max_Its = Max_Its

    call KSPCreate(PETSC_COMM_WORLD, This%ksp, ierr)
    call KSPSetOperators(This%ksp, A%mat, A%mat, ierr)
    call KSPGetPC(This%ksp, Precon, ierr)
    select case (PC_Type)
    case ('J')
      call PCSetType(precon, PCJACOBI, ierr)
    case ('C')
      call PCSetType(precon, PCICC, ierr)
    case ('L')
      call PCSetType(precon, PCILU, ierr)
    case ('H')
      call PCSetType(precon, PCHYPRE, ierr)
    case ('N')
      call PCSetType(precon, PCNONE, ierr)
    end select
    call KSPSetTolerances(This%ksp, This%Rel_Tol, This%Abs_Tol, PETSC_DEFAULT_REAL, This%Max_Its, ierr)
    call KSPSetFromOptions(This%ksp, ierr)
  end subroutine Create_PETSc_Ksp

  subroutine Destroy_PETSc_Ksp(This)
    class(pSol) :: This
    PetscErrorCode :: ierr
    call KSPDestroy(This%ksp, ierr)
  end subroutine Destroy_PETSc_Ksp

  subroutine Analysis_PETSc_Ksp(This)
    class(pSol) :: This
    PetscErrorCode :: ierr
    integer :: Its, Reason
    real(kind=dp) :: RNorm
    call KSPGetIterationNumber(This%ksp, Its, ierr)
    call KSPGetResidualNorm(This%ksp, RNorm, ierr)
    call KSPGetConvergedReason(This%ksp, Reason, ierr)
    if (Reason < 0) then
      write(output_unit, *) "---KSP Convergence Failed---"
      write(output_unit, *) "Failed after iterations:", Its, "with residual norm:", RNorm, "for reason:", Reason
      write(output_unit, *) "----------------------------"
      select case (Reason)
      case (-3)
        write(output_unit, *) "Reason => Did not converge after required iterations"
        write(output_unit, *) "-------------------------------"
      case (-4)
        write(output_unit, *) "Reason => Residual norm increased by Divtol"
        write(output_unit, *) "-------------------------------"
      case (-5)
        write(output_unit, *) "Reason => Breakdown in method"
        write(output_unit, *) "-------------------------------"
      case (-6)
        write(output_unit, *) "Reason => Initial residual orth to preconditioned initial residual"
        write(output_unit, *) "-------------------------------"
      case (-7)
        write(output_unit, *) "Reason => Asymmetric matrix"
        write(output_unit, *) "-------------------------------"
      case (-9)
        write(output_unit, *) "Reason => Residual term becan NaN"
        write(output_unit, *) "-------------------------------"
      case default
        write(output_unit, *) "Reason => Description not implemented"
        write(output_unit, *) "-------------------------------"
      end select
      error stop "KSP Convergence Failed"
    else
      write(output_unit, *) "---KSP Convergence Succeeded---"
      write(output_unit, '(g0)', advance='no') "Succeeded after iterations:  "
      write(output_unit, '(g0)', advance='no') Its
      write(output_unit, '(g0)', advance='no') "  with residual norm:"
      write(output_unit, '(E14.6)', advance='no') RNorm
      write(output_unit, '(g0)', advance='no') "  for reason  :"
      write(output_unit, '(g0)') Reason
      write(output_unit, *) "-------------------------------"
      select case (Reason)
      case (2)
        write(output_unit, *) "Reason => Passed Relative Tolerance"
        write(output_unit, *) "-------------------------------"
      case (3)
        write(output_unit, *) "Reason => Passed Absolute Tolerance"
        write(output_unit, *) "-------------------------------"
      case default
        write(output_unit, *) "Reason => Description not implemented"
        write(output_unit, *) "-------------------------------"
      end select
    end if

  end subroutine Analysis_PETSc_Ksp

  subroutine Solve_PETSc_Ksp(This, b, x)
    class(pSol) :: This
    class(pVec) :: b, x
    PetscErrorCode :: ierr
    call KSpSolve(This%ksp, b%vec, x%vec, ierr); CHKERRQ(ierr)
#   ifdef DEBUG
    call This%analysis()
#   endif

  end subroutine Solve_PETSc_Ksp
#endif

end module PETSc_Ksp_Mod
