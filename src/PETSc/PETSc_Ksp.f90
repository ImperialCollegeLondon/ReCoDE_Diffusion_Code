Module PETSc_Ksp_Mod

#include <petsc/finclude/petscksp.h>
#include <petsc/finclude/petscsys.h>
    use petscSys
    use petscKsp
    use Constants_Mod
    use PETSc_Mat_Mod
    use PETSc_Vec_Mod
  Implicit None
  type :: pSol
    type(tksp) :: ksp
    Integer :: Max_Its
    Real(kind=dp) :: Rel_Tol, Abs_Tol
  contains
    procedure :: Create => Create_PETSc_Ksp
    procedure :: Destroy => Destroy_PETSc_Ksp
    procedure :: Analysis => Analysis_PETSc_Ksp
    procedure :: Solve => Solve_PETSc_Ksp
  end type

contains



Subroutine Create_PETSc_Ksp(This,A,PC_type,Rel_Tol,Abs_Tol,Max_Its)
  class(pSol) :: This
  class(pMat) :: A
  PC               Precon
  PetscErrorCode ierr
  Real(kind=dp) :: Rel_Tol, Abs_Tol
  Integer :: Max_Its
  Character :: PC_type

  This%Rel_Tol = Rel_Tol
  This%Abs_Tol = Abs_Tol
  This%Max_Its = Max_Its

  call KSPCreate(PETSC_COMM_WORLD,This%ksp,ierr)
  call KSPSetOperators(This%ksp,A%mat,A%mat,ierr)
  call KSPGetPC(This%ksp,Precon,ierr)
  select case(PC_Type)
  case('J')
    call PCSetType(precon,PCJACOBI,ierr)
  case('C')
    call PCSetType(precon,PCICC,ierr)
  case('L')
    call PCSetType(precon,PCILU,ierr)
  case('H')
    call PCSetType(precon,PCHYPRE,ierr)
  case('N')
    call PCSetType(precon,PCNONE,ierr)
  end select
  call KSPSetTolerances(This%ksp,This%Rel_Tol,This%Abs_Tol,PETSC_DEFAULT_REAL,This%Max_Its,ierr)
  call KSPSetFromOptions(This%ksp,ierr)
End Subroutine Create_PETSc_Ksp


Subroutine Destroy_PETSc_Ksp(This)
  class(pSol) :: This
  PetscErrorCode ierr
  call KSPDestroy(This%ksp,ierr)
End Subroutine Destroy_PETSc_Ksp


Subroutine Analysis_PETSc_Ksp(This)
  class(pSol) :: This
  PetscErrorCode ierr
  Integer :: Its, Reason
  Real(kind=dp) :: RNorm
  Call KSPGetIterationNumber(This%ksp,Its,ierr)
  Call KSPGetResidualNorm(This%ksp,RNorm,ierr)
  Call KSPGetConvergedReason(This%ksp,Reason,ierr)
  If (Reason .LT. 0) Then
    Write(*,*) "---KSP Convergence Failed---"
    Write(*,*) "Failed after iterations:", Its, "with residual norm:", RNorm, "for reason:", Reason
    Write(*,*) "----------------------------"
    Select Case(Reason)
    Case(-3)
      Write(*,*) "Reason => Did not converge after required iterations"
      Write(*,*) "-------------------------------"
    Case(-4)
      Write(*,*) "Reason => Residual norm increased by Divtol"
      Write(*,*) "-------------------------------"
    Case(-5)
      Write(*,*) "Reason => Breakdown in method"
      Write(*,*) "-------------------------------"
    Case(-6)
      Write(*,*) "Reason => Initial residual orth to preconditioned initial residual"
      Write(*,*) "-------------------------------"
    Case(-7)
      Write(*,*) "Reason => Asymmetric matrix"
      Write(*,*) "-------------------------------"
    Case(-9)
      Write(*,*) "Reason => Residual term becan NaN"
      Write(*,*) "-------------------------------"
    Case Default
      Write(*,*) "Reason => Description not implemented"
      Write(*,*) "-------------------------------"
    End Select
    Error Stop "KSP Convergence Failed"
  Else
    Write(*,*) "---KSP Convergence Succeeded---"
    Write(*,'(g0)',advance='no') "Succeeded after iterations:  "
    Write(*,'(g0)',advance='no') Its
    Write(*,'(g0)',advance='no') "  with residual norm:"
    Write(*,'(E14.6)',advance='no') RNorm
    Write(*,'(g0)',advance='no') "  for reason  :"
    Write(*,'(g0)') Reason
    Write(*,*) "-------------------------------"
    Select Case(Reason)
    Case(2)
      Write(*,*) "Reason => Passed Relative Tolerance"
      Write(*,*) "-------------------------------"
    Case(3)
      Write(*,*) "Reason => Passed Absolute Tolerance"
      Write(*,*) "-------------------------------"
    Case Default
      Write(*,*) "Reason => Description not implemented"
      Write(*,*) "-------------------------------"
    End Select
  EndIf


End Subroutine Analysis_PETSc_Ksp


Subroutine Solve_PETSc_Ksp(This,b,x)
  Class(pSol) :: This
  class(pVec) :: b,x
  PetscErrorCode ierr
  call KSpSolve(This%ksp,b%vec,x%vec,ierr); CHKERRQ(ierr)
#   ifdef DEBUG
      call This%analysis()
#   endif

End Subroutine Solve_PETSc_Ksp



End Module
