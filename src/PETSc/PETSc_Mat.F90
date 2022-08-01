module PETSc_Mat_Mod

#ifdef PETSC
#include <petsc/finclude/petscmat.h>
#include <petsc/finclude/petscsys.h>
  use petscSys
  use petscMat
  use Constants_Mod
  use PETSc_Vec_Mod

  implicit none
  private
  public :: PMat

  type :: PMat
    type(tmat) :: mat
  contains
    procedure :: Create => Create_PETSc_Mat
    procedure :: Destroy => Destroy_PETSc_Mat
    procedure :: SwitchAssemble => SwitchAssemble_PETSc_Mat
    procedure :: Assemble => Assemble_PETSc_Mat
    procedure :: InsertVal => InsertVal_PETSc_Mat
    procedure :: AddVal => AddVal_PETSc_Mat
    procedure :: InsertMat => InsertMat_PETSc_Mat
    procedure :: AddMat => AddMat_PETSc_Mat
    procedure :: GetSizeMat => GetSizeMat_PETSc_Mat
    procedure :: GetValMat => GetValMat_PETSc_Mat
    procedure :: GetValsMat => GetValsMat_PETSc_Mat
    procedure :: MultMat => MultMat_PETSc_Mat
    procedure :: MultAddMat => MultAddMat_PETSc_Mat
    procedure :: MatView => MatView_PETSc_Mat
    procedure :: Symmetry => Symmetry_PETSc_Mat
  end type

contains

  subroutine Create_PETSc_Mat(This, N_row, N_col, N_nz)
    class(PMat), intent(inout) :: This
    integer :: N_row, N_col, N_nz
    PetscErrorCode :: ierr
    call MatCreateSeqAIJ(PETSC_COMM_SELF, N_row, N_col, N_nz, PETSC_NULL_INTEGER, This%mat, ierr)
  end subroutine Create_PETSc_Mat

  subroutine Destroy_PETSc_Mat(This)
    class(PMat), intent(inout) :: This
    PetscErrorCode :: ierr
    call MatDestroy(This%mat, ierr); CHKERRQ(ierr)
  end subroutine Destroy_PETSc_Mat

  subroutine SwitchAssemble_PETSc_Mat(This)
    class(PMat), intent(inout) :: This
    PetscErrorCode :: ierr
    call MatAssemblyBegin(This%mat, MAT_FLUSH_ASSEMBLY, ierr)
    call MatAssemblyend(This%mat, MAT_FLUSH_ASSEMBLY, ierr)
  end subroutine SwitchAssemble_PETSc_Mat

  subroutine Assemble_PETSc_Mat(This)
    class(PMat), intent(inout) :: This
    PetscErrorCode :: ierr
    call MatAssemblyBegin(This%mat, MAT_FINAL_ASSEMBLY, ierr)
    call MatAssemblyend(This%mat, MAT_FINAL_ASSEMBLY, ierr)
  end subroutine Assemble_PETSc_Mat

  subroutine InsertVal_PETSc_Mat(This, V_row, V_col, Value)
    class(PMat), intent(inout) :: This
    PetscErrorCode :: ierr
    integer :: V_row, V_Col
    real(kind=dp) :: Value
    if ((V_row < 1) .OR. (V_col < 1)) then
      write(output_unit, *) "### Error ###"
      write(output_unit, *) "Inserting Value outside of bounds"
      stop
    end if
  !!Convert the row and col integers to P format
    call MatSetValue(This%mat, V_row - 1, V_Col - 1, Value, INSERT_VALUES, ierr)
  end subroutine InsertVal_PETSc_Mat

  subroutine AddVal_PETSc_Mat(This, V_row, V_col, Value)
    class(PMat), intent(inout) :: This
    PetscErrorCode :: ierr
    integer :: V_row, V_Col
    real(kind=dp) :: Value
    if ((V_row < 1) .OR. (V_col < 1)) then
      write(output_unit, *) "### Error ###"
      error stop "Inserting Value outside of bounds"
    end if
  !!Convert the row and col integers to P format
    call MatSetValue(This%mat, V_row - 1, V_col - 1, Value, ADD_VALUES, ierr)
  end subroutine AddVal_PETSc_Mat

  subroutine InsertMat_PETSc_Mat(This, V_rows, V_cols, Values)
    class(PMat), intent(inout) :: This
    PetscErrorCode :: ierr
    integer :: N_rows, N_cols
    integer :: V_rows(:), V_cols(:)
    real(kind=dp) :: Values(:, :)
    N_rows = Size(V_rows)
    N_cols = Size(V_cols)
  !!Temporarily convert the row and col integers to P format
    V_rows(:) = V_rows(:) - 1
    V_cols(:) = V_cols(:) - 1
    call MatSetValues(This%mat, N_rows, V_rows, N_cols, V_cols, Values, INSERT_VALUES, ierr)
  !!Revert to F format
    V_rows(:) = V_rows(:) + 1
    V_cols(:) = V_cols(:) + 1
  end subroutine InsertMat_PETSc_Mat

  subroutine AddMat_PETSc_Mat(This, V_rows, V_cols, Values)
    class(PMat), intent(inout) :: This
    PetscErrorCode :: ierr
    integer :: N_rows, N_cols
    integer :: V_rows(:), V_cols(:)
    real(kind=dp) :: Values(:, :)
    N_rows = Size(V_rows)
    N_cols = Size(V_cols)
  !!Temporarily convert the row and col integers to P format
    V_rows(:) = V_rows(:) - 1
    V_cols(:) = V_cols(:) - 1
    call MatSetValues(This%mat, N_rows, V_rows, N_cols, V_cols, Values, ADD_VALUES, ierr)
  !!Revert to F format
    V_rows(:) = V_rows(:) + 1
    V_cols(:) = V_cols(:) + 1
  end subroutine AddMat_PETSc_Mat

  subroutine GetSizeMat_PETSc_Mat(This, N_rows, N_cols)
    class(PMat), intent(inout) :: This
    PetscErrorCode :: ierr
    integer :: N_rows, N_cols
    call MatGetSize(this%mat, N_rows, N_cols, ierr)
  end subroutine GetSizeMat_PETSc_Mat

  function GetValMat_PETSc_Mat(This, V_row, V_col) Result(Value)
    class(PMat), intent(inout) :: This
    PetscErrorCode :: ierr
    integer :: V_row, V_col
    real(kind=dp) :: Value
  !!Temporarily convert the row and col integers to P format
    V_row = V_row - 1
    V_col = V_col - 1
    call MatGetValues(This%mat, 1, V_row, 1, V_col, Value, ierr)
  !!Revert to F format
    V_row = V_row + 1
    V_col = V_col + 1
  end function GetValMat_PETSc_Mat

  subroutine GetValsMat_PETSc_Mat(This, V_rows, V_cols, Values)
    class(PMat), intent(inout) :: This
    PetscErrorCode :: ierr
    integer :: N_rows, N_cols
    integer :: V_rows(:), V_cols(:)
    real(kind=dp), dimension(:, :) :: Values
    N_rows = Size(V_rows)
    N_cols = Size(V_cols)
  !!Temporarily convert the row and col integers to P format
    V_rows(:) = V_rows(:) - 1
    V_cols(:) = V_cols(:) - 1
    call MatGetValues(This%mat, N_rows, V_rows, N_cols, V_cols, Values, ierr)
  !!Revert to F format
    V_rows(:) = V_rows(:) + 1
    V_cols(:) = V_cols(:) + 1
  end subroutine GetValsMat_PETSc_Mat

  subroutine MultMat_PETSc_Mat(This, In_Vec, Out_Vec)
    class(PMat), intent(inout) :: This
    class(pVec) :: In_Vec, Out_Vec
    PetscErrorCode :: ierr
    call MatMult(This%mat, In_Vec%vec, Out_Vec%vec, ierr)
  end subroutine MultMat_PETSc_Mat

  subroutine MultAddMat_PETSc_Mat(This, In_Vec, Add_Vec, Out_Vec)
    class(PMat), intent(inout) :: This
    class(pVec) :: In_Vec, Add_Vec, Out_Vec
    PetscErrorCode :: ierr
    call MatMultAdd(This%mat, In_Vec%vec, Add_Vec%vec, Out_Vec%vec, ierr)
  end subroutine MultAddMat_PETSc_Mat

  subroutine MatView_PETSc_Mat(This)
    class(PMat), intent(inout) :: This
    PetscErrorCode :: ierr
    call MatView(This%mat, PETSC_VIEWER_STDOUT_WORLD, ierr)
  end subroutine MatView_PETSc_Mat

  subroutine Symmetry_PETSc_Mat(This, SymLog)
    class(PMat), intent(inout) :: This
    PetscErrorCode :: ierr
    logical :: SymLog
    call MatIsSymmetric(This%mat, 1E-6_dp, SymLog, ierr)
  end subroutine Symmetry_PETSc_Mat
#endif

end module PETSc_Mat_Mod
