Module PETSc_Mat_Mod

#include <petsc/finclude/petscmat.h>
#include <petsc/finclude/petscsys.h>
    use petscSys
    use petscMat
    use Constants_Mod
    use PETSc_Vec_Mod
    Implicit None

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


Subroutine Create_PETSc_Mat(This,N_row,N_col,N_nz)
  Class(PMat), intent(inout) :: This
  Integer :: N_row, N_col, N_nz
  PetscErrorCode ierr
  call MatCreateSeqAIJ(PETSC_COMM_SELF,N_row,N_col,N_nz,PETSC_NULL_INTEGER,This%mat,ierr)
End Subroutine Create_PETSc_Mat


Subroutine Destroy_PETSc_Mat(This)
  Class(PMat), intent(inout) :: This
  PetscErrorCode ierr
  Call MatDestroy(This%mat,ierr); CHKERRQ(ierr)
End Subroutine Destroy_PETSc_Mat


Subroutine SwitchAssemble_PETSc_Mat(This)
  Class(PMat), intent(inout) :: This
  PetscErrorCode ierr
  call MatAssemblyBegin(This%mat,MAT_FLUSH_ASSEMBLY,ierr)
  call MatAssemblyEnd(This%mat,MAT_FLUSH_ASSEMBLY,ierr)
End Subroutine SwitchAssemble_PETSc_Mat


Subroutine Assemble_PETSc_Mat(This)
  Class(PMat), intent(inout) :: This
  PetscErrorCode ierr
  call MatAssemblyBegin(This%mat,MAT_FINAL_ASSEMBLY,ierr)
  call MatAssemblyEnd(This%mat,MAT_FINAL_ASSEMBLY,ierr)
End Subroutine Assemble_PETSc_Mat


Subroutine InsertVal_PETSc_Mat(This,V_row,V_col,Value)
  Class(PMat), intent(inout) :: This
  PetscErrorCode ierr
  Integer :: V_row, V_Col
  Real(kind=dp) :: Value
  If ((V_row .LT. 1) .OR. (V_col .LT. 1)) Then
    Write(*,*) "### Error ###"
    Write(*,*) "Inserting Value outside of bounds"
    Stop
  EndIf
  !!Convert the row and col integers to P format
  call MatSetValue(This%mat,V_row-1,V_Col-1,Value,INSERT_VALUES,ierr)
End Subroutine InsertVal_PETSc_Mat


Subroutine AddVal_PETSc_Mat(This,V_row,V_col,Value)
  Class(PMat), intent(inout) :: This
  PetscErrorCode ierr
  Integer :: V_row, V_Col
  Real(kind=dp) :: Value
  If ((V_row .LT. 1) .OR. (V_col .LT. 1)) Then
    Write(*,*) "### Error ###"
    Error Stop "Inserting Value outside of bounds"
  EndIf
  !!Convert the row and col integers to P format
  call MatSetValue(This%mat,V_row-1,V_col-1,Value,ADD_VALUES,ierr)
End Subroutine AddVal_PETSc_Mat


Subroutine InsertMat_PETSc_Mat(This,V_rows,V_cols,Values)
  Class(PMat), intent(inout) :: This
  PetscErrorCode ierr
  Integer :: N_rows, N_cols
  Integer :: V_rows(:), V_cols(:)
  Real(kind=dp) :: Values(:,:)
  N_rows = Size(V_rows)
  N_cols = Size(V_cols)
  !!Temporarily convert the row and col integers to P format
  V_rows(:) = V_rows(:) - 1
  V_cols(:) = V_cols(:) -1
  call MatSetValues(This%mat,N_rows,V_rows,N_cols,V_cols,Values,INSERT_VALUES,ierr)
  !!Revert to F format
  V_rows(:) = V_rows(:) + 1
  V_cols(:) = V_cols(:) + 1
End Subroutine InsertMat_PETSc_Mat


Subroutine AddMat_PETSc_Mat(This,V_rows,V_cols,Values)
  Class(PMat), intent(inout) :: This
  PetscErrorCode ierr
  Integer :: N_rows, N_cols
  Integer :: V_rows(:), V_cols(:)
  Real(kind=dp) :: Values(:,:)
  N_rows = Size(V_rows)
  N_cols = Size(V_cols)
  !!Temporarily convert the row and col integers to P format
  V_rows(:) = V_rows(:) - 1
  V_cols(:) = V_cols(:) -1
  call MatSetValues(This%mat,N_rows,V_rows,N_cols,V_cols,Values,ADD_VALUES,ierr)
  !!Revert to F format
  V_rows(:) = V_rows(:) + 1
  V_cols(:) = V_cols(:) + 1
End Subroutine AddMat_PETSc_Mat


Subroutine GetSizeMat_PETSc_Mat(This,N_rows,N_cols)
  Class(PMat), intent(inout) :: This
  PetscErrorCode ierr
  Integer :: N_rows, N_cols
  call MatGetSize(this%mat,N_rows,N_cols,ierr)
End Subroutine GetSizeMat_PETSc_Mat


Function GetValMat_PETSc_Mat(This,V_row,V_col) Result(Value)
  Class(PMat), intent(inout) :: This
  PetscErrorCode ierr
  Integer :: V_row, V_col
  Real(kind=dp) :: Value
  !!Temporarily convert the row and col integers to P format
  V_row = V_row - 1
  V_col = V_col - 1
  call MatGetValues(This%mat,1,V_row,1,V_col,Value,ierr)
  !!Revert to F format
  V_row = V_row + 1
  V_col = V_col + 1
End Function GetValMat_PETSc_Mat


Subroutine GetValsMat_PETSc_Mat(This,V_rows,V_cols,Values)
  Class(PMat), intent(inout) :: This
  PetscErrorCode ierr
  Integer :: N_rows, N_cols
  Integer :: V_rows(:), V_cols(:)
  Real(kind=dp), dimension(:,:) :: Values
  N_rows = Size(V_rows)
  N_cols = Size(V_cols)
  !!Temporarily convert the row and col integers to P format
  V_rows(:) = V_rows(:) - 1
  V_cols(:) = V_cols(:) - 1
  call MatGetValues(This%mat,N_rows,V_rows,N_cols,V_cols,Values,ierr)
  !!Revert to F format
  V_rows(:) = V_rows(:) + 1
  V_cols(:) = V_cols(:) + 1
End Subroutine GetValsMat_PETSc_Mat


Subroutine MultMat_PETSc_Mat(This,In_Vec,Out_Vec)
  Class(PMat), intent(inout) :: This
  class(pVec) :: In_Vec, Out_Vec
  PetscErrorCode ierr
  call MatMult(This%mat,In_Vec%vec,Out_Vec%vec,ierr)
End Subroutine MultMat_PETSc_Mat


Subroutine MultAddMat_PETSc_Mat(This,In_Vec,Add_Vec,Out_Vec)
  Class(PMat), intent(inout) :: This
  Class(pVec) :: In_Vec, Add_Vec, Out_Vec
  PetscErrorCode ierr
  call MatMultAdd(This%mat,In_Vec%vec,Add_Vec%vec,Out_Vec%vec,ierr)
End Subroutine MultAddMat_PETSc_Mat


Subroutine MatView_PETSc_Mat(This)
  Class(PMat), intent(inout) :: This
  PetscErrorCode ierr
  call MatView(This%mat,PETSC_VIEWER_STDOUT_WORLD,ierr)
End Subroutine MatView_PETSc_Mat


Subroutine Symmetry_PETSc_Mat(This,SymLog)
  Class(PMat), intent(inout) :: This
  PetscErrorCode ierr
  Logical :: SymLog
  call MatIsSymmetric(This%mat,1E-6_dp,SymLog,ierr)
End Subroutine Symmetry_PETSc_Mat

End Module
