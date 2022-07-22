Module PETSc_Vec_Mod

#include <petsc/finclude/petscvec.h>
#include <petsc/finclude/petscsys.h>
    use petscSys
    use petscVec
    use Constants_Mod
  Implicit None
  type :: PVec
    type(tvec) :: vec
  contains
    procedure :: Create => Create_PETSc_Vec
    procedure :: Destroy => Destroy_PETSc_Vec
    procedure :: Assemble => Assemble_PETSc_Vec
    procedure :: ConvTo => ConvTo_PETSc_Vec
    procedure :: ConvFrom => ConvFrom_PETSc_Vec
    procedure :: GetSize => GetSize_PETSc_Vec
    procedure :: VecView => VecView_PETSc_Vec
    procedure :: Reset => Reset_PETSc_Vec
    procedure :: Insert => InsertValue_PETSc_Vec
    procedure :: Add => AddValue_PETSc_Vec
    procedure :: SetAll => SetAll_PETSc_Vec
  end type

contains



Subroutine Create_PETSc_Vec(This,N_vec)
  Class(PVec), intent(inout) :: This
  Integer :: N_vec
  PetscErrorCode ierr
  call VecCreate(PETSC_COMM_WORLD,This%vec,ierr); CHKERRQ(ierr)
  call VecSetType(This%vec,'seq',ierr); CHKERRQ(ierr)
  call VecSetSizes(This%vec,PETSC_DECIDE,N_vec,ierr); CHKERRQ(ierr)
End Subroutine Create_PETSc_Vec


Subroutine Destroy_PETSc_Vec(This)
  Class(PVec), intent(inout) :: This
  PetscErrorCode ierr
  Call VecDestroy(This%vec,ierr); CHKERRQ(ierr)
End Subroutine Destroy_PETSc_Vec


Subroutine Assemble_PETSc_Vec(This)
  Class(PVec), intent(inout) :: This
  PetscErrorCode ierr
  call VecAssemblyBegin(This%vec,ierr)
  call VecAssemblyEnd(This%vec,ierr)
End Subroutine Assemble_PETSc_Vec


Subroutine ConvTo_PETSc_Vec(This,Val_vec)
  Class(PVec), intent(inout) :: This
  PetscErrorCode ierr
  Integer :: N_Vec, ii
  Integer, allocatable, dimension(:) :: Pos_vec
  Real(kind=dp) :: Val_vec(:)
  N_Vec = Size(Val_vec)
  Allocate(Pos_vec(N_Vec))
  Do ii = 1, N_Vec
    Pos_vec(ii) = ii-1
  EndDo
  call VecSetValues(This%vec,N_vec,Pos_vec,Val_vec,INSERT_VALUES,ierr)
  Deallocate(Pos_vec)
  call This%Assemble()
End Subroutine ConvTo_PETSc_Vec


Subroutine ConvFrom_PETSc_Vec(This,Val_vec)
  Class(PVec), intent(inout) :: This
  PetscErrorCode ierr
  Integer :: N_Vec, ii
  Integer, allocatable, dimension(:) :: Pos_vec
  Real(kind=dp) :: Val_vec(:)
  call This%GetSize(N_Vec)
  If (N_Vec .NE. Size(Val_vec)) Then
    Write(*,*) "### Error ###"
    Error Stop "PVec and Output vec size do not match"
  EndIf
  Allocate(Pos_vec(N_Vec))
  Do ii = 1, N_Vec
    Pos_vec(ii) = ii-1
  EndDo
  Call VecGetValues(This%vec,N_vec,Pos_vec,Val_vec,ierr)
  Deallocate(Pos_vec)
  ! call This%Destroy()
End Subroutine ConvFrom_PETSc_Vec


Subroutine GetSize_PETSc_Vec(This,N_GetSize)
  Class(PVec), intent(inout) :: This
  PetscErrorCode ierr
  Integer :: N_GetSize
  Call VecGetSize(This%vec,N_GetSize,ierr)
End Subroutine GetSize_PETSc_Vec


Subroutine VecView_PETSc_Vec(This)
  Class(PVec), intent(inout) :: This
  PetscErrorCode ierr
  call VecView(This%vec,PETSC_VIEWER_STDOUT_WORLD,ierr); CHKERRQ(ierr)
End Subroutine VecView_PETSc_Vec


Subroutine Reset_PETSc_Vec(This)
  Class(PVec), intent(inout) :: This
  PetscErrorCode ierr
  call VecZeroEntries(this%vec,ierr)
End Subroutine Reset_PETSc_Vec


Subroutine InsertValue_PETSc_Vec(this,Position,Value)
  Class(PVec), intent(inout) :: this
  Real(kind=dp) :: Value 
  Integer :: Position 
  call VecSetValue(this%vec,Position-1,Value,INSERT_VALUES)
End Subroutine InsertValue_PETSc_Vec


Subroutine AddValue_PETSc_Vec(this,Position,Value)
  Class(PVec), intent(inout) :: this
  Real(kind=dp) :: Value 
  Integer :: Position
  call VecSetValue(this%vec,Position-1,Value,ADD_VALUES)
End Subroutine AddValue_PETSc_Vec


Subroutine SetAll_PETSc_Vec(This,Val_All)
  Class(PVec), intent(inout) :: This
  PetscErrorCode ierr
  Real(kind=dp) :: Val_All
  call VecSet(This%vec,Val_All,ierr)
End Subroutine SetAll_PETSc_Vec


End Module
