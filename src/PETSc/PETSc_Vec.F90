module PETSc_Vec_Mod

#ifdef PETSC
#include <petsc/finclude/petscvec.h>
#include <petsc/finclude/petscsys.h>
  use petscSys
  use petscVec
  use Constants_Mod
  implicit none
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

  subroutine Create_PETSc_Vec(This, N_vec)
    class(PVec), intent(inout) :: This
    integer :: N_vec
    PetscErrorCode :: ierr
    call VecCreate(PETSC_COMM_WORLD, This%vec, ierr); CHKERRQ(ierr)
    call VecSetType(This%vec, 'seq', ierr); CHKERRQ(ierr)
    call VecSetSizes(This%vec, PETSC_DECIDE, N_vec, ierr); CHKERRQ(ierr)
  end subroutine Create_PETSc_Vec

  subroutine Destroy_PETSc_Vec(This)
    class(PVec), intent(inout) :: This
    PetscErrorCode :: ierr
    call VecDestroy(This%vec, ierr); CHKERRQ(ierr)
  end subroutine Destroy_PETSc_Vec

  subroutine Assemble_PETSc_Vec(This)
    class(PVec), intent(inout) :: This
    PetscErrorCode :: ierr
    call VecAssemblyBegin(This%vec, ierr)
    call VecAssemblyend(This%vec, ierr)
  end subroutine Assemble_PETSc_Vec

  subroutine ConvTo_PETSc_Vec(This, Val_vec)
    class(PVec), intent(inout) :: This
    PetscErrorCode :: ierr
    integer :: N_Vec, ii
    integer, allocatable, dimension(:) :: Pos_vec
    real(kind=dp) :: Val_vec(:)
    N_Vec = Size(Val_vec)
    allocate(Pos_vec(N_Vec))
    do ii = 1, N_Vec
      Pos_vec(ii) = ii - 1
    end do
    call VecSetValues(This%vec, N_vec, Pos_vec, Val_vec, INSERT_VALUES, ierr)
    deallocate(Pos_vec)
    call This%Assemble()
  end subroutine ConvTo_PETSc_Vec

  subroutine ConvFrom_PETSc_Vec(This, Val_vec)
    class(PVec), intent(inout) :: This
    PetscErrorCode :: ierr
    integer :: N_Vec, ii
    integer, allocatable, dimension(:) :: Pos_vec
    real(kind=dp) :: Val_vec(:)
    call This%GetSize(N_Vec)
    if (N_Vec /= Size(Val_vec)) then
      write(output_unit, *) "### Error ###"
      error stop "PVec and Output vec size do not match"
    end if
    allocate(Pos_vec(N_Vec))
    do ii = 1, N_Vec
      Pos_vec(ii) = ii - 1
    end do
    call VecGetValues(This%vec, N_vec, Pos_vec, Val_vec, ierr)
    deallocate(Pos_vec)
    ! call This%Destroy()
  end subroutine ConvFrom_PETSc_Vec

  subroutine GetSize_PETSc_Vec(This, N_GetSize)
    class(PVec), intent(inout) :: This
    PetscErrorCode :: ierr
    integer :: N_GetSize
    call VecGetSize(This%vec, N_GetSize, ierr)
  end subroutine GetSize_PETSc_Vec

  subroutine VecView_PETSc_Vec(This)
    class(PVec), intent(inout) :: This
    PetscErrorCode :: ierr
    call VecView(This%vec, PETSC_VIEWER_STDOUT_WORLD, ierr); CHKERRQ(ierr)
  end subroutine VecView_PETSc_Vec

  subroutine Reset_PETSc_Vec(This)
    class(PVec), intent(inout) :: This
    PetscErrorCode :: ierr
    call VecZeroEntries(this%vec, ierr)
  end subroutine Reset_PETSc_Vec

  subroutine InsertValue_PETSc_Vec(this, Position, Value)
    class(PVec), intent(inout) :: this
    real(kind=dp) :: Value
    integer :: Position
    call VecSetValue(this%vec, Position - 1, Value, INSERT_VALUES)
  end subroutine InsertValue_PETSc_Vec

  subroutine AddValue_PETSc_Vec(this, Position, Value)
    class(PVec), intent(inout) :: this
    real(kind=dp) :: Value
    integer :: Position
    call VecSetValue(this%vec, Position - 1, Value, ADD_VALUES)
  end subroutine AddValue_PETSc_Vec

  subroutine SetAll_PETSc_Vec(This, Val_All)
    class(PVec), intent(inout) :: This
    PetscErrorCode :: ierr
    real(kind=dp) :: Val_All
    call VecSet(This%vec, Val_All, ierr)
  end subroutine SetAll_PETSc_Vec
#endif

end module PETSc_Vec_Mod
