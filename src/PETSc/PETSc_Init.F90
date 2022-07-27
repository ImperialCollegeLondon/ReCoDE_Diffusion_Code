module PETSc_Init_Mod
#ifdef PETSC
!!Initialize the PETSc Database
#include <petsc/finclude/petscsys.h>
  use petscsys
  implicit none
contains
  subroutine PETSc_Init
    PetscErrorCode :: ierr
    logical :: Called = .false.
    if (.not. Called) then
      call PetscInitialize(PETSC_NULL_CHARACTER, ierr)
      Called = .TRUE.
      if (ierr /= 0) error stop "Failed to Initialize PETSc"
    end if
  end subroutine PETSc_Init
#endif
end module PETSc_Init_Mod
