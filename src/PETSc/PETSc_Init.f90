Module PETSc_Init_Mod
!!Initialize the PETSc Database
#include <petsc/finclude/petscsys.h>
use petscsys
Implicit None
contains
  Subroutine PETSc_Init
    PetscErrorCode ierr
    Logical :: Called = .FALSE.
    If (.NOT. Called) Then
      Call PetscInitialize(PETSC_NULL_CHARACTER,ierr)
      Called = .TRUE.
      If (ierr .NE. 0) Then
        Error Stop "Failed to Initialize PETSc"
      EndIf
    EndIf
  End Subroutine PETSc_Init
End Module
