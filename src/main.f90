Program Main


  use Constants_Mod
  use Problem_Mod
  use Materials_Mod
  use Output_Mod
  use DiamondDiff_Mod
  use Solver_Mod
  use PETScSolver_Mod
  use PETSc_Init_Mod
  use PETSc_Mat_Mod
  use PETSc_Vec_Mod
  
  Implicit None
  type(t_Problem) :: Problem 
  type(t_material), allocatable, dimension(:) :: Material
  type(t_DDiff) :: DDiff
  type(t_output) :: Output
  type(pMat) :: pMatA
  type(pVec) :: pVecb, pVecx


  Write(*,*) "Reading Input..."
  call Problem%ReadInput(Material)
  Write(*,*) "...Input Read Successfully"

  call DDiff%CreatePETSc(Problem,pMatA,pVecb,pVecx)

  Write(*,*) "Assembling Problem..."
  call DDiff%SolveProblem(Material,Problem,pMatA,pVecb,pVecx)
  Write(*,*) "...Problem Assembled Successfully"

  Write(*,*) "Generating Output..."
  call Output%GenerateVTU(Material,Problem,pVecx)
  Write(*,*) "...Generated Output Successfully"

  call DDiff%DestroyPETSc(pMatA,pVecb,pVecx)
  call Problem%DestroyProblem(Material)

  Write(*,*) "Code Executed"
End program
