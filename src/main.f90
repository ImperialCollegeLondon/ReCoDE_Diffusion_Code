Program Main


  use Constants_Mod
  use Problem_Mod
  use Materials_Mod
  use Output_Mod
  use DiamondDiff_Mod
  use PETScSolver_Mod
  use PETSc_Mat_Mod
  use PETSc_Vec_Mod
  
  Implicit None
  type(t_Problem) :: Problem 
  type(t_material), allocatable, dimension(:) :: Material
  type(t_DDiff) :: DDiff
  type(t_output) :: Output
  type(pMat) :: pMatA
  type(pVec) :: pVecb, pVecx
  Real(kind=dp) :: time_start, time_stop

  call cpu_time(time_start)
  call Problem%ReadInput(Material)
  Write(*,*) ">Input Read"

  call DDiff%CreatePETSc(Problem,pMatA,pVecb,pVecx)

  call DDiff%SolveProblem(Material,Problem,pMatA,pVecb,pVecx)
  Write(*,*) ">Problem Assembled"

  call Output%GenerateVTU(Material,Problem,pVecx)
  Write(*,*) ">Output Generated"

  call DDiff%DestroyPETSc(pMatA,pVecb,pVecx)
  call Problem%DestroyProblem(Material)

  call  cpu_time(time_stop)
  Write(*,'(g0)',advance='no') " >Problem Solved in:"
  Write(*,'(E14.6)',advance='no') time_stop-time_start
  Write(*,'(g0)') " seconds"
End program
