Program Main


  use Constants_Mod
  use Problem_Mod
  use Materials_Mod
  use Output_Mod
  use DiamondDiff_Mod
# ifdef PETSC
    use PETSc_Init_Mod
    use PETSc_Vec_Mod
    use PETSc_Mat_Mod
    use PETScSolver_Mod
# endif
# ifndef PETSC
    use CRS_Mod
    use Solver_Mod
# endif
  
  Implicit None
  
  type(t_Problem) :: Problem 
  type(t_material), allocatable, dimension(:) :: Material
  type(t_DDiff) :: DDiff
  type(t_output) :: Output
  Real(kind=dp) :: time_start, time_stop
  Real(kind=dp), allocatable, dimension(:) :: Flux

  call cpu_time(time_start)
  call Problem%ReadInput(Material)
  Write(*,*) ">Input Read"

  call DDiff%Create(Problem)

  call DDiff%Solve(Material,Problem,Flux)
  Write(*,*) ">Problem Assembled"

  call Output%GenerateVTU(Problem,Flux)
  Write(*,*) ">Output Generated"

  call DDiff%Destroy(Flux)
  call Problem%DestroyProblem(Material)

  call  cpu_time(time_stop)
  Write(*,'(g0)',advance='no') " >Problem Solved in:"
  Write(*,'(E14.6)',advance='no') time_stop-time_start
  Write(*,'(g0)') " seconds"
End program
