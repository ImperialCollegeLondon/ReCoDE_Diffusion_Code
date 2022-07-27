Program Main

  use Constants_Mod
  use Problem_Mod
  use Materials_Mod
  use Output_Mod
  use MatGen_Mod
#ifdef PETSC
  use PETSc_Init_Mod
  use PETSc_Vec_Mod
  use PETSc_Mat_Mod
  use PETScSolver_Mod
#else
  use CRS_Mod
  use Solver_Mod
#endif

  Implicit None

  type(t_Problem) :: Problem
  type(t_material), allocatable, dimension(:) :: Material
  type(t_MatGen) :: MatGen
  Real(kind=dp) :: time_start, time_stop
  Real(kind=dp), allocatable, dimension(:) :: Flux

  call cpu_time(time_start)
  call Problem%ReadInput(Material)
  Write(output_unit, *) ">Input Read"

  call MatGen%Create(Problem)
  Write(output_unit, *) ">Matrices Created"

  call MatGen%Solve(Material, Problem, Flux)
  Write(output_unit, *) ">Problem Assembled"

  call GenerateVTU(Problem, Material, Flux)
  Write(output_unit, *) ">Output Generated"

  call MatGen%Destroy(Flux)
  call Problem%DestroyProblem(Material)

  call cpu_time(time_stop)
  Write(output_unit, '(g0)', advance='no') " >Problem Solved in:"
  Write(output_unit, '(E14.6)', advance='no') time_stop - time_start
  Write(output_unit, '(g0)') " seconds"
End program
