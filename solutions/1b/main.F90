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
  !! $$ Exercise 1b
  Integer :: ii

  call cpu_time(time_start)
  call Problem%ReadInput(Material)
  Write(*, *) ">Input Read"

  !! $$ Exercise 1b
  Do ii = 1, Size(Material)
    call Material(ii)%PrintMaterial()
  End Do

  call MatGen%Create(Problem)
  Write(*, *) ">Matrices Created"

  call MatGen%Solve(Material, Problem, Flux)
  Write(*, *) ">Problem Assembled"

  call GenerateVTU(Problem, Flux)
  Write(*, *) ">Output Generated"

  call MatGen%Destroy(Flux)
  call Problem%DestroyProblem(Material)

  call cpu_time(time_stop)
  Write(*, '(g0)', advance='no') " >Problem Solved in:"
  Write(*, '(E14.6)', advance='no') time_stop - time_start
  Write(*, '(g0)') " seconds"
End program
