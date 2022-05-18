Program Main


  use Constants_Mod
  use Problem_Mod
  use Materials_Mod
  use Output_Mod
  use DiamondDiff_Mod
  use Solver_Mod
  use PETScSolver_Mod
  
  Implicit None
  Type(t_Problem) :: Problem 
  type(t_material), allocatable, dimension(:) :: Material


  call Problem%ReadInput(Material)


  Write(*,*) "Code Executed"
End program
