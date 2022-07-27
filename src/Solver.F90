module Solver_Mod

  use Constants_Mod
  use CRS_Mod
  implicit none

  private
  public :: BCG_Solve

    !!Solves the system of equations to generate a result which can be fed into the output module
    !!Solver used when code is compiled without PETSc
contains

  subroutine BCG_Solve(mat_CRS, Vecb, N_Size, phi)
    class(t_matrix_base), pointer :: mat_CRS, M
    integer :: N_Size
    integer :: ii, BCG_Iterations
    real(kind=dp) :: alpha, beta, omega, rho1, rho2
    real(kind=dp), dimension(N_Size) :: phi, p, ph, s, sh, t, v, r, rh, Ax, b
    real(kind=dp), dimension(:) :: Vecb
    logical :: BCG_Conv = .False.

    !!Inverse Jacobi Preconditioner Matrix M
    allocate(t_crs :: M)
    call M%construct(N_Size, N_Size)
    do ii = 1, N_Size
      call M%set(ii, ii, (1._dp/mat_CRS%get(ii, ii)))
    end do

    !!Biconjugate Gradient Algorithm
    phi = 1._dp
    b = Vecb
    call mat_CRS%operate(phi, Ax)
    r = b - Ax
    rh = r

    do BCG_Iterations = 1, N_Size
      rho1 = dot_product(rh, r)
      if (SQRT(rho1) < 1E-4) then
        BCG_Conv = .True.
        exit
      end if
      if (BCG_Iterations /= 1) then
        beta = (rho1/rho2)*(alpha/omega)
        p = r + beta*(p - omega*v)
      else
        p = r
      end if
      call M%operate(p, ph)
      call mat_CRS%operate(ph, v)
      alpha = rho1/dot_product(rh, v)
      s = r - alpha*v
      call M%operate(s, sh)
      call mat_CRS%operate(sh, t)
      omega = dot_product(t, s)/dot_product(t, t)
      phi = phi + alpha*ph + omega*sh
      r = s - omega*t
      rho2 = rho1
    end do

    call M%Destroy()

    !!Exit if convergence failed
    if (.Not. BCG_Conv) then
      write(output_unit, *) "---BCG Convergence Failed---"
      write(output_unit, *) "Maximum number of iterations reached"
      write(output_unit, *) "Terminating"
      write(output_unit, *) "-------------------------------"
      error stop "Solver failed to converge"
    end if

  !!Debug writes residual and iterations to terminal
# ifdef DEBUG
    write(output_unit, *) "---BCG Convergence Succeeded---"
    write(output_unit, '(g0)', advance='no') "Succeeded after iterations:  "
    write(output_unit, '(g0)', advance='no') BCG_Iterations
    write(output_unit, '(g0)', advance='no') "  with residual:"
    write(output_unit, '(E14.6)') rho1
    write(output_unit, *) "-------------------------------"
# endif

  end subroutine BCG_Solve

end module
