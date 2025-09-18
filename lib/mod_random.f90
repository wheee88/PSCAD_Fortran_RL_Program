module mod_random

  ! Provides a random number generator with
  ! normal distribution, centered on zero.

  implicit none

  private
  public :: randn

  real, parameter :: pi = 3.1415926

  interface randn
    module procedure randn1d, randn2d
  end interface randn

contains

  function randn1d(n) result(r)
    ! Generates n random numbers with a normal distribution.
    integer, intent(in) :: n
    real :: r(n), r2(n)
    integer :: i
    call random_number(r)
    call random_number(r2)
    ! Avoid log(0) by ensuring r > 0
    do i = 1, n
      if (r(i) <= 0.0) r(i) = 1.0e-10
    end do
    r = sqrt(-2.0 * log(r)) * cos(2.0 * pi * r2)
  end function randn1d

  function randn2d(m, n) result(r)
    ! Generates m x n random numbers with a normal distribution.
    integer, intent(in) :: m, n
    real :: r(m, n), r2(m, n)
    integer :: i, j
    call random_number(r)
    call random_number(r2)
    ! Avoid log(0) by ensuring r > 0
    do i = 1, m
      do j = 1, n
        if (r(i,j) <= 0.0) r(i,j) = 1.0e-10
      end do
    end do
    r = sqrt(-2.0 * log(r)) * cos(2.0 * pi * r2)
  end function randn2d

end module mod_random
