

! Fomular:
! 	bisection						resula falsi
! 	x3 = (x(1) + x(2))/2			x3 = x1 - ((x2-x1)*f(x1))/(f(x2)-f(x1))

! The Bisection method has the faster convergence than the FalsePosition method (in this case)
! Because of their number of iteration.

! It is confused when we say which method is faster than another
! Because the rate of convergence of Bisection is roughly equal 1/2
! However, the rate of convergence of Regular-Falsi is C = 1/2 f''(e)/f'(e)
! Therefore, the rate of convergence of Regular-Falsi is up to the 
! type of function.
module functionmodule
    implicit none
contains
    real(8) function f(x) result (res)
        implicit none
        real(8), intent(in) :: x
    res = 2*exp(x) - 2*x**2 - 3
end function f
end module functionmodule

module bisectionmodule
    use functionmodule
    implicit none

contains
    subroutine bisection(x, y, eps, bis)
        real(8), intent(in) :: x, y, eps
        character(len = *), intent(in) :: bis
        real(8) :: a, b, c
        integer :: u = 1, ios, i = 0
        a = x; b = y
        open(unit = u, iostat = ios, file = bis, &
        status = 'replace', action = 'write')
        write(u, *) 'step    root value'
        write(u, *) '------------------'
        do
            i = i + 1
            c = (a + b)/2.0
            write(u,'(i3, f15.7)') i, c
            if ((f(c)*f(b)) < 0) then
                a = c
            else
                b = c 
            end if
            if (abs((a-b)) < eps) then
                exit
            end if
        end do
        close(u)
    end subroutine bisection
end module bisectionmodule

program main
    use bisectionmodule
    implicit none
    real(8) :: a, b, eps

    ! [a b] is the interval containing the solution.
    a = -2.0
    b = 2.0
    eps = 10e-7
call bisection(a, b, eps, 'bisection.dat')
end program main