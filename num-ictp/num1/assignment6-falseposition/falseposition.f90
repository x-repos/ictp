

! fomular:
! 	bisection						resula falsi
! 	x3 = (x(1) + x(2))/2			x3 = x1 - ((x2-x1)*f(x1))/(f(x2)-f(x1))

! the bisection method has the faster convergence than the falseposition method (in this case)
! because of their number of iteration.

! it is confused when we say which method is faster than another
! because the rate of convergence of bisection is roughly equal 1/2
! however, the rate of convergence of regular-falsi is c = 1/2 f''(e)/f'(e)
! therefore, the rate of convergence of regular-falsi is up to the 
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

module falsepositionmodule
    use functionmodule
    implicit none

contains

    subroutine falseposition(x, y, eps, filename)
        real(8), intent(in) :: x, y, eps
        character(len = *), intent(in) :: filename
        real(8) :: a, b, c, ctemp
        integer :: u = 1, ios, i = 0
        a = x; b = y
        open(unit = u, iostat = ios, file = filename, &
        status = 'replace', action = 'write')
        write(u, *) 'step    root value'
        write(u, *) '------------------'
        do
            i = i + 1
            ctemp = c
            c = (f(b)*a - f(a)*b)/(f(b)-f(a))
            if (abs((c-ctemp)) < eps) then
                exit
            end if
            write(u,'(i3, f15.7)') i, c
            if ((f(c)*f(b)) < 0) then
                a = c
            end if
            if ((f(c)*f(a)) < 0) then
                b = c 
            end if
        end do
        close(u)
    end subroutine falseposition
end module falsepositionmodule

program main
    use falsepositionmodule
    implicit none
    real(8) :: a, b, eps

    ! [a b] is the interval containing the solution.
    a = -2.0
    b = 2.0
    eps = 10e-7
call falseposition(a, b, eps, 'falseposition.dat')
end program main
