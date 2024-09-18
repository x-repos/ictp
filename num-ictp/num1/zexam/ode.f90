module ode
    implicit none
contains

    ! Define the function
    function f(x) result(res)
        real, intent(in) :: x
        real :: res
        res = x
    end function f

    ! Calculate the integral with interval
    subroutine eulerMidpointwithInterval(xmin, xmax, h, filename)
        real, intent(in) :: xmin, xmax, h
        character(len = *), intent(in) :: filename
        real :: x, k
        integer :: n, i, u, ios
        u = 1
        open(unit = u, iostat = ios, file = filename, status = 'replace', action = 'write')
        n = int((abs(xmin-xmax))/h)
        x = xmin
        do i = 0, n
            write(u,*) i*h, x
            k = x + h/2*f(x)
            x = x + h*f(k)
        end do
        close(u)
    end subroutine eulerMidpointwithInterval

    ! Calculate the integral with the number of points
    subroutine eulerMidpointwithPoints(xmin, h, n, filename)
        real, intent(in) :: xmin, h
        integer, intent(in) :: n
        character(len = *), intent(in) :: filename
        real :: x, k
        integer :: i, u, ios
        u = 1
        open(unit = u, iostat = ios, file = filename, status = 'replace', action = 'write')
        x = xmin
        do i = 0, n
            write(u,*) i*h, x
            k = x + h/2*f(x)
            x = x + h*f(k)
        end do
        close(u)
    end subroutine eulerMidpointwithPoints

end module ode

program main
    use ode
    implicit none
    real :: h, a, b
    integer :: n
    a = 1.0; b = 10.0
    n = 200; h = 0.05

    call eulerMidpointwithPoints(a, h, n, 'eulerMidpointPoint.dat')
    call eulerMidpointwithInterval(a, b, h, 'eulerMidpointInterval.dat')
    call execute_command_line('gnuplot -p plot-ode.plt')

end program main