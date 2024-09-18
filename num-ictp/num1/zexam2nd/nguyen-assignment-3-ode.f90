! function dx/x = x (slope) or x' = x
! x0 = 1

! f(x + a)  = f(x) + f'(x)*a + f''(x)*a^2*1/2 + O

module ode
    implicit none
    
contains

    function f(x, t) result(res)
        real, intent(in) :: x, t
        real :: res
        res = (x-t)/2.0
        
    end function f    

    subroutine euler(y0, a, b, h, filename)
        real, intent(in) :: y0, a, b, h
        character(len = *), intent(in) :: filename
        real :: y, t
        integer :: n, i, u, ios
        u = 1
        open(unit = u, iostat = ios, file = filename, status = 'replace', action = 'write')
        n = int((abs(b-a))/h)
        y = y0
        t = a
        do i = 1, n+1
            write(u,*) t, y
            t = t + h
            y = y + h * f(y, t)
        end do
        close(u)
    end subroutine euler


    subroutine eulerMidpointwithInterval(y0, a, b, h, filename)
        real, intent(in) :: y0, a, b, h
        character(len = *), intent(in) :: filename
        real :: y, k, t
        integer :: n, i, u, ios
        u = 1
        open(unit = u, iostat = ios, file = filename, status = 'replace', action = 'write')
        n = int((abs(b-a))/h)
        y = y0
        t = a
        do i = 0, n
            write(u,*) t, y
            t = t + h
            k = y + h/2*f(y, t)
            y = y + h*f(y, t)
        end do
        close(u)
    end subroutine eulerMidpointwithInterval

    ! subroutine eulerMidpointwithPoints(y0, a, h, n, filename)
    !     real, intent(in) :: y0, h, a
    !     integer, intent(in) :: n
    !     character(len = *), intent(in) :: filename
    !     real :: y, k, t
    !     integer :: i, u, ios
    !     u = 1
    !     open(unit = u, iostat = ios, file = filename, status = 'replace', action = 'write')
    !     y = y0
    !     t = a
    !     do i = 0, n
    !         write(u,*) t, y
    !         t = t + h
    !         k = y + h/2*f(y, t)
    !         y = y + h*f(y, t)
    !     end do
    !     close(u)
    ! end subroutine eulerMidpointwithPoints


    subroutine analyticFunc(a, b, h, filename)
        real, intent(in) :: a, b, h
        character(len = *), intent(in) :: filename
        integer :: n, i, u, ios
        real :: t
        u = 1
        open(unit = u, iostat = ios, file = filename, status = 'replace', action = 'write')
        n = int((abs(b-a))/h)
        t = a
        do i = 1, n
            write(u,*) t, t + 2 + 3/(exp(-t/2))
            t = t + h
        end do
        close(u)
    end subroutine analyticFunc
end module ode


!https://www.mathway.com/Algebra
program main
    use ode
    real :: h, a, b, y0
    integer :: n
    a = 0.0; b = 20.0; h = 0.1
    y0 = 5.0
    ! n = 200
    call euler(y0, a, b, h, 'euler.dat')
    call eulerMidpointwithInterval(y0, a, b, h, 'eulerMidpoint.dat')
    ! call eulerMidpointwithPoints(y0, a, h, n, 'eulerMidpoint.dat')
    call analyticFunc(a, b, h, 'analyticFunc.dat')
    call execute_command_line('gnuplot -p plot-ode.plt')
end program main