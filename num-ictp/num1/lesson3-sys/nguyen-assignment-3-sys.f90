module funcmodule
    implicit none
    
contains
    function preyfunc(x, y) result(res)
        real(8), intent(in) :: x, y
        real(8) :: res

        real(8) :: alpha, beta
        alpha =  2.
        beta = 1.

        res = alpha*x - beta*x*y
    end function preyfunc    

    function predfunc(x, y) result(res)
        real(8), intent(in) :: x, y
        real(8) :: res
        real(8) :: sigma, gamma
        sigma = 1.
        gamma = 1.
        res = sigma*x*y - gamma*y
    end function predfunc
end module funcmodule

module submodule
    use funcmodule
    implicit none
contains
    subroutine preypredator(xmin, ymin, a, h, n, filename)
        real(8), intent(in) :: xmin, ymin, a, h
        integer, intent(in) :: n
        character(len = *), intent(in) :: filename
        real(8) :: x, y, xtemp, ytemp, t
        integer :: i, u, ios

        u = 1
        open(unit = u, iostat = ios, file = filename, status = 'replace', action = 'write')
        x = xmin
        y = ymin
        t = a
        do i = 0, n
            write(u,*) t, x, y
            t = t + h       
            xtemp = x + h/2*preyfunc(x,y)
            x = x + h*preyfunc(xtemp, y)
            ytemp = y + h/2*predfunc(x,y)
            y = y + h*predfunc(x,ytemp)
        end do
        close(u)
    end subroutine preypredator
end module submodule

program main
    use submodule
    implicit none
    real(8) :: xmin, ymin, t0, h
    integer :: n
    xmin = 1.0
    ymin = 0.6
    t0 = 0
    h = 0.1
    n = 100
    call preypredator(xmin, ymin, t0, h, n,'preypredator.dat')
    call execute_command_line('gnuplot -p plot-sys.plt')
end program main