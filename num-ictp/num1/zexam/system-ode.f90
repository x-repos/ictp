module sys
    implicit none
contains

    ! Define the first function
    function preyfunc(x, y) result(res)
        real, intent(in) :: x, y
        real :: res

        real :: alpha, beta
        alpha =  2.
        beta = 1.

        res = alpha*x - beta*x*y
    end function preyfunc    

    ! Define the second function
    function predfunc(x, y) result(res)
        real, intent(in) :: x, y
        real :: res
        real :: sigma, gamma
        sigma = 1.
        gamma = 1.
        res = sigma*x*y - gamma*y
    end function predfunc  

    ! Solve the ode-system
    subroutine preypredator(xmin, ymin, h, n, filename)
        real, intent(in) :: xmin, ymin, h
        integer, intent(in) :: n
        character(len = *), intent(in) :: filename
        real :: x, y, xtemp, ytemp
        integer :: i, u, ios

        u = 1
        open(unit = u, iostat = ios, file = filename, status = 'replace', action = 'write')
        x = xmin
        y = ymin
        do i = 0, n
            write(u,*) i*h, x, y            
            xtemp = x + h/2*preyfunc(x,y)
            x = x + h*preyfunc(xtemp, y)
            ytemp = y + h/2*predfunc(x,y)
            y = y + h*predfunc(x,ytemp)
        end do
        close(u)
    end subroutine preypredator    
end module sys

program main
    use sys
    implicit none
    
    call preypredator(1.0, 0.6, 0.1, 100,'preypredator.dat')
    call execute_command_line('gnuplot -p plot-sys.plt')
end program main