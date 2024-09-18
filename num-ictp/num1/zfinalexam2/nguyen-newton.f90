module functionmodule
    implicit none
    
contains
    function f(x) result(res)
        real(8), intent(in) :: x
        real(8) :: res
        res = exp(x) + 2*x
    end function f

    function f1(x) result(retval)
        real(8), intent(in) :: x
        real(8) :: retval
        retval = exp(x) + 2
    end function f1
end module functionmodule

module newtonraphsonmodule
    use functionmodule
    implicit none
    
contains
    subroutine newtonraphson(x0, eps, filename)
        implicit none
        real(8), intent(in) :: x0, eps
        character(len = *), intent(in) :: filename
        integer :: i = 0, u = 1, ios
        real(8) :: x1, x2
        x1 = x0
        open(unit = u, iostat = ios, file = filename, &
        status = 'replace', action = 'write')
        100 i = i + 1
            x2 = x1
            x1 = x1 - f(x1)/f1(x1)
            write(u,*) i, x1
            if (abs(x2-x1) < eps) then
                goto 200
            end if
        goto 100
        200 continue
    end subroutine newtonraphson  
end module newtonraphsonmodule

program main
    use newtonraphsonmodule
    implicit none
    real(8) :: x0, eps

    ! x0 is the first position to find root.
    ! position of x0 is need to be same valey of root.
    x0= -1.0
    eps = 1e-4
    call newtonraphson(x0, eps, 'newton_result.dat')
!     call execute_command_line('gnuplot -p plot.plt')
end program main
