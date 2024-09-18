module functionmodule
    implicit none
    
contains
    function f(x) result(res)
        real(8), intent(in) :: x
        real(8) :: res
        res = 2*exp(x) - 2*x**2 - 4
    end function f
end module functionmodule

module secantmodule
    use functionmodule
    implicit none
contains
    subroutine secantmethod(a, b, eps, sec)
        real(8), intent(in) :: a, b, eps
        character(len = *), intent(in) :: sec
        real(8) :: x2, x0, x1
        integer :: u = 1, ios, i = 0
        x0 = a; x1 = b
        open(unit = u, iostat = ios, file = sec, &
        status = "replace", action = "write")
        write(u, *) 'step    root value'
        write(u, *) '------------------'
        100 x2 = x1 - f(x1)*(x1-x0)/(f(x1) - f(x0))
            i = i + 1
            if (abs(x2-x1) < eps) then
                goto 200
            end if
            x0 = x1
            x1 = x2
            write(u,'(i3, f15.7)') i, x2
            goto 100
        200 continue
    end subroutine secantmethod
end module secantmodule

program main
    use secantmodule
    implicit none
    real(8) :: a, b, eps

    ! [a b] is the interval containing the solution.
    a = -2.0
    b = 3.0
    eps = 1e-7

    call secantmethod(a, b, eps, "secantmethod.dat")
end program main