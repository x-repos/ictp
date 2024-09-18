module functionmodule
    implicit none
    
contains
    function f(x) result(res)
        real(8), intent(in) :: x
        real(8) :: res
        res = x**3 - 5*x
    end function f

    function f1(x) result(retval)
        real(8), intent(in) :: x
        real(8) :: retval
        retval = 3*x**2 - 5
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
        write(u, *) 'Newton method'
        write(u, *) 'Iternation num.     Guess value             df'
        write(u, *) '-----------------------------------------------------'
        100 i = i + 1
            x2 = x1
            x1 = x1 - f(x1)/f1(x1)
            write(u,'(i3, f30.10, f20.10)') i, x1, abs(x2 - x1)
            if (abs(x2-x1) < eps) then
                goto 200
            end if
        goto 100
        200 continue
        write(u, *) 
        write(u, *) 
        write(u, *) '-----------------------------------------------------'
        ! close(u)
    end subroutine newtonraphson  
end module newtonraphsonmodule

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
        status = "old", action = "write")
        write(u, *) 'Secant method'
        write(u, *) 'Iternation num.     Guess value             dx'
        write(u, *) '-----------------------------------------------------'
        100 x2 = x1 - f(x1)*(x1-x0)/(f(x1) - f(x0))
            i = i + 1
            if (abs(x2-x1) < eps) then
                goto 200
            end if
            x0 = x1
            x1 = x2
            write(u,'(i3, f30.10, f20.10)') i, x2, abs(x1 - x0)
            goto 100
        200 continue
        write(u, *) '-----------------------------------------------------'
        write(u, *) 'In this case, newton is faster'
        close(u)
    end subroutine secantmethod
end module secantmodule


program main
    use newtonraphsonmodule
    use secantmodule
    implicit none
    real(8) :: x0, eps
    real(8) :: a, b

    a =  1.2;
    b = 2.8;
    x0 = 2.8;
    eps = 1e-10

    ! x0 is the first position to find root.
    ! position of x0 is need to be same valey of root.
    
    call newtonraphson(x0, eps, 'newton-secant.dat')
    call secantmethod(a, b, eps, 'newton-secant.dat')

end program main