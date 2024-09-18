module pkgfunction
    implicit none
contains

    ! Function
    real(8) function f(x) result(res)
        real(8), intent(in) :: x
        res = sin(x**2)
    end function f

    ! Calculate the integeral with the limit from a and b
    ! n is the number of point

    real(8) function trapezoid(a, b, n) result (res)
        integer, intent(in) :: n
        real(8), intent(in) :: a, b
        integer :: i
        real(8) :: x1, x2, h
        res = 0.0
        h = (b-a)/real(n,8)
        x2 = a
        do i = 0, n-1
            x1 = x2
            x2 = x1 + h
            res = res + h*(f(x1) + f(x2))/2.0
        end do
    end function trapezoid
end module pkgfunction

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
module pkgmodule
    use pkgfunction
    implicit none
contains
    
    ! Calculate the integeral with the precision of epsilon
    ! Cal trapezoid
    subroutine trapezoidInt(a, b, eps, filename)
        real(8), intent(in) :: a, b, eps
        real(8) :: resEstOld, resEstNew
        character(len = *) :: filename
        integer :: i, n, ios, u
        u = 1
        i = 0
        n = 1
        open(unit = u, iostat = ios, file = filename, &
        status = 'replace', action = 'write')
        do
            i = i + 1
            n = n * 2
            resEstOld = resEstNew
            resEstNew = trapezoid(a, b, n)
            write(u, '(i10, i10, f20.7)') i, n, resEstNew
            ! Compare the new integral and old integral
            if (ABS(resEstNew-resEstOld) < eps) then
                exit
            end if
        end do
        print*, 'trapezoid'
        print('(i10, f15.7)'), n, resEstNew
        close(u)
    end subroutine trapezoidInt

  
end module pkgmodule

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
program main
    use pkgfunction
    use pkgmodule
    real(8) :: a, b, eps
    a = 0.0
    b = 1.0
    eps = 1e-5
    print*, 'final result: n and integral value'
    call trapezoidInt(a, b, eps, "trapezoid.dat")
end program main
