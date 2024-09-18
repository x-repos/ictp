module pkgfunction
    implicit none
contains

    ! Function
    real(8) function f(x) result(res)
        real(8), intent(in) :: x
        res = exp(x)
    end function f

    ! Calculate the integeral with the limit from a and b
    ! n is the number of point
    real(8) function rectangle(a, b, n) result (res)
        integer, intent(in) :: n
        real(8), intent(in) :: a, b
        integer :: i
        real(8) :: x, h
        res = 0.0
        h = (b-a)/real(n,8)
        x = a
        do i = 0, n-1
            res = res + h*f(x)
            x = x + h
        end do
    end function rectangle


    real(8) function midpoint(a, b, n) result (res)
        integer, intent(in) :: n
        real(8), intent(in) :: a, b
        integer :: i
        real(8) :: x, h
        res = 0.0
        h = (b-a)/real(n,8)
        x = a+h/2
        do i = 0, n-1
            res = res + h*f(x)
            x = x + h
        end do
    end function midpoint


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


    real(8) function simpson(a, b, n) result (res)
        integer, intent(in) :: n
        real(8), intent(in) :: a, b
        integer :: i
        real(8) :: x, h
        x = a
        res = 0.0
        h = (b-a)/real(n,8)
        do i = 1, n-1
            x = x + h
            if (mod(i,2) .eq. 1) then
                res = res + 4*f(x)
            else
                res = res + 2*f(x)
            end if
        end do
        res = (res + f(a) + f(b))*h/3.0
    end function simpson
end module pkgfunction

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
module pkgmodule
    use pkgfunction
    implicit none
contains
    
    ! Calculate the integeral with the precision of epsilon
    ! Call rectangle
    subroutine rectangleInt(a, b, eps, filename)
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
            resEstNew = rectangle(a, b, n)
            write(u, '(i10, i10, f20.7)') i, n, resEstNew
            ! Compare the new integral and old integral
            if (ABS(resEstNew-resEstOld) < eps) then
                exit
            end if
        end do
        print*, 'rectangle'
        print('(i10, f15.7)'), n, resEstNew
        close(u)
    end subroutine rectangleInt

    ! call midpoint
    subroutine midpointInt(a, b, eps, filename)
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
            resEstNew = midpoint(a, b, n)
            write(u, '(i10, i10, f20.7)') i, n, resEstNew
            ! Compare the new integral and old integral
            if (ABS(resEstNew-resEstOld) < eps) then
                exit
            end if
        end do
        print*, 'midpoint'
        print('(i10, f15.7)'), n, resEstNew
        close(u)
    end subroutine midpointInt

    !Cal trapezoid
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

    ! call simpson
    subroutine simpsonInt(a, b, eps, filename)
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
            resEstNew = simpson(a, b, n)
            write(u, '(i10, i10, f20.7)') i, n, resEstNew
            ! Compare the new integral and old integral
            if (ABS(resEstNew-resEstOld) < eps) then
                exit
            end if
        end do
        print*, 'simpson'
        print('(i10, f15.7)'), n, resEstNew
        close(u)
    end subroutine simpsonInt

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
    call rectangleInt(a, b, eps, "rectangle.dat")
    call midpointInt(a, b, eps, "midpoint.dat")
    call trapezoidInt(a, b, eps, "trapezoid.dat")
    call simpsonInt(a, b, eps, "simpson.dat")
    print*, 'precise result of analytic solution'
    print('(f15.7)'), real(exp(1.0)-1,8)
end program main
