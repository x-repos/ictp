module integral
    implicit none
contains

    ! Define the function
    function f(x) result(res)
        real(8), intent(in) :: x
        real(8) :: res
        res = exp(x)
    end function f

    ! Calculate the integral of function
    function simpsonInt(a, b, n) result (res)
        integer, intent(in) :: n
        real(8), intent(in) :: a, b
        integer :: i
        real(8) :: x, h, res
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
    end function simpsonInt

    ! Loop until getting the desirable precision
    ! subroutine simpson(a, b, eps, filename)
    !     real(8), intent(in) :: a, b, eps
    !     real(8) :: resEstOld, resEstNew
    !     character(len = *) :: filename
    !     integer :: i, n, ios, u
    !     u = 1
    !     i = 0
    !     n = 1
    !     open(unit = u, iostat = ios, file = filename, &
    !     status = 'replace', action = 'write')
    !     do
    !         i = i + 1
    !         n = n * 2
    !         resEstOld = resEstNew
    !         resEstNew = simpsonInt(a, b, n)
    !         write(u, '(i10, i10, f20.7)') i, n, resEstNew
    !         ! Compare the new integral and old integral
    !         if (ABS(resEstNew-resEstOld) < eps) then
    !             exit
    !         end if
    !     end do
    !     print*, 'simpson'
    !     print('(i10, f15.7)'), n, resEstNew
    !     close(u)
    ! end subroutine simpson
end module integral

program main
    use integral
    implicit none
    real(8) :: a, b, eps
    a = 0.; b = 1.; eps = 0.00001

    ! call simpson(a, b, eps, 'abc.txt')
    print*, simpsonInt(a, b, 10)

end program main


   


