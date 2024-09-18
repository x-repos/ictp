module functionmodule
    implicit none
    
contains
    function f(x) result(res)
        real(8), intent(in) :: x
        real(8) :: res
        res =  x**3 - sin(x)
    end function f

    function f1(x) result(retval)
        real(8), intent(in) :: x
        real(8) :: retval
        retval = 3*x**2 - cos(x)
    end function f1

end module functionmodule

module newtonraphsonmodule
    use functionmodule
    implicit none
    
contains
    subroutine newtonraphson(x0, eps, x1, nint)
        implicit none
        real(8), intent(in) :: x0, eps
        real(8), intent(out) :: x1
        integer, intent(out) :: nint
        ! character(len = *), intent(in) :: filename
        integer :: i, u = 1, ios
        real(8) :: x2
        x1 = x0
        i = 0
        ! open(unit = u, iostat = ios, file = filename, &
        ! status = 'replace', action = 'write')
        ! write(u, *) 'Newton method'
        ! write(u, *) 'Iternation num.     Guess value             df'
        ! write(u, *) '-----------------------------------------------------'
        100 i = i + 1
            x2 = x1
            x1 = x1 - f(x1)/f1(x1)
            ! write(u,'(i3, f30.10, f20.10)') i, x1, abs(x2 - x1)
            if (abs(x2-x1) < eps) then
                goto 200
            end if
        goto 100
        200 continue
        nint = i


        ! write(u, *) 
        ! write(u, *) 
        ! write(u, *) '-----------------------------------------------------'
        ! close(u)
    end subroutine newtonraphson  
end module newtonraphsonmodule
module sortmodule
    implicit none
contains
    subroutine sort(x)
        real(8),dimension(:), intent(inout) :: x
        real(8) :: temp
        integer :: i, j

        do i = 1, size(x)
            do j = i+1, size(x)
                if ( x(i) > x(j) ) then
                    temp = x(j)
                    x(j) = x(i)
                    x(i) = temp
                end if
            end do
        end do
    end subroutine sort
end module sortmodule

program main
    use newtonraphsonmodule
    use sortmodule
    implicit none
    real(8) :: eps
    real(8) :: a, b
    integer :: i, n, check
    real(8), dimension(:), allocatable :: x0, x1
    integer, dimension(:), allocatable :: niter
    
    eps = 1e-8
    a = -2.0;
    b = 2.0
    n = 100
    allocate(x0(n))
    allocate(x1(n))
    allocate(niter(n))
    do i = 1, n
        x0(i) = a + 0.04*(i-1)
    end do

    
    ! x0 is the first position to find root.
    ! position of x0 is need to be same valey of root.

    do i = 1, n
        call newtonraphson(x0(i), eps, x1(i), niter(i))
        ! print'(f30.10, i3, f20.10)', x0(i), niter(i), x1(i)
    end do
    
    call sort(x1)

    check = 0
    do i = 1, n
        print*, x1(i)
    end do
    print*
    print*
    do i = 1, n-1
        if ((x1(i+1) - x1(i) > 100*eps) .and. (check .eq. 0)) then
            print*, x1(i)
            print*, x1(i + 1)
            check = 1
            go to 200
        end if
        if ((x1(i+1) - x1(i) > 100*eps) .and. (check .eq. 1)) then
            print*, x1(i + 1)
            print*, 'xxx'
        end if
        200 continue
    end do

end program main