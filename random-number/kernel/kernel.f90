module filemodule
    implicit none

contains
    subroutine writearray(x, n, filename)
        real(8), intent(in), dimension(:) :: x
        integer, intent(in) :: n
        character(len = *), intent(in) :: filename

        integer :: ios, i

        open(100, iostat = ios, file = filename, &
        status = 'replace', action = 'write')

        write(100, *) n
        do i = 1, n
            write(100, *) x(i)
        end do
        close(100)
    end subroutine writearray

    subroutine readarray(x, n, filename)
        integer, intent(inout) :: n
        real(8), intent(inout), dimension(:), allocatable :: x
        character(len = *), intent(in) :: filename
        integer :: u, ios, i

        u = 1
        open(unit = u, iostat = ios, file = filename, status = 'old', action = 'read')
        read(u,*) n
        allocate(x(n))
        do i = 1, n
            read(u, *) x(i)
        end do
    end subroutine readarray
end module filemodule

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

module kernelmodule
    implicit none
    
contains
    function gaussianfunc(x,s,xi) result(res)
        real(8), intent(in) :: x,s,xi
        real(8) :: res
        real(8), parameter :: pi = 4.0 * atan(1.0)
        res = 1/(s*sqrt(2*pi)) * exp(-1.0/2*((x-xi)/s)**2)
    end function gaussianfunc

    subroutine kernel(x, xmin, xmax, n, filename)
        real(8),dimension(:), intent(inout) :: x
        real(8), intent(in) :: xmin, xmax
        integer, intent(in) :: n
        character(len = *), intent(in) :: filename
        
        real(8) :: h, xi, iqr, a, s, x14, x34
        integer :: ngauss, i, j, u, ios
        real(8), allocatable, dimension(:) :: pointkernel
        u = 1
        open(unit = u, iostat = ios, file = filename,&
        status = 'replace', action = 'write')

        ! calculate the iqr

        x14 = x(int(1.0/4*n));
        x34 = x(int(3.0/4*n));
        iqr = x34 - x14

        ! A = max(sigma, iqr/1.34). Normally sigma = 0.5
        a = iqr/1.34
        s = 0.9*a/n**(1.0/5)
        ngauss = 1000
        allocate(pointkernel(ngauss))
        pointkernel = 0.0
        h = (xmax - xmin)/ngauss
        do i = 1, ngauss+1
            xi = xmin + (i-1)*h
            do j = 1, n
                pointkernel(i) = pointkernel(i) + gaussianfunc(x(j), s, xi)
            end do
            pointkernel(i) = pointkernel(i)/n
            write(u,*) xi, pointkernel(i)
        end do
        close(u)

    end subroutine kernel
end module kernelmodule


program main
    use filemodule
    use sortmodule
    use kernelmodule
    implicit none
    
    real(8), allocatable, dimension(:) :: x
    integer :: n
    real(8) :: xmin, xmax
    
    call readarray(x, n, 'rejection.dat')
    call sort(x)

    xmin = 0
    xmax = 1
    call kernel(x, xmin, xmax, n, 'kernel.dat')
    call execute_command_line('gnuplot -p plot-kernel.plt')
end program main
