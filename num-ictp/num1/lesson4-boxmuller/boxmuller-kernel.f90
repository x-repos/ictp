! f(x) = 3*x^2
! => integral (0->x) of 3*x^2 = x^3
! => u = sqrt(3) of x

module functionmodule
    implicit none
contains
    function func(x,sigma) result(res)
        implicit none
        real(8), intent(in)::x, sigma
        real(8) :: res
        res = sqrt(-2*sigma**2*log(1-x))
    end function func
end module functionmodule

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

module seedmodule
    implicit none
contains
    subroutine randseed(init)
        implicit none
        integer :: n
        integer, allocatable, dimension(:) :: seed
        integer, intent(in) :: init
        call random_seed(size = n)
        allocate(seed(n))
        seed = init
        call random_seed(put = seed)
        deallocate(seed)
    end subroutine randseed
end module seedmodule

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

module boxmullermodule
    use functionmodule
    implicit none
contains
    subroutine boxmuller(x, y, sigma, n)
        real(8), intent(inout), allocatable :: x(:), y(:)
        integer, intent(in) :: n
        real(8), intent(in) :: sigma

        real(8), parameter :: pi = atan(1.0) * 4
        real(8) :: u, theta, r
        integer :: i
        allocate(x(n))
        allocate(y(n))
        
        ! do i = 1, n
        !     call random_number(u)
        !     x(i) = func(u) * (xmax - xmin) + xmin
        ! end do

        do i = 1, n
            call random_number(u)
            r = func(u,sigma)
            call random_number(u)
            theta = 2*pi*u
            x(i) = r * sin(theta)
            y(i) = r * cos(theta)
        end do

    end subroutine boxmuller
end module boxmullermodule


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
    use functionmodule
    use seedmodule
    use sortmodule
    use boxmullermodule
    use kernelmodule
    use filemodule

    implicit none
    
    real(8), dimension(:), allocatable :: x, y
    real(8) :: xmin, xmax, ymin, ymax, sigma
    integer :: n

    call randseed(9999)

    xmin = 0.0
    xmax = 1.0
    n = 10000
    sigma = 0.5
    call boxmuller(x, y, sigma, n)
    call sort(x)
    call sort(y)
    xmin = minval(x)
    xmax = maxval(x)
    ymin = minval(y)
    ymax = maxval(y)
    
    call kernel(x, xmin, xmax, n, 'boxmuller-kernel-x.dat')
    call kernel(y, ymin, ymax, n, 'boxmuller-kernel-y.dat')
    call execute_command_line('gnuplot -p plot-boxmuller-kernel.plt')

end program main    