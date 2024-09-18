! Probability distribution function : 3*x^2

module functionmodule
    implicit none
contains
    function func(x) result(res)
        implicit none
        real(8) :: x
        real(8) :: res
        res = 3*x**2
    end function func
end module functionmodule

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

module rejectionmodule
    use functionmodule
    implicit none
contains
    subroutine rejection(x, xmin, xmax, npoints, fmax)
        real(8), intent(inout), allocatable :: x(:)
        real(8), intent(in) :: xmin, xmax, fmax
        integer, intent(in) :: npoints

        integer :: i
        real(8) :: u, r
        
        ! the fmax is required to find before proceeding rejection
        allocate(x(npoints))
        do i = 1, npoints
            do
                call random_number(r)
                r = xmin + (xmax - xmin)*r
                call random_number(u)
                if ( u < func(r)/fmax ) then
                    exit
                end if
            end do
            x(i) = r
        end do
    end subroutine rejection
end module rejectionmodule

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

module histogrammodule
    implicit none
    
contains
    subroutine histogram(x, xmin, xmax, n, filename)
        real(8), intent(in), dimension(:) :: x
        real(8), intent(in) :: xmin, xmax
        integer, intent(in) :: n
        character(len = *), intent(in) :: filename


        real(8) :: iqr, x14, x34, dxapprox, dx
        integer :: i, j, nbin, ios
        real(8), dimension(:), allocatable :: h, xh

        x14 = x(int(1.0/4*n));
        x34 = x(int(3.0/4*n));
        iqr = x34 - x14

        ! freedman-diaconis rule
        dxapprox = iqr*2.0/n**(1.0/3)
        nbin = floor((xmax - xmin)/dxapprox) + 1
        dx = (xmax - xmin)/nbin
        
        ! count for historgram
        allocate(h(nbin));  h = 0.0
        allocate(xh(nbin)); xh = 0.0

        do i = 1, n
            j = floor((x(i)-xmin)/dx) + 1
            if ( j>nbin ) then
                go to 100
            end if
            h(j) = h(j) + 1
            100 continue
        end do

        ! normalize
        do i = 1, nbin
            h(i) = h(i)/(n*dx)
            xh(i) = xmin + (i-0.5)*dx
        end do

        ! write into file to use gnuplot
        open(unit = 100, iostat = ios, file = filename, status = 'replace', action = 'write')
        do i = 1, nbin
            write(100, *) xh(i), h(i)
        end do
        close (100)
        deallocate(h)
        deallocate(xh)
      
    end subroutine histogram
end module histogrammodule

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
        ngauss = 10000
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
    use rejectionmodule
    use filemodule
    use histogrammodule
    use kernelmodule

    implicit none
    
    real(8), dimension(:), allocatable :: x
    real(8) :: xmin, xmax
    integer :: n
    real(8), parameter :: fmax = 5.0

    call randseed(9999)

    ! condition
    xmin = 0.0
    xmax = 1.0
    n = 1000

    ! call rejection
    call rejection(x, xmin, xmax, n, fmax)
    call sort(x)
    ! histogram
    ! call histogram(x, xmin, xmax, n, 'histogram-rejection.dat')
    call kernel(x, xmin, xmax, n, 'kernel-rejection.dat')
    call execute_command_line('gnuplot -p plot-kernel-rejection.plt')
    ! write into file
    call writearray(x, n, 'rejection.dat')
end program main    