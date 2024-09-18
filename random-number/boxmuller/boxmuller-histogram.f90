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


program main
    use functionmodule
    use seedmodule
    use sortmodule
    use boxmullermodule
    use histogrammodule
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
    
    call histogram(x, xmin, xmax, n, 'boxmuller-histogram-x.dat')
    call histogram(y, ymin, ymax, n, 'boxmuller-histogram-y.dat')
    call execute_command_line('gnuplot -p plot-boxmuller-histogram.plt')

end program main    