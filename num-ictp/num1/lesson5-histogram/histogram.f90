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

program main
    use filemodule
    use sortmodule
    use histogrammodule

    implicit none
    
    real(8), allocatable, dimension(:) :: x
    integer :: n
    real(8) :: xmin, xmax
    
    call ranseed(9999)
    call readarray(x, n, 'rejection.dat')
    call sort(x)
    
    xmin = 0.0
    xmax = 1.0
    ! general case:
    ! xmin = minval(x)
    ! xmax = maxval(x)
    call histogram(x, xmin, xmax, n, 'histogram.dat')
    call execute_command_line('gnuplot -p plot-histogram.plt')
end program main
