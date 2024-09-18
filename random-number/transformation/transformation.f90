! f(x) = 3*x^2
! => integral (0->x) of 3*x^2 = x^3
! => u = sqrt(3) of x

module functionmodule
    implicit none
contains
    function func(x) result(res)
        implicit none
        real(8) :: x
        real(8) :: res
        res = x**(1.0/3)
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

module transformationmodule
    use functionmodule
    implicit none
contains
    subroutine transformation(x, xmin, xmax, n)
        real(8), intent(inout), allocatable :: x(:)
        real(8), intent(in) :: xmin, xmax
        integer, intent(in) :: n

        real(8) :: u
        integer :: i
        allocate(x(n))

        do i = 1, n
            call random_number(u)
            ! u = u * 11
            x(i) = func(u) * (xmax - xmin) + xmin
        end do
        ! since xmin and xmax of x already from 0->1 we dont need to do anything more.
    end subroutine transformation
end module transformationmodule

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

program main
    use functionmodule
    use seedmodule
    use sortmodule
    use transformationmodule
    use filemodule
    use histogrammodule

    implicit none
    
    real(8), dimension(:), allocatable :: x
    real(8) :: xmin, xmax
    integer :: n

    call randseed(9999)

    ! condition
    xmin = 0.0
    xmax = 1.0
    n = 1000

    ! call transformation
    call transformation(x, xmin, xmax, n)
    call sort(x)
    
    ! histogram
    call histogram(x, xmin, xmax, n, 'histogram-transformation.dat')
    call execute_command_line('gnuplot -p plot-histogram-transformation.plt')
    call writearray(x, n, 'transformation.dat')


end program main    