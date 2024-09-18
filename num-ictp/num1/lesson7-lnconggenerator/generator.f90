module genernatormodule
    implicit none
    
contains
    subroutine linearcongruential(x, a, c, m, x0)
        real(8), intent(inout), dimension(:) :: x
        integer(selected_int_kind(32)), intent(in) :: a, c, m, x0
        integer(selected_int_kind(32)) :: i, n
        integer(selected_int_kind(32)), dimension(:), allocatable :: y

        n = size(x)
        allocate(y(n))
        y(1) = x0
        x(1) = y(1)/real(m,8)
        do i = 1, n-1
            y(i+1) = mod((y(i) * a + c), m)
            x(i+1) = y(i+1)/real(m,8)
        end do
    end subroutine linearcongruential
end module genernatormodule

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
        print*, xmin, xmax
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
        print*, nbin
        do i = 1, nbin
            write(100, *) xh(i), h(i)
        end do
        close (100)
        deallocate(h)
        deallocate(xh)
        
      
    end subroutine histogram
end module histogrammodule


program main
    use genernatormodule
    use sortmodule
    use histogrammodule
    implicit none
    
    real(8), dimension(:), allocatable :: x
    integer(selected_int_kind(32)) :: a, c, m, n, i, x0

    a = 777121212
    c = 147442321
    m = 12145453
    x0 = 1174712

    n = 10000
    allocate(x(n))
    call linearcongruential(x, a, c, m, x0)
    ! do i = 1, n
    !     print*, x(i)
    ! end do
    call sort(x)
    call histogram(x, real(0,8), real(1,8), int(n), 'random.dat')
    call execute_command_line('gnuplot -p plot-random.plt')


end program main