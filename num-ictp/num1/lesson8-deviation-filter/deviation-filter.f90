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

module meandeviationmodule
    implicit none
    
contains
    subroutine meandeviation(x, n, mean, sigma)
        real(8), dimension(:), intent(in) :: x
        integer, intent(in) :: n
        real(8) :: m
        real(8), intent(out) :: mean, sigma
        integer :: i

        
        mean = sum(x)/n
        m = 0.0
        do i = 1, n
            m = m + (x(i) - mean)**2
        end do
        m = m/n
        sigma = sqrt(m)
    end subroutine meandeviation
end module meandeviationmodule


module meanfiltermodule
    implicit none
    
contains
    subroutine meanfilter(x, n, nfilter, xnew)
        real(8), dimension(:), intent(in) :: x
        real(8), dimension(:), allocatable, intent(inout) :: xnew
        integer, intent(in) :: n, nfilter
        integer :: nboundary, i, j
        
        allocate(xnew(n))
        xnew = 0.0
        nboundary = floor(nfilter/2.0)
        do i = 1, n
            if ((i > nboundary) .and. (i < n - nboundary + 1)) then
                do j = i-nboundary, i + nboundary
                    xnew(i) = xnew(i) + x(j)
                end do
                xnew(i) = xnew(i)/nfilter
            elseif (i <= nboundary) then
                do j = 1, i + nboundary
                    xnew(i) = xnew(i) + x(j)
                end do
                xnew(i) = xnew(i) + (nboundary-i+1)*x(1)
                xnew(i) = xnew(i)/nfilter
            elseif (i >= n - nboundary + 1) then
                do j = i - nboundary, n
                    xnew(i) = xnew(i) + x(j)
                end do
                xnew(i) = xnew(i) + (i + nboundary - n)*x(n)
                xnew(i) = xnew(i)/nfilter
            end if 
        end do
    end subroutine meanfilter
end module meanfiltermodule

program main
    use filemodule
    use meandeviationmodule
    use meanfiltermodule
    implicit none

    real(8), dimension(:), allocatable :: x
    real(8), dimension(:), allocatable :: xnew
    integer :: n, nfilter, i
    real(8) :: mean, sigma, t1, t2

    call cpu_time(t1)

    call readarray(x, n, 'data2.txt')
    call meandeviation(x, n, mean, sigma)
    print'(f12.5, x, f12.5)', mean, sigma
    print*
    
    nfilter = 5
    call meanfilter(x, n, nfilter, xnew)
    do i = 1, n
        print'(f12.5, x, f12.5)', x(i), xnew(i)
    end do

    call cpu_time(t2)
    print'(f12.5, x, f12.5, x, f12.5)', t1, t2, t2-t1
end program main