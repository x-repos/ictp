module filemodule
    implicit none
    
contains
    subroutine readfile(x, n, filename)
        real, dimension(:), allocatable, intent(inout) :: x
        character(len = *), intent(in) :: filename
        integer, intent(inout) :: n
        integer :: ios, i
        open(100, iostat = ios, file = filename, status = 'old', action = 'read')
        read(100, *) n
        allocate(x(n))
        do i = 1, n
            read(100,*) x(i)
        end do
        close(100)
    end subroutine readfile

    subroutine writefile(x, n, filename)
        real, intent(in) :: x(:)
        character(len = *), intent(in) :: filename
        integer, intent(in) :: n
        integer :: ios, i
        open(100, iostat = ios, file = filename, status = 'replace', action = 'write')
        
        do i = 1, n
            write(100,*) x(i)
        end do
        close(100)
    end subroutine writefile
end module filemodule

program main
    use filemodule


    implicit none
    real, allocatable :: x(:)
    integer :: i, n

    
    call readfile(x, n, 'abc.txt')
    call writefile(x, n, 'bcd.txt')


end program main