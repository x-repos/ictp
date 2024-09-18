module hitormissmodule
    implicit none
    
contains
    function f(x) result(res)
        real(8), intent(in) :: x
        real(8) :: res
    
        res = x**2
    end function f

    function hitormiss(a, b, n) result(retval)
        real(8), intent(in) :: a, b
        real(8) :: retval
        integer, intent(in) :: n

        integer :: i
        real(8) :: u
        retval = 0

        do i = 1, n
            call random_number(u)
            u = u*(b-a) + a
            retval = retval + f(u)
        end do
        retval = retval/n
        
    end function hitormiss
end module hitormissmodule

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
    use seedmodule
    use hitormissmodule

    implicit none
    
    call randseed(99999)
    print*, hitormiss(real(0.0,8), real(1.0,8), 100000)

end program main