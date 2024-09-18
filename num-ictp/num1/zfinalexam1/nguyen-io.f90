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
        
        allocate(x(n))
        do i = 1, n
            read(u, *) x(i)
        end do
        close(u)

    end subroutine readarray
end module filemodule

module averagetempmodule
implicit none
contains
    subroutine averagetemp(x,n,a,b,meantemp)
        real(8), intent(in), dimension(:) :: x 
        integer, intent(in) :: n, a, b
        real(8), intent(out) :: meantemp
        
        integer :: i
        
        meantemp = 0.0
        do i = a, b
            meantemp = meantemp + x(i)
        end do
        meantemp = meantemp/(30+31+31)
    end subroutine
end module averagetempmodule

module warmestdaymodule
implicit none
contains
    subroutine warmestday(x, n, m)
    real(8), intent(in), dimension(:) :: x 
    integer, intent(in) :: n
    integer, intent(out) :: m
    real(8) :: maxtemp
    integer :: i
    m = 0
    maxtemp = maxval(x)
    do i = 1, n
        if (abs(x(i)-maxtemp) < 1e-5) then
            if ((i >= 31+28+31+30+31+1) .and. (i <= 31+28+31+30+31+30)) then
                m = 6
            end if
            if ((i >= 31+28+31+30+31+30+1) .and. (i <= 31+28+31+30+31+30+31)) then
                m = 7
            end if
            if ((i >= 31+28+31+30+31+30+31+1) .and. (i <= 31+28+31+30+31+30+31+31)) then
                m = 8
            end if
        end if
        if (m == 0) then
            m = mod(i,30)
        end if
    end do
    end subroutine warmestday
end module warmestdaymodule


program main

    use filemodule
    use averagetempmodule
    use warmestdaymodule
    implicit none
    
    real(8), dimension(:), allocatable :: x1, x2, x3
    integer :: i, n1, n2, n3, m1, m2, m3
    real(8) :: mean1, mean2, mean3
    integer :: firstday1, lastday1, firstday2, lastday2, firstday3, lastday3
    real(8) :: maxtemp1, maxtemp2, maxtemp3
    n1 = 365
    n2 = 365
    n3 = 366
    call readarray(x1, n1, 'temperature_2010_Trieste.dat')
    call readarray(x2, n2, 'temperature_2011_Trieste.dat')
    call readarray(x3, n3, 'temperature_2012_Trieste.dat')
    
    print*, "        2010        2011       2012"
    print*
    do i = 1, n1
        print('(f12.1, f12.1, f12.1)'), x1(i), x2(i), x3(i)
    end do
    print('(f36.1)'), x3(n3)
    
    firstday1 = 31+28+31+30+31+1
    lastday1 = firstday1 + 30 + 31 + 31
    firstday2 = firstday1
    lastday2 = lastday1
    firstday3 = firstday1 + 1
    lastday3 = lastday1 + 1

    call averagetemp(x1, n1, firstday1, lastday1, mean1)
    call averagetemp(x2, n2, firstday2, lastday2, mean2)
    call averagetemp(x3, n3, firstday3, lastday3, mean3)

    print*
    print*, "Average summer temperature of 2010, 2011, 2012 (respectively):"
    print('(f12.5, f12.5, f12.5)'), mean1, mean2, mean3
    print*
    if ((mean1 > mean2) .and. (mean1 > mean3)) then
        print*, "2010 is the day has greatest daily average summer temperature"
    end if
    if ((mean2 > mean3) .and. (mean2 > mean1)) then
        print*, "2011 is the day has greatest daily average summer temperature"
    else
        print*, "2012 is the day has greatest daily average summer temperature"
    end if

    print*
    print*,"Bonus question"
    call warmestday(x1, n1, m1)
    call warmestday(x2, n2, m2)
    call warmestday(x3, n3, m3)
    print*, "The month contains the warmest day in years 2010, 2011, 2012:"
    print*, m1, m2, m3
end program main
