module functionPkg
	implicit none
	!use modulePkg
contains
	
	real function maxvaluetemp(day, temp) result(res)
		integer, intent(in), dimension(365) :: day
		real, intent(in), dimension(365) :: temp
		integer :: i
		res = temp(1)
		do i = 1, 365
			if( res < temp(i))then
				res = temp(i)
			end if
		end do
	end function maxvaluetemp
	
	real function minvaluetemp(day, temp) result(res)
		integer, intent(in), dimension(365) :: day
		real, intent(in), dimension(365) :: temp
		integer :: i
		res = temp(1)
		do i = 1, 365
			if( res > temp(i))then
				res = temp(i)
			end if
		end do
	end function minvaluetemp
	
	real function avgvaluetemp(day, temp) result(res)
		integer, intent(in), dimension(365) :: day
		real, intent(in), dimension(365) :: temp
		integer :: i
		res = sum(temp)/real(365)
	end function avgvaluetemp

	real function temparatureEuq(x, tempavg) result (res)
		real, intent(in) :: x, tempavg
		real :: pi
		pi = 4*atan(1.0)
		res = 10*sin((pi*((real(x)-109.5)/183.0))) + 12.2 - tempavg
	end function temparatureEuq

	real function rootindatebisection(x, y, day, temp) result(res)	
		integer, intent(in), dimension(:)::day
		real, intent(in), dimension(:) :: temp
		real, intent(in) :: x, y
		REAL :: a, b, c
		INTEGER ::  i = 0
		real :: eps = 0.01, avgtemp
		avgtemp = avgvaluetemp(day, temp)
		print*, avgtemp

		a = x; b = y
		DO	
			i = i + 1
			c = (a + b)/2.0						
			IF ((temparatureEuq(c,avgtemp)*temparatureEuq(b,avgtemp)) < 0) THEN
				a = c
			ELSE
				b = c 
			END IF
			IF (ABS((a-b)) < eps) THEN			
				EXIT							
				res = c
			END IF
		END DO
		
	end function rootindatebisection


end module functionPkg



module modulePkg
	use functionPkg
	implicit none
contains
	subroutine readdata(filename, day, temp)
		character(len = *), intent(in) :: filename
		integer, intent(inout), dimension(365) :: day
		real, intent(inout), dimension(365) :: temp
		integer :: u = 1, ios, i
		open(unit = u, iostat = ios, file = filename, &
		status = "old", action = "read")
		do i = 1, 365
			read(u, *) day(i), temp(i)
		end do
	end subroutine readdata
	

	
	subroutine tempbelowApril(day, temp)
		integer, intent(in), dimension(:)::day
		real, intent(in), dimension(:) :: temp

		real :: a, b
		real :: date
		integer :: i
		a = real(31+28+31+1); b = real(31+28+31+30)
		print*, rootindatebisection(a,b, day, temp)
	end subroutine tempbelowApril
end module modulePkg

program main
	use functionPkg
	use modulePkg
	implicit none
	!real, external :: maxvaluetemp
	integer, dimension(365) :: day
	real, dimension(365) :: temp
	integer :: i
	real :: maxtemp, mintemp, avgtemp
	call readdata("temperature_2010_Trieste.dat", day, temp)
	!write(*,"(365(i10,f12.5))") day, temp
	!print*, day, temp
	maxtemp = maxvaluetemp(day, temp)
	mintemp = minvaluetemp(day, temp)
	avgtemp = avgvaluetemp(day, temp)
	print*, maxtemp, mintemp, avgtemp
	call tempbelowApril(day, temp)

	
end program main
