PROGRAM main											! Open the program	
	IMPLICIT NONE										! Make sure that i, j, k, l, m, n are not taken
														! as integer variable automatically.
	INTEGER :: j = 0, m									! Declare Variables
	REAL, PARAMETER :: piStandard = 4*atan(1.0)
	REAL :: pi = 0.0, epsi
	
	PRINT*, 'Enter the value of m'
	READ*, m

	DO													! Loop by using the sum formular
		pi = pi + 4*((-1)**j/(2.*j + 1));				
		epsi =  abs(pi - piStandard)
		IF (j == m) THEN 								! Print the results of pi by m
			PRINT*, 'The value of Pi:', pi
		END IF
		IF (epsi < 1.0e-2 .OR. j > 100) THEN			! Condition to exit the loop when reach the requirements
			Exit										
		END IF
		j = j + 1
	END DO
	PRINT*, 'The final value of m, pi and epsilon are:'	! Print the values
	PRINT*,  j, pi, epsi
END PROGRAM main

! We can get the precision < 10E-2 by using this formula by increase the value of m.