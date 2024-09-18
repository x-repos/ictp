MODULE assig_1
        IMPLICIT NONE
CONTAINS
        ! function to calculate the f
	REAL(8) FUNCTION f(x) RESULT(res)
		REAL(8), INTENT(IN) :: x
        res = EXP(x)
        END FUNCTION f
        ! function to calculate the integral
	REAL(8) FUNCTION int_rectangles(a, b, n) RESULT(res)
		REAL(8), INTENT(IN) :: a, b
                INTEGER, INTENT(IN) :: n
		REAL(8) :: h, x
                INTEGER :: i
                res = 0.0
                x = a
                h = (b-a)/n
                DO i = 0, n-1
                        res = res + h*f(x)
                        x = x + h
                END DO
        END FUNCTION int_rectangles
END MODULE assig_1

PROGRAM assignment_int_1
	USE assig_1
	IMPLICIT NONE
	REAL(8) :: a, b, eps, resEstOld, resEstNew, res
	INTEGER :: n, i, u, ios
	a = 1.0; b = 3.0; eps = 1e-2
	n = 1; i = 0; u = 1
	res = f(b) - f(a)
	! Open file
	OPEN(UNIT = u, IOSTAT = ios, FILE = 'int.dat', &
	STATUS = 'REPLACE', ACTION = 'WRITE')
	resEstNew = int_rectangles(a, b, n)	
	WRITE(u,'(A)') '              N points    Integrated Value'
	WRITE(u,'(A)') '---------------------------------------------'

	DO 
		i = i + 1
		n = n * 2
		resEstOld = resEstNew
		resEstNew = int_rectangles(a, b, n)
		WRITE(u, '(i10, i10, f20.7)') i, n, resEstNew
		! Compare the new integral and old integral
		IF (ABS(resEstNew-resEstOld) < eps) THEN
			EXIT
		END IF
	END DO
	! Print out the final results
	WRITE(u, '(/, A)') "PRECISE RESULT BY ANALYTIC SOLUTION:"
	WRITE(u, '(f15.7)') res
	WRITE(u, '(A)') "ESTIMATED RESULT BY RECTANGULAR METHOD:"
	WRITE(u, '(f15.7)') resEstNew
	
!	Calculate the integral (0-1) exp(x)dx with precision 1e-5
	n = 1
	DO 
		n = n * 2
		resEstOld = resEstNew
		resEstNew = int_rectangles(real(0,8), real(1,8), n)
		IF (ABS(resEstNew-resEstOld) < 1E-5) THEN
			EXIT
		END IF
	END DO
	! Print out the value integral from 0 to 1
	WRITE(u, '(A)') "INTEGRAL 0 -> 1 OF exp(x)dx with the precision 1E-5:"
	WRITE(u, '(f15.7)') resEstNew
END PROGRAM assignment_int_1
