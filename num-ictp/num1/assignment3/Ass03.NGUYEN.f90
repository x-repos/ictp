LOGICAL FUNCTION MarkPoint(x, y) RESULT(mark)       ! Check the point is outside or inside the Circle
    REAL(4), INTENT(in) :: x, y                     ! Declare x, y as input.
    mark = .false.
    IF (sqrt(x**2 + y**2) <= 1.0) THEN              ! Condition.
        mark = .true.
    END IF
END FUNCTION MarkPoint

PROGRAM main
    IMPLICIT NONE
    INTEGER :: i, count = 0, nloop = 1000000
    REAL(4) :: x, y, pi
    REAL, PARAMETER :: piStandard = 4*atan(1.0)     ! Precise pi by using the trigonometric function
    LOGICAL, EXTERNAL :: MarkPoint                  ! Import the MarkPoint function.
    DO i = 1, 10
        CALL RANDOM_NUMBER(x)
        CALL RANDOM_NUMBER(y)
        IF (MarkPoint(x,y) .EQV. .TRUE.) THEN       ! Check the status of a point by using MarkPoint
            count = count + 1                       ! function.
        END IF
        pi = REAL(count,4)/(i-1)*4                  ! Calcuate the value of pi.
        IF (ABS(pi - piStandard) < 1E-5) THEN       ! Exit if Estimated pi almost reaches the 
        	PRINT*, i
            EXIT                                    ! precise pi. Chose 1E-5 as the epsilon to make
        END IF                                      ! sure that they have at least four exace digits
    END DO                                          ! after the dot.
    print*, 'The Loop is terminated at ith:', i
    print*, 'Estimated Pi:  ', pi
    print*, 'Precise Pi:    ', piStandard
    print*, 'epsilon:       ', pi - piStandard
END PROGRAM main

! The result of estimated pi changes after each run because we are using the random numbers (coordinate).
! Each point lays inside the circle with the distinct probability.
