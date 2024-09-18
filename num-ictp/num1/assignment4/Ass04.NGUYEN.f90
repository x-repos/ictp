MODULE modulex                                      ! Module 
    IMPLICIT NONE                                   
CONTAINS                                            ! Module contains: initialize, swap and sort
    SUBROUTINE initialize(x)                        
        IMPLICIT NONE
        REAL, DIMENSION(:), INTENT(INOUT) :: x
        CALL RANDOM_NUMBER(x)                       ! Save the random numer to the x array
        x = x*100.0
    END SUBROUTINE initialize

    SUBROUTINE swap(x, y)                           ! Swap x, y if x > y
        IMPLICIT NONE
        REAL, INTENT(INOUT) :: x, y
        REAL :: a
        a = x
        x = y
        y = a
    END SUBROUTINE swap

    SUBROUTINE sort(x)                              ! Sort array x
        IMPLICIT NONE
        REAL, DIMENSION(:), INTENT(INOUT) :: x
        INTEGER :: i, j
        DO i = 1, size(x)-1
            DO j = i + 1, size(x)
                IF (x(i) > x(j)) THEN
                    CALL swap(x(i), x(j))           ! Call swap
                END IF
            END DO
        END DO
    END SUBROUTINE sort
END MODULE modulex

PROGRAM main
    USE modulex
    IMPLICIT NONE
    REAL, DIMENSION(:), ALLOCATABLE :: x
    INTEGER :: i, n
    PRINT*, 'Press the number of array: '
    READ*, n                                        ! Input: the number of array
    ALLOCATE(x(n))
    CALL initialize(x)                              ! Initialize a random array
    PRINT*, 'Random array before sorting'           ! Print the array before sorting
    DO i = 1, n
        PRINT*, x(i)
    END DO
    CALL sort(x)
    PRINT*, 'Random array after sorting'            ! Print the array after sorting 
    DO i = 1, n
        PRINT*, x(i)
    END DO
END PROGRAM main