MODULE modulex                                                      !Module
    IMPLICIT NONE
CONTAINS
    REAL FUNCTION meanCal(array, nElement) RESULT(res)              !Function to calcuate the mean value
        INTEGER, INTENT(IN) :: nElement
        REAL, DIMENSION(nElement), INTENT(IN) :: array
        res = SUM(array)/nElement
    END FUNCTION meanCal
END MODULE modulex

program main                                                        !Program
    USE modulex                                                     !Call the module
    IMPLICIT NONE
    REAL, DIMENSION(:), ALLOCATABLE :: precipitation, temperature
    REAL :: preAverage, temAverage
    INTEGER :: i = 0, nElement = 500                                !Decleare the number of array
    INTEGER :: u = 1, ios
    OPEN(UNIT = u, IOSTAT = ios, FILE = 'numerical.dat', &          !Open the file, with u = 1.
    STATUS='old', ACTION = 'read')
    ALLOCATE(precipitation(nElement))
    ALLOCATE(temperature(nElement))
    READ(u,*)                                                       !Read the first 3 comment line
    READ(u,*)
    READ(u,*)
    
    IF ( ios .NE. 0 ) THEN                                              
        PRINT*, 'Cannot open the file'
    ELSE
        DO
            i = i + 1
            READ(u, '(f12.5, x, f12.5)', IOSTAT = ios) &
            precipitation(i), temperature(i)
            IF (ios .NE. 0) EXIT                                    !Exit if go the the end of the file
        END DO
        nElement = i-1
        preAverage = meanCal(precipitation, nElement)
        temAverage = meanCal(temperature, nElement)
        PRINT*, 'Average Precipitation and Teperature:'
        PRINT('(f12.5, x, f12.5)'), preAverage, temAverage         !Print screen the values of Precipitation end Temp
    END IF
    CLOSE(u)
END PROGRAM main