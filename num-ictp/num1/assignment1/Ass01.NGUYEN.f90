PROGRAM myfirstprogram                      !Open the program
    IMPLICIT NONE                           !make sure that i, j, k, l, m, n are not taken
                                            !as integer variable automatically.
    REAL :: a, b, res                       !declare that a, b and res are real variables
    INTEGER :: n                            !declare that n is the interger variable
    PRINT*, 'Enter two numbers a and b:'    !print to screen
    READ*, a, b                             !read a and b value from the keyboard
    PRINT*, 'n: '                           !print to screen
    READ*, n                                !read n value from the keyboard
    res = (a+b)**n                          !calcuate the value of res as res = (a+b)^n
    PRINT*, 'The value of res: ', res       !print the value of res to the screen
END PROGRAM myfirstprogram                  !Close the program