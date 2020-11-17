

1.1.5 Teaser
What is the purpose of the program?
    The program sorts the characters in the given string by the ASCII collating sequence.

Describe the structure of the program?  What is the new (i.e., not yet discussed) feature used here and why is it useful?
    It is the standard Fortran structure:

    program Teaser
    implicit none

    ! Declaration Part

    ! Execution Part

    end program Teaser

    But instead of executing the algorithm from inside the execution part directly, we call a subroutine containing our algorithm from outside the program.
    This makes the code more readable and should be the preferred way of dealing with larger algorithms. Debugging will be easier and testing the program can
    be reduced to smaller unit tests of single routines.

What changes could improve the readability of the program?
    Give the subroutine a name that describes what it does. E.g. "sort_string".

Can you identify the algorithm that is used in the program?
    Bubble Sort. It compares two characters next to each other and changes them, if the second one precedes the first one.


1.2.2 Bug Hunting
The term i/(n-1) will result in 0 for i in [0,9] and 1 for i=10 because of integer division.
For i = 0 i/(n-1) evaluates to 0.
For i in [1,9] the calculation gives some value < 1 which gets rounded to the integer 0.
This can be fixed by converting the data type using intrinsic functions --> REAL(a, KIND=8)
