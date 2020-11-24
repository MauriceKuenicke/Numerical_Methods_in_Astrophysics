2.1.1 Teaser
    a) The RandomNumbers folder contains one src (source) folder and a Makefile. Make is a build management tool which manages
    the compiling and building process of software. It is useful in projects containing large amounts of files to automate the process
    of compiling and running large codebases. It contains a set of operations executed in the shell.  In this example running "make" will change to the src directory and start a different make file.
    The makefile inside the src directory will then compile the Fortran code.


    b) You can compile thie program by simply running "make" inside the RandomNumbers directory.

    c) The output is always the same, which is unexpected for a program that should give us random numbers. This is done by using random seed keys. This way
    you can make sure to always get the same "random" numbers. 

    d) This is especially useful for testing purposes or generating test data. A seed key makes
    the part of your code which deals with random data predictable and therefore easier to test and debug. You might aswell compare two functions doing the same
    thing but handling random input in a different way. E.g sorting algorithms which do the same thing differently. They have to run on the same set of "random" 
    data. You can deactivate it in this program by not giving any input to the init_random_seed() function in line 13 in RandomNumbers.f90. This will still
    generate a seed but it's dynamically generated based on the system's clock which makes it not perfectly random but random enough for our case.

    e) Line 19 in RandomNumbers.f90 can be changed to PRINT*, random_uniform(x,y) to call this function for a specific range of values.



2.2 Bug Hunting Exercise

    Adding PRINT statements to the code shows that the matrix elements are not correctly scanned for calculation in the loops with the index variables. In the example for the calculation of the ne-to-sw diagonals, the elements are in the order 2, 0, 7; 3, 4, 8; 1, 5, 0, where the elements separated by commas are multiplied. But instead of the 0 you would expect to multiply a 9 as the last digit. This can be achieved by changing the "ind" variable as noted in the program. For a correct calculation of the determinant, the second "ind" variable has to be changed as well, because there is also an error here, according to which the matrix elements are not passed through completely.
