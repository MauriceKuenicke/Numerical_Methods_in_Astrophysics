3.1 Assignements

3.1.1 Random Number Generator
    c) The period of the LCG with the parameters a = 7, c = 4 and m = 15 with seed = 4 is 12 numbers, the period of the LCG with the same parameters but seed = 11
    is only 3 numbers.
    
    f)If you look at the vectors generated from the random numbers with the LCG algorithm, you might think at first sight that they are well distributed random
    numbers. But if you rotate the plot with the mouse, you will notice that the random numbers are collected in layers in the 3D view. So the LCG algorithm does
    not provide good pseudo-random numbers. In contrast, the method integrated in Fortran90 provides a uniformly filled cube.

Translated with www.DeepL.com/Translator (free version)

3.1.3 Root finding program
    1) Function: Both algorithms return 0.0707107 as a root value. The bisection algorithm took 35 iterations while the Newton-Raphson algorithm took 6 iterations.
    Second root at -0.07 can't be found with those initial values.

    2) Function: Bisection return -1.769292 after 38 iterations
    Newton-Raphson with 
    x_zero = 0 --> endless loop
    x_zero = 5 --> -1.769292 (19 Iterations)
    x_zero = -5 --> -1.769292 (8 Iterations)
    x_zero = -0.5 --> endless loop

    3) Function: Bisection gets stuck in a endless loop
    Newton-Raphson with
    x_zero = 0.1 --> 2.5 (6 Iterations)
    x_zero = 1.1 --> 3.5 (6 Iterations)
    x_zero = 2 --> endless loop (2.1 would find 4.5 in 6 iterations)
    
----------------------------------------------------------------------------------

3.2 Bug Huntig Exercise
    For large values of i (i>12) it can be seen that the mean value becomes negative. This indicates an integer overflow, because REAL(n) is always positive. So in
    line 26 of the code shown in the equation mean = SUM(values)/REAL(n), at least partially negative values for "values" must have been summed. If one looks
    closely at the output, it is noticeable that the mean value does not increase as expected when i = 9. This means that the effect of the integer overflow can
    already be felt with a ten-digit number. In the second case, this leads to errors in the displayed calculation of the standard deviation, since the "values"
    values are squared. This explains the observed deviation at i=4, since such a 5-digit number squared already represents a 10-digit number.
    
    The first formula for the standard deviation is the better formula, that is the formula for "stddev". It is to be preferred, since this formula constantly
    delivers the same standard deviation despite different offsets. Even with very large offsets, the formula does not fail.
