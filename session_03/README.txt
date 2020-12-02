3.1 Assignements

3.1.1 Random Number Generator
    a) Both functions are implemented in RandomNumbers_mod.f90.
    
    b) The variables are declared as private in the upper part of RandomNumbers_mod.f90. For funcionality an additional subroutine was added to change the
    parameters of the LCG algorithm in RandomNumbers_mod.f90.
    
    c) The period of the LCG with the parameters a = 7, c = 4 and m = 15 with seed = 4 is 12 numbers, the period of the LCG with the same parameters but seed = 11
    is only 3 numbers. --> FindPeriod.f90
    
    d) This parameters were added as the default for the in b) implemented subroutine.
    
    e) This is done in RandomNumbers.f90. The output can be found in RandomNumberGenerator/data.
    
    f) Plot with plotDist.sh
    If you look at the vectors generated from the random numbers with the LCG algorithm, you might think at first sight that they are well distributed random
    numbers. But if you rotate the plot with the mouse, you will notice that the random numbers are collected in layers in the 3D view. So the LCG algorithm does
    not provide good pseudo-random numbers. In contrast, the method integrated in Fortran90 provides a uniformly filled cube.
    
    g) The function was added in RandomNumbers_mod.f90. The main program is located in RandomNormal.f90. The output can be found in RandomNumberGenerator/data
    
    h) The histogram can be plotted with the histogram.plt file.
 ---------------------------------------------------------------------------------

3.1.3 Root finding program --> CalcRoots.f90
    1) Function: Both algorithms return 0.0707107 as a root value. The bisection algorithm took 35 iterations while the Newton-Raphson algorithm took 6
    iterations. Second root at -0.07 can't be found with those initial values.

    2) Function: Bisection return -1.769292 after 38 iterations
    Newton-Raphson with 
    x_zero = 0 --> Loop breaks after 500000 iterations
    x_zero = 5 --> -1.769292 (19 Iterations)
    x_zero = -5 --> -1.769292 (8 Iterations)
    x_zero = -0.5 --> Loop breaks after 500000 iterations

    3) Function: Bisection Loop breaks after 500000 iterations
    Newton-Raphson with
    x_zero = 0.1 --> 2.5 (6 Iterations)
    x_zero = 1.1 --> 3.5 (6 Iterations)
    x_zero = 2 --> Loop breaks after 500000 iterations (2.1 would find 4.5 in 6 iterations)
    
    The Bisection algorithm heavily depends on the initial interval. The algorithm
    is simply not able to find a root that is not contained inside the bracket. In cases where
    multiple roots are available the algorithm only converges into one of those. The Newton-Rhapson
    method will converge much faster which is shown in the number of iterations needed but can be unstable
    at times. A clear example would be the calculation for Function 3 at x_zero = 2.
    Here the method runs in a endless loop while doing a slight change in the starting condition
    could make it converge in 6 iterations.

    c)
    Run newton(x=2) and bisection([0,2) for function1 and plot_convergence.plt afterwards.
    The resulting picture clearly shows the linear behaviour of the
    the Bisection algorithm while the Newton-Rhapson method converges quadratically.
----------------------------------------------------------------------------------
3.1.4

    c) Program can be found in CalcRoots_Complex.f90
    Plot with plot_complex.sh
    The real_root.png picture shows a rather complex axis symmetric structure with rings forming at the right edge near the real root z=1.
    We expect to see a lot more possible initial conditions near the root since the Newton-Rhapson method is used which really shines when the
    initial condition is close to the root. The convergence plot shows the convergence as a function of the x and y positions aka. the initial conditions.
    Positions closer to the root at the right edge converge faster than positions far away. This is shown as a colour gradient in the plot.


3.2 Bug Huntig Exercise
    For large values of i (i>12) it can be seen that the mean value becomes negative. This indicates an integer overflow, because REAL(n) is always positive. So 
    in line 26 of the code shown in the equation mean = SUM(values)/REAL(n), at least partially negative values for "values" must have been summed. If one looks
    closely at the output, it is noticeable that the mean value does not increase as expected when i = 9. This means that the effect of the integer overflow can
    already be felt with a ten-digit number. In the second case, this leads to errors in the displayed calculation of the standard deviation, since the "values"
    values are squared. This explains the observed deviation at i=4, since such a 5-digit number squared already represents a 10-digit number.
    
    The first formula for the standard deviation is the better formula, that is the formula for "stddev". It is to be preferred, since this formula constantly
    delivers the same standard deviation despite different offsets. Even with very large offsets, the formula does not fail.
