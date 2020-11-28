

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
