4.1.2   Calculating the integrals using the test_integration.f90 program will result in the following estimates:
        Function 1: 6.000000447035 (Trapez)
                    6.000000298023 (Simpson)
        Accuracy was set to 1e-7 which we can see from the results.
        Groundtruth:6.0 (Wolfram Alpha)

        Function 2: 8.153364636096 (Trapez)
                    8.153364464001 (Simpson)
        Accuracy was set to 1e-7 which we can see from the results.
        Groundtruth:8.1534 (Wolfram Alpha)

        For Function 3 we went for a naive approach and iteratively increased the upper bound until the changes to the area
        are smaller than a given accuracy level. 
        Function 3: 0.499999955293 (Trapez)
                    0.499999985091 (Simpson)
        Accuracy was set to 1e-7 which we can see from the result.
        Groundtruth:0.5 (Wolfram Alpha)

       The plot can be generated using the accuracy.plt file. Here we use the number of evaluations testfunction done by the trapez_next function and
       plot it against the relative error calculated at every iteration(recursion level). The error shows a strong exponential decay in the log-log-plot
       for both algorithms. This shows how fast they can approach a relatively decent accuracy while still taking a lot of time to get to higher accuracy level.
       Both algorithm seem to perfom equally good which might be because of the way the Simpson rule is implemented. We are bound to the accuracy and performance
       of the trapezoidal rule since we calculate it through it.