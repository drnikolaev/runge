function r=right(t,x)
    r = [
        -10000*x(1) + 100*x(2)  - 10*x(3) +  x(4)
                    - 1000*x(2) + 10*x(3) - 10*x(4)
                                - x(3)    + 10*x(4)    
                                          - 0.1*x(4)
        ];