function r=solved(t,x)
    r = zeros(4,1);
    r(1) = 10*exp(-100*t);
    r(2) = r(1) + exp(-t)*(cos(t)+sin(t));
    r(3) = r(2) + 100*exp(-10000*t)*cos(10*t);
    r(4) = r(3) + 10*exp(-10000*t)*sin(10*t);
