function y=solved(t,x)
    y = zeros(4,1);
    y(4) = exp(-0.1*t);
    t1 = exp(-t);
    y(3) = -9.1/0.9*t1 + 10/0.9*y(4);
    t2 = exp(-1.e3*t);
    a = -91/(0.9*999);
    b = 91/(0.9*999.9);
    c = 1 - a - b;
    y(2) = c*t2+a*t1+b*y(4);
    t3 = exp(-1.e4*t);
    dd = c / 90;
    ff = (100*a+91/0.9)/9999;
    gg = (100*b-100/0.9+1)/9999.9;
    qq =  1 - dd - ff - gg;
    y(1) = qq*t3 + dd*t2 + ff*t1 + gg*y(4);
    