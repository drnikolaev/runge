function r=rightT(t,x)
    a=-2000*exp(-100*t) - 2*exp(-t)*(cos(t)+sin(t));
    r = [ 0
          a
          a
          a ];
