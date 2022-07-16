%
%   odeLawsonU - numerical solution of initial value problem of 
%   Ordinary Differential Equations
%
%   [Y,H]=odeLawsonU(B, 'FU', T0, T, X0, H, Hmin, Hmax, eps, P)
%   
%   Returns numerical soluton Y=X(t) of X'=BX+u(t,X) system of ODE for a 
%   given initial values X=X(T0). The solution is calculated using 
%   exponential method modification described at [Lawson J.D. Generalized 
%   Runge-Kutta processes for stable systems with large Lipschitz constants 
%   // SIAM Journal on Numerical Analisys, 1967, V. 4, No 3]. It's 
%   recommended for linear and quasi-linear systems including stiff ones. 
%   The method is A-stable for linear systems, i.e. residuals do not depend 
%   on time step length.
%
%   Input arguments:
%
%   B    - square matrix B
%   'FU' - m-function returning u(t,X)
%   T0   - beginning time value T0
%   T    - ending time value
%   X    - beginning solution value X0=X(T0) 
%   H    - beginning time step (might be changed internally to satisfy 
%          given precision)
%   Hmin - minimum time step allowed
%   Hmax - maximum time step allowed
%   eps  - relative precision per time step
%   P    - precision measure (if some solution goes beyound this value, 
%          it gets divided by P before comparing with eps)
%
%   Output arguments:
%
%   Y    - solution vector
%   H    - (optional argument) step recommended for continuation
%
%
%   See also ode23.
%
%   Copyright Sergei Nikolaev 1990-2010
%
%   Distributed under the Boost Software License, Version 1.0.
%   (See accompanying file LICENSE_1_0.txt or copy at
%   http://www.boost.org/LICENSE_1_0.txt)
%