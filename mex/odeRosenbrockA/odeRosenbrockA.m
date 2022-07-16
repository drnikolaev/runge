%
%   odeRosenbrockA - numerical solution of initial value problem of 
%   Ordinary Differential Equations (autonomous case)
%
%   [Y,H]=odeRosenbrockA('F', 'FJ', T0, T, X0, H, Hmin, Hmax, eps, P)
%   
%   Returns numerical soluton Y=X(t) of autonomous X'=F(X) system of ODE 
%   for a given initial values X=X(T0). The solution is calculated using 
%   implicit processes developed by H.H. Rosenbrock [H.H. Rosenbrock "Some 
%   general implicit processes for the numerical solution of differential 
%   equations"  Comput. J., 5 (1963) pp.329–330]. It's recommended for 
%   non-linear systems including stiff ones. 
%
%   Input arguments:
%
%   'F'  - m-function returning column vector F(X)
%   'FJ' - m-function returning Jacobian matrix of the system F(X)
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