%
%   odeEngland - numerical solution of initial value problem of 
%   Ordinary Differential Equations
%
%   [Y,H]=odeEngland('F', T0, T, X0, H, Hmin, Hmax, eps, P)
%   
%   Returns numerical soluton Y=X(t) of X'=F(t,X) system of ODE for a 
%   given initial values X=X(T0). The solution is calculated using 
%   Runge-Kutta process modification developed by R. England in
%   [R. England. "Error Estimates for Runge-Kutta Type Solutions to 
%   Systems of Ordinary Differential Equations" 
%   // Research and Development Department, Pressed Steel Fisher Ltd., 
%   Cowley, Oxford, UK. October 1968].
%
%   Input arguments:
%
%   'F'  - m-function returning F(t,X)
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