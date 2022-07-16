Y=zeros(2,1);
N=2000;
T0 = 0;
T = 10;
x=[1 1]';
Ts = T0 : (T - T0)/N : T;
H = 0.01;
Hmin = 1.e-7;
Hmax = 0.1;
eps = 1.e-8;
P = 10;

xt = x;
Y(:,1) = xt;
tStart=tic;
for i = 1:N
    [xt,H] = odeEngland('right', Ts(i), Ts(i+1), xt, H, Hmin, Hmax, eps, P);
    Y(:,i+1) = xt;
end
tElapsed=toc(tStart);
fprintf('odeEngland\t%0.5f sec\n', tElapsed)

figure;
hold off;
plot (Ts, Y(1,:),'b', Ts, Y(2,:),'r');
xlabel('\it t');
ylabel('\it x');
legend('\it x_1', '\it x_2');
title('odeEngland solution');
hold on;


xt = x;
Y(:,1) = xt;
options = odeset('AbsTol',[eps eps], 'InitialStep', H, 'MaxStep', Hmax, 'Jacobian', @rightJ);
tStart=tic;
for i = 1:N
    [To,Yo] = ode23s(@right,[Ts(i), Ts(i+1)],xt',options);
    [m,n] = size(Yo);
    xt = Yo(m,:)';
    Y(:,i+1) = xt;
end
tElapsed=toc(tStart);
fprintf('ode23s\t%0.5f sec\n', tElapsed)

figure;
hold off;
plot (Ts, Y(1,:),'b', Ts, Y(2,:),'r');
xlabel('\it t');
ylabel('\it x');
legend('\it x_1', '\it x_2');
title('od23s solution');
hold on;


xt = x;
Y(:,1) = xt;
for i = 1:N
    xt = solved(Ts(i+1),xt);
    Y(:,i+1) = xt;
end

figure;
hold off;
plot (Ts, Y(1,:),'b', Ts, Y(2,:),'r');
xlabel('\it t');
ylabel('\it x');
legend('\it x_1', '\it x_2');
title('precise solution');
hold on;


xt = x;
Y(:,1) = xt;
tStart=tic;
for i = 1:N
    [To,Yo] = ode23tb(@right,[Ts(i), Ts(i+1)],xt',options);
    [m,n] = size(Yo);
    xt = Yo(m,:)';
    Y(:,i+1) = xt;
end
tElapsed=toc(tStart);
fprintf('ode23tb\t%0.5f sec\n', tElapsed)

xt = x;
Y(:,1) = xt;
tStart=tic;
for i = 1:N
    [To,Yo] = ode23t(@right,[Ts(i), Ts(i+1)],xt',options);
    [m,n] = size(Yo);
    xt = Yo(m,:)';
    Y(:,i+1) = xt;
end
tElapsed=toc(tStart);
fprintf('ode23t\t%0.5f sec\n', tElapsed)

xt = x;
Y(:,1) = xt;
tStart=tic;
for i = 1:N
    [To,Yo] = ode15s(@right,[Ts(i), Ts(i+1)],xt',options);
    [m,n] = size(Yo);
    xt = Yo(m,:)';
    Y(:,i+1) = xt;
end
tElapsed=toc(tStart);
fprintf('ode15s\t%0.5f sec\n', tElapsed)

xt = x;
Y(:,1) = xt;
tStart=tic;
for i = 1:N
    [To,Yo] = ode23(@right,[Ts(i), Ts(i+1)],xt',options);
    [m,n] = size(Yo);
    xt = Yo(m,:)';
    Y(:,i+1) = xt;
end
tElapsed=toc(tStart);
fprintf('ode23\t%0.5f sec\n', tElapsed)

xt = x;
Y(:,1) = xt;
tStart=tic;
for i = 1:N
    [To,Yo] = ode45(@right,[Ts(i), Ts(i+1)],xt',options);
    [m,n] = size(Yo);
    xt = Yo(m,:)';
    Y(:,i+1) = xt;
end
tElapsed=toc(tStart);
fprintf('ode45\t%0.5f sec\n', tElapsed)

xt = x;
Y(:,1) = xt;
tStart=tic;
for i = 1:N
    [To,Yo] = ode113(@right,[Ts(i), Ts(i+1)],xt',options);
    [m,n] = size(Yo);
    xt = Yo(m,:)';
    Y(:,i+1) = xt;
end
tElapsed=toc(tStart);
fprintf('ode113\t%0.5f sec\n', tElapsed)

