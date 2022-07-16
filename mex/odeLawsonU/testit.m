Y=zeros(4,1);
YL=zeros(4,1);
YO=zeros(4,1);
YP=zeros(4,1);
N=2000;
T0 = 0;
T = 20;
x=[10 11 111 111]';
Ts = T0 : (T - T0)/N : T;
H = 0.01;
Hmin = 1.e-8;
Hmax = 0.1;
eps = 1.e-6;
P = 10;

    B = [ -100     0      0       0
          -100    -2      0       0
          -100  9998  -9990     -10
          -100  9988     20  -10010];

xt = x;
YL(:,1) = xt;
tStart=tic;
for i = 1:N
    [xt,H] = odeLawsonU(B, 'fu', Ts(i), Ts(i+1), xt, H, Hmin, Hmax, eps, P);
    YL(:,i+1) = xt;
end
tElapsed=toc(tStart);
fprintf('odeLawsonU\t%0.5f sec\n', tElapsed)


figure;
hold off;
plot (Ts, YL(1,:),'k', Ts, YL(2,:),'r', Ts, YL(3,:),'g', Ts, YL(4,:),'b');
xlabel('\it t');
ylabel('\it x');
legend('\it x_1', '\it x_2', '\it x_3', '\it x_4');
title('odeLawsonU solution');
hold on;


xt = x;
YO(:,1) = xt;
options = odeset('AbsTol', 1.e-10, 'RelTol', 1.e-8, 'InitialStep', H, 'MaxStep', Hmax, 'Jacobian', @rightJ);
tStart=tic;
for i = 1:N
    [To,Yo] = ode23s(@right,[Ts(i), Ts(i+1)],xt',options);
    [m,n] = size(Yo);
    xt = Yo(m,:)';
    YO(:,i+1) = xt;
end
tElapsed=toc(tStart);
fprintf('ode23s\t%0.5f sec\n', tElapsed)


figure;
hold off;
plot (Ts, YO(1,:),'k', Ts, YO(2,:),'r', Ts, YO(3,:),'g', Ts, YO(4,:),'b');
xlabel('\it t');
ylabel('\it x');
legend('\it x_1', '\it x_2', '\it x_3', '\it x_4');
title('od23s solution');
hold on;


xt = x;
YP(:,1) = xt;
for i = 1:N
    xt = solved(Ts(i+1),xt);
    YP(:,i+1) = xt;
end

figure;
hold off;
plot (Ts, YP(1,:),'k', Ts, YP(2,:),'r', Ts, YP(3,:),'g', Ts, YP(4,:),'b');
xlabel('\it t');
ylabel('\it x');
legend('\it x_1', '\it x_2', '\it x_3', '\it x_4');
title('precise solution');
hold on;


figure;
hold off;
plot (Ts, YL(1,:) - YP(1,:),'k', Ts, YL(2,:) - YP(2,:),'r', Ts, YL(3,:) - YP(3,:),'g', Ts, YL(4,:) - YP(4,:),'b');
xlabel('\it t');
ylabel('\it tol');
legend('\it x_1', '\it x_2', '\it x_3', '\it x_4');
title('odeLawsonU residuals');
hold on;

figure;
hold off;
plot (Ts, YO(1,:) - YP(1,:),'k', Ts, YO(2,:) - YP(2,:),'r', Ts, YO(3,:) - YP(3,:),'g', Ts, YO(4,:) - YP(4,:),'b');
xlabel('\it t');
ylabel('\it tol');
legend('\it x_1', '\it x_2', '\it x_3', '\it x_4');
title('ode23s residuals');
hold on;



% xt = x;
% Y(:,1) = xt;
% tStart=tic;
% for i = 1:N
%     [To,Yo] = ode23tb(@right,[Ts(i), Ts(i+1)],xt',options);
%     [m,n] = size(Yo);
%     xt = Yo(m,:)';
%     Y(:,i+1) = xt;
% end
% tElapsed=toc(tStart);
% fprintf('ode23tb\t%0.5f sec\n', tElapsed)
% 
% xt = x;
% Y(:,1) = xt;
% tStart=tic;
% for i = 1:N
%     [To,Yo] = ode23t(@right,[Ts(i), Ts(i+1)],xt',options);
%     [m,n] = size(Yo);
%     xt = Yo(m,:)';
%     Y(:,i+1) = xt;
% end
% tElapsed=toc(tStart);
% fprintf('ode23t\t%0.5f sec\n', tElapsed)
% 
% xt = x;
% Y(:,1) = xt;
% tStart=tic;
% for i = 1:N
%     [To,Yo] = ode15s(@right,[Ts(i), Ts(i+1)],xt',options);
%     [m,n] = size(Yo);
%     xt = Yo(m,:)';
%     Y(:,i+1) = xt;
% end
% tElapsed=toc(tStart);
% fprintf('ode15s\t%0.5f sec\n', tElapsed)
% 
% xt = x;
% Y(:,1) = xt;
% tStart=tic;
% for i = 1:N
%     [To,Yo] = ode23(@right,[Ts(i), Ts(i+1)],xt',options);
%     [m,n] = size(Yo);
%     xt = Yo(m,:)';
%     Y(:,i+1) = xt;
% end
% tElapsed=toc(tStart);
% fprintf('ode23\t%0.5f sec\n', tElapsed)
% 
% xt = x;
% Y(:,1) = xt;
% tStart=tic;
% for i = 1:N
%     [To,Yo] = ode45(@right,[Ts(i), Ts(i+1)],xt',options);
%     [m,n] = size(Yo);
%     xt = Yo(m,:)';
%     Y(:,i+1) = xt;
% end
% tElapsed=toc(tStart);
% fprintf('ode45\t%0.5f sec\n', tElapsed)

% xt = x;
% Y(:,1) = xt;
% tStart=tic;
% for i = 1:N
%     [To,Yo] = ode113(@right,[Ts(i), Ts(i+1)],xt',options);
%     [m,n] = size(Yo);
%     xt = Yo(m,:)';
%     Y(:,i+1) = xt;
% end
% tElapsed=toc(tStart);
% fprintf('ode113\t%0.5f sec\n', tElapsed)

