clc; clear; close all;

% read data and pre-process
gpsdata = importdata("P780.tenv3");
t = gpsdata.data(:,1);
east = gpsdata.data(:,7)*1000;
north = gpsdata.data(:,9)*1000;
up = gpsdata.data(:,11)*1000;
eaststd = gpsdata.data(:,13)*1000;
northstd = gpsdata.data(:,14)*1000;
upstd = gpsdata.data(:,15)*1000;
eqtime = [2019.7290, 2020.0137, 2020.0164, 2020.0274]; % earthquakes

n = length(t);

t = t(t>2017.9); % change time length
east = east(n-length(t)+1:end);
north = north(n-length(t)+1:end);
up = up(n-length(t)+1:end);
eaststd = eaststd(n-length(t)+1:end);
northstd = northstd(n-length(t)+1:end);
upstd = upstd(n-length(t)+1:end);

east = east - median(east);
north = north - median(north);
up = up - median(up);
qeast = diag(eaststd.^2);   % covariance matrix
qnorth = diag(northstd.^2); % covariance matrix
qup = diag(upstd.^2);       % covariance matrix

%% regression
% process
[eEst, eResidual, epar, eStdEstPar] = lsquare(east, qeast, t, eqtime);
[nEst, nResidual, npar, nStdEstPar] = lsquare(north, qnorth, t, eqtime);
[uEst, uResidual, upar, uStdEstPar] = lsquare(up, qup, t, eqtime);

for i = 1:length(epar)
  fprintf("%15.5f\t%15.5f\n", epar(i), eStdEstPar(i));
end
fprintf("parameters, rms of pars for North Displacement\n")
for i = 1:length(npar)
  fprintf("%15.5f\t%15.5f\n", npar(i), nStdEstPar(i));
end
fprintf("parameters, rms of pars for Up Displacement\n")
for i = 1:length(upar)
  fprintf("%15.5f\t%15.5f\n", upar(i), uStdEstPar(i));
end

% plot regression
figure("Name","Regression");
ax(3) = 0;
for i=1:3
  ax(i) = subplot(3,1,i);
  xline(ax(i),eqtime,"k--",LineWidth=1)
  hold(ax(i), "on");
  pbaspect(ax(i),[10, 4, 1])
  box(ax(i),"on")
  xlim(ax(i), [min(t), max(t)])
end
ylabel(ax(1),"North (mm)")
ylabel(ax(2),"East (mm)")
ylabel(ax(3),"Up (mm)")
xlabel(ax(3),"Time (year)")
plot(ax(1), t, east, "bo","MarkerSize",2,"MarkerFaceColor","b")
plot(ax(2), t, north, "bo","MarkerSize",2,"MarkerFaceColor","b")
plot(ax(3), t, up, "bo","MarkerSize",2,"MarkerFaceColor","b")
plot(ax(1), t, eEst,LineWidth=2,Color="r");
plot(ax(2), t, nEst,LineWidth=2,Color="r");
plot(ax(3), t, uEst,LineWidth=2,Color="r");

% plot residual
figure("Name","Residual");
ax1(3) = 0;
for i=1:3
  ax1(i) = subplot(3,1,i);
  hold(ax1(i), "on");
  pbaspect(ax1(i),[10, 4, 1])
  box(ax1(i),"on")
  xlim(ax1(i), [min(t), max(t)])
end
ylabel(ax1(1),"North (mm)")
ylabel(ax1(2),"East (mm)")
ylabel(ax1(3),"Up (mm)")
xlabel(ax1(3),"Time (year)")
plot(ax1(1), t, eResidual, "bo","MarkerSize",2,"MarkerFaceColor","b")
plot(ax1(2), t, nResidual, "bo","MarkerSize",2,"MarkerFaceColor","b")
plot(ax1(3), t, uResidual, "bo","MarkerSize",2,"MarkerFaceColor","b")

%% maps
figure("Name","Maps");
data = importdata("us70006vll_24hrRapid_14-Jan-2020.txt");
p780 = data.data(6,:);
coef = 45;

% Line
a(1) = p780(2);
b(1) = p780(1);
a(2) = p780(2)+p780(4)*coef;
b(2) = p780(1)+p780(3)*coef;
geoplot(a,b,'-r.')
hold

% Ellipse 
C = [a(2), b(2)];  % center 
a = p780(7)*coef*2.5;
b = p780(6)*coef*2.5;
th = linspace(0,2*pi) ; 
xe = C(1)+a*cos(th) ; 
ye = C(2)+b*sin(th) ;
geoplot(xe,ye,'r')

% Map
text(p780(2), p780(1), 'P780')
geolimits([17 19],[-68 -64])
geobasemap streets

%%
% fitting function
function res = fitfunc(t, eqtime)
  n = length(t);
  o = 2*pi; % omega
  res = [ones(n,1), t, sin(o*t), cos(o*t), sin(2*o*t), cos(2*o*t)];
  for i = 1:length(eqtime)
    res = [res, stepFunc(t, eqtime(i))]; %#ok
  end
end

% least square scheme
function [res, residual, coef, stdEstPar] = lsquare(motion, Q, t, eqtime)
  A = fitfunc(t, eqtime);
  N = A'*Q^-1*A;
  coef = N^-1*A'*Q^-1*motion;
  A = fitfunc(t, eqtime); % earthquake
  res = A*coef;
  residual = motion - res;
  posteriorvar = (residual'*Q^-1*residual)./(length(t)-length(coef));
  corvar = posteriorvar*N^-1;
  stdEstPar = diag(corvar);
  stdEstPar = sqrt(stdEstPar);
end

% heaviside function
function res = stepFunc(t, tpoint)
  res = zeros(length(t), 1.);
  res = res(t<tpoint);
  res = [res; 0.5];
  n = length(res);
  res(n+1: length(t)) = 1.;
end