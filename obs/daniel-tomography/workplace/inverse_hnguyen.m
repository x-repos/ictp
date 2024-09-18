clear;clc;
% Station location
x = [35.0;-44.0; -11.0; 23.0; 42.0; -12.0; -45.0; 5.0; -1.0; 20.0];
y = [9.0; 10.0; -25.0; -39.0; -27.0; 50.0; 16.0; -19.0; -11.0; 11.0];
z = zeros(10,1);

x0 = 0; y0 = 0; z0 = 10; t = 0; v = 5;
d_obs = time(x0, x, y0, y, z0, z, t, v);

% initial model pars
M = [3; 4; 20; 2];
MM = zeros(4,1);
MM(:,1) = M;
for i = 2:3
    x0 = M(1); y0 = M(2); z0 = M(3); t = M(4);
    G1 = dev_x(x0, x, y0, y, z0, z, v);
    G2 = dev_y(x0, x, y0, y, z0, z, v);
    G3 = dev_z(x0, x, y0, y, z0, z, v);
    G4 = ones(10,1);
    G = [G1, G2, G3, G4];
    d_pred = time(x0, x, y0, y, z0, z, t, v);
    delta_d = d_obs - d_pred;
    delta_M = (G'*G)^-1*G'*delta_d;
    M = M + delta_M;
    MM(:,i) = M; 
end
fprintf("Model for interation number:\n\n")
fprintf("       0        1          2")
MM

function res = time(x0, x, y0, y, z0, z, t, v)
res = 1/v*sqrt((x0-x).^2 + (y0 - y).^2 + (z0-z).^2)+t;
end
function res = dev_x(x0, x, y0, y, z0, z, v)
res = (x0 - x)/v.*((x0-x).^2 + (y0 - y).^2 + (z0-z).^2).^(-1/2);
end
function res = dev_y(x0, x, y0, y, z0, z, v)
res = (y0 - y)/v.*((x0-x).^2 + (y0 - y).^2 + (z0-z).^2).^(-1/2);
end
function res = dev_z(x0, x, y0, y, z0, z, v)
res = (z0 - z)/v.*((x0-x).^2 + (y0 - y).^2 + (z0-z).^2).^(-1/2);
end
