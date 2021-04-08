rand('seed',0); 
randn('seed',0);

m = 1000;
n = 3000;
p = 10/n;

x0 = sprandn(n,1,p);
A = randn(m,n);
A = A*spdiags(1./sqrt(sum(A.^2))',0,n,n);
b = A*x0 + sqrt(0.001)*randn(m,1);

tic
cvx_begin
    variable x(n)
    minimize(0.5*sum_square(A*x - b) + norm(x,1))
cvx_end
toc

prox_f = @(v,lambda) (1/(1 + lambda))*(v - b) + b;
prox_g = @(v,lambda) max(0, v - lambda) - max(0, -v - lambda);
x_admm = admm(prox_f, prox_g, A);
