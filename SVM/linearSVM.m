% data from example 5.5, pp. 264, tan, stainbach, kumar book
load linsep.mat;
n = size(x,1);
p = size(x,2);

cvx_begin
    variables w(p) b 
    minimize 1/2*sum(w.*w)
    y.*(x*w + b) >= 1;
cvx_end

min_x1 = min(x(:,1));
max_x1 = max(x(:,1));

% coordinate points (x1,x2) of the boundary hyper plane (line)
b_x1 = linspace(min_x1, max_x1, 200);
b_x2 = (-b - w(1)*b_x1)/w(2);

% hyperplane for y=1 support vectors
margin1_x2 = (-b - w(1)*b_x1 +1)/w(2);

% margin for y=-1 support vectors
margin2_x2 = (-b - w(1)*b_x1 -1)/w(2);

idx1 = find(y==1);
idx2 = find(y==-1);

% gscatter(x(:,1),x(:,2),y);
plot(x(idx1,1), x(idx1,2), 'g+'); 
hold on
plot(x(idx2,1), x(idx2,2), 'r*'); 
plot(b_x1, b_x2, '-b', b_x1, margin1_x2, '--g',  b_x1, margin2_x2, '--r');
hold off;
