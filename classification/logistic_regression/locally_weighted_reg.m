% Using Newton method to fitt locally-weighted logistic regression
% parameters theta. Then use dot(theta,x) to predict the value y_hat
% 

function y_hat = locally_weighted_reg(X, T, x, tau) 
    lambda = 1e-4;
    n = size(X,1);
    p = size(X,2);  
    theta = zeros(p,1);    
    
    % compute weights of each training point for the query point
    w = exp(-sum((repmat(x',n,1) -X).^2, 2) /(2*tau));    
    
    grad = ones(p,1);

    while (norm(grad) > 1e-6)   
        Y_h = logistic(X * theta);
        grad = X' * (w.*(T - Y_h)) - lambda*theta;
        H = -X' * diag(w.*Y_h.*(1 - Y_h)) * X - lambda*eye(p);
        theta = theta - H \ grad;
    end    
    y_hat = double(x'*theta > 0);
end

function y = logistic(x) 
 y = 1./(1+exp(-x));
end