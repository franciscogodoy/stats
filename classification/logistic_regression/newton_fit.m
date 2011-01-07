% Fitting parameters theta by maximizing the log likelihood with added
% regularization parameter.
%
% Logistic model predicts output y from input vector x using this rule: 
%       p(y) = g(theta'*x) 
%       where g(.) is the sigmoid function. g(z) = 1/(1+exp(-z))
% 
% Fitting using newton method with regularization parameter

function theta = newton_fit(trains, targets) 
    lambda = 1e-4;
    p = size(trains,2);     % p number of predictor variables         
    
    % initial weights
    theta = zeros(p,1);    
    grad = ones(p,1);

    while (norm(grad) > 1e-4)   
        y_hat = logistic(trains * theta);
        grad = trains' * (targets - y_hat) - lambda*theta;
        H = -trains' * diag(y_hat.*(1 - y_hat)) * trains - lambda*eye(p);
        theta = theta - H \ grad;
    end
end

function y = logistic(x) 
 y = 1./(1+exp(-x));
end