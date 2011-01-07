% faster than logistic_fitting function
function theta = logistic_fit2(maxiter, X, Y)
    p = size(X,2);     % p number of predictor variables
    n = size(Y,1);    % n number of training records       
    alpha = 0.01;           % learning rate = 0.1
    lambda = 1e-4;
    % initial weights
    theta = zeros(p,1); 
    grad = ones(p,1);        
    for iter = 1:maxiter                 
        y_hat = logistic(X * theta);
        grad = X' * (Y - y_hat) - lambda*theta;
        theta = theta + alpha .* grad;
        if  (norm(grad) < 1e-6)
            % converged
            break;
        end                                          
    end  
end

function y = logistic(x) 
 y = 1./(1+exp(-x));
end
