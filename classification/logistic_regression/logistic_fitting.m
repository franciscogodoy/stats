% Fit a logistic model 
% Logistic model predicts output y from input vector x using this rule: 
%       p(y) = g(theta'*x) 
%            = g(w'*x)
%            = g(w0 + w1*x1 + w2*x2 * ... + wp*xp)
%       where g(.) is the sigmoid function. g(z) = 1/(1+exp(-z))
% 
% The probability that output y=1 is computed first from a linear function of x, 
%   followed by a monotone nonlinear (logistic) function (aka link function) 
%   which makes the output be bounded between 0 and 1.  
%
%   The derivative g'(z) = g(z)(1-g(z))
% 
% fitting using gradient ascent, 

function theta = logistic_fitting(maxiter, trains, targets) 
    p = size(trains,2);     % p number of predictor variables         
    n = size(targets,1);    % n number of training records       
    alpha = 0.1;           % learning rate = 0.1
    
    % initial weights
    theta = (rand(p,1) - 0.5)*2*0.1; 
    error  =  exp(20);
    old_err1 = error + 1;
    old_err2 = error + 2;            
   
    for iter = 1:maxiter                 
        delta = zeros(p,1);        
        for j = 1:p                   
           for i = 1:n
                x = trains(i,:)';
                y = targets(i,:);     
                h = logistic(theta' * x);
                delta(j) = delta(j) + (y - h) * x(j);
           end
           theta(j) = theta(j) + alpha * delta(j);                                  
        end         
        
        %compute the error 
        y_hat = logistic(trains * theta);
        old_err2 = old_err1;
        old_err1 = error;  
        error = norm(targets-y_hat);
        if  (error > old_err1 - 1e-16) && (old_err1 > old_err2 - 1e-16)
            break;
        end                                          
    end  
end

function y = logistic(x) 
 y = 1./(1+exp(-x));
end