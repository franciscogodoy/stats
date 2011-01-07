% classify salmon fish using logistics regression 
% use glmfit function in statistics toolbox to fit 
% the logistic model. 
% use holdout procedure to estimate the error rate

function confmat = classify_salmon()
    mydata = load('salmon.data');
    targets = mydata(:,1) -1;
    trains = mydata(:,2:4);
    n = size(trains,1);        
    y = zeros(n,1);    
    for i = 1:n
            X = trains;
            x = [1, X(i,:)];
            
            Y = targets;
            
            X(i,:) = [];
            Y(i,:) = [];            
            
            theta = glmfit(X, [Y ones(99,1)], 'binomial', 'link', 'logit');            
            y(i) = double(x*theta > 0);
    end
    confmat = crosstab(targets, y);
    fprintf('confusion matrix \n');    
    disp(confmat);
    holdout_error =  1 - trace(confmat)/sum(sum(confmat));   
    fprintf('The classification error is %5.2f pecent\n', holdout_error*100);    
end
