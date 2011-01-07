% classify salmon fish using locally-weighted version 
% of logistic regression
% using holdout prececure to estimate the error rate

function confmat = lwlr_classify_salmon()
    mydata = load('salmon.data');
    targets = mydata(:,1) -1;
    trains = mydata(:,2:4);
    n = size(targets,1);
    y = zeros(n,1);
    for i = 1:n
        x = trains(i,:)';  
        X = trains;
        X(i,:) = [];
        Y = targets;
        Y(i,:) = [];
        
        y(i) = locally_weighted_reg(X,Y, x, 0.9);
    end
    
    confmat = crosstab(targets, y);
    fprintf('confusion matrix \n');
    disp(confmat);
    holdout_error =  1 - trace(confmat)/sum(sum(confmat));   
    fprintf('The classification error is %5.2f pecent\n', holdout_error*100);    
end