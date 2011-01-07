% classify salmon fish using logistics regression 
% use newton method with regularization 

function confmat = classify_salmon3()
    mydata = load('salmon.data');
    targets = mydata(:,1) -1;
    trains = mydata(:,2:4);
    n = size(targets,1);
    y = zeros(n,1);
    for i = 1:n
        x = trains(i,:);  
        X = trains;
        X(i,:) = [];
        Y = targets;
        Y(i,:) = [];
        theta = newton_fit(X, Y);
        
        y(i) = double(x*theta > 0);
    end
    
    confmat = crosstab(targets, y);
    fprintf('confusion matrix \n');
    disp(confmat);
    holdout_error =  1 - trace(confmat)/sum(sum(confmat));   
    fprintf('The classification error is %5.2f pecent\n', holdout_error*100);        
end
