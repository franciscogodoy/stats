% classify salmon fish using logistics regression.  
% here paramaters fitting is done with gradient descent algorithm 

function confmat = classify_salmon2()
    mydata = load('salmon.data');
    targets = mydata(:,1) -1;
    trains = mydata(:,2:4);
    n = size(trains,1);        
    y = zeros(n,1);    
    for i = 1:n
            X = [ones(n,1) trains];
            x = X(i,:);            
            Y = targets;            
            X(i,:) = [];
            Y(i,:) = [];            
            % old slow
            %theta = logistic_fitting(200, X, Y);
            
            % new impl, faster
            theta = logistic_fit2(200, X, Y);
            y(i) = double(x*theta > 0);
    end
    confmat = crosstab(targets, y);
    fprintf('confusion matrix \n');    
    disp(confmat);    
    
    holdout_error =  1 - trace(confmat)/sum(sum(confmat));   
    fprintf('The classification error is %5.2f pecent\n', holdout_error*100);            
end