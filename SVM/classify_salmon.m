% classify salmon using a L1-norm soft margin SVM
% use holdout (leave-one-out) procedure to estimate the error rate

function confmat = classify_salmon()
    salmon = load('salmon.data');
    targets = salmon(:,1) -1;
    targets = 2*(targets-0.5);    
    trains = salmon(:,2:4);
    
    n = size(trains,1);            
    y = zeros(n,1);  
    C = 1;
    for i = 1:n
            X = trains;
            x = X(i,:)';
            Y = targets;
            
            X(i,:) = [];
            Y(i,:) = [];            
            
            [w, b] = l1normSVM(C, X, Y);
            y(i) = w'*x + b > 1;
            y(i) = 2*(y(i)-0.5);
    end
    confmat = crosstab(targets, y);
    fprintf('confusion matrix \n');    
    disp(confmat);
    holdout_error =  1 - trace(confmat)/sum(sum(confmat));   
    fprintf('The classification error is %5.2f pecent\n', holdout_error*100);    
end
