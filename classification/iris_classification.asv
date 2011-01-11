%   Example 11.12, pp. 619, Johnson-Wichern, 6th edition 
%   Use holdout procedure to classify iris data and compute the validation
%   error rate

mydata = load('iris.data');
x1_idx = find(mydata(:,5)==1);
x1 = mydata(x1_idx, 1:4);

x2_idx = find(mydata(:,5)==2);
x2 = mydata(x2_idx, 1:4);

x3_idx = find(mydata(:,5)==3);
x3 = mydata(x3_idx, 1:4);

y = mydata(:,5);
y_hat = zeros(size(y));

for i= 1:size(x1,1) 
    x1h = x1;
    x0 = x1h(i,:)';
    x1h(i,:) = [];
    
    % x is a cell array 
    x = {x1h x2 x3};
    y_hat(i) = discriminate(x0, x,y);    
    
end

for j= 1:size(x2,1) 
    x2h = x2;
    x0 = x2h(j,:)';
    x2h(j,:) = [];
    
    % x is a cell array 
    x = {x1 x2h x3};
    y_hat(50+j) = discriminate(x0, x,y);
end

for k= 1:size(x3,1) 
    x3h = x3;
    x0 = x3h(k,:)';
    x3h(k,:) = [];
    
    % x is a cell array 
    x = {x1 x2 x3h};
    y_hat(100+k) = discriminate(x0, x,y);
end

confmat = crosstab(y, y_hat);
disp(confmat);
holdout_error =  1 - trace(confmat)/sum(sum(confmat));   
fprintf('The classification error is %5.2f pecent\n', holdout_error*100);
