%   classification based on estimated minimum ECM rule for two gaussian
%   populations. pp. 586, Johnson Wichern, 6th edition 
%
%   Classification rule 11-18:
%   --classify x0 to population 1 if
%        (x1.bar - x2.bar)' * inv(S.pooled)*x0 -0.5(x1.bar - x2.bar)' *
%        inv(S.pooled) * (x1.bar + x2.bar) > log{ [c(1|2)/c(1|2)]/[p2/p1]}
%   --classify x0 to population 2 otherwise
%
% example: 11.7 estimating the error rate using the holdout procedure 

% compute the training error (aka APER)
x1 = [2,12; 4,10; 3,8];
x2 = [5,7;  3,9;  4,5];

x = [x1;x2];
y = [1; 1; 1; 2;2;2];

x1_bar = mean(x1)';
x2_bar = mean(x2)';
y_hat = [];
n1 = size(x1,1);
n2 = size(x2,1);
S1 = cov(x1);
S2 = cov(x2);
S_pooled = ((n1-1)/(n1+n2-2))*S1 + ((n2-1)/(n1+n2-2))*S2;

%classify using rule 11-18, pp. 586
% See if this l
for i= 1:size(x,1) 
    x0 = x(i,:)';
    S_pooled_inv = inv(S_pooled);
    y0 = (x1_bar-x2_bar)' * S_pooled_inv * x0 - 0.5 * (x1_bar-x2_bar)' * S_pooled_inv * (x1_bar+x2_bar);
    if y0 >= log(1) 
        y_hat(i) = 1;
    else 
        y_hat(i) = 2;        
    end    
end

confmat = crosstab(y, y_hat);
disp(confmat);
training_error =  1 - trace(confmat)/sum(sum(confmat));   
disp(training_error);

%holdout procedure 
x2_bar = mean(x2)';
S2 = cov(x2);
for i= 1:size(x1,1) 
    x1h = x1;
    x0 = x1h(i,:)';
    x1h(i,:) = [];
    x1h_bar = mean(x1h)';
    s1h = cov(x1h);
    n1h = n1 -1;
    Sh_pooled = ((n1h-1)/(n1h+n2-2))*s1h + ((n2-1)/(n1h+n2-2))*S2;
    Sh_pooled_inv = inv(Sh_pooled);     
    y0 = (x1h_bar-x2_bar)' * Sh_pooled_inv * x0 - 0.5 * (x1h_bar-x2_bar)' * Sh_pooled_inv * (x1h_bar+x2_bar);
    if y0 >= log(1) 
        y_hat(i) = 1;
    else 
        y_hat(i) = 2;
    end    
end

x1_bar = mean(x1)';
S1 = cov(x1);
for j= 1:size(x2,1) 
    x2h = x2;
    x0 = x2h(j,:)';
    x2h(j,:) = [];
    x2h_bar = mean(x2h)';
    s2h = cov(x2h);
    n2h = n2 -1;
    Sh_pooled = ((n1-1)/(n1+n2h-2))*S1 + ((n2h-1)/(n1+n2h-2))*s2h;
    Sh_pooled_inv = inv(Sh_pooled);
    y0 = (x1_bar-x2h_bar)' * Sh_pooled_inv * x0 - 0.5 * (x1_bar-x2h_bar)' * Sh_pooled_inv * (x1_bar+x2h_bar);
    if y0 >= log(1) 
        y_hat(3+j) = 1;
    else 
        y_hat(3+j) = 2;
    end    
end

confmat = crosstab(y, y_hat);
disp(confmat);
holdout_error =  1 - trace(confmat)/sum(sum(confmat));   
disp(holdout_error);









