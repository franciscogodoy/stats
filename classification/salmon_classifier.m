%   classify salmon fishes 
%       group 1 - Alaskan-born salmon
%       group 2 - Canadian-born salmon
%
%   example 11.8, pp. 602, Johnson-Wichern, 6th edition 
%
%   Classification rule 11-19:
%   allocate x to population-1 if:
%     y.hat = a'*x >= m.hat   
%   allocate x to population-2, otherwise

mydata = load('salmon.data');
x1_idx = find(mydata(:,1)==1);
x1 = mydata(x1_idx, 3:4);

x2_idx = find(mydata(:,1)==2);
x2 = mydata(x2_idx, 3:4);

x = mydata(:, 3:4);
y = mydata(:,1);
y_hat = [];

n1 = size(x1,1);
n2 = size(x2,1);
S1 = cov(x1);
S2 = cov(x2);
S_pooled = ((n1-1)/(n1+n2-2))*S1 + ((n2-1)/(n1+n2-2))*S2;

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
    
    a_hat = (x1h_bar-x2_bar)' * Sh_pooled_inv;    
    a_hat = a_hat';
    y0_hat = a_hat' * x0;
    m_hat = 0.5 * (a_hat' * x1h_bar + a_hat' * x2_bar);
    
    if y0_hat >= m_hat
        % classify x0 as from population 1
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
    
    a_hat = (x1_bar-x2h_bar)' * Sh_pooled_inv;   
    a_hat = a_hat';
    
    y0_hat = a_hat' * x0;
    m_hat = 0.5 * (a_hat' * x1_bar + a_hat' * x2h_bar);
    
    if y0_hat >= m_hat
        % classify x0 as from population 1
        y_hat(50+j) = 1;
    else 
        y_hat(50+j) = 2;
    end  
end

confmat = crosstab(y, y_hat);
disp(confmat);
holdout_error =  1 - trace(confmat)/sum(sum(confmat));   
fprintf('The classification error is %5.2f pecent\n', holdout_error*100);





