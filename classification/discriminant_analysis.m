%   Example 11.11, pp. 614, Johnson-Wichern, 6th edition 
%   
%   Classifying a potential business school graduate student
%
%   Classification rule: 11-54, pp. 612
% 
%   allocate x to population(i) for which 
%            -0.5 * Di^2(x) + log(pi) is largest
%
%       where: 
%           Di^2(x) = (x-xi.bar)' * S_pooled_inv (x-xi.bar)
%
%   Since the prior probabilities are unknown, we set pi = 1/g for all i. 
%   Thus, new observation is assigned to the closest population. 

mydata = load('addmision.data');
x1_idx = find(mydata(:,3)==1);
x1 = mydata(x1_idx, 1:2);

x2_idx = find(mydata(:,3)==2);
x2 = mydata(x2_idx, 1:2);

x3_idx = find(mydata(:,3)==3);
x3 = mydata(x3_idx, 1:2);

x = mydata(:, 1:2);
y = mydata(:,3);

n1 = size(x1,1);
n2 = size(x2,1);
n3 = size(x3,1);

S1 = cov(x1);
S2 = cov(x2);
S3 = cov(x3);
S_pooled = ((n1-1)/(n1+n2+n3-3))*S1 + ((n2-1)/(n1+n2+n3-3))*S2 + ((n3-1)/(n1+n2+n3-3))*S3;
S_pooled_inv = inv(S_pooled);

x1_bar = mean(x1)';
x2_bar = mean(x2)';
x3_bar = mean(x3)';

% GPA and GMAT score of a potential business school graduate student
x0 = [3.21; 497];

d_square(1)  = (x0-x1_bar)' * S_pooled_inv * (x0-x1_bar);
d_square(2)  = (x0-x2_bar)'  * S_pooled_inv * (x0-x2_bar);
d_square(3)  = (x0-x3_bar)'  * S_pooled_inv * (x0-x3_bar);

[C,I] = min(d_square);
fprintf('the potential student is classified into group %d \n', I);

