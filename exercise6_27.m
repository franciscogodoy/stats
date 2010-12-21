% ---------------------------------------------------------------------
% Problem:      Excercise 6.27 - a       
% ---------------------------------------------------------------------

% PART aaaaaaaaaaaaaaaaa 
% Plots mean vectors for husband & wifes as sample profiles 

load ex6_27.dat;
X = ex6_27;

% X0 is husband rating wife data, X1 is wife rating husband data
x1_idx = find(ex6_27(:,5) == 0);
x2_idx = find(ex6_27(:,5) == 1);
X1 = X(x1_idx,1:4);
X2 = X(x2_idx,1:4);

x1_mean = mean(X1)';
x2_mean = mean(X2)';

plot(x1_mean,'r','LineWidth',2);
title('Mean vectors for husband and wife as sample profiles')
xlabel('Variable');
ylabel('Sample mean response');
hold on; 
plot(x2_mean,'b','LineWidth',2,'Linestyle','--');
legend('Husband','Wife');
axis([1 5 0 5]);
hold off;

% PART bbbbbbbbbbbbbbbbbbbbbb

% Is husband rating wife profile parallel to wife rating husband profile?
% Test for parallel profiles with alpha = .05
[n1,p] = size(X1);
[n2,p] = size(X2);

S1 = cov(X1);
S2 = cov(X2);
S_pooled =  ((n1-1)/(n1+n2-2)) * S1 + ((n2-1)/(n1+n2-2)) * S2;
C = [-1 1 0 0;  0 -1 1 0; 0 0 -1 1];

T_square = (x1_mean - x2_mean)' * C' * inv((1/n1 + 1/n2)*C*S_pooled*C')*C*(x1_mean - x2_mean)
c_square = (((n1 + n2 - 2)*(p-1))/(n1 + n2 -p)) * finv(.95,p-1,n1+n2-p)

if T_square <= c_square  
    sprintf('Husband rating wife profile parallel to wife rating husband profile. T square = %.2f <= %.2f', T_square, c_square)
else   
    sprintf('Husband rating wife profile NOT parallel to wife rating husband profile. T square = %.2f > %.2f', T_square, c_square)
end      

% Test for coincident profiles at the same level alpha = .05
one = [1 1 1 1]';
T_square = ((one' * (x1_mean - x2_mean))/sqrt((1/n1 + 1/n2) * one' * S_pooled * one))^2
f_crit_val = finv(.95,1,n1+n2-2)

if T_square <= f_crit_val  
    sprintf('Husband rating wife profile coincident to wife rating husband profile. T square = %.2f <= %.2f', T_square, f_crit_val)
else   
    sprintf('Husband rating wife profile NOT coincident to wife rating husband profile. T square = %.2f > %.2f', T_square, f_crit_val)
end 

% Test for level profiles at the same level alpha = .05
X12 = X(:,1:4);
S = cov(X12);
x12_mean = mean(X12)';

T_square = (n1 + n2) * x12_mean' * C' * inv(C * S * C') * C * x12_mean
c_square = (((n1 + n2 - 1)*(p-1))/(n1 + n2 -p + 1)) * finv(.95,p-1,n1+n2-p+1)

if T_square <= c_square  
    sprintf('Husband rating wife profile & wife rating husband profile level. T square = %.2f <= %.2f', T_square, c_square)
else   
    sprintf('Husband rating wife profile & wife rating husband profile NOT level. T square = %.2f <= %.2f', T_square, c_square)
end 


