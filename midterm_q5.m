% Midterm q5.
load TT1-5.DAT
X = TT1_5;
X(:,1) = X(:,1) + 1;
X(:,2) = X(:,2) + 1;
X(:,6) = X(:,1)*10 + X(:,2);

nl_arr = [12 12];
g = 2; b = 2; p = 3;
n = 6;
X_mean = mean(X(:,3:5))';

SSP_f1 = [0 0 0;0 0 0;0 0 0];
for l = 1:g 
    groupl_idx = find(X(:,1) == l); 
    Xl_mean = mean(X(groupl_idx,3:5))';    
    SSP_f1 = SSP_f1 + b*n*(Xl_mean - X_mean)*(Xl_mean - X_mean)';
end;
SSP_f1 

SSP_f2 = [0 0 0;0 0 0;0 0 0];
for k = 1:b 
    groupk_idx = find(X(:,2) == k); 
    Xk_mean = mean(X(groupk_idx,3:5))';    
    SSP_f2 = SSP_f2 + b*n*(Xk_mean - X_mean)*(Xk_mean - X_mean)';
end;

SSP_f2

SSP_int = [0 0 0;0 0 0;0 0 0];
for l = 1:g 
    for k = 1:b 
        groupl_idx = find(X(:,1) == l); 
        groupk_idx = find(X(:,2) == k);
        grouplk_idx = find(X(:,6) == l*10+k);
        Xl_mean = mean(X(groupl_idx,3:5))';    
        Xk_mean = mean(X(groupk_idx,3:5))';
        Xlk_mean = mean(X(grouplk_idx,3:5))';        
        SSP_int = SSP_int + n*(Xlk_mean - Xl_mean - Xk_mean + X_mean)*(Xlk_mean - Xl_mean - Xk_mean + X_mean)';
    end;
end;
SSP_int

SSP_res = [0 0 0;0 0 0;0 0 0];
row = 0;
for l = 1:g 
    for k = 1:b 
        for r = 1:n 
            row = row + 1;
            grouplk_idx = find(X(:,6) == l*10+k);
            Xlk_mean = mean(X(grouplk_idx,3:5))';        
            SSP_res = SSP_res + (X(row,3:5)' - Xlk_mean)*(X(row,3:5)' - Xlk_mean)';
        end;
    end;
end;
SSP_res

SST = SSP_f1 + SSP_f2 + SSP_int + SSP_res

wilk_lambda = det(SSP_res)/det(SSP_int + SSP_res)
F_val = ((1-wilk_lambda)/wilk_lambda)*((g*b*(n-1) - p + 1 )/2)/((abs((g-1)*(b-1)-p)+1)/2)

wilk_lambda1 = det(SSP_res)/det(SSP_f1 + SSP_res)
F1_val = ((1-wilk_lambda1)/wilk_lambda1)*((g*b*(n-1) - p + 1 )/2)/((abs((g-1)-p)+1)/2)

wilk_lambda2 = det(SSP_res)/det(SSP_f2 + SSP_res)
F2_val = ((1-wilk_lambda2)/wilk_lambda2)*((g*b*(n-1) - p + 1 )/2)/((abs((g-1)-p)+1)/2)

group1_idx = find(X(:,1) == 1); 
X1_mean = mean(X(groupl_idx,3:5))'  
group2_idx = find(X(:,1) == 2); 
X2_mean = mean(X(group2_idx,3:5))'  

X1_mean -  X2_mean