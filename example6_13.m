% Example 6.13

F1 = [repmat(1,10,1); repmat(2,10,1)];
F2 = repmat([repmat(1,5,1); repmat(2,5,1)],2,1);
X1 = [6.5, 6.2, 5.8, 6.5, 6.5, 6.9, 7.2, 6.9, 6.1, 6.3, 6.7, 6.6, 7.2, 7.1, 6.8, 7.1, 7.0, 7.2, 7.5, 7.6]';
X2 = [9.5, 9.9, 9.6, 9.6, 9.2, 9.1, 10.0, 9.9, 9.5, 9.4, 9.1, 9.3, 8.3, 8.4, 8.5, 9.2, 8.8, 9.7, 10.1, 9.2]';
X3 = [4.4, 6.4, 3.0, 4.1, 0.8, 5.7, 2.0, 3.9, 1.9, 5.7, 2.8, 4.1, 3.8, 1.6, 3.4, 8.4, 5.2, 6.9, 2.7, 1.9]';
F12 = [repmat(11,5,1); repmat(12,5,1); repmat(21,5,1); repmat(22,5,1)];
X = [F1, F2, X1, X2, X3, F12];

nl_arr = [10 10];
g = 2; b = 2; p = 3;
n = 5;
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
