% Data comes from a study by Hogg and Ledolter of bacteria
% counts in shipments of milk. 
% The columns of the matrix hogg represent different shipments. 
% The rows are bacteria counts from cartons of milk chosen randomly from each shipment. 
% Do some shipments have higher counts than others?

load hogg

% using anova function in statistics toolbox
[p,tbl,stats] = anova1(hogg);
fprintf('p-value: %10.4f \n',p);
disp(tbl);


% compute f-statistics manually 
overall_mean = mean(mean(hogg));
SStr = 0;
SSres = 0;
[n, num_shipments] = size(hogg);

for i = 1:num_shipments
    mean_i = mean(hogg(:,i));
    diff = repmat(mean_i,n,1) - repmat(overall_mean,n,1);    
    SStr = SStr + diff'*diff;
    
    diff2 = hogg(:,i) - mean(hogg(:,i));
    SSres =  SSres + diff2'*diff2;    
end


fprintf('Sum of square for treament effect: %10.4f \n',SStr);
fprintf('Sum of square for error: %10.4f \n',SSres);

f = (SStr/(num_shipments-1))/(SSres/(n*num_shipments-num_shipments));
fprintf('F-statistics value: %10.4f \n',f);

pval = fcdf(f,num_shipments-1,n*num_shipments-num_shipments);
fprintf('p-value: %10.4f \n',1-pval);

