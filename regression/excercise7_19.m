% Excercise 7.19, Thanh Doan
data = load('T7-5.DAT');
n = size(data,1);

X = data(:,1:5);
y = log(data(:,6));

% Use stepwise to include appropriate (best) subset
% of predictor variables for the regression model.
% No guarantee that a different initial model will not lead to a better fit. 
% Stepwise models are locally optimal, but may not be globally optimal.
stepwisefit(X,y, 'penter',0.15,'premove',0.15);

initialModel = [true true true true true]; 
stepwisefit(X,y, 'inmodel',initialModel, 'penter',0.15,'premove',0.15);

% include all terms in initial model;
% the result of including all terms in inital model 
% (in this case) is identical to 
% the result of including zero term in initial model
% ------------------------------------------------
% initialModel = [true true true true true]; 
% stepwisefit(X,y, 'inmodel',initialModel, 'penter',0.05,'premove',0.10);
% ------------------------------------------------

X1 = data(:,1:5);

r = rank(X1); % number of predictor variables in full model (model1) 
m1_stats = regstats(y,X1);
fprintf('SS error for complete model %10.4f \n', SSE_m1);

X2 = data(:,4);
q = rank(X2); % number of predictor variables in reduced model (model2) 
m2_stats = regstats(y,X2);
fprintf('SS error for reduced model %10.4f \n', SSE_m2);

f_val  = ((SSE_m2 - SSE_m1)/(r-q)) / (SSE_m1/(n-r));
crit_val = finv(0.95, r-q, n-r);
p_val = fcdf(f_val, r-q, n-r);

fprintf('f-statistic: %10.4f, crit_value: %10.4f, p-value: %10.4f \n', f_val, crit_val, p_val);    


