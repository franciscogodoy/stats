% Excercise 5.22- Constructing confidence intervals for data in table 5.13
load T5-13.dat;

X = T5_13;
[n,p] = size(X);
X_bar = mean(X)';
S = cov(X);

% Q-Q plots of the marginal distribution of fuel, repair and capital costs
normplot(X(:,1));
normplot(X(:,2));
normplot(X(:,3));

% scatter plots from pairs of observation on different variables
plotmatrix(X);
scatter(X(:,1),X(:,2), 'filled')
scatter(X(:,1),X(:,3), 'r', 'filled')
scatter(X(:,2),X(:,3),'+')

% Find suspect outliers who is more than three standard deviations away
% from the mean.
MeanMat = repmat(mean(X),n,1); %Replicate mean vector for n rows
DeviationMat = repmat(std(X),n,1);
outliers = abs(X - MeanMat) > 3*DeviationMat;
outliers

% Remove observations 9, 21 because it appears to be outliers 
X(9,1) = NaN;
X(21,1) = NaN;
X

% Remove any rows containing NaNs from a matrix X.
X(any(isnan(X),2),:) = [];
X

[n,p] = size(X);
X_bar = mean(X)';
S = cov(X);

% Better way to remove observation 9 and 21.  
% X(9,:) = [];
% X(21,:) = [];

% Sii containing p variances S11, S22,...,Spp. 
Sii = diag(S);

% Compute simultanoous T_square intervals at the level of 95% confidence
t_square_int_weight = (p*(n-1)/(n-p)) * finv(.95,p,n-p);
t_square_intervals(:,1) = X_bar - sqrt(t_square_int_weight)*sqrt(Sii/n);
t_square_intervals(:,2) = X_bar + sqrt(t_square_int_weight)*sqrt(Sii/n);

% Compute Bonfferoni intervals at the level of 95% confidence
bonfferoni_int_weight = tinv(1-0.05/(2*p), n-1); 
bonfferoni_intervals(:,1) = X_bar - bonfferoni_int_weight*sqrt(Sii/n);
bonfferoni_intervals(:,2) = X_bar + bonfferoni_int_weight*sqrt(Sii/n);

t_square_intervals
bonfferoni_intervals