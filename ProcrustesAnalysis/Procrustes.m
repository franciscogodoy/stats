% Procrustes analysis
    d = procrustes(X,Y);    
    [d,Z] = procrustes(X,Y);
    [d,Z,transform] = procrustes(X,Y);
    [...] = procrustes(...,'scaling',flag);
    [...] = procrustes(...,'reflection',flag);

% d = procrustes(X,Y) determines a linear transformation (translation, reflection, 
% orthogonal rotation, and scaling) of the points in matrix Y to best conform 
% them to the points in matrix X. The goodness-of-fit criterion is the 
% sum of squared errors. 
% procrustes returns the minimized value of this dissimilarity measure in d. d 
% is standardized by a measure of the scale of X, given by:
    sum(sum((X-repmat(mean(X,1),size(X,1),1)).^2,1));

% X and Y must have the same number of points (rows), and procrustes matches 
% Y(i) to X(i). Points in Y can have smaller dimension than those in X. 
% In this case, procrustes adds columns of zeros to Y as necessary.

% [d,Z] = procrustes(X,Y) also returns the transformed Y values.

% [d,Z,transform] = procrustes(X,Y) also returns the transformation that maps Y to Z. 
% transform is a structure array with fields:

%   c — Translation component
%   T — Orthogonal rotation and reflection component
%   b — Scale component

% That is:
    c = transform.c;
    T = transform.T;
    b = transform.b;

    Z = b*Y*T + c;


% Examples
    % This example creates some random points in two dimensions, 
    % then rotates, scales, translates, and adds some noise to those points. 
    % It uses procrustes to conform Y to X, then plots the original X and Y with the transformed Y.

    
    n = 10;  
    X = normrnd(0,1,[n 2]);
    S = [0.5 -sqrt(3)/2; sqrt(3)/2 0.5];
    Y = normrnd(0.5*X*S+2,0.05,n,2);
    [d,Z,tr] = procrustes(X,Y);
    plot(X(:,1),X(:,2),'rx', Y(:,1),Y(:,2),'b.', Z(:,1),Z(:,2),'bx');
     