% Use cvx to learn w, b for a linear SVM model
% from a nonseparable dataset (x,y).

function [w b] = l1normSVM(C, X, y)
    n = size(X,1);
    p = size(X,2);
    
    cvx_begin
        variables w(p) b e(n);
        minimize 1/2*sum(w.*w) + C*sum(e);
        y.*(X*w + b) >= 1 - e;
        e >= 0;
    cvx_end
end
