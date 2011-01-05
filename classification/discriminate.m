function y0_hat = discriminate(x0, x, y)
    % compute linear discriminant scores (eq. 11-47, pp.) 
    g = size(x,2);
    d_scores = zeros(g,1);
    
    for i = 1:g
        xi = x{i};
        xi_bar = mean(xi)';
        Si = cov(xi);
        Si_inv = inv(Si);
        d_scores(i) = -0.5 * log(det(Si)) - 0.5 * (x0-xi_bar)' * Si_inv * (x0-xi_bar);
    end
    
     % classify x0 based on discriminant scores (11-47) 
    [scores, idx ] = max(d_scores);           
    y0_hat = idx;
end 