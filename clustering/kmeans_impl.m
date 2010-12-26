% Original Andrew Ng - Stanford.edu
%
% INPUT:   
%   - As input, this function takes the n x p data matrix X 
%   - and the number of clusters k.
%
% PERFORM: 
%   - The function initialize k centroids randomly 
%   - Then repeatedly perform the following steps 
%      - (1) Assignming each training example x(i) to the closest cluster
%      - (2) Moving each cluster centroid to the mean of the points assigned to it 
% 
% OUTPUT: 
%   - an n-element vector, clusters, indicating which cluster each 
%        data point belongs to, 
%   - and a k*m matrix, centroids, which contain the centroids of each cluster


function [clusters, centroids] = kmeans_impl(X, k)
    [n,p] = size(X);
    oldcentroids  = zeros(k,p);
    centroids = X(ceil(rand(k,1)*n),:);
    while (norm(oldcentroids - centroids) > 1e-15) 
        oldcentroids = centroids;
        %compute clustering assignments
        for i=1:n
            dist_to_k_means = sum((repmat(X(i,:), k,1) - centroids).^2, 2);
            [min_dist, clusters(i,1)] = min(dist_to_k_means);
        end
        
        draw_clusters(X, clusters, centroids);
        pause(0.1);
        
         % compute cluster centroids
        for i=1:k,
          centroids(i,:) = mean(X(clusters == i, :));
        end  
    end
end


function draw_clusters(X, clusters, centroids)
    % This function plot cluster assignments and centroids 
    clf;
    hold on;
    % need to actually handle all the different cases due to bug in octave
    if (max(clusters) == 1)
      plot(X(clusters==1,1), X(clusters==1,2), 'b.');
    elseif (max(clusters) == 2)
      plot(X(clusters==1,1), X(clusters==1,2), 'b.', ...
        X(clusters==2,1), X(clusters==2,2), 'g.');
    elseif (max(clusters) == 3)
      plot(X(clusters==1,1), X(clusters==1,2), 'b.', ...
        X(clusters==2,1), X(clusters==2,2), 'g.', ...
        X(clusters==3,1), X(clusters==3,2), 'r.');
    elseif (max(clusters) == 4)
      plot(X(clusters==1,1), X(clusters==1,2), 'bo', ...
        X(clusters==2,1), X(clusters==2,2), 'go', ...
        X(clusters==3,1), X(clusters==3,2), 'ro', ...
        X(clusters==4,1), X(clusters==4,2), 'co');
    elseif (max(clusters) == 5)
      plot(X(clusters==1,1), X(clusters==1,2), 'bo', ...
        X(clusters==2,1), X(clusters==2,2), 'go', ...
        X(clusters==3,1), X(clusters==3,2), 'ro', ...
        X(clusters==4,1), X(clusters==4,2), 'co', ...
        X(clusters==5,1), X(clusters==5,2), 'mo');
    else
      plot(X(clusters==1,1), X(clusters==1,2), 'bo', ...
        X(clusters==2,1), X(clusters==2,2), 'go', ...
        X(clusters==3,1), X(clusters==3,2), 'ro', ...
        X(clusters==4,1), X(clusters==4,2), 'co', ...
        X(clusters==5,1), X(clusters==5,2), 'mo', ...
        X(clusters==6,1), X(clusters==6,2), 'yo');
    end
    plot(centroids(:,1), centroids(:,2), 'kx');
end
