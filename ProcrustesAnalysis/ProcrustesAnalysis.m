% procrustes function analyzes the distribution of a set of shapes 
% Procrustes analysis matches landmark data 
% (geometric locations representing significant features in a given shape) 
% to calculate the best shape-preserving Euclidian transformations. 
% These transformations minimize the differences in location between compared landmark data.

% Procrustes analysis is also useful in conjunction with multidimensional scaling. 
% Two different applications of multidimensional scaling could produce 
% reconstructed points that are very similar in principle, 
% but that look different because they have different orientations. 
% The procrustes function transforms one set of points to make them more comparable to the other.


% The procrustes function takes two matrices as input:
%    (1) The target shape matrix X has dimension n × p. n is the  number of landmarks 
%         in the shape and p is the number of measurements per landmark.

%    (2) The comparison shape matrix Y has dimension n × q with q ? p. 
%        If there are fewer measurements per landmark for the comparison shape 
%           than the target shape (q < p), the function adds columns of zeros to Y, yielding an n × p matrix.
 
% The equation to obtain the transformed shape, Z, is
%        Z = bYT + c
%   where: 
%       (a) b is a scaling factor that stretches (b > 1) or shrinks (b < 1)
%             the points.
%       (b) T is the orthogonal rotation and reflection matrix.
%       (c) c is a matrix with constant values in each column, used to shift the points.

% The procrustes function chooses b, T, and c to minimize the distance between 
% the target shape X and the transformed shape Z as measured by the least squares criterion:
%       sum_i(sum_j((X_ij-Z_ij)^2));

% Comparing Handwritten Shapes
% use Procrustes analysis to compare two handwritten number threes. 
% Visually and analytically explore the effects of forcing size and reflection changes:

% (1)  Load and Display the Original Data. i.e. Input landmark data for two
%       handwritten number threes:

    A = [11 39;17 42;25 42;25 40;23 36;19 35;30 34;35 29;30 20;18 19];
    B = [15 31;20 37;30 40;29 35;25 29;29 31;31 31;35 20;29 10;25 18];

    % Create X and Y from A and B, moving B to the side to make each shape more visible:
    X = A;
    Y = B + repmat([25 0], 10,1); 

    % Plot the shapes, using letters to designate the landmark points. Lines in
    % the figure join the points to indicate the drawing path of each shape.
    plot(X(:,1), X(:,2),'r-', Y(:,1), Y(:,2),'b-');
    text(X(:,1), X(:,2),('abcdefghij')');
    text(Y(:,1), Y(:,2),('abcdefghij')');
    legend('X = Target','Y = Comparison','location','SE');
    set(gca,'YLim',[0 55],'XLim',[0 65]);

% (2) Calculate the Best Transformation
%   Use Procrustes analysis to find the transformation that minimizes
%   distances between landmark data points

    [d, Z, tr] = procrustes(X,Y);
    
    % The outputs of the function are:
    %   d – A standardized dissimilarity measure.
    %   Z – A matrix of the transformed landmarks.
    %   tr – A structure array of the computed transformation with fields T, 
    %   b, and c which correspond to the transformation equation, Equation 10-1.

    % Visualize the transformed shape, Z, using a dashed blue line:
    plot(X(:,1), X(:,2),'r-', Y(:,1), Y(:,2),'b-', Z(:,1),Z(:,2),'b:');
    text(X(:,1), X(:,2),('abcdefghij')')
    text(Y(:,1), Y(:,2),('abcdefghij')')
    text(Z(:,1), Z(:,2),('abcdefghij')')
    legend('X = Target','Y = Comparison',...
    'Z = Transformed','location','SW')
    set(gca,'YLim',[0 55],'XLim',[0 65]);


% (3) Examine the Similarity of the Two Shapes
%       Use two different numerical values to assess the similarity of the target
%       shape and the transformed shape.

        % The dissimilarity measure d gives a number between 0 and 1
        % describing the difference between the target shape and the
        % transformed shape. Values near 0 imply more similar shapes, 
        % while values near 1 imply dissimilarity. 
        
        disp(d);

        %The small value of d in this case shows that the two shapes are
        %similar.
        
        % procrustes calculates d by comparing the sum of squared
        % deviations between the set of points with the sum of squared 
        % deviations of the original points from their column means:

        % dis = zeros(10,1);
        % for j = 1:10 
        %    dev = X(j,:) - Z(j,:);
        %    dis(j) = dev * dev';
        % end
        % disp(sum(dis));
        
        numerator = sum(sum((X-Z).^2));
        meanX  = repmat(mean(X), 10,1);
        denominator = sum(sum((X-meanX).^2));
        ratio = numerator/denominator;
        disp(ratio);

        % Examining the Scaling Measure b shows how to examine the size
        % similarity of the shapes. 
        
        % The target and comparison threes in the previous figure visually
        % show that the two numbers are of a similar size. The closeness of
        % calculated value of the scaling factor b to 1 supports this
        % observation as well:   
        
        disp(tr.b);
        
        % The sizes of the target and comparison shapes appear similar.
        % This visual impression is reinforced by the value of b = 0.93,
        % which implies that the best transformation results in shrinking the comparison shape by a factor .93 (only 7%)
        
% (4) Restrict the Form of the Transformations        
%       Explore the effects of manually adjusting the scaling and
%       reflection coefficients        
%       - Fixing the Scaling Factor b = 1.  
%           Force b to equal 1 (set 'Scaling' to false) to examine the
%           amount of dissimilarity in size of the target and transformed
%           figures 

            % ds = procrustes(X,Y,'Scaling',false);
            % disp(ds);    
            
            % In this case, setting 'Scaling' to false increases the
            % calculated value of d only 0.0049, which further supports the 
            % similarity in the size of the two number threes. 
            % A larger increase in d would have indicated a greater size discrepancy.
            
            % If you need a reflection in the transformation, the determinant of T is -1. 
            % You can force a reflection into the transformation as follows:

            % [dr,Zr,trr] = procrustes(X,Y,'Reflection',true);

            