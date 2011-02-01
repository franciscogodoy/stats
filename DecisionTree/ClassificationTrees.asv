% From 
% http://www.mathworks.com/help/toolbox/stats/bq4embr-1.html

load fisheriris;

% create a classification tree for predicting species using measurements of
% sepal length, sepal width, petal length, and petal width as predictors.
tree = classregtree(meas,species, 'names',{'SL' 'SW' 'PL' 'PW'});

% to show the type of the tree:
treetype = type(tree);

% To view the tree
view(tree);


% as tree does not use sepal measurements for predicting species. 
% enter unmeasured attribute as NaN values for predictions. 

% predict the species of an iris with petal length 4.8 and petal width 1.6,
predicted = tree([NaN NaN 4.8 1.6]);

% Use cutvar and cuttype to get more information about the split at node 6 
% that makes the final distinction between versicolor and virginica

% see what variable determines the split
disp(cutvar(tree,6));

% see what type of split is
disp(cuttype(tree,6));

% Find assigned class names
sfit = eval(tree,meas);

%confusion maTRIX
crosstab (sfit, species);

%Compute correctly classified proportion 
pct = mean(strcmp(sfit,species));

% Classification trees fit the training data well, but may not do a good
% job classifying new values. Lower branches, especially, may be strongly
% affected by outliers. 

% To avoid overfitting...
% Use the prune method of the classregtree class to find the next largest
% tree from an optimal pruning sequence
pruned = prune(tree,'level',1);
pruned_fit = eval(pruned,meas);
crosstab (pruned_fit, species);
pruned_pct = mean(strcmp(pruned_fit,species));

% Find the minimum-cost tree using cross validation
[c,s,n,best] = test(tree,'cross',meas,species);
tmin = prune(tree,'level',best);
view(tmin);
tmin_fit = eval(tmin,meas);
crosstab (tmin_fit, species);
tmin_pct = mean(strcmp(tmin_fit,species));
