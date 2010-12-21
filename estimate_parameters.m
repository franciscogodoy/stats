function res = estimate_parameters(stats)

    %produce std regression anova table from fstat field from stats outout
    %structure    
    t = stats.tstat;
    fprintf('\n')
    fprintf('Parameter Estimates \n');
    coeffTable = dataset({t.beta,'Coef'},{t.se,'StdErr'},...
        {t.t,'tStat'},{t.pval,'pVal'});
    disp(coeffTable);
    fprintf('\n')                 
    res = t;    
end
