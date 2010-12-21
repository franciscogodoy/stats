function f1 = anova(stats)

    %produce std regression anova table from fstat field from stats outout
    %structure
    
    f = stats.fstat;

    fprintf('\n')
    fprintf('Regression ANOVA');
    fprintf('\n\n')

    fprintf('%6s','Source');
    fprintf('%10s','df','SS','MS','F','P');
    fprintf('\n')

    fprintf('%6s','Model');
    fprintf('%10.4f',f.dfr,f.ssr,f.ssr/f.dfr,f.f,f.pval);
    fprintf('\n')

    fprintf('%6s','Error');
    fprintf('%10.4f',f.dfe,f.sse,f.sse/f.dfe);
    fprintf('\n')

    fprintf('%6s','Total');
    fprintf('%10.4f',f.dfe+f.dfr,f.sse+f.ssr);
    fprintf('\n')
    f1 = f;
    
end
