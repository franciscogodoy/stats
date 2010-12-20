	install.packages("ISwR");

# generating a factor
	attach(juul);
	juul$tanner <- factor(juul$tanner, labels=c("I","II","III","IV","V"));

	detach(juul);
	attach(juul);
	summary(tanner);

	# assuming equal variances	
	anova(lm(igf1~tanner));	

# folate example
	attach(red.cell.folate);

# One-way analysis of variance	
	anova(lm(folate~ventilation))
	summary(red.cell.folate);
	by(folate, ventilation, summary);

# not assuming equal variances
	oneway.test(folate~ventilation) 


#F test shows there is a difference between groups
#Now the question is, where the difference lies.
#Compare the individual groups.
	summary(lm(folate~ventilation));

# pairwise.t.test computes all possible two-group comparisons
# capable of making adjustments for multiple comparisons
	pairwise.t.test(folate, ventilation, p.adj="bonferroni");

#Manually compute each SS
	SStr = ((by(folate, ventilation, mean) - mean(folate))^2) %*%c(8,9,5);
	SScor = sum((folate - mean(folate))^2)
	SSres = SScor - SStr;
	f.stat = (SStr/(3-1))/(SSres/((nrow(red.cell.folate) - 3)));
	# compare f.stat with f.quant 
	f.quant = qf(0.95, 3-1, nrow(red.cell.folate) - 3);
	if (f.stat > f.quant) {
		print('reject Ho: no treatment effect');
	} else {
		print('Would not reeject Ho: no treatment effect');
	}

# perform pairwise t tests so that they do not use
# a common pooled standard deviation.
	pairwise.t.test(folate,ventilation,pool.sd=F)

# Graphical presentation
	xbar <- tapply(folate, ventilation, mean)
	s <- tapply(folate, ventilation, sd)
	n <- tapply(folate, ventilation, length)
	sem <- s/sqrt(n)
	stripchart(folate~ventilation, method="jitter", jitter=0.05, pch=16, vert=T)
	arrows(1:3,xbar+sem,1:3,xbar-sem,angle=90,code=3,length=.1)
	lines(1:3,xbar,pch=4,type="b",cex=2)

# Test whether a variable has the same variance in all groups
# nonrobust against departures from normal assumption 
# assumed the data are from independent groups.
	bartlett.test(folate~ventilation)


# Kruskal–Wallis test
# nonparametric counterpart of a one-way analysis of variance
	kruskal.test(folate~ventilation);


