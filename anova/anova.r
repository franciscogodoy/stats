# notation using pp.300, johnson mva book 
install.packages("ISwR");
	attach(red.cell.folate);
	summary(red.cell.folate);
	by(folate, ventilation, summary);

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

	attach(juul);
	juul$tanner <- factor(juul$tanner, labels=c("I","II","III","IV","V"));

	detach(juul);
	attach(juul);
	summary(tanner);

	anova(lm(igf1~tanner));


#F test shows there is a difference between groups
#Now the question is, where the difference lies.
#Compare the individual groups.
	summary(lm(folate~ventilation));




# use R anova function
anova(lm(folate~ventilation));
