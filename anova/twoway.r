# analyze data that are cross-classified according to
# several criteria.
	install.packages("ISwR");

# assumption: cross-classified design is balanced
# for a two-way classification,
# the condition is that the cell counts be equal

# heart rate after administration of enalaprilate
	#attach(heart.rate)

	heart.rate <- data.frame(hr = c(96,110,89,95,128,100,72,79,100,
		92,106,86,78,124,98,68,75,106,
		86,108,85,78,118,100,67,74,104,
		92,114,83,83,118,94,71,74,102),
		subj=gl(9,1,36),
		time=gl(4,9,36,labels=c(0,30,60,120)));

#two-way analysis of variance is specified by
	anova(lm(hr~subj+time))

# interaction.plot graphs the values against one factor 
# while connecting data for the other factor with line segments to form traces.
	interaction.plot(ordered(time),subj,hr)

# Friedman test
# nonparametric counterpart of two-way analysis of variance
	friedman.test(hr~time|subj,data=heart.rate)
