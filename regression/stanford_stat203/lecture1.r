calcium.table=read.table('Data/CalciumBloodPressure.txt',header=T)
attach(calcium.table)

# Numerical summaries of the two groups

treated =Decrease[(Treatment == 'Calcium')]
placebo =Decrease[(Treatment == 'Placebo')]

# Plot the descriptive statistics

# boxplot 
boxplot(Decrease ~ Treatment, col='orange', pch=23, bg='red')

# histogram for treatment group 
hist(treated, main='', xlab='Decrease', col='orange')

# histogram for placebo group 
jpeg('calcium_placebo.jpg', height=800,width=800) 
hist(placebo, main='', xlab='Decrease', col='orange')
dev.off()

# Get summaries

summary(placebo)
summary(treated)

# Also, each piece of information can be extracted individually

min(treated) 
max(treated) 
median(treated) 
print(quantile(treated,probs=c(0.25,0.75))) 
sd(treated) 
var(treated)

# Get confidence interval for population mean Decrease (ignoring Treatment)

CI <- c(mean(Decrease)-qt(0.975,20)*sd(Decrease)/sqrt(21),mean(Decrease)+qt(0.975,20)*sd(Decrease)/sqrt(21)) 
print(CI)

# Test whether population mean Decrease is 0 (also gives the same CI!)

t.test(Decrease)

# Test for differential effect of Treatment, assuming equal
variances

t.test(Decrease ~ Treatment, var.equal=T)

# Fitting a regression model
summary(lm(Decrease~Treatment))
