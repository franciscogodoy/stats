
url = 'salary.table';
salary.table <- read.table(url, header=T)
salary.table$E <- factor(salary.table$E)
salary.table$M <- factor(salary.table$M)
attach(salary.table)


# triangles for management, diamonds for non-management
# red for education=0, green for education=1, blue for education=2

plot(X,S, type='n', xlab='Experience', ylab='Salary')
colors <- c('red', 'green', 'blue')
symbols <- c(23,24)
for (i in 1:3) {
  for (j in 0:1) {
    subset <- as.logical((E == i) * (M == j))
    points(X[subset], S[subset], pch=symbols[j+1], bg=colors[i], cex=2)
  }
}

salary.lm <- lm(S ~ E + M + X)
summary(salary.lm)

# the design matrix
model.matrix(salary.lm)[1:20,]

# the data frame it was based on
# we see the 0-1 coding of factors in
# the design matrix
model.frame(salary.lm)[1:20,]

library(car)
influence.measures(salary.lm)
outlierTest(salary.lm)
r = resid(salary.lm)

k = 1

# this type gives an empty plotting window 'n' for 'none'

plot(X, r, xlim=c(1,6), type='n', xlab='Group', ylab='Residuals')

for (i in 1:3) {
  for (j in 0:1) {
    subset <- as.logical((E == i) * (M == j))
    points(rep(k, length(r[subset])), r[subset], pch=symbols[j+1], bg=colors[i], cex=2)
    k = k+1
  }
}

##########Testing for interactions
##  Fit models with two-way interactions. 
## To test for interactions, use either anova or f.test.lm

interactionX.lm <- lm(S ~ E * X + M)
summary(interactionX.lm)
outlierTest(interactionX.lm)


# is there an interaction between education and experience?
anova(salary.lm, interactionX.lm)


# is there an interaction between education and management?
interactionM.lm <- lm(S ~ X + E * M)
summary(interactionM.lm)
anova(salary.lm, interactionM.lm)
model.matrix(interactionM.lm)[1:20,]
r = rstandard(interactionM.lm)
plot(X, r, type='n')
for (i in 1:3) {
  for (j in 0:1) {
    subset <- as.logical((E == i) * (M == j))
    points(X[subset], r[subset], pch=symbols[j+1], bg=colors[i], cex=2)
  }
}
print(outlierTest(interactionM.lm))

subs33 = c(1:length(S))[-33]

salary.lm33 = lm(S ~ E + X + M, subset=subs33)
interactionX.lm33 = lm(S ~ E * X + M, subset=subs33)

summary(salary.lm33)
summary(interactionX.lm33)
anova(salary.lm33,interactionX.lm33)

interactionM.lm33 = lm(S ~ X + E * M, subset=subs33)
summary(interactionM.lm33)
anova(salary.lm33,interactionM.lm33)
r = rstandard(interactionM.lm33)
plot(X[subs33], r, type='n')
for (i in 1:3) {
  for (j in 0:1) {
    subset <- as.logical((E == i) * (M == j))
    points(X[subset], r[subset], pch=symbols[j+1], bg=colors[i], cex=2)
  }
}
salaryfinal.lm = lm(S ~ X + E * M, subset=subs33)
mf = model.frame(salaryfinal.lm)
plot(mf$X, mf$S, type='n', xlab='Experience', ylab='Salary')
colors <- c('red', 'green', 'blue')
ltys <- c(2,3)
symbols <- c(23,24)
for (i in 1:3) {
  for (j in 0:1) {
    subset <- as.logical((E == i) * (M == j))
    subset33 <- as.logical((mf$E == i) * (mf$M == j))
    points(X[subset], S[subset], pch=symbols[j+1], bg=colors[i], cex=2)
    lines(mf$X[subset33], fitted(salaryfinal.lm)[subset33], lwd=2, lty=ltys[j], col=colors[i])
  }
}

####################################
############ Visualizing
####################################

## From first look at the data...
## the difference between Master’s and PhD in the management group is different than in the non-management group. 
## This is an interaction between the two qualitative variables management,M and education,E. 
## visualize this by first removing the effect of experience, then plotting the means within each of the 6 groups using interaction.plot.

U = S - X * interactionX.lm33$coef['X']
interaction.plot(E, M, U, type='b', col=c('red', 'blue'), lwd=2, pch=c(23,24));
detach(salary.table)


#####################
# Kidney example
# Two-way ANOVA
#####################


url = 'kidney.table'
kidney.table <- read.table(url, header=T)
kidney.table$Duration <- factor(kidney.table$Duration)
kidney.table$Weight <- factor(kidney.table$Weight)
kidney.table$logDays <- log(kidney.table$Days + 1) # Use log days to "stabilize" variance
attach(kidney.table)

# Plot data: is there visual evidence of interactions?

interaction.plot(Weight, Duration, logDays, type='b', col=c('red',
                'blue'), lwd=2, pch=c(23,24))
# Fit model

kidney.lm <- lm(logDays ~ Duration * Weight)

# Look at ANOVA table

anova(kidney.lm)

# Look at interactions

anova(lm(logDays ~ Duration + Weight), kidney.lm)

# Look for main effect of Weight: note it is different from
# anova(.) output! Should use anova output in general,
# unless you really know there are no interactions.
anova(lm(logDays ~ Duration), lm(logDays ~ Duration + Weight))
anova(lm(logDays ~ Weight), lm(logDays ~ Duration + Weight))

# Sums of squares
# There are different types of sums of squares (I,II,III and more). 
# R uses type I by default but for balanced design you can code the dummy variables with contrasts to yield the same ANOVA table. 
# The other two types, which are more common in practice can be found in the “car” library.

library(car)
sum.lm = lm(logDays ~ Duration * Weights, contrasts=list(Duration=contr.sum, Weight=contr.sum))


anova(sum.lm)
Anova(sum.lm, type='II')
Anova(sum.lm, type='III')

# these sums of squares are different if you use the default
# contrast codings, i.e. contr.treatment
nosum.lm = lm(logDays ~ Duration * Weights)
anova(nosum.lm)
Anova(nosum.lm, type='II')
Anova(nosum.lm, type='III')

