
  plot(X,S, type='n', xlab='Experience', ylab='Salary');
  colors=c('red', 'green', 'blue');
  symbols = c(23,24);
  for (i in 1:3) {
    for (j in 0:1) {
      subset = as.logical((E==i) * (M==j))
      points(X[subset], S[subset], pch=symbols[j+1], bg=colors[i],cex=2)
    }
  }
  


  salary.lm = lm(S ~ E + M + X);
  summary(salary.lm)
  model.matrix(salary.lm)
  model.frame(salary.lm)[1:20,]
  influence.measures(salary.lm)
  
  outlierTest(salary.lm);
  
  
  r = resid(salary.lm)
  k = 1;
  plot(X,r, type='n', xlim = c(1,6), xlab='Group', ylab='Residuals');
  for (i in 1:3) {
    for (j in 0:1) {
      subset = as.logical((E==i) * (M==j))
      points(rep(k,length(r[subset])), r[subset], pch=symbols[j+1], bg=colors[i],cex=2)
      k = k+1;
    }
  }
  
  r = rstandard(interactionM.lm)
  plot(X, r, type='n')
  for (i in 1:3) {
   for (j in 0:1) {
     subset <- as.logical((E == i) * (M == j))
     points(X[subset], r[subset], pch=symbols[j+1], bg=colors[i], cex=2)
   }
  }
  
  
  interactionX.lm = lm(S ~ E * X + M);
  summary(interactionX.lm);
  influence.measures(interactionX.lm);
  model.matrix(interactionX.lm);
  anova(interactionX.lm, salary.lm);


  interactionM.lm = lm(S ~ E * M + X);
  summary(salary.lm);
  summary(interactionM.lm);
  anova(interactionM.lm, salary.lm);

  

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

