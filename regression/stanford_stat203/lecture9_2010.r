

# ---------------------------- #
# Transformations: bacteria
# example.
# ---------------------------- #

bacteria.table <- read.table("Bacteria.txt", header=T)
attach(bacteria.table)

##PLOT: bacteria vs. time -- exponential decay?
#jpeg('bacteria.jpg', height=600, width=600)
plot(bacteria.table, pch=23, cex=2, bg='orange')
#dev.off()
##PLOTEND

# Fit model with untransformed data

bacteria.lm <- lm(N_t ~ t)
plot(bacteria.lm, cex=2, pch=23, bg='orange')
plot(bacteria.table, pch=23, cex=2, bg='orange')
lines(t, fitted(bacteria.lm), lwd=2, col='red')

# Fit model with log-transformed data

bacteria.log.lm <- lm(log(N_t) ~ t)
par(mfrow=c(2,2))
plot(bacteria.log.lm, cex=2, pch=23, bg='orange')


par(mfrow=c(1,1))
plot(bacteria.table, pch=23, cex=2, bg='orange')
lines(t, fitted(bacteria.lm), lwd=2, col='red')
lines(t, exp(fitted(bacteria.log.lm)), lwd=2, col='green')



# ------------------------------------- #
# Weighted Least Squares
# ------------------------------------- #

#---- Example of estimating sd from the data:

manager=read.table("Manager.txt",header=T)
attach(manager)
manager.lm=lm(Y~X,data=manager)
manager.absres = abs(rstandard(manager.lm))

plot(manager$X,manager$Y, xlab="Number of workers", ylab="Number of managers")
abline(manager.lm)

plot(manager$X,manager.absres)
stdres.fit = lm(manager.absres~manager$X)
abline(stdres.fit)
sthat = fitted(stdres.fit)
manager.lm2 = lm(Y~X,data=manager,weights=1/(sthat^2))

jpeg("manager_fits.jpg",width=600,height=600)
plot(manager$X,manager$Y,pch=15)
abline(manager.lm2,col="red",lwd=2)
abline(manager.lm,col="blue",lwd=2)
dev.off()

#---- Example of a data set with multiple Y's at each X:
X= c(1:10)
sdx = c(6, 8, 4, 6, 3, 4 ,8, 1, 2, 1)
nx = c(20,10,10,15, 12,40,10,15,20,18)
beta = c(1,1,1,1,1,1,1,0.5, 0.5, 0.5)
x=matrix(0,1,0)
y=matrix(0,1,0)
for(i in 1:length(X)){
    y = cbind(y, matrix(rnorm(nx[i],beta[i]*X[i],sdx[i]),1,nx[i]))
    x = cbind(x, matrix(rep(X[i],nx[i]),1,nx[i]))
}
y=t(y)
x=t(x)


# Estimate the variance in each region.
sdhatx = matrix(0,1,length(sdx))
for(i in 1:length(X)){
    sdhatx [i]= sd(y[x==i])
}

w = matrix(0,1,0)
for (i in  1:length(X)){
    w = cbind(w,matrix(rep(sdhatx[i],nx[i]),1,nx[i]))
}

w=1/w
w=t(w)

# fit two models
lm1=lm(y~x)
lm2=lm(y~x,weights=w)



#jpeg("groupedls.jpg",height=600,width=600)
plot(x,y,pch=15)
abline(lm1,col="red",lwd=2)
abline(lm2,col="blue",lwd=2)
#dev.off()
