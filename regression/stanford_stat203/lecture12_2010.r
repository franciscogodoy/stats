
library(lars) # contains lars
library(MASS) # contains lm.ridge
# ----- Building Prices

building=read.table("Data\\BuildingPrices.txt",header=T)
attach(building)


# ----- Ridge regression -----
building.ridge = lm.ridge(Y ~ ., data=building,
                   lambda = seq(0,12,0.1))

#jpeg("building_ridge.jpg",height=600,width=600)
plot(building.ridge,xlab="lambda",ylab="t(beta)",lwd=2)
#dev.off()

plot(building.ridge$lambda,building.ridge$GCV)

# ----- lars -----

X <- model.matrix(lm(Y~X1+X2+X3+X4+X5+X6+X7+X8+X9, data=building))
building.lars = lars(X,building$Y,type="lar",trace=TRUE)
plot(building.lars)

#jpeg("building_larscp.jpg",height=600,width=600)
plot(building.lars$Cp)
Cpmin = which.min(building.lars$Cp)
keepvars = seq(0:9)[building.lars$entry<=Cpmin]
keepvars

#dev.off()



# ----- Cross validation on LARS
#jpeg("building_lars_cv.jpg",height=600,width=600)
building.lars.cv=cv.lars(x=X,y=Y)
#dev.off()


