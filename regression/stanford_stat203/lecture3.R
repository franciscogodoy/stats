rivers<-read.table("Data\\Rivers.txt",header=T)
attach(rivers)

plot(Agr,Nitrogen,col="blue",lwd=3)
agr.lm=lm(Nitrogen~Agr);

rstandard(agr.lm)
qqplot(rstandard(agr.lm), rt(1000, length(Agr)-3), cex=2, pch=17)
qqline(rstandard(agr.lm), rt(1000, length(Agr)-3))

hatvalues(agr.lm)
plot(Agr, hatvalues(agr.lm), pch=17, cex=2)

plot(agr.lm,which=3)# fitted versus std. res.
plot(agr.lm,which=2) # qqnorm of std res.
plot(agr.lm, which=4) # Cook's distance

# Try these measures of influence

# plot the data points by their index numbers.
plot(Agr, Nitrogen, cex=0.1)
for(i in 1:length(Agr)){
    text(Agr[i],Nitrogen[i],paste(i))
}



influence.measures(agr.lm)
hatvalues(agr.lm) # hatvalues in R is same thing as leverage.
cooks.distance(agr.lm)


# ----------------------------
# Commercial Industrial Index
# ----------------------------

plot(ComIndl,Nitrogen,col="blue",lwd=3)
ComIndl.lm=lm(Nitrogen~ComIndl);

plot(hatvalues(ComIndl.lm))

plot(ComIndl.lm,which=3) 
plot(ComIndl.lm,which=2) 
plot(agr.lm, which=4) 
plot(ComIndl.lm,which=5) # leverage versus residuals


influence.measures(ComIndl.lm)
hatvalues(ComIndl.lm) # hatvalues in R is same thing as leverage.
cooks.distance(ComIndl.lm)

dffits(ComIndl.lm) 
dfbetas(ComIndl.lm)


# You can plot them, for example:
plot(hatvalues(ComIndl.lm),type="h")

# Use the help documentation to learn more about what each does.
?dffits
?dfbetas

# Take out a data point and do a fit without it.
# Here, I took out datapoint number 4 in the slides.

png("rivers_ComIndlvsNitrogen_no4.jpg", height=500, width=500)
ComIndl.lm.no4=lm(Nitrogen[-4]~ComIndl[-4])
plot(ComIndl,Nitrogen,col="blue",lwd=3)
abline(ComIndl.lm,col="red",lwd=2)
abline(ComIndl.lm.no4,col="green",lwd=2)
dev.off()


