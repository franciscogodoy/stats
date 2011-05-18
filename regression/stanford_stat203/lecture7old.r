# ------ Two-way Random Effects --------
library(nlme)   # this library contains all mixed-effect models, linear and nonlinear.  
                # Here we only use the linear model function "lme".

#library(trellis)  # A library for plotting fancy graphics.

# if your R installation doesn't have trellis, try the following:
library(lattice)


miles = read.table("Data\\MilesPGallon.txt",header=T)
miles$Driver=as.factor(miles$Driver)
miles$Car=as.factor(miles$Car)
miles$Trial=as.factor(miles$Trial)
attach(miles)

#jpeg("miles_trellis_CAR.jpg",height=600,width=600)
xyplot(MPG~Driver|Car,main="Gasoline Consumption, sorted by car")
#dev.off()
#jpeg("miles_trellis_Driver.jpg",height=600,width=600)
xyplot(MPG~Car|Driver,main="Gasoline Consumption, sorted by driver")
#dev.off()

miles.anova=anova(lm(MPG~Driver*Car,data=miles))
print(miles.anova)
F.driver=miles.anova$Mean[1]/miles.anova$Mean[3]
1-pf(F.driver,miles.anova$Df[1],miles.anova$Df[3])
F.car=miles.anova$Mean[2]/miles.anova$Mean[3]
1-pf(F.car,miles.anova$Df[2],miles.anova$Df[3])


# ------ Two-way Mixed Effects ---------
pearls = read.table("Data\\Pearls.txt",header=T)
pearls$Coats=as.factor(pearls$Coats)
pearls$Batch=as.factor(pearls$Batch)
pearls$Bead=as.factor(pearls$Bead)
attach(pearls)

# You can define a groupedData which records the hierarchical
# nature of data collection (e.g. Batch).
pearls.grouped=groupedData(Value~Coats|Batch,data=pearls)
plot(pearls.grouped)
# "lme" is the mixed effect model analog of "lm".
pearls.lme=lme(Value~Coats,data=pearls.grouped)


#jpeg("pearls_trellis_Batch.jpg",height=600,width=600)
xyplot(Value~Coats|Batch,main="Pearl value, sorted by batch")
#dev.off()

# Another way to do the above.
pearls.lme = lme(Value~Coats, data=pearls,random=~1|Batch)
summary(pearls.lme)
fixed.effects(pearls.lme)

