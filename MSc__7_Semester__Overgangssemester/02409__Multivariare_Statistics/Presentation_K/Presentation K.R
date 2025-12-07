

# Slide 16:

heatclimate<-read.table("Data/heatclimate.txt",header=T, sep=" ")
heatclimate$sex<-as.factor(heatclimate$sex)
summary(heatclimate)


model0.1<-lm(evap~height+sex+temp,data=heatclimate)
model0.2<-lm(temp~height+sex,data=heatclimate)

par(mfrow=c(2,2))
plot(model0.1,which=1:4)
plot(model0.2,which=1:4)
par(mfrow=c(1,1))

# Slide 18:
library(MASS)
boxcox(evap~height+sex+temp,data=heatclimate)

# Slide 19:
boxcox(log(evap)~height+sex+log(temp),data=heatclimate)
boxcox(1/log(evap)~height+sex+1/log(temp),data=heatclimate)

# Slide 21:
model0.1<-lm(1/log(evap)~height+sex+1/log(temp),data=heatclimate)
model0.2<-lm(1/log(temp)~height+sex,data=heatclimate)

par(mfrow=c(2,2))
plot(model0.1,which=1:4)
plot(model0.2,which=1:4)
par(mfrow=c(1,1))

# Slide 22:
summary(heatclimate)
heatclimate[c(30,32),]

# Slide 23:
library(car)
residualPlots(model0.1)
temp<-lm(1/log(evap)~height+sex+1/log(temp)+I(1/log(temp)^2),data=heatclimate)
drop1(temp,test="F")

# Slide 24:
residualPlots(model0.2)

# slide 43:

analysis<-manova(cbind(1/log(evap),1/log(temp))~sex+height,data=heatclimate)
summary(analysis,test="Wilks")

# slide 44:
analysis<-manova(cbind(1/log(evap),1/log(temp))~sex,data=heatclimate)
summary(analysis,test="Wilks")
