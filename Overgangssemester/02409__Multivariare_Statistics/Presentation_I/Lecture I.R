# Slide 7:
crime <- read.delim("Data/us_statewide_crime.txt")
crime<-crime[,c(1,3,6)]
head(crime)

plot(crime$college, crime$murder.rate, xlab="Percentage with college education",
     ylab="Murder rate", las=1, cex = 1.5, col = "blue", lwd = 2)

# Slide 14:
x <- crime$college
y <- crime$murder.rate
(beta <- sum((x-mean(x))*(y-mean(y)))/sum((x-mean(x))^2))

(alpha <- mean(y)- beta*mean(x))

Fitted <- alpha + beta*x
Resid <- y - Fitted
sigma2 <- sum(Resid^2)/(length(y)-2)
sqrt(sigma2)
# [1] 5.6146


# Slide 15:
reg1 <- lm(murder.rate ~  college, data = crime)
summary(reg1)


# Slide 16:
confint(reg1)
#Nice table
tab <- cbind(coef(summary(reg1))[ , 1:2], "Lower" = confint(reg1)[ , 1],
             "Upper" = confint(reg1)[ , 2])
tab
#Nice table with p-values
data.frame(round(tab, 2),
           "p-value" = format.pval(coef(summary(reg1))[ , 4], digits = 3, eps = 1e-3))


# Slide 21:

par(mfrow = c(2, 2))
plot(reg1, which = 1:4)
par(mfrow = c(1, 1))


crime[9,]

# Slide 23:

1-(1-2*(1-pnorm(reg1$res[9]/sqrt(sigma2))))^51


# Slide 24:

crime50 <- crime[-9, ]
reg2 <- lm(murder.rate ~  college, data = crime50)

(sigma.new<-summary(reg2)$sigma)

1-(1-2*(1-pnorm(reg1$res[9]/sigma.new)))^51


# Slide 26:


par(mfrow = c(2, 2))
plot(reg2, which = 1:4)
par(mfrow = c(1, 1))


# Slide 27:

summary(reg2)

# Slide 28:
plot(murder.rate ~ college, data = crime, las=1)
abline(reg2,font=2,col="blue")
abline(reg1,font=2,col="red",lty=2)

legend(15,30,c("Including Washington DC","Excluding Washington DC"),
       lty=c(2,1),col=c("red","blue"),text.col=c("red","blue"),bty="n")

plot(murder.rate ~ college, data = crime50, las=1)
abline(reg2,font=2,col="blue")

# Slide 29:

plot(residuals(reg2) ~ college, data=crime50)
abline(h = 0, lty = 2,col="red")

# Slide 30:
#Using the car package (Companion to Applied Regression)
# install.packages("car")
library(car)
residualPlots(reg2)

# Slide 31:

# curvature college:
reg3<-update(reg2,~.+I(college^2))
drop1(reg3,test="F")

# curvature fitted values (Tukeys test):
reg3<-update(reg2,~.+I(fitted(reg2)^2))
drop1(reg3,test="F")

# Slide 34:

set.seed(1238)
ks.test(rnorm(100),"pnorm")
# p=0.82

ks.test(abs(rnorm(100)),"pexp")
# p=0.17

# Slide 35:

ks.test(rnorm(100)+0.008*rexp(100),"pnorm")
# p=0.34 

# Slide 36:

ks.test(rnorm(100000)+0.008*rexp(100000),"pnorm")
# p=0.0003 significant

# Slide 37:

qt(0.025,df=48)

# Slide 38:

xval <- seq(from=15, to=35, length.out=500)
newData <- data.frame(college=xval)
Pred.ci <- predict(reg2, newdata=newData,
                   interval="confidence",
                   level=.95)

plot(murder.rate ~ college, data = crime50, pch = 20, las = 1)
lines(xval, Pred.ci[, "fit"], lwd=2)  ## or use: abline(reg2)
lines(xval, Pred.ci[, "lwr"], lty=2, col="red", lwd=2)
lines(xval, Pred.ci[, "upr"], lty=2, col="red", lwd=2)


# Slide 40:

# redoing for wider range:
plot(murder.rate ~ college, data = crime50, pch = 20, las = 1, 
     ylim=c(-2,14),xlab="College education rate",ylab="murder rate")
lines(xval, Pred.ci[, "fit"], lwd=2)
lines(xval, Pred.ci[, "lwr"], lty=2, col="red", lwd=2)
lines(xval, Pred.ci[, "upr"], lty=2, col="red", lwd=2)

## Prediction interval for a new observation:
Pred.pi <- predict(reg2, newdata=newData,
                   interval="prediction")

## Add prediction intervals to plot:
lines(xval, Pred.pi[, "lwr"], lty=2,
      col="blue", lwd=2)
lines(xval, Pred.pi[, "upr"], lty=2,
      col="blue", lwd=2)
legend("topright",c("Confidence Interval","Prediction Interval"),
       lty=rep(2,1),lwd=rep(2,2),col=c("red","blue"),bty="n")


# Slide 45: 

#Nice table with p-values
tab <- cbind(coef(summary(reg2))[ , 1:2], "Lower" = confint(reg2)[ , 1],
             "Upper" = confint(reg2)[ , 2])
data.frame(round(tab, 2),
           "p-value" = format.pval(coef(summary(reg2))[ , 4], digits = 3, eps = 1e-3))


# Slide 47:


janka <- read.table("Data/janka.txt", header=TRUE, quote="\"")
names(janka) <- c("Density", "Hardness")
summary(janka)

reg3 <- lm(Hardness ~ Density, data = janka)
plot(Hardness ~ Density, data = janka, pch = 20, las = 1)
abline(reg3)


# Slide 48:

par(mfrow=c(2, 2))
plot(reg3, which=1:4)
par(mfrow=c(1, 1))

plot(residuals(reg3) ~ Density, data = janka, 
       ylab="residuals")
abline(h = 0,col="red",lty=2)


# Slide 49:
#Using the car package
residualPlots(reg3)



# Slide 51:

janka$LogHard <- log(janka$Hardness)
reg4 <- lm(LogHard ~ Density, data = janka)
plot(reg4, which=1)

# Slide 52:

reg5 <- lm(LogHard ~ Density + I(Density^2), data = janka)
plot(residuals(reg5) ~ Density, data = janka, 
     ylab="Residuals")
abline(h = 0,lty=2,col="red")


# Slide 53:

par(mfrow=c(2, 2))
plot(reg5, which=1:4)
par(mfrow=c(1, 1))


# Slide 54:

residualPlots(reg5)



# Slide 55:
summary(reg5)
drop1(reg5,test="F")


# Slide 58:

xval <- seq(from=25, to=70, length.out=500)
newData <- data.frame(Density=xval)
Pred.ci <- predict(reg5, newdata=newData,
                   interval="confidence",
                   level=.95)

plot(Hardness ~ Density, data = janka, pch = 20, las = 1)
lines(xval, exp(Pred.ci[, "fit"]), lwd=2)  
lines(xval, exp(Pred.ci[, "lwr"]), lty=2, col="red", lwd=2)
lines(xval, exp(Pred.ci[, "upr"]), lty=2, col="red", lwd=2)


# Slide 59:
my.se.fit<-predict(reg5, newdata=newData,
                   interval="confidence",
                   level=.95,se.fit=T)$se.fit

summary(exp(my.se.fit^2/2))

