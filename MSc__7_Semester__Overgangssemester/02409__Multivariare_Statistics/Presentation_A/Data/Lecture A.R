
# Slide 15:
Challenger <- data.frame(flt = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23),
                        temp = c(66,70,69,68,67,72,73,70,57,63,70,78,67,53,67,75,70,81,76,79,75,76,58),
                          td = c(0,1,0,0,0,0,0,0,1,1,1,0,0,1,0,0,0,0,0,0,1,0,1))
Challenger <- Challenger[order(Challenger$td),]; row.names(Challenger)<-1:23
# OR, more compact: 

Challenger<-read.csv2(”Data/Challenger.csv”)
Challenger <- Challenger[order(Challenger$td),]; row.names(Challenger)<-1:23
head(Challenger)

t.test(Challenger$temp[Challenger$td == 0], Challenger$temp[Challenger$td == 1])
t.test(Challenger$temp[Challenger$td == 0], Challenger$temp[Challenger$td == 1], var.eq = TRUE)

# OR, more compact: 
# (formula form does not support var.equal=T);

with(Challenger, t.test(temp~td))
with(Challenger, t.test(temp[td==0],temp[td==1],var.eq=T))

# just the p-value:
with(Challenger, t.test(temp~td))$p.value
with(Challenger, t.test(temp[td==0],temp[td==1],var.eq=T))$p.value

# alternative, using the bracket operator [:
t.test(Challenger$temp[Challenger$td == 0], Challenger$temp[Challenger$td == 1])
t.test(Challenger$temp[Challenger$td == 0], Challenger$temp[Challenger$td == 1], 
var.eq = TRUE)

# Slide 16:

Challenger

#Slide 17:
# Histograms:

layout.matrix<-matrix(c(1,1,1,2,2,2,3,3),ncol=1)
layout(layout.matrix)
with(Challenger,
       hist(temp[td==0],xlim=c(30,100),probability=T,xlab='Temperature (F)',
       main ="Temperature, no distress",col="blue",cex.main=2))
with(Challenger,
       curve(dnorm(x,mean=mean(temp[td==0]),sd=sd(temp[td==0])),30,100,
             col="red",lwd=2,add=T))
with(Challenger,
       lines(density(temp[td==0])$x,density(temp[td==0])$y,type="l",
             lwd=2,col="green"))
legend(30,0.09,c("Approximating normal\ndensity","Kernel density"),lty=1,
       col=c("red","green"),bty="n",cex=1.4,text.font=2,lwd=2)

with(Challenger,
       hist(temp[td==1],xlim=c(30,100),probability=T,xlab='Temperature (F)',
       main ="Temperature, distress",col="turquoise",cex.main=2))
with(Challenger,
       curve(dnorm(x,mean=mean(temp[td==1]),sd=sd(temp[td==1])),30,100,
             col="red",lwd=2,add=T))
with(Challenger,
       lines(density(temp[td==1])$x,density(temp[td==1])$y,type="l",
             lwd=2,col="green"))
legend(30,0.05,c("Approximating normal\ndensity","Kernel density"),lty=1,
       col=c("red","green"),bty="n",cex=1.4,text.font=2,lwd=2)

with(Challenger, boxplot(temp~1-td,horizontal=T,at=c(1,0),col=c("blue","turquoise"),
     xlab="Temperature (F)",ylab="Thermal distress",ylim=c(30,100)))
legend(30,1.5,c("No distress","Distress"),col=c("blue","turquoise"),bty="n",
       pch=15,cex=1.4,text.font=2)



#Slide 19:

# means and summaries:

with(Challenger,by(temp,td,summary))

# t-test:

my.t.test<-data.frame(td=0:1)
my.t.test$n<-with(Challenger,tapply(temp,td,length))
my.t.test$mean<-with(Challenger,tapply(temp,td,mean))
my.t.test$sem<-with(Challenger,tapply(temp,td,sd))/
               sqrt(my.t.test$n)
my.t.test$lower<-my.t.test$mean-qt(0.95,df=my.t.test$n-1)*my.t.test$sem
my.t.test$upper<-my.t.test$mean+qt(0.95,df=my.t.test$n-1)*my.t.test$sem
my.t.test$t.equal.var<-c(NA,with(Challenger, 
                         t.test(temp[td==0],temp[td==1],var.eq=T))$stat)
my.t.test$t<-c(NA,with(Challenger, 
               t.test(temp[td==0],temp[td==1]))$stat)
my.t.test$p.equal.var<-c(NA,with(Challenger, 
                         t.test(temp[td==0],temp[td==1],var.eq=T))$p.value)
my.t.test$p<-c(NA, with(Challenger, 
               t.test(temp[td==0],temp[td==1]))$p.value)

my.t.test[,3:8]<-round(my.t.test[,3:8],digits=2)
my.t.test[,9]<-format.pval(my.t.test[,9],eps=0.0001,digits=1)
my.t.test[,10]<-format.pval(my.t.test[,10],eps=0.0001,digits=1)
my.t.test


# How does the variance look?

with(Challenger, tapply(temp,td,var))

library(car)
leveneTest(temp~as.factor(td),data=Challenger)

# borderlining significance; doubtful if support for equal variances. 


# Slide 20:

2*pt(-3.10,df=21); 2*pt(-2.54,df=7.9)


curve(dt(x,df=7.9),-3,3,lty=0,ylab='Density',xlab='test statistic',xaxt="n")
axis(1)
polygon(c(-4,-4,-4+(1:100/100)*(4-2.54),-2.54),
        c(0,dt(-4+(0:100/100)*(4-2.54),df=7.9),0),col="red",lty=0)
polygon(c(4,4,4+(1:100/100)*(-4+2.54),2.54),
        c(0,dt(4+(0:100/100)*(-4+2.54),df=7.9),0),col="red",lty=0)
curve(dt(x,df=7.9),-4,4,add=T)
lines(c(-4,4),rep(0,2),type="l")
lines(rep(-2.54,2),c(par("usr")[3],dt(-2.54,df=7.9)),lty=3,lwd=2)
lines(rep(2.54,2),c(par("usr")[3],dt(2.54,df=7.9)),lty=3,lwd=2)
lines(rep(2.54,2),c(par("usr")[3],dt(2.54,df=7.9)),lty=3,lwd=2)
axis(1,labels=c("-t","t"),at=c(-2.54,2.54),cex=0.3,col.axis="red")
text(1.5,0.25,"qt(x,df=7.9)",font=2)

# Slide 23:

logit<-function(p){log(p/(1-p))}
logistic<-function(y){exp(y)/(1+exp(y))}

par(mfrow=c(1,2))
curve(logit,0.001,0.999,xlab='p',ylab='logit(p)', main="The logit function")
curve(logistic,-6,6,xlab='y',ylab='logistic(y)',main=" The logistic curve")
par(mfrow=c(1,1))


# Slide 24:

model <- glm(td ~ temp, family=binomial(link='logit'), data=Challenger)
summary(model)
drop1(model,test="Chisq")

# Slide 25:
newdat <- data.frame(temp=seq(min(Challenger$temp), 
                              max(Challenger$temp),length=100))
newdat$fit = predict(model, newdata=newdat)
newdat$se<-predict(model, newdata=newdat,se.fit=T)$se.fit
plot(td ~ temp, data=Challenger, col="red",xlab="Temperature (F)",
     ylab="P(thermal distress)")
lines(logistic(fit)~ temp, data=newdat, col="green", lwd=2)
lines(logistic(fit+1.96*se)~ temp, data=newdat, col="red", lty=2,lwd=2)
lines(logistic(fit-1.96*se)~ temp, data=newdat, col="red", lty=2,lwd=2)

# Slide 26:
# number of 0-rings with Thermal distress:

p.temp<-logistic(newdat$fit)
p.temp.lower<-logistic(newdat$fit-1.96*newdat$se)
p.temp.upper<-logistic(newdat$fit+1.96*newdat$se)
p.star<-1-(1-p.temp)^(1/6)
p.star.lower<-1-(1-p.temp.lower)^(1/6)
p.star.upper<-1-(1-p.temp.upper)^(1/6)

plot(rep(newdat$temp,2),6*c(p.star.lower,p.star.upper), pch="",
     xlab="Temperature",ylab="0-rings with td")
lines(6*p.star~ temp, data=newdat, col="green", lwd=2)
lines(6*p.star.lower~ temp, data=newdat, col="red",lty=2, lwd=2)
lines(6*p.star.upper~ temp, data=newdat, col="red",lty=2, lwd=2)

# prediction at 29F:

newdat <- data.frame(temp=29)
newdat$fit = predict(model, newdata=newdat)
newdat$se<-predict(model, newdata=newdat,se.fit=T)$se.fit
logistic(newdat$fit)
# [1] 0.9997541



p.temp<-logistic(newdat$fit)
6*(1-(1-p.temp)^(1/6))
# [1] 4.498164

# Uncertainty; not on slide
p.temp.lower<-logistic(newdat$fit-1.96*newdat$se)
p.temp.upper<-logistic(newdat$fit+1.96*newdat$se)
p.star<-1-(1-p.temp)^(1/6)
# lower limit:
6*(1-(1-p.temp.lower)^(1/6))
# upper limit:
6*(1-(1-p.temp.upper)^(1/6))

# large uncertainty


# Prediction at Redline (40 degrees) for engineers (not on slides):
 

Prediction at redline (40F):
newdat <- data.frame(temp=40)
newdat$fit = predict(model, newdata=newdat)
newdat$se<-predict(model, newdata=newdat,se.fit=T)$se.fit
logistic(newdat$fit)
[1] 0.9968475
 
p.temp<-logistic(newdat$fit)
6*(1-(1-p.temp)^(1/6))
[1] 3.70247

# still rather high


# Slide 28:

# example of a 3D R graph:

x <- seq(-6.5, 6.5, length.out = 100)
y <- seq(-6.5, 6.5, length.out = 100)
z <- outer(x, y, function(a, b) exp(-((a^2+b^2)/2)/(2*pi)))

# some fun to create nice colors:
nrz <- nrow(z)
ncz <- ncol(z)
# Create a function interpolating colors in the range of specified colors
jet.colors <- colorRampPalette( c("blue", "green") )
# Generate the desired number of colors from this palette
nbcol <- 100
color <- jet.colors(nbcol)
# Compute the z-value at the facet centres
zfacet <- z[-1, -1] + z[-1, -ncz] + z[-nrz, -1] + z[-nrz, -ncz]
# Recode facet z-values into color indices
facetcol <- cut(zfacet, nbcol)

# Actual plot:
persp(x, y, z, col = color[facetcol], phi = 40, theta = 50,box=F)


# Slide 29:
organ<-read.csv2("Data/organ.csv")

par(mfrow=c(1,2))
hist(organ$weight,xlab="Weight",ylab="Percent",probability=T,
     main="Distribution of weight",col="lightblue1",axes=F)
axis(1)
axis(2,labels=c(0,5,10,15),at=c(0,5,10,15)/100)
box()
curve(dnorm(x,mean=mean(organ$weight),sd=sd(organ$weight)),5,55,
      col="blue", lwd=2,add=T)
plot(sort(organ$weight),1:length(organ$weight)/length(organ$weight),
     type="s",xlab="Weight",ylab="Cumulative percent",axes=F,
     main="Cumulative distribution of weight")
axis(1)
axis(2,labels=c(0,20,40,60,80,100),at=c(0,20,40,60,80,100)/100)
box()
curve(pnorm(x,mean=mean(organ$weight),sd=sd(organ$weight)),5,55,
      col="blue", lwd=2,add=T)
par(mfrow=c(1,1))

# Slide 31:

par(mfrow=c(2,1))

plot(0:31-10,c(0,rep(0.4,31)),pch='',xlab=' ',ylab='Density')
lines(0:3100/100-10,dnorm(0:3100/100-10,mean=0,sd=1),col="blue",lwd=3)
lines(0:3100/100-10,dnorm(0:3100/100-10,mean=0,sd=4),col="red",lwd=3)
lines(0:3100/100-10,dnorm(0:3100/100-10,mean=6,sd=1),col="green4",lwd=3)


plot(0:31-10,c(0,rep(0.4,31)),pch='',xlab=' ',ylab='Density')
polygon(c(0:3100/100-10,-10),c(dnorm(0:3100/100-10,mean=6,sd=1),0),col="green4")
polygon(c(0:3100/100-10,-10),c(dnorm(0:3100/100-10,mean=0,sd=4),0),col="red")
polygon(c(0:3100/100-10,-10),c(dnorm(0:3100/100-10,mean=0,sd=1),0),col="blue")

par(mfrow=c(1,1))


# Slide 32:

head(organ,n=20)

# Slide 33:

par(mfrow=c(2,2))

hist(organ$weight,xlab="Weight",ylab="Density",probability=T,
     main="Distribution of weight",col="lightblue1")
curve(dnorm(x,mean=mean(organ$weight),sd=sd(organ$weight)),5,55,
      col="blue", lwd=2,n=500,add=T)

hist(organ$organ,xlab="Organ",ylab="Density",probability=T,
     main="Distribution of organ",col="lightblue1")
curve(dnorm(x,mean=mean(organ$organ),sd=sd(organ$organ)),0,4,
      col="blue", lwd=2,n=500,add=T)

plot(sort(organ$weight),1:length(organ$weight)/length(organ$weight),
     type="s",xlab="Weight",ylab="Cumulative density",
     main="Cumulative distribution of weight")
curve(pnorm(x,mean=mean(organ$weight),sd=sd(organ$weight)),5,55,
      col="blue",n=500,lwd=2,add=T)

plot(sort(organ$organ),1:length(organ$organ)/length(organ$organ),
     type="s",xlab="Organ",ylab="Cumulative density",
     main="Cumulative distribution of organ")
curve(pnorm(x,mean=mean(organ$organ),sd=sd(organ$organ)),0,4,
      col="blue",n=500,lwd=2,add=T)

par(mfrow=c(1,1))

# Slide 34:
library(car)
scatterplotMatrix(organ,diagonal=list(method ="histogram"),ellipse=T,
                  regLine=F,smooth=F)


#Slide 35:

mu.x<-mean(organ$weight)
mu.y<-mean(organ$organ)
Sigma<-cov(organ)
Sigmainv<-solve(Sigma)

x<-(20*16):(20*30)/20
y<-(2*7):(20*3)/20
z<-matrix(nrow=length(x),ncol=length(y))
for(i in 1:length(x)){
  for(j in 1:length(y)){
    z[i,j]<-(1/(2*pi*det(Sigma)))*
            exp(-(t(c(x[i]-mu.x,y[j]-mu.y))%*%Sigmainv%*%(c(x[i]-mu.x,y[j]-mu.y))/2))
    }
  }

persp(x,y,z,theta=40,phi=40,xlab="Weight",ylab="Organ",zlab="Density",col="green",
      ticktype="detailed")

# now with the multivariate distribution evaluated directly:

library(mvtnorm)
x<-(20*16):(20*30)/20
y<-(2*7):(20*3)/20
z<-matrix(nrow=length(x),ncol=length(y))
for(i in 1:length(x)){
  for(j in 1:length(y)){
    z[i,j]<-pmvnorm(lower=c(-Inf,-Inf),upper=c(x[i],y[j]),mean=c(mu.x,mu.y),sigma=Sigma)[1]
    }
  }

persp(x,y,z,theta=40,phi=40,xlab="Weight",ylab="Organ",zlab="Probability",col="green",
      ticktype="detailed")

#Slide 36

library(MASS)
density.3d<-kde2d(organ$weight,organ$organ)

persp(density.3d,theta=40,phi=40,xlab="Weight",ylab="Organ",zlab="Density",col="green",
      ticktype="detailed")

library(ggplot2)
library(ggExtra)
p <- ggplot(organ, aes(weight, organ)) + geom_point() + theme_classic()
ggExtra::ggMarginal(p, type = "histogram")


# Slide 39:

par(mfrow=c(3,3))
my.rho<-c(0,0.25,0.5,0.75,0.90,0.95,0.99)
x<-(-100:100)/10
y<-x
z<-matrix(nrow=length(x),ncol=length(y))
for(i in 1:7){
  Sigma<-matrix(c(9,9*my.rho[i],9*my.rho[i],9),nrow=2)
  for(j in 1:length(x)){
   for(k in 1:length(y)){
     z[j,k]<-dmvnorm(c(x[j],y[k]),sigma=Sigma)
     }
   }
# exchange title;
   persp(x,y,z,theta=-40,phi=40,xlab="x",ylab="y",zlab="Density",col="green",shade=.4,ltheta=0,
        ticktype="detailed",main=expression(paste("Bivariate Gaussian Density, ",rho," = ",0.99)))
 }


# Slide 40:

for(i in 1:7){
  Sigma<-matrix(c(9,9*my.rho[i],9*my.rho[i],9),nrow=2)
  for(j in 1:length(x)){
   for(k in 1:length(y)){
     z[j,k]<-min(dmvnorm(c(x[j],y[k]),sigma=Sigma),0.01)
     }
   }
# exchange title;
   persp(x,y,z,theta=-40,phi=40,xlab="x",ylab="y",zlab="Density",col="green",shade=.4,ltheta=0,
        ticktype="detailed",main=expression(paste("Bivariate Gaussian Density, ",rho," = ",0.99)))
 }



# slide 41:

var(organ)
cor(organ)

round(var(organ),digits=2)
round(cor(organ),digits=2)

#slide 42:
my.corr<-cor(organ)[1,2]
my.T<-sqrt(147)*my.corr/sqrt(1-my.corr^2)
my.T

# p-value:
2*pt(-my.T,df=147)


# Slide 43:

cor.test(organ[,1],organ[,2])

# Slide 49:


X1<-c(1,3,1)
X2<-c(0,4,5)
X3<-c(2,5,9)
X<-cbind(X1,X2,X3)
Xbar<-(X1+X2+X3)/3



#Slide 50:
n<-3
my.S<-(X1%*%t(X1)+X2%*%t(X2)+X3%*%t(X3))/(n-1)-(n/(n-1))*Xbar%*%t(Xbar)-
      (X1-Xbar+X2-Xbar+X3-Xbar)/(n-1)
my.S
# faster:
var(t(X))







# Slide 53:

my.T<-diag(1/sqrt(diag(my.S)))
my.T

my.T%*%my.S%*%my.T

# faster:

cor(t(X))



# Slide 59:
p <- ggplot(organ, aes(weight, organ)) + geom_point(col="red",cex=2) + theme_classic()
ggExtra::ggMarginal(p, type = "histogram",fill="red")



# slide 61:
round(data.frame(n=sapply(organ,length),
           Mean=sapply(organ,mean),
           SD=sapply(organ,sd),
           Min=sapply(organ,min),
           Max=sapply(organ,mean)),digits=4)

cov(organ)

cor(organ)
cor.test(organ$weight,organ$organ)






with(organ,plot(weight,organ,pch=19,col="red",cex=2,xlab="Weight",ylab="Organ"))

library(car)
my.index<-c(0.25,0.50,0.75,0.9)
for(i in 1:4){
with(organ,dataEllipse(weight,organ,level=my.index[i],add=T,plot.points=F,col=i+1))
  }

legend("topleft", c("Weight and Organ","Center","25% contour ellipse",
                    "50% contour ellipse","75% contour ellipse","90% contour ellipse"),
       pch=c(19,19,rep(NA,4)),lty=c(NA,NA,rep(1,4)),col=c("red",5,2:5),
       pt.cex=c(2,1.2,rep(NA,4)),lwd=c(NA,NA,rep(2,4)),
       bty="n")

# slide 63:

eigen(var(organ))$values
my.eigenvectors<--eigen(var(organ))$vectors
my.center<-colMeans(organ)
arrows(my.center[1],my.center[2],my.center[1]+eigen(var(organ))$values[1]*my.eigenvectors[1,1],
       my.center[2]+eigen(var(organ))$values[1]*my.eigenvectors[2,1],col="black",lwd=2,
       angle=10,length=0.15)
arrows(my.center[1],my.center[2],my.center[1]+eigen(var(organ))$values[2]*my.eigenvectors[1,2],
       my.center[2]+eigen(var(organ))$values[2]*my.eigenvectors[2,2],col="black",lwd=2,
       angle=10,length=0.1)

 # note that the eigenvectors do not appear perpendicular; it is because the axes
# are scaled different. 



# Slide 63
library(plot3D)
x_c <- cut(organ$weight,10)
y_c <- cut(organ$organ,10)
z <- table(x_c, y_c)

hist3D(z=z, border="black",xlab="Weight",ylab="Organ",zlab="Count",ticktype="detailed")
image2D(z=z,border="black",lighting=T,xlab="Weight",ylab="Organ")

density.3d<-kde2d(organ$weight,organ$organ)
persp(density.3d,theta=40,phi=40,xlab="Weight",ylab="Organ",zlab="Density",col="grey",
      ticktype="detailed")

# Slide 64:


mu.x<-mean(organ$weight)
mu.y<-mean(organ$organ)
Sigma<-cov(organ)
Sigmainv<-solve(Sigma)


set.seed(5904)
organ.sim<-as.data.frame(rmvnorm(149, mean = c(mu.x,mu.y), sigma = Sigma))
names(organ.sim)<-c("weight","organ")

x_c <- cut(organ.sim$weight,10)
y_c <- cut(organ.sim$organ,10)
z <- table(x_c, y_c)

hist3D(z=z, border="black",xlab="Weight",ylab="Organ",zlab="Count",ticktype="detailed")
image2D(z=z,border="black",lighting=T,xlab="Weight",ylab="Organ")

density.3d<-kde2d(organ.sim$weight,organ.sim$organ)
persp(density.3d,theta=40,phi=40,xlab="Weight",ylab="Organ",zlab="Density",col="grey",
      ticktype="detailed")

# Slide 65

heiwei<-read.table("Data/heiwei.txt",header=T,sep="\t",dec=".")

summary(heiwei)
head(heiwei)

# converting to cm and kg:
heiwei[,2]<-2.54*heiwei[,2]
heiwei[,3]<-0.453592*heiwei[,3]
heiwei<-heiwei[,2:3]
names(heiwei)<-c("height","weight")
head(heiwei)


p <- ggplot(heiwei, aes(height,weight)) + geom_point(col="red") + theme_classic()
ggExtra::ggMarginal(p, type = "histogram",fill="lightblue")

par(mfrow=c(2,1))
hist(heiwei$weight,col="lightblue",probability=T,xlab="Weight (kg)")
with(heiwei,curve(dnorm(x,mean=mean(weight),sd=sd(weight)),min(weight),max(weight),
     lwd=2,col="red",add=T))
box()
hist(heiwei$height,col="lightblue",probability=T,xlab="Height (cm)")
with(heiwei,curve(dnorm(x,mean=mean(height),sd=sd(height)),min(height),max(height),
     lwd=2,col="red",add=T))
box()
par(mfrow=c(1,1))


# Slide 67:
par(mfrow=c(2,1))
qqnorm(heiwei$weight,main="QQ plot, weight")
lines((-5):5,mean(heiwei$weight)+sd(heiwei$weight)*((-5):5),type="l",lwd=2,col="red")
qqnorm(heiwei$height,main="QQ plot, height")
lines((-5):5,mean(heiwei$height)+sd(heiwei$height)*((-5):5),type="l",lwd=2,col="red")
par(mfrow=c(1,1))


# Slide 68:

plot(heiwei$height,heiwei$weight,xlab="Height",ylab="Weight",col="lightblue",pch=".")


x_c <- cut(heiwei$height,10)
y_c <- cut(heiwei$weight,10)
z <- table(x_c, y_c)

hist3D(z=z, border="black",xlab="Height",ylab="Weight",zlab="Count",ticktype="detailed")
image2D(z=z,border="black",lighting=T,xlab="Height",ylab="Weight")

density.3d<-kde2d(heiwei$height,heiwei$weight)
persp(density.3d,theta=40,phi=40,xlab="Height",ylab="Weight",zlab="Density",col="lightblue",
      ticktype="detailed")

#Slide 69:

my.rho<-0.5
x<-(-100:100)/10
y<-x
z<-matrix(nrow=length(x),ncol=length(y))
  Sigma<-matrix(c(9,9*my.rho,9*my.rho,9),nrow=2)
  for(j in 1:length(x)){
   for(k in 1:length(y)){
     z[j,k]<-dmvnorm(c(x[j],y[k]),sigma=Sigma)*(x[j]>-3)
     }
   }
   persp(x,y,z,theta=-60,phi=40,xlab="x",ylab="y",zlab="Density",col="green",shade=.4,ltheta=0,
        ticktype="detailed",main=expression(paste("Bivariate Gaussian Density, ",rho," =0.5")))


# Slide 73:

analysis<-lm(weight~height,data=heiwei)


par(mfrow=c(2,1))
hist(analysis$res,col="lightblue",probability=T,xlab="Weight (kg)",
main="Histogram of Estimated Residuals \n Regressing Weight on Height" )
curve(dnorm(x,mean=mean(analysis$res),sd=sd(analysis$res)),min(analysis$res),max(analysis$res),
     lwd=2,col="red",add=T)
box()
hist(heiwei$height,col="lightblue",probability=T,xlab="Height (cm)",main="Histogram of Height")
with(heiwei,curve(dnorm(x,mean=mean(height),sd=sd(height)),min(height),max(height),
     lwd=2,col="red",add=T))
box()
par(mfrow=c(1,1))


par(mfrow=c(2,1))
qqnorm(analysis$res,main="QQ plot, Estimated Residuals \n Regressing Weight on Height")
lines((-5):5,sd(analysis$res)*((-5):5),type="l",lwd=2,col="red")
qqnorm(heiwei$height,main="QQ plot of Height")
lines((-5):5,mean(heiwei$height)+sd(heiwei$height)*((-5):5),type="l",lwd=2,col="red")
par(mfrow=c(1,1))


