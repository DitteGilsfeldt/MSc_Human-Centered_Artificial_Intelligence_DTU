
# Slide 8:
skulls<-read.csv2("Data/skulls2.csv")[,-1]
skulls$epoch<-as.factor(paste(rep(1:5,each=30),skulls$epoch,sep="-"))

summary(skulls)

# Slide 9:
par(mfrow=c(2,2))
for(i in 1:4){boxplot(skulls[,i+1]~skulls[,1],col=2:5,main=names(skulls)[i+1])}

############################
# Detailed model control, not on slides:

model.11<-lm(MB~BH+BL+NH,data=skulls[skulls$epoch==levels(skulls$epoch)[1],])
model.12<-lm(BH~BL+NH,data=skulls[skulls$epoch==levels(skulls$epoch)[1],])
model.13<-lm(BL~NH,data=skulls[skulls$epoch==levels(skulls$epoch)[1],])
model.14<-lm(NH~1,data=skulls[skulls$epoch==levels(skulls$epoch)[1],])

plot(model.11,which=1:4, main="MB Given BH, BL, NH, c4000BC")
plot(model.12,which=1:4, main="BH given BL, NH, c4000BC")
plot(model.13,which=1:4, main="BL given NH, c4000BC")
plot(model.14,which=1:4, main="NH, c4000BC")


model.21<-lm(MB~BH+BL+NH,data=skulls[skulls$epoch==levels(skulls$epoch)[2],])
model.22<-lm(BH~BL+NH,data=skulls[skulls$epoch==levels(skulls$epoch)[2],])
model.23<-lm(BL~NH,data=skulls[skulls$epoch==levels(skulls$epoch)[2],])
model.24<-lm(NH~1,data=skulls[skulls$epoch==levels(skulls$epoch)[2],])

plot(model.21,which=1:4, main="MB Given BH, BL, NH, c3300BC")
plot(model.22,which=1:4, main="BH given BL, NH, c3300BC")
plot(model.23,which=1:4, main="BL given NH, c3300BC")
plot(model.24,which=1:4, main="NH, c3300BC")


model.31<-lm(MB~BH+BL+NH,data=skulls[skulls$epoch==levels(skulls$epoch)[3],])
model.32<-lm(BH~BL+NH,data=skulls[skulls$epoch==levels(skulls$epoch)[3],])
model.33<-lm(BL~NH,data=skulls[skulls$epoch==levels(skulls$epoch)[3],])
model.34<-lm(NH~1,data=skulls[skulls$epoch==levels(skulls$epoch)[3],])

plot(model.31,which=1:4, main="MB Given BH, BL, NH, c1850BC")
plot(model.32,which=1:4, main="BH given BL, NH, c1850BC")
plot(model.33,which=1:4, main="BL given NH, c1850BC")
plot(model.34,which=1:4, main="NH, c1850BC")

model.41<-lm(MB~BH+BL+NH,data=skulls[skulls$epoch==levels(skulls$epoch)[4],])
model.42<-lm(BH~BL+NH,data=skulls[skulls$epoch==levels(skulls$epoch)[4],])
model.43<-lm(BL~NH,data=skulls[skulls$epoch==levels(skulls$epoch)[4],])
model.44<-lm(NH~1,data=skulls[skulls$epoch==levels(skulls$epoch)[4],])

plot(model.41,which=1:4, main="MB Given BH, BL, NH, c200BC")
plot(model.42,which=1:4, main="BH given BL, NH, c200BC")
plot(model.43,which=1:4, main="BL given NH, c200BC")
plot(model.44,which=1:4, main="NH, c200BC")


model.51<-lm(MB~BH+BL+NH,data=skulls[skulls$epoch==levels(skulls$epoch)[5],])
model.52<-lm(BH~BL+NH,data=skulls[skulls$epoch==levels(skulls$epoch)[5],])
model.53<-lm(BL~NH,data=skulls[skulls$epoch==levels(skulls$epoch)[5],])
model.54<-lm(NH~1,data=skulls[skulls$epoch==levels(skulls$epoch)[5],])

plot(model.51,which=1:4, main="MB Given BH, BL, NH, cAD150")
plot(model.52,which=1:4, main="BH given BL, NH, cAD150")
plot(model.53,which=1:4, main="BL given NH, cAD150")
plot(model.54,which=1:4, main="NH, cAD150")

###################################


# Slide 12:

# calculating quantities for Bartlett's test
n<-150
n_i<-rep(30,5)
p<-4
k<-5

my.W<-list()
for(i in 1:5){
  temp<-residuals(manova(cbind(MB,BH,BL,NH)~1,
          data=skulls[skulls$epoch==levels(skulls$epoch)[i],]))
  my.W[[i]]<-t(temp)%*%temp
  }
W<-my.W[[1]]+my.W[[2]]+my.W[[3]]+my.W[[4]]+my.W[[5]]

# Slide 13:

logL<-0
for(i in 1:5){logL<-logL+((n_i[i]-1)/2)*log(det(my.W[[i]]))}
logL<-logL-((n-k)/2)*log(det(W))
logL<-logL+(p*(n-k)/2)*log(n-k)
logL<-logL-sum((p*(n_i-1)/2)*log(n_i-1))

f<-(1/2)*(k-1)*p*(p+1)
rho<-1-(sum(1/n_i)-1/n)*(2*p^2+3*p-1)/(6*(p+1)*(k-1))
omega2<-(1/48)*p*(p+1)*((p-1)*(p+2)*(sum(1/n_i^2)-1/n^2)-6*(k-1)*(1-rho)^2)

# test statistic:
(z<--2*rho*logL)

# p-value:
(1-(pchisq(z,df=f)+omega2*(pchisq(z,df=f+4)-pchisq(z,df=f))))

# Slide 14:

# install.packages("heplots")
library("heplots")

boxM(skulls[,2:5],skulls[,1])



# Slide 16
model.01<-lm(MB~epoch+BH+BL+NH,data=skulls)
model.02<-lm(BH~epoch+BL+NH,data=skulls)
model.03<-lm(BL~epoch+NH,data=skulls)
model.04<-lm(NH~epoch,data=skulls)


par(mfrow=c(2,2))
plot(model.01,which=1:4, main="MB Given BH, BL, NH")
plot(model.02,which=1:4, main="BH given BL, NH")
plot(model.03,which=1:4, main="BL given NH")
plot(model.04,which=1:4, main="NH")

par(mfrow=c(1,1))


# Slide 18:
head(skulls)


# Slide 22:

analysis<-manova(cbind(MB,BH,BL,NH)~epoch,data=skulls)

summary(analysis,test="Wilks")

# Slide 23:

summary(analysis)
summary(analysis,test="Hotelling-Lawley")
summary(analysis,test="Roy")

"Pillai", "Wilks", "Hotelling-Lawley", "Roy")

# Slide 24:

(theta<-coef(manova(cbind(MB,BH,BL,NH)~epoch-1,data=skulls)))

R<-residuals(manova(cbind(MB,BH,BL,NH)~as.factor(epoch),data=skulls))
(Sigma<-(1/(150-5))*t(R)%*%R)

# Slide 25:

mh.dist<-numeric(5)
for(i in 1:5){
  mh.dist[i]<-(theta[i,]-theta[1,])%*%solve(Sigma)%*%t(t((theta[i,]-theta[1,])))
  }
mh.dist<-sqrt(mh.dist)
plot(c(0,700,2250,3800,4150),mh.dist,type="b",
     xlab="Time since epoch 1 (years)",
     ylab="Mahanalobis distance",col="red",lwd=2)

# Slide 26:

plotdata<-as.matrix(theta)-
rbind(theta[1,],theta[1,],theta[1,],theta[1,],theta[1,])
plotdata<-plotdata%*%diag(1/sqrt(diag(Sigma)))

plot(rep(c(0,700,2250,3800,4150),4),plotdata,pch='',     
     xlab="Time since epoch 1 (years)",
     ylab="Changes in SDs")
for(i in 1:4){
  lines(c(0,700,2250,3800,4150),plotdata[,i],type="b",col=i+1,lwd=2)
  }
legend("topleft",c("Maximum Breath",
                   "Basibregmatic Height",
                   "Basialiveolar Length",
                   "Nasal Length"),col=2:5,lty=1,lwd=2,bty="n")


# Slide 37:

yield<-read.table("Data/plant yield.txt",header=T)
yield$Type<-as.factor(yield$Type)
yield$Yield<-as.factor(yield$Yield)
summary(yield)

# Slide 39:
yield2<-data.frame(Type=rep(c("Marchigiana","Kayseri","Atlantic"),each=6))
yield2$Block<-rep(paste("B",1:6,sep=""),3)
yield2$Dry.matter<-c(t(yield[c(1,4,7),-(1:2)]))
yield2$Nitrogen<-c(t(yield[c(2,5,8),-(1:2)]))
yield2$Green.matter<-c(t(yield[c(3,6,9),-(1:2)]))
yield2

par(mfrow=c(3,1))
boxplot(Dry.matter~Type,col=2:4,data=yield2,main="Dry Matter")
boxplot(Nitrogen~Type,col=2:4,data=yield2,main="Nitrogen")
boxplot(Green.matter~Type,col=2:4,data=yield2,main="Green Matter")

boxplot(Dry.matter~Block,col=2:7,data=yield2,main="Dry Matter")
boxplot(Nitrogen~Block,col=2:7,data=yield2,main="Nitrogen")
boxplot(Green.matter~Block,col=2:7,data=yield2,main="Green Matter")

# Slide 40:
analysis.01<-lm(Dry.matter~Type+Block+Nitrogen+Green.matter,data=yield2)
analysis.02<-lm(Nitrogen~Type+Block+Green.matter,data=yield2)
analysis.03<-lm(Green.matter~Type+Block,data=yield2)

par(mfrow=c(2,2))
plot(analysis.01,which=1:4,main="Dry matter")
plot(analysis.02,which=1:4,main="Nitrogen")
plot(analysis.03,which=1:4,main="Green Matter")

# Slide 41:
summary(yield2)
yield2[13,]

# Slide 42:
analysis<-manova(cbind(Dry.matter,Nitrogen,Green.matter)~Type+Block,data=yield2)

summary(analysis,test="Wilks")


# Slide 43:
summary(analysis)$SS$Block
summary(analysis)$SS$Type
summary(analysis)$SS$Residuals

# Total variation
summary(analysis)$SS$Block+
summary(analysis)$SS$Type+
summary(analysis)$SS$Residuals

# Slide 44:

analysis.dm<-lm(Dry.matter~Type+Block,data=yield2)
analysis.ni<-lm(Nitrogen~Type+Block,data=yield2)
analysis.gm<-lm(Green.matter~Type+Block,data=yield2)

drop1(analysis.dm,test="F")
drop1(analysis.ni,test="F")
drop1(analysis.gm,test="F")


# Slide 46:
strength<-read.csv2("Data/Strength data.csv")
strength$program<-as.factor(strength$program)
summary(strength)

# Slide 47:

plot(rep(2*(1:7),57),unlist(strength[,-(1:2)]),pch="",xlab="Time (days)",
     ylab="Strength",las=1,main="Control")
for(i in 1:20){
  lines(2*(1:7),strength[i,-(1:2)],type="l",col=(2:4)[strength$program[i]])
  }
plot(rep(2*(1:7),57),unlist(strength[,-(1:2)]),pch="",xlab="Time (days)",
     ylab="Strength",las=1,main="RI")
for(i in 21:36){
  lines(2*(1:7),strength[i,-(1:2)],type="l",col=(2:4)[strength$program[i]])
  }


# Slide 48:
par(mfrow=c(1,2))
plot(rep(2*(1:7),57),unlist(strength[,-(1:2)]),pch="",xlab="Time (days)",
     ylab="Strength",las=1)
for(i in 1:57){
  lines(2*(1:7),strength[i,-(1:2)],type="l",col=(2:4)[strength$program[i]])
  }
legend("topleft",legend=c("Control","RI","WI"),col=2:4,lty=1,bty="n")

plot(rep(2*(1:7),57),unlist(strength[,-(1:2)]),pch="",xlab="Time (days)",
     ylab="Strength",las=1)
(mean.curves<-tapply(strength[,-(1:2)],strength$program,colMeans))
lines(2*(1:7),mean.curves$co,type="l",col=2)
lines(2*(1:7),mean.curves$ri,type="l",col=3)
lines(2*(1:7),mean.curves$wi,type="l",col=4)
legend("topleft",legend=c("Control","RI","WI"),col=2:4,lty=1,bty="n",cex=1.5)
par(mfrow=c(1,1))


# slide 53: 

strength2<-data.frame(subject=rep(1:dim(strength)[1],each=7))
strength2$program<-rep(strength$program,each=7)
strength2$time<-rep(2*(1:7),dim(strength)[1])
strength2$strength<-c(t(as.matrix(strength[,3:9])))
head(strength2)

# Slide 54:
library(nlme)
model1 <- lme(strength ~ time+I(time^2)+program,
              random = ~1 | subject, data = strength2,method="ML")

anova(model1)

# Slide 55:

model1 <- lme(strength ~ time+I(time^2)+program-1,
              random = ~1 | subject, data = strength2,method="REML")
summary(model1)


 3.097147^2/(3.097147^2 + 1.128686^2)


# Slide 56:

summary(model1)$tTable
(my.coef<-summary(model1)$tTable[,1])

plot(rep(2*(1:7),57),unlist(strength[,-(1:2)]),pch="",xlab="Time (days)",
     ylab="Strength",las=1,ylim=c(78,85))
lines(2*(1:7),mean.curves$co,type="l",col=2,lwd=3)
lines(2*(1:7),mean.curves$ri,type="l",col=3,lwd=3)
lines(2*(1:7),mean.curves$wi,type="l",col=4,lwd=3)
legend("topleft",legend=c("Control Data","RI Data","WI Data",
                          "Control fitted","RI fitted","WI fitted"),
col=rep(2:4,2),lwd=c(rep(3,3),rep(1,3)),lty=1,bty="n",cex=1.5)

index<-2*(100:700)/100
my.prediction1<- my.coef[3] + index*my.coef[1] + index^2*my.coef[2]
my.prediction2<- my.coef[4] + index*my.coef[1] + index^2*my.coef[2]
my.prediction3<- my.coef[5] + index*my.coef[1] + index^2*my.coef[2]
 
lines(index,my.prediction1,type="l",col=2)
lines(index,my.prediction2,type="l",col=3)
lines(index,my.prediction3,type="l",col=4)


par(mfrow=c(1,1))






# Slide 57:
par(mfrow=c(1,2))
qqnorm(residuals(model1),main="Residuals")
lines((-5):5,sd(residuals(model1))*(-5):5,lwd=2,col="red")

qqnorm(unlist(ranef(model1)),main="Random effects")
lines((-5):5,sd(unlist(ranef(model1)))*(-5):5,lwd=2,col="red")
par(mfrow=c(1,2))




# Slide 61:

model2<-lme(strength ~ time+I(time^2)+program,random=~1|subject,
            correlation=corGaus(form=~time|subject,nugget=T),method="ML",
            data=strength2)

anova(model2)


# Slide 62:

summary(model2)

#nu2:
2.721507^2

# tau2:
1.712461^2-0.1394383*1.712461^2

# sigma2:
0.1394383*1.712461^2

# total:

2.721507^2+2.523617+0.408906


# Slide 67:
gamma<-numeric(6)

gamma[1]<-var(c(strength$S2-strength$S1,strength$S3-strength$S2,
                strength$S4-strength$S3,strength$S5-strength$S4,
                strength$S6-strength$S5,strength$S7-strength$S6))/2

gamma[2]<-var(c(strength$S3-strength$S1,strength$S4-strength$S2,
                strength$S5-strength$S3,strength$S6-strength$S4,
                strength$S7-strength$S5))/2

gamma[3]<-var(c(strength$S4-strength$S1,strength$S4-strength$S2,
                strength$S6-strength$S3,strength$S7-strength$S4))/2

gamma[4]<-var(c(strength$S5-strength$S1,strength$S6-strength$S2,
                strength$S7-strength$S3))/2

gamma[5]<-var(c(strength$S6-strength$S1,strength$S7-strength$S1))/2

gamma[6]<-var(c(strength$S7-strength$S1))/2
gamma

plot(2*(1:6),gamma,col="blue",xlab="Distance",ylab="Semivariogram",las=1)

sigma2hat<-as.numeric(VarCorr(model1)[2,1])
lines(2*(1:6),rep(sigma2hat,6),col="blue")



# Slide 68:
plot(Variogram(model2,form=~time|subject,data=strength2))

