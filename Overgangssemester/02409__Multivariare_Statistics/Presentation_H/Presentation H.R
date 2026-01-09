
# Slide 17: 


synprod <- data.frame("Acontent"=c(1,0,0.5), 					  
                      "Bcontent"=c(0,1,0.5), 
		         "Qual"=c(90,30,75)) 

glmSyn <- lm(Qual ~ Acontent + Bcontent - 1, data=synprod) 

summary(glmSyn)


# Slide 18:

glmSynA <- lm(Qual ~ Acontent - 1, data=synprod) 
summary(glmSynA)


# Slide 25

X<-as.matrix(synprod[,1:2])
Y<-as.matrix(synprod[,3])
theta<-solve(t(X)%*%X)%*%t(X)%*%Y
res<-Y-X%*%theta

X1<-as.matrix(synprod[,1])
theta1<-solve(t(X1)%*%X1)%*%t(X1)%*%Y
res1<-Y-X1%*%theta1

#P_MY-P_HY:
res-res1

# ||Y-P_MY||^2:
sum(res^2)

Q_F<-sum((res-res1)^2)/sum(res^2)

1-pf(Q_F,1,1)


# Slide 26:

anova(glmSyn)

Anova(glmSyn)

Anova(glmSyn,type=3)


# Slide 27:

drop1(glmSyn,test="F")

temp <- lm(Qual ~ Bcontent + Acontent -1, data=synprod) 
anova(temp)




# Slide 29:

summary(glmSyn)$cov.unscaled
summary(glmSyn)$sigma^2*summary(glmSyn)$cov.unscaled


# Slide 34:

curve(dnorm,-4,4,axes=F,ylab='',ylim=c(0,0.45))
lines(c(-5,5),rep(0,2))
polygon(c((-400):(-196)/100,rev((-400):(-196)/100)),
        c(rep(0,205),dnorm(rev((-400):(-196)/100))),col="red")
polygon(c((400):(196)/100,rev((400):(196)/100)),
        c(rep(0,205),dnorm(rev((400):(196)/100))),col="red")
axis(1,labels=c("-1.96","0","1.96"),at=c(-1.96,0,1.96))


# Slide 35:


curve(dnorm,-4,4,axes=F,ylab='',ylim=c(0,0.45))
lines(c(-5,5),rep(0,2))
polygon(c((-400):(-196)/100,rev((-400):(-196)/100)),
        c(rep(0,205),dnorm(rev((-400):(-196)/100))),col="red")
polygon(c((400):(196)/100,rev((400):(196)/100)),
        c(rep(0,205),dnorm(rev((400):(196)/100))),col="red")
axis(1,labels=c(expression(paste("-1.96",sigma)),"0",
                expression(paste("1.96",sigma))),
     at=c(-1.96,0,1.96))

# Slide 36:


curve(dnorm,-4,4,axes=F,ylab='',ylim=c(0,0.45))
lines(c(-5,5),rep(0,2))
polygon(c((-400):(-196)/100,rev((-400):(-196)/100)),
        c(rep(0,205),dnorm(rev((-400):(-196)/100))),col="red")
polygon(c((400):(196)/100,rev((400):(196)/100)),
        c(rep(0,205),dnorm(rev((400):(196)/100))),col="red")
axis(1,labels=c(expression(paste(mu,"-1.96",sigma)),expression(mu),
                expression(paste(mu,"+1.96",sigma))),
     at=c(-1.96,0,1.96))


# Slide 37:

curve(dt(x,1),-15,15,axes=F,ylab='',ylim=c(0,0.05),
      xlab=expression(hat(theta)[B]))
lines(c(-20,20),rep(0,2))
polygon(c((-2000):(-1271)/100,rev((-2000):(-1271)/100)),
        c(rep(0,730),dt(rev((-2000):(-1271)/100),1)),col="red")
polygon(c((2000):(1271)/100,rev((2000):(1271)/100)),
        c(rep(0,730),dt(rev((2000):(1271)/100),1)),col="red")
axis(1,labels=c(expression(paste(theta[B],"-12.71",hat(sigma))),
                expression(theta[B]),
                expression(paste(theta[B],"+12.71",hat(sigma)))),
     at=c(-12.17,0,12.71))

# Slide 43:

Y<-c(0.3,1.5,1.3,1.9,4.2,8)
X<-cbind((1:6)^2)
Sigma<-diag((1:6)^2)
(thetahat<-solve(t(X)%*%solve(Sigma)%*%X)%*%t(X)%*%solve(Sigma)%*%Y)
(sigma2hat<-(1/(6-1))*t(Y-X%*%thetahat)%*%solve(Sigma)%*%(Y-X%*%thetahat))


# Slide 45:

#sd:
qt(0.975,df=5)*sqrt(sigma2hat/91)

#ci:
thetahat-qt(0.975,df=5)*sqrt(sigma2hat/91)
thetahat+qt(0.975,df=5)*sqrt(sigma2hat/91)

100*(thetahat-qt(0.975,df=5)*sqrt(sigma2hat/91))
100*(thetahat+qt(0.975,df=5)*sqrt(sigma2hat/91))


# Slide 48:

qt(0.975,df=5)*sqrt(19100*sigma2hat/91)

# ci:
100*thetahat-qt(0.975,df=5)*sqrt(19100*sigma2hat/91)
100*thetahat+qt(0.975,df=5)*sqrt(19100*sigma2hat/91)



