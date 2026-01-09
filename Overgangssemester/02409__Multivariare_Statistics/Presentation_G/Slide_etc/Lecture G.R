
# Slide 8:

library(car)
plot((-10):10,(-10):10,pch='',xlab='x1',ylab='x2')
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "red")

f<-function(x1){(x1^2-10*x1+18)/(2*x1-6)}

index<-(-1500):290/100
polygon(c(index,par("usr")[1],par("usr")[1]),
        c(f(index),par("usr")[2],f(par("usr")[3])),
        col="turquoise")
index<-310:1200/100
polygon(c(index,par("usr")[4],3.1),
        c(f(index),par("usr")[3],par("usr")[3]),
        col="turquoise")

ellipse(c(4,2),matrix(c(1,1,1,2),nrow=2),sqrt(qchisq(0.5,df=2)),center.pch='',fill=T,col="white",
        fill.alpha=1)
ellipse(c(1,1),matrix(c(1,0,0,1),nrow=2),sqrt(qchisq(0.5,df=2)),center.pch='',fill=T,col="white",
        fill.alpha=1)
ellipse(c(4,2),matrix(c(1,1,1,2),nrow=2),sqrt(qchisq(0.5,df=2)),center.pch='',col="black")
ellipse(c(1,1),matrix(c(1,0,0,1),nrow=2),sqrt(qchisq(0.5,df=2)),center.pch='',col="black")
text(8,8,expression(R[1]),col="white",cex=2.5,font=2)
text(-8,-4,expression(R[2]),col="white",cex=2.5,font=2)
text(8,-8,expression(R[2]),col="white",cex=2.5,font=2)


# Slide 9


y<-cbind(rnorm(100000),rnorm(100000))
sigma.temp<-matrix(c(1,1,1,2),nrow=2)

my.T<-eigen(sigma.temp)$vectors
my.T%*%diag(sqrt(eigen(sigma.temp)$values))%*%t(my.T)


x<-y%*%my.T%*%diag(sqrt(eigen(sigma.temp)$values))%*%t(my.T)
x1<-x[,1]+4
x2<-x[,2]+2

g<-function(x1,x2){-x1^2+2*x1*x2+10*x1-6*x2-18}
R1<-1*(g(x1,x2)>=0)
1-mean(R1)

x<-cbind(rnorm(100000),rnorm(100000))
x1<-x[,1]+1
x2<-x[,2]+1

R1<-1*(g(x1,x2)>=0)
mean(R1)


# Slide 25:
f1<-function(x1){5*x1/2-19/4}
f2<-function(x1){4*x1/3}
f3<-function(x1){3*x1/4+19/8}
index<-(-500):1200/100

plot((-4):10,(-4):10,pch='',xlab='x1',ylab='x2',main="Pairwise Separation")

ellipse(c(4,2),matrix(c(1,1,1,2),nrow=2),sqrt(qchisq(0.5,df=2)),center.pch='',fill=T,col="red",
        fill.alpha=1)
ellipse(c(1,1),matrix(c(1,1,1,2),nrow=2),sqrt(qchisq(0.5,df=2)),center.pch='',fill=T,col="green",
        fill.alpha=1)
ellipse(c(2,6),matrix(c(1,1,1,2),nrow=2),sqrt(qchisq(0.5,df=2)),center.pch='',fill=T,col="blue",
        fill.alpha=1)
ellipse(c(4,2),matrix(c(1,1,1,2),nrow=2),sqrt(qchisq(0.5,df=2)),center.pch='',col="black")
ellipse(c(1,1),matrix(c(1,1,1,2),nrow=2),sqrt(qchisq(0.5,df=2)),center.pch='',col="black")
ellipse(c(2,6),matrix(c(1,1,1,2),nrow=2),sqrt(qchisq(0.5,df=2)),center.pch='',col="black")

lines(index,f1(index),lwd=2,col="green")
lines(index,f1(index)-0.11,lwd=2,col="red")
lines(index,f2(index),lwd=2,col="blue")
lines(index,f2(index)-0.11,lwd=2,col="red")
lines(index,f3(index),lwd=2,col="blue")
lines(index,f3(index)-0.10,lwd=2,col="green")

text(5.5,4,expression(pi[1]),col="red",cex=2,font=2)
text(0,-1.3,expression(pi[2]),col="green",cex=2,font=2)
text(3.5,8,expression(pi[3]),col="blue",cex=2,font=2)

text(1.5,-4,expression(u[12](x)==0),font=2)
text(-3,-2.5,expression(u[13](x)==0),font=2)
text(-2.5,1.5,expression(u[23](x)==0),font=2)




# Slide 26:


plot((-4):10,(-4):10,pch='',xlab='x1',ylab='x2',main="Discrimination Regions")
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "blue")

polygon(c(index[1:908],index[908:1],par("usr")[1]),
        c(f3(index[1:908]),f1(index[908:1]),par("usr")[3]),
        col="green")

polygon(c(index,par("usr")[4],par("usr")[4],par("usr")[1]),
        c(f1(index[1:908]),f2(index[909:1701]),par("usr")[2],par("usr")[3],
        par("usr")[3]),
        col="red")
ellipse(c(4,2),matrix(c(1,1,1,2),nrow=2),sqrt(qchisq(0.5,df=2)),center.pch='',fill=T,col="white",
        fill.alpha=1)
ellipse(c(1,1),matrix(c(1,1,1,2),nrow=2),sqrt(qchisq(0.5,df=2)),center.pch='',fill=T,col="white",
        fill.alpha=1)
ellipse(c(2,6),matrix(c(1,1,1,2),nrow=2),sqrt(qchisq(0.5,df=2)),center.pch='',fill=T,col="white",
        fill.alpha=1)
ellipse(c(4,2),matrix(c(1,1,1,2),nrow=2),sqrt(qchisq(0.5,df=2)),center.pch='',col="black")
ellipse(c(1,1),matrix(c(1,1,1,2),nrow=2),sqrt(qchisq(0.5,df=2)),center.pch='',col="black")
ellipse(c(2,6),matrix(c(1,1,1,2),nrow=2),sqrt(qchisq(0.5,df=2)),center.pch='',col="black")

text(5.5,4,expression(pi[1]),col="white",cex=2,font=2)
text(0,-1.3,expression(pi[2]),col="white",cex=2,font=2)
text(3.5,8,expression(pi[3]),col="white",cex=2,font=2)
text(8,-3,expression(R[1]),col="white",cex=2,font=2)
text(-3,-3,expression(R[2]),col="white",cex=2,font=2)
text(-3,8,expression(R[3]),col="white",cex=2,font=2)

# Slide 46:

psychotic<-read.csv2("Data/psychotic.csv")
psychotic$group<-as.factor(psychotic$group)
summary(psychotic)

# Slide 47
plot(psychotic$size,psychotic$shape,xlab='Size',ylab='Shape',
     pch=c("n","p")[as.numeric(psychotic$group)],
     col=c("blue","red")[as.numeric(psychotic$group)],
     main="Psychotic data")

# Slide 47:
data.all<-as.matrix(psychotic[,2:3])
data.normal<-as.matrix(psychotic[psychotic$group=="normal",2:3])
data.psychotic<-as.matrix(psychotic[psychotic$group=="psychotic",2:3])

xbar.all<-matrix(rep(1,100),ncol=2)%*%diag(colMeans(data.all))
xbar.normal<-matrix(rep(1,50),ncol=2)%*%diag(colMeans(data.normal))
xbar.psychotic<-matrix(rep(1,50),ncol=2)%*%diag(colMeans(data.psychotic))

# Slide 48:
W_normal<-t(data.normal-xbar.normal)%*%(data.normal-xbar.normal)
W_psychotic<-t(data.psychotic-xbar.psychotic)%*%(data.psychotic-xbar.psychotic)

W<-W_normal+W_psychotic
B<-25*(colMeans(data.normal)-colMeans(data.all))%*%
       t(colMeans(data.normal)-colMeans(data.all))+
   25*(colMeans(data.psychotic)-colMeans(data.all))%*%
       t(colMeans(data.psychotic)-colMeans(data.all))

T<-W+B

W; B; T

# Slide 49:

d1<-eigen(solve(W)%*%B)$vectors[,1]
d1<-(1/as.numeric(sqrt(t(d1)%*%W%*%d1)))*d1

d2<-eigen(solve(W)%*%B)$vectors[,2]
d2<-(1/as.numeric(sqrt(t(d2)%*%W%*%d2)))*d2


d1
d2

# loadings plot
par(mfrow=c(1,2))
barplot(d1,names.arg=names(psychotic)[2:3],col=c("green","purple"),main="d1 Loadings",
        ylim=c(-0.03,0.03))
lines(0:3,rep(0,4),type="l")
box()
barplot(d2,names.arg=names(psychotic)[2:3],col=c("green","purple"),main="d2 Loadings",
        ylim=c(-0.03,0.03))
lines(0:3,rep(0,4),type="l")
box()
par(mfrow=c(1,1))

#Slide 50:

# score plot
plot((data.all-xbar.all)%*%d1,(data.all-xbar.all)%*%d2,xlab='d1 scores',ylab='d2 scores',
     pch=c("n","p")[as.numeric(psychotic$group)],
     col=c("blue","red")[as.numeric(psychotic$group)])

# Slide 51:
plot(data.all[,1],data.all[,2],xlab='Size',ylab='Shape',
     pch=c("n","p")[as.numeric(psychotic$group)],
     col=c("blue","red")[as.numeric(psychotic$group)])
lines(0:30,-(d1[1]/d1[2])*((0:30)-16.8)+24.36,type="l",lty=2)

library(MASS)

(my.lda<-lda(group ~ size + shape, prior=c(1/2,1/2), data=psychotic) )

d1/my.lda$scaling
# constant; ie. same directions; same discrimination.

# Slide 54:
library(klaR)
partimat(group ~ shape + size, prior=c(1/2,1/2), data=psychotic, method="lda",prec=1000)

# Slide 55:

table(psychotic$group,predict(my.lda)[[1]],dnn=c("Nature","Classification"))


# Slide 56:

# test for additional information
# does shape contribute significantly?

Lambda.21<-(T[1,1]/det(T))*(det(W)/W[1,1])

my.F<-47*(1-Lambda.21)/Lambda.21

1-pf(47*(1-Lambda.21)/Lambda.21,1,47)


# Slide 57:
W_normal/24;W_psychotic/24

# Slide 58:

(my.qda<-qda(group ~ size + shape, prior=c(1/2,1/2), data=psychotic) )

#Slide 59:

partimat(group ~ shape + size, prior=c(1/2,1/2), data=psychotic, method="qda",prec=1000)


table(psychotic$group,predict(my.qda)[[1]],dnn=c("Nature","Classification"))






# Slide 61:

iris<-datasets::iris
names(iris)
pairs(iris[,1:4], col=iris$Species, pch=19)


# Slide 62:

iris.all<-as.matrix(iris[,1:4])
iris.setosa<-as.matrix(iris[iris$Species=="setosa",1:4])
iris.versicolor<-as.matrix(iris[iris$Species=="versicolor",1:4])
iris.virginica<-as.matrix(iris[iris$Species=="virginica",1:4])

xbar.all<-matrix(rep(1,4*150),ncol=4)%*%diag(colMeans(iris.all))
xbar.setosa<-matrix(rep(1,4*50),ncol=4)%*%diag(colMeans(iris.setosa))
xbar.versicolor<-matrix(rep(1,4*50),ncol=4)%*%diag(colMeans(iris.versicolor))
xbar.virginica<-matrix(rep(1,4*50),ncol=4)%*%diag(colMeans(iris.virginica))

# Slide 63:
W_setosa<-t(iris.setosa-xbar.setosa)%*%(iris.setosa-xbar.setosa)
W_versicolor<-t(iris.versicolor-xbar.versicolor)%*%(iris.versicolor-xbar.versicolor)
W_virginica<-t(iris.virginica-xbar.virginica)%*%(iris.virginica-xbar.virginica)

W_setosa/49

W_versicolor/49

W_virginica/49


# Slide 65:

mu.setosa<-colMeans(iris.setosa)
Sigma.setosa<-W_setosa/49

# Displaying:
mu.setosa
Sigma.setosa
log(det(Sigma.setosa))
solve(Sigma.setosa)

# Slide 66:
S.setosa.Q<-function(x){-(log(det(Sigma.setosa))/2+
                          t(x)%*%solve(Sigma.setosa)%*%x-                           
                          t(mu.setosa)%*%solve(Sigma.setosa)%*%mu.setosa/2}

# Slide 67:
my.qda <-qda(Species ~ Sepal.Length+Sepal.Width + Petal.Length+
              Petal.Width, prior=c(1,1,1)/3, data=iris)

head(predict(my.qda)[[2]])
head(predict(my.qda)[[1]])


# Slide 68:
par(mfrow=c(3,1))
barplot(t(as.matrix(predict(my.qda)[[2]][1:50,])),
        names.arg=paste("obs",1:50,sep=""),
        col=c("red","green","blue"),
        main="Posteriors, Setosa",beside=TRUE,
        cex.names=0.8,cex.main=2)
barplot(t(as.matrix(predict(my.qda)[[2]][51:100,])),
        names.arg=paste("obs",51:100,sep=""),
        col=c("red","green","blue"),
        main="Posteriors, Versicolor",beside=TRUE,
        cex.names=0.8,cex.main=2)
barplot(t(as.matrix(predict(my.qda)[[2]][101:150,])),
        names.arg=paste("obs",101:150,sep=""),
        col=c("red","green","blue"),
        main="Posteriors, Virginica",beside=TRUE,
        cex.names=0.8,cex.main=2)
par(mfrow=c(1,1))


# Slide 69:
table(iris$Species,predict(my.qda)[[1]],dnn=c("Nature","Classification"))

# Slide 82:

synprod <- data.frame("Acontent"=c(1,0,0.5), 					  
                      "Bcontent"=c(0,1,0.5), 
		         "Qual"=c(90,30,75)) 

glmSyn <- lm(Qual ~ Acontent + Bcontent - 1, data=synprod) 
# the "-1" in the line above removes the intercept 
# If not specified an intercept is estimated by default

# Slide 83

summary(glmSyn)
