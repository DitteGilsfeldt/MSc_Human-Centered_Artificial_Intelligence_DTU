
# Slide 4:
salesdata<-read.csv2("Data/Salesdata.csv")
names(salesdata)

# Slide 6:
Y<-salesdata[,1:3]
X<-salesdata[,4:7]


#Slide 7:

Sigmayy<-var(Y)
Sigmaxx<-var(X)
Sigmayx<-cov(Y,X)
Sigmaxy<-t(Sigmayx)

E1<-solve(Sigmayy)%*%Sigmayx%*%solve(Sigmaxx)%*%Sigmaxy
E2<-solve(Sigmaxx)%*%Sigmaxy%*%solve(Sigmayy)%*%Sigmayx
E1
E2

# Slide 8:
(my.cor<-sqrt(eigen(E1)$values))

sqrt(round(eigen(E2)$values,digits=6))


# Slide 9:

A<-eigen(E1)$vectors
B<-eigen(E2)$vectors[,1:3]

round(cov(as.matrix(Y)%*%A,as.matrix(X)%*%B),digits=4)
A[,2]<--A[,2]

# Slide 10:

round(t(A)%*%Sigmayy%*%A,digits=4)
round(t(B)%*%Sigmaxx%*%B,digits=4)



A<-A%*%diag(1/sqrt(diag(t(A)%*%Sigmayy%*%A)))
B<-B%*%diag(1/sqrt(diag(t(B)%*%Sigmaxx%*%B)))

# Slide 11:

A
B

# Slide 12:

par(mfrow=c(2,1))
barplot(A[,1],names.arg=names(Y),col=rainbow(4))
box()

barplot(B[,1],names.arg=names(X),col=rainbow(4))
box()
par(mfrow=c(1,1))


plot(as.matrix(X)%*%B[,1], as.matrix(Y)%*%A[,1], xlab="W1",ylab="V1")

# Slide 16:

#values:

my.Q<-rev(cumprod(1-rev(my.cor)^2))
my.Q

# Slide 21:
nu<-c(sqrt(7),2,1)
f1<-c(12,6,2)
f2<-nu*(50-5)-f1/2+1

my.F<-((1-my.Q^(1/nu))/my.Q^(1/nu))*f2/f1
p_F<-1-pf(my.F,f1,f2)
my.chisq<--(50-5)*log(my.Q)
p_chisq<-1-pchisq(my.chisq,df=c(12,6,2))

data.frame(my.Q, f1,f2,"F(f1,f2)"=my.F,p_F=format.pval(p_F,digits=1,eps=0.0001),
"Chisq_12,6,2"=my.chisq,p_chisq=format.pval(p_chisq,digits=1,eps=0.0001))


# Slide 22:

# Using p.asym:
library(CCP)
p.asym(my.cor,n,p,q, tstat="Wilks")


# Slide 23:

V<-as.data.frame(as.matrix(Y)%*%A)
W<-as.data.frame(as.matrix(X)%*%B[,1:3])
names(V)<-c("V1","V2","V3")
names(W)<-c("W1","W2","W3")

my.cor<-cor(cbind(Y,X,V,W))
round(my.cor,digits=2)

# Slide 50:

f<-function(c){2*pnorm((log(c)-6.5)/sqrt(13))+pnorm((log(c)+6.5)/sqrt(13))}

curve(f,0.1,1,xlab='c',lwd=2,col="blue")
lines(0:2,rep(1,3),lty=2)
lines(rep(0.5666,2),c(1,par("usr")[3]),col="red",lwd=2,type="l",lty=2)

#
f(0.5666)

# Slide 51:

# misclassification probs:

pnorm((log(0.5666)-6.5)/sqrt(13))


1-pnorm((log(0.5666)+6.5)/sqrt(13))


# constant in the linear discriminator:

-t(c(4,2))%*%matrix(c(2,-1,-1,1),nrow=2)%*%c(4,2)/2+
t(c(1,1))%*%matrix(c(2,-1,-1,1),nrow=2)%*%c(1,1)/2-log(0.5666)