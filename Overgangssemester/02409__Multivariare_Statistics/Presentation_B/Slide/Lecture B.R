

# Slide 9:

set.seed(4197)
rho=-0.6
X<-rnorm(100)
W1<-rnorm(100)
W2<-rnorm(100)
W3<-rnorm(100)
Y<-rho*X+sqrt(1-rho^2)*W1
Z<-rho*X+sqrt(1-rho^2)*W2

plot(X,Y,main="Correlation of X and Y: -0.6",cex=1.2,pch=16,col="blue")
lines(sort(X),rho*sort(X),type="l",lwd=2,col="red")


plot(X,Z,main="Correlation of X and Z: -0.6",cex=1.2,pch=16,col="blue")
lines(sort(X),rho*sort(X),type="l",lwd=2,col="red")

# Slide 12:

W2<--W1
Z<-rho*X+sqrt(1-rho^2)*W2
plot(Y,Z,main="Correlation of Y and Z: -0.28",cex=1.2,pch=16,col="blue")
lines(sort(Y),-0.28*sort(Y),type="l",lwd=2,col="red")

# Slide 13:

W2<- 0.5*W1+(sqrt(3)/2)*W3
Z<-rho*X+sqrt(1-rho^2)*W2
plot(Y,Z,main="Correlation of Y and Z: 0.68",cex=1.2,pch=16,col="blue")
lines(sort(Y),0.68*sort(Y),type="l",lwd=2,col="red")


# Slide 15:

heiwei<-read.table("Data/heiwei.txt",header=T,sep="\t",dec=".")

summary(heiwei)
head(heiwei)

# converting to cm and kg:
heiwei[,2]<-2.54*heiwei[,2]
heiwei[,3]<-0.453592*heiwei[,3]
heiwei<-heiwei[,2:3]
names(heiwei)<-c("height","weight")
head(heiwei)

my.n<-length(heiwei[,1])
my.1<-rep(1,my.n)
X<-as.matrix(heiwei)

mu.heiwei<-colMeans(heiwei)
mu.heiwei
Sigma.heiwei<-(t(X)%*%X- my.n*mu.heiwei%*%t(mu.heiwei))/(my.n-1)
Sigma.heiwei

# Faster:
# Sigma.heiwei<-var(heiwei)
# Sigma.heiwei

# Slide 16:

plot(heiwei$height,heiwei$weight,xlab='Height',ylab='weight',col="lightblue1")

# major axis of contour ellipse:
pc1<-eigen(Sigma.heiwei)$vectors[,1]
with(heiwei,lines(min(height):max(height),
                  mu.heiwei[2]+(pc1[2]/pc1[1])*((min(height):max(height))-mu.heiwei[1]),
                  type="l",lwd=2))


# slide 23:
# conditional mean:

my.height<-min(heiwei$height):max(heiwei$height)
my.weight<-(round(100*min(heiwei$weight),digits=2):round(100*max(heiwei$weight),digits=2))/100


lines(my.height,mu.heiwei[2]+(Sigma.heiwei[1,2]/Sigma.heiwei[2,2])*(my.height-mu.heiwei[1]),
      type="l",lwd=2,col="green")
lines(mu.heiwei[1]+(Sigma.heiwei[2,1]/Sigma.heiwei[1,1])*(my.weight-mu.heiwei[2]),my.weight,
      type="l",lwd=2,col="blue")

# Slide 24: 

#slope and intercept given height: 
alpha_1<-mu.heiwei[2]-(Sigma.heiwei[2,1]/Sigma.heiwei[1,1])*mu.heiwei[1]
beta_1<-(Sigma.heiwei[2,1]/Sigma.heiwei[1,1])

alpha_1;beta_1

#slope and intercept given weight:

alpha_2<-mu.heiwei[1]-(Sigma.heiwei[1,2]/Sigma.heiwei[2,2])*mu.heiwei[2]
beta_2<-(Sigma.heiwei[1,2]/Sigma.heiwei[2,2])

alpha_2;beta_2

# given 180cm:
alpha_1+beta_1*180

# given 65 kg:
alpha_2+beta_2*65


# Slide 31:

Sigma <- matrix(c(1, 0.5, 0.7,
                    0.5, 1, 0.7,
                    0.7, 0.7, 1),ncol=3)
Sigma.11<-Sigma[1:2,1:2]
Sigma.12<-Sigma[1:2,3]
Sigma.21<-Sigma[3,1:2]
Sigma.22<-Sigma[3,3]
(Sigma1.2<-Sigma.11-Sigma.12%*%solve(Sigma.22)%*%Sigma.21)



# Slide  40:

my.corr<-diag(rep(1,5))
my.corr[lower.tri(my.corr)]<-c(-0.309,     
                                0.091,0.192,
                                0.158,0.120,0.745,
                                0.344,-0.166,0.320,0.464)
my.corr<-my.corr+t(my.corr)-diag(rep(1,5))

colnames(my.corr)<-c("C3A","C3S","Blaine","Strgth3","Strgth28")
row.names(my.corr)<-colnames(my.corr)

#principal components and values:
eigen(my.corr)

#partial covariance:

Sigma.11<-my.corr[-3,-3]
Sigma.12<-my.corr[-3,3]
Sigma.21<-my.corr[3,-3]
Sigma.22<-my.corr[3,3]
Sigma.1.2<-Sigma.11-Sigma.12%*%solve(Sigma.22)%*%Sigma.21

# partial correlation:
cov2cor(Sigma.1.2)


