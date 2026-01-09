
# Slide 3:

Hay_var <- matrix(c(4.42^2,4.42*1.1*0.8,-4.42*85*0.4,
                   4.42*1.1*0.8,1.1^2,-1.1*85*0.56,
                   -4.42*85*0.4,-1.1*85*0.56,85^2),ncol=3)
Hay_means <- c(28.02,4.91,594)

my.eigen.values<-eigen(Hay_var)$values
my.eigen.vectors<-eigen(Hay_var)$vectors

# simulate data:
k<-5000
my.x<-cbind(rnorm(k),rnorm(k),rnorm(k))

my.y<-my.x%*%diag(sqrt(my.eigen.values))%*%t(my.eigen.vectors)+
      matrix(rep(Hay_means,each=k),ncol=3)

# check variance (compare with Hay_cor)
round(var(my.y),digits=2)
Hay_var

my.y<-as.data.frame(my.y)
names(my.y)<-c("yield","rainfall","temp")
par(mfrow=c(2,2))
frame()
plot(my.y$rainfall,my.y$yield,col="blue",pch="+",xlab="Rainfall",ylab="Yield")
temp<-matrix(nrow=2,ncol=101)
for(i in 0:101){
  temp[,i]<-Hay_means[1:2]+(i-51)*eigen(Hay_var[1:2,1:2])$vectors[,1]
  }
lines(temp[2,],temp[1,],type="l", col="red",lwd=2)
plot(my.y$temp,my.y$rainfall,col="blue",pch="+",xlab="Temperature",ylab="Rainfall")
temp<-matrix(nrow=2,ncol=101)
for(i in 0:101){
  temp[,i]<-Hay_means[2:3]+50*(i-51)*eigen(Hay_var[2:3,2:3])$vectors[,1]
  }
lines(temp[2,],temp[1,],type="l", col="red",lwd=2)
plot(my.y$temp,my.y$yield,col="blue",pch="+",xlab="Temperature",ylab="Yield")
temp<-matrix(nrow=2,ncol=101)
for(i in 0:101){
  temp[,i]<-Hay_means[c(1,3)]+50*(i-51)*eigen(Hay_var[c(1,3),c(1,3)])$vectors[,1]
  }
lines(temp[2,],temp[1,],type="l", col="red",lwd=2)
par(mfrow=c(1,1))

# 3d plot:
library(rgl)
plot3d(my.y, type = "s", size = 0.75, lit = FALSE,col="blue",pch="+")
rgl.snapshot("haydata.png", fmt = "png", top = TRUE )

# Slide 4:
# sampling from the conditional distribution,
# given rainfall at its mean:
yield.given.rainfall<-Hay_means[1]+
        sqrt(Hay_var[1,1]-Hay_var[1,2]^2/Hay_var[2,2])*rnorm(k)

temp.given.rainfall<-Hay_means[3]+
        sqrt(Hay_var[3,3]-Hay_var[3,2]^2/Hay_var[2,2])*rnorm(k)


plot(my.y$temp,my.y$yield,col="blue",pch="+",xlab="Temperature",ylab="Yield")
points(temp.given.rainfall,yield.given.rainfall,col="red",pch="+")

cov2cor(var(cbind(yield.given.rainfall,temp.given.rainfall)))

# theoretical correlation:

my.partial.var<-Hay_var[c(1,3),c(1,3)]-
  Hay_var[c(1,3),2]%*%solve(Hay_var[2,2])%*%Hay_var[2,c(1,3)]

cov2cor(my.partial.var)[1,2]

# 0.09656091


# Slide 7:

n <- length(Fit$Oxygen)
p <-2 
shape <- var(Fit) 
center <- sapply(Fit,mean,1)

conf95 <- 
  sqrt(p * (n-1) * qf(0.95, p, n-p)/(n*(n-p))) 
pred95 <- sqrt(n+1)*rconf95 

library(car)
pred.elip95 <- 
car::ellipse(center, shape, pred95,draw = FALSE) 
conf.elip95 <- 
car::ellipse(center, shape, conf95,draw = FALSE) 

plot(pred.elip95, type='l', col = 2,lwd=2,xlab="Oxygen",ylab="Runtime") 
lines(conf.elip95, type='l', col = 3,lwd=2) 
points(Fit$Oxygen, Fit$Runtime,col=2) 
points(center[1],center[2],cex=2,pch=19,col=3) 


# Slide 14:

2*pt(-0.243,df=48)

# p=0.81

curve(dt(x,df=48),-3,3)
polygon(c(-(3000:243)/1000,-243/1000,-3,-3),
        c(dt(-(3000:243)/1000,df=48),0,0,dt(-3,df=48)),
        col="blue")
polygon(c((3000:243)/1000,243/1000,3,3),
        c(dt((3000:243)/1000,df=48),0,0,dt(3,df=48)),
        col="blue")



# Slide 18-19:

R2<-1-det(matrix(c(1,0.158,0.12,0.158,1,-0.309,0.12,-0.309,1),nrow=3))/
      det(matrix(c(1,-0.309,-0.309,1),nrow=2))
R2

(R2/(1-R2))*48/2

# 1.436


1-pf((R2/(1-R2))*48/2,2,48)

#p=0.25

curve(df(x,2,48),0,3)
polygon(c((3000:1436)/1000,1436/1000,3,3),
        c(df((3000:1436)/1000,2,48),0,0,df(3,2,48)),
        col="blue")



# Slide 21:
# reading in data:
load("Data/Winedata.Rdata")

# two objects: wines, with characteristics, and vintages the wine type.
ls()

#Slide 22:
par(mfrow = c(4,4))
for (i in 1:13) boxplot(wines[,i] ~ vintages, col = 2:4, 
                        main=paste(names(wines)[i]))
par(mfrow = c(1,1))

# Slide 23:

library(car)
scatterplotMatrix(wines)


#slide 25:
# looking at variation:
round(var(wines),digits=2)

# Slide 26:
round(var(scale(wines)),digits=2)

# Slide 27:
Sigma<-var(scale(wines))

sum(diag(Sigma))


# Slide 29:
T<-eigen(Sigma)$vectors

# The inverse of T is equal to the matrix transpose t(T):

Lambda<-t(T)%*%Sigma%*%T

round(Lambda, digits=2)

# Slide 30:

T[,1]

# Slide 31:

round(T[,1],digits=2)

# Slide 32:
round(T[,1]*sqrt(diag(var(wines))),digits=2)




# Slide 17:

sum(diag(Lambda))

# Slide 18:


plot(100*(13-cumsum(diag(Lambda)))/13,type="b",main="Percentage Variance Unexplained",
      xlab='Number of eigenvectors included',
      ylab='Percentage of total variance')

data.frame("Eigenvectors"=1:13,"Variance Explained"=round(100*(cumsum(diag(Lambda)))/13))


# Slide 44:
install.packages("remotes")
library(remotes)
install_github("rwehrens/ChemometricsWithR")

library(ChemometricsWithR)
wines.PC<- PCA(scale(wines))
names(wines.PC)
summary(wines.PC)

# Slide 45:

head(wines.PC$loadings,n=3)
head(T,n=3)

# Slide 46:

head(wines.PC$scores,n=3)
head(scale(wines)%*%T,n=3)

# Slide 47:

wines.PC$var
wines.PC$totalvar
wines.PC$centered.data

# Slide 48:

plot(1:13,wines.PC$var,xlab="PC",ylab="var")


# Slide 49:

scoreplot(wines.PC, col = vintages, pch= as.numeric(vintages), lwd=2)
legend("topright",levels(vintages), col=1:3,pch=1:3)

# Slide 50:

loadingplot(wines.PC, show.names= TRUE)

# Slide 51:

par(mfrow=c(1,2))
loadingplot(wines.PC, pc=c(1,3), show.names= TRUE)
loadingplot(wines.PC, pc=c(2,3), show.names= TRUE)
par(mfrow=c(1,1))

# Slide 52:

biplot(wines.PC, score.col = vintages, show.names = "loadings")
legend("topright",levels(vintages), col=1:3,pch=1:3)

# Slide 54:
install.packages("chemometrics")
library(chemometrics)
wines.PCA<- princomp(wines, cor = TRUE)
res<-pcaDiagplot(wines, wines.PCA, a=3)

# Slide 55:

par(mfrow=c(1,1))
plot(res$SDist, res$ODist, type="n")
text(res$SDist, res$ODist, labels=as.character(1:178))



# Slide 57:

install.packages("png")
library(png)


pigs<-readPNG("Data/Picture2.png")

ncol(pigs)
## [1] 719
	
nrow(pigs)
## [1] 719

#719*719 pixels - 719*719*4=2067844 numbers


# array with 4 layers
str(pigs)


number1<-pigs[,,1]
number2<-pigs[,,2]
number3<-pigs[,,3]
number4<-pigs[,,4]

pigs.number1.pca <-  prcomp(number1, center = FALSE)
pigs.number2.pca <-  prcomp(number2, center = FALSE)
pigs.number3.pca <-  prcomp(number3, center = FALSE)
pigs.number4.pca <-  prcomp(number4, center = FALSE)

# Gather PCA object sin one list:
	
full.pca <- list(pigs.number1.pca,pigs.number2.pca,
                 pigs.number3.pca,pigs.number4.pca)



index<-c(3,6,9,12,15,18,50,100)
# function for reconstruction from pc (scores):

my.reconstruct<-function(j) {
    return(j$x[,1:i] %*% t(j$rotation[,1:i]))
  }
# reconstructs and writes to disc:

for (i in index) {
  pca.picture <- sapply(full.pca,my.reconstruct,simplify = 'array')
  writePNG(pca.picture, 
    paste("picture/pigs_compressed_", i, "_components.jpg", sep =""))
  }


# Slide 60:

read.csv2("Data/BoxData.csv")

library(car)
scatterplotMatrix(BoxData,regLine=F,smooth=F,
  ellipse=list(levels=0.9, robust=TRUE, fill=F),
  diagonal=list(method="histogram", breaks="FD"))
install.packages("GGally")
library(GGally)
ggpairs(BoxData)


#Slide 61:

install.packages("rgl")
library(rgl)

plot3d( 
  x=BoxData$longax, y=BoxData$intermax, z=BoxData$longdia, 
  col = 2:4, 
  type = 's', 
  radius = .1,
  xlab="longax", ylab="intermax", zlab="longdia")

# Slide 62:

round(var(BoxData),digits=2)
round(cor(BoxData),digits=2)


# Slide 63:

eigen(var(BoxData))$values
eigen(var(BoxData))$values/sum(eigen(var(BoxData))$values)
cumsum(eigen(var(BoxData))$values/sum(eigen(var(BoxData))$values))
eigen(var(BoxData))$vectors

# Slide 64:

par(mfrow=c(1,2))
plot(1:7,eigen(var(BoxData))$values,type="b",xlab="Principal Components",
     ylab="Variance",main="Scree plot")

plot(1:7,cumsum(eigen(var(BoxData))$values)/
         sum(eigen(var(BoxData))$values),type="b",xlab="Principal Components",
     ylab="Variance explained",main="Cumulative relative variance")

# Slide 65:

cor(BoxData,as.matrix(BoxData)%*%eigen(var(BoxData))$vectors)

PCA(BoxData)


cor(BoxData,eigen(var(BoxData))$vectors)

cor(BoxData,as.matrix(BoxData)%*%eigen(var(BoxData))$vectors)


# Slide 70:

cbind(eigen(cor(BoxData))$values,
eigen(cor(BoxData))$values/7,
cumsum(eigen(cor(BoxData))$values/7))


lambdastar<-mean(eigen(cor(BoxData))$values[6:7])

-dim(BoxData)[1]*log( prod(eigen(cor(BoxData))$values[6:7])/(lambdastar^2))

# 33.13

1-pchisq(33.14,df=2)

# p=0.0000006

