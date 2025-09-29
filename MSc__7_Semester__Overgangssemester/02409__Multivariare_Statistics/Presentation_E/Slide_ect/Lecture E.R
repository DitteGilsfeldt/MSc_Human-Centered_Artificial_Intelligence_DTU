


# Slide 11:

heptathlon<-read.csv2("Data/heptathlon.csv")


heptathlon2<-heptathlon[,-(1:2)]
# Slide 13:

library(psych)

# Correlation matrix
R<-cor(heptathlon2)

# squared multiple correlation coefficients

smc<-numeric(7)
for(i in 1:7){
  smc[i]<- R[i,-i]%*%solve(R[-i,-i])%*%R[-i,i]
  }
smc

# Construction of V matrix
V<-R
diag(V)<-smc

# Slide 14:
principal(cor(heptathlon2),nfactors = 3,rotate = "none")$loadings[,]

principal(V,nfactors = 3,rotate = "none")$loadings[,]

# Slide 15:

eigen(V)$vectors[,1:3]%*%diag(sqrt(eigen(V)$values[1:3]))

principal(V,nfactors = 3,rotate = "none")$loadings[,]


# Slide 16:

eigen(R)$vectors[,1:3]%*%diag(sqrt(eigen(R)$values[1:3]))

principal(R,nfactors = 3,rotate = "none")$loadings[,]


# Slide 17:

fa3 <- principal(R,nfactors = 3,rotate = "none") 
fa31 <- fa3$loadings[,1:3] 

fa2 <- principal(R,nfactors = 2,rotate = "none") 
fa21 <- fa2$loadings[,1:2] 

fa3ml<-fa(R,nfactors = 3,rotate = "none",scores="regression",fm="ml")
fa3ml.1<-fa3ml$loadings[,1:3]

# rotating:

fa3rot <- principal(R,nfactors = 3,rotate = "varimax") 
fa3rot1 <- fa3rot$loadings[,1:3] 

fa2rot <- principal(R,nfactors = 2,rotate = "varimax") 
fa2rot1 <- fa2rot$loadings[,1:2] 

fa3mlrot<-fa(R,nfactors = 3,rotate = "varimax",
             scores="regression",fm="ml")
fa3mlrot.1<-fa4mlrot$loadings[,1:3]

# plot factor structure:


circle = seq(-3.2,3.2,by=0.1) 
# Different combinations of plots 
Names = names(heptathlon2)
plotdata1<-list(fa21,fa31,fa3ml.1)
plotdata2<-list(fa2rot1,fa3rot1,fa3mlrot.1)
my.type<-c("2 factors","3 Factors","3 factors ML")
par(mfrow=c(2,3))
for (i in 1:3){ 
  x<-plotdata1[[i]]
	#Plot for the Factors 
	plot(0,0,xlim = c(-1.2,1.2),ylim = c(-1.2,1.2),xlab = paste0("Factor ",1), 
           ylab = paste0("Factor ",2),
      main = paste("Initial Factor Pattern\n",my.type[i])) 
	points(1*cos(circle),1*sin(circle),type='l') 	
      arrows(c(rep(0,7)),c(rep(0,7)),x[,1],x[,2],length = 0.1,col="blue") 	
      text(x[,1],x[,2]+0.1,Names,cex = 0.7) 
	grid() 
  }
for (i in 1:3){ 
  y<-plotdata2[[i]]

	#Plot for the Factors 
	plot(0,0,xlim = c(-1.2,1.2),ylim = c(-1.2,1.2),xlab = paste0("Factor ",1), 
           ylab = paste0("Factor ",2),
      main = paste("Rotated Factor Pattern\n",my.type[i])) 
	points(1*cos(circle),1*sin(circle),type='l') 	
      arrows(c(rep(0,7)),c(rep(0,7)),y[,1],y[,2],length = 0.1,col="blue") 	
      text(y[,1],y[,2]+0.1,Names,cex = 0.7) 
	grid() 
  }
par(mfrow=c(1,1))

# Slide 20:

temp<- fa(R,nfactors = 3,rotate = "varimax",smc=T,n.obs=25,  
          scores="regression",fm="ml")

A<-temp$loadings[,]
n<-25
k<-7
m<-3

my.Z<-(n-1-(2*k+5)/6-2*m/3)*log(det(A%*%t(A)+diag(temp$uniquenesses))/
                                det(R))
my.Z

1-pchisq(my.Z,df=((k-m)^2-k-m)/2)

# Slide 22:

plot(eigen(R)$values,xlab='',ylab='Eigenvalue',type="b")
grid()

# Slide 23:


par(mfrow=c(1,3))

  x<-temp$loadings[,]
	#Plot for the Factors 
	plot(0,0,xlim = c(-1.2,1.2),ylim = c(-1.2,1.2),xlab = paste0("Factor ",1), 
           ylab = paste0("Factor ",2),
      main = paste("Loadingsplot, Factors 1 and 2")) 
	points(1*cos(circle),1*sin(circle),type='l') 	
      arrows(c(rep(0,7)),c(rep(0,7)),x[,1],x[,2],length = 0.1,col="blue") 	
      text(x[,1],x[,2]+0.1,Names,cex = 0.7) 
	grid() 
	plot(0,0,xlim = c(-1.2,1.2),ylim = c(-1.2,1.2),xlab = paste0("Factor ",1), 
           ylab = paste0("Factor ",3),
      main = paste("Loadingsplot, Factors 1 and 3")) 
	points(1*cos(circle),1*sin(circle),type='l') 	
      arrows(c(rep(0,7)),c(rep(0,7)),x[,1],x[,3],length = 0.1,col="blue") 	
      text(x[,1],x[,3]+0.1,Names,cex = 0.7) 
	grid() 
	plot(0,0,xlim = c(-1.2,1.2),ylim = c(-1.2,1.2),xlab = paste0("Factor ",2), 
           ylab = paste0("Factor ",3),
      main = paste("Loadingsplot, Factors 2 and 3")) 
	points(1*cos(circle),1*sin(circle),type='l') 	
      arrows(c(rep(0,7)),c(rep(0,7)),x[,2],x[,3],length = 0.1,col="blue") 	
      text(x[,2],x[,3]+0.1,Names,cex = 0.7) 
	grid() 
par(mfrow=c(1,1))


# Slide 26:

data.frame(Name=heptathlon[,1],  scale(heptathlon2)[,]%*%solve(R)%*%A)


# Slide 27:

barplot(temp$uniquenesses,col=rainbow(7),ylim=c(0,1))
box()


# Slide 43:


frets<-read.csv2("Data/frets.csv")
head(frets)


# Slide 44
frets2<-scale(frets,center=T,scale=T)
round(cov(frets2),digits=2)

# Slide 45:
brother1<-frets2[,1:2]
brother2<-frets2[,3:4]

round(cov(brother1,brother2),digits=2)

library(GGally)

ggpairs(brother1)
ggpairs(brother2)

# Slide 46:

libary(CCA)
my.cca<-cc(brother1,brother2)

# The A matrix:
(A<-my.cca$xcoef)


# The B matrix:
(B<-my.cca$ycoef)

# canonincal correlations:

my.cca$cor

# note:
round(t(A)%*%cor(brother1)%*%A,digits=2)

round(t(B)%*%cor(brother2)%*%B,digits=2)


# Slide 47:

# Canonical variables values for each observation.
my.cca$scores$xscores[1:4,]   #V_1, V_2
my.cca$scores$yscores[1:4,]   #W_1, W_2

par(mfrow=c(1,2))

plot(my.cca$scores$xscores[,1], my.cca$scores$yscores[,1],pch=19,
     main="Girth brother 2 vs brother 1, cor(V1, W1)=0.7885",
     xlab='Girth brother 1',ylab='Girth brother 2')
plot(my.cca$scores$xscores[,2], my.cca$scores$yscores[,2], pch=19,
     main="Shape brother 1 vs shape brother 2, cor(V2, W2)=0.0537",
     xlab='Shape brother 1',ylab='Shape brother 2')
par(mfrow=c(1,1))


# Slide 48:

#Correlation between variables:

my.cca$scores$corr.X.xscores  #(l1, b1) (V_1, V_2)
my.cca$scores$corr.X.yscores  #(l1, b1) (W_1, W_2)

my.cca$scores$corr.Y.xscores  #(l2, b2) (V_1, V_2)
my.cca$scores$corr.Y.yscores  #(l2, b2) (W_1, W_2)

# note:
cor(brother2)%*%B


# Slide 49:


#Test of hypothesis for the canonical dimensions
# rho: canonical correlations

(rho<-my.cca$cor)

(n<-dim(frets2)[1])    # number of observations
(p<-dim(brother1)[2]) # number of variables in the first set of variables
(q<-dim(brother2)[2]) ## number of variables in the second set of variables

#Compute p-values using the F-approximations
# for different test statistics.
?p.asym
(HypTest<-p.asym(rho,n,p,q, tstat="Wilks"))

# Standardization of brother1's canonical coefficients
# is not necessary, since we work with correlations 

####------
#  Notice that eigen(invE%*%H) are equal to rho^2/(1-rho^2)
# (see proof of Theorem 6.16 on p 390 of the book)
(aux<-(rho^2/(1-rho^2))) 
(VarPerc<- aux/sum(aux))
(VarCum<- cumsum(VarPerc))

#------------

Results <- data.frame("CanCor" = rho,
                      "Squared CanCor" = rho^2,
                      "eigenvaluesInvEH"=aux,
                      "proportion"=VarPerc, 
                      "cumulative" = VarCum,
                      HypTest)

Results


