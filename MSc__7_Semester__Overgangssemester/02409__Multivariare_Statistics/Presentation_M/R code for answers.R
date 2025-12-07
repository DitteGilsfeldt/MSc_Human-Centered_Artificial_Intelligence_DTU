

occupancy<-readRDS('occupancy.rds')
summary(occupancy)

# Q1.1
cov(occupancy$S1_Temp,occupancy$S3_Temp)

#0.1424 - option 1

# Q1.2

# Theorem 1.37
(n<-dim(occupancy)[1])
(my.c<-qt(0.9,df=n-2)/sqrt(n-2))

cor(occupancy$S1_Sound,occupancy$S2_Sound)-sqrt(my.c^2/(1+my.c^2))
cor(occupancy$S1_Sound,occupancy$S2_Sound)+sqrt(my.c^2/(1+my.c^2))

# option 2

# Q1.3
(my.T<-cor(occupancy$S1_Sound,occupancy$S2_Sound)/
sqrt(1+cor(occupancy$S1_Sound,occupancy$S2_Sound)^2))

my.T*sqrt(c(6,8,10,13,15,16)-2)
qt(0.95,df=c(6,8,10,13,15,16)-2)

# actually, the answer is 16...

# probably meant 
qt(0.90,df=c(6,8,10,13,15,16)-2)

# which gives 13, option 2.

# Q1.4:

summary(lm(S1_Sound~-1+S1_Temp+S2_Temp+S3_Temp+S4_Temp,data=occupancy))$r.square

# R^2=0.301, option 3.

# Q1.5:

# Theorem 1.45:
(0.1530/(1-0.1530))*(10129-5)/4

# 457.1936, option 3.


# Q1.6:

temperature<-occupancy[,1:4]
sound<-occupancy[,9:12]

R11<-cor(sound)
R22<-cor(temperature)
R12<-cor(sound,temperature)
R21<-t(R12)

E1<-solve(R11)%*%R12%*%solve(R22)%*%R21
# Finding the degree of explanation:
eigen(E1)$values

# 0.302, option 3.


Q2.1:

library(MASS)
my.lda<-lda(as.factor(RoomCount) ~ S1_Sound+S2_Sound+S3_Sound+S4_Sound+
                                   S1_Temp+S2_Temp+S3_Temp+S4_Temp , 
 prior=c(1/4,1/4,1/4,1/4), data=occupancy) 

my.qda<-qda(as.factor(RoomCount) ~ S1_Sound+S2_Sound+S3_Sound+S4_Sound+
                                   S1_Temp+S2_Temp+S3_Temp+S4_Temp , 
 prior=c(1/4,1/4,1/4,1/4), data=occupancy)

lda.classification<-table(occupancy$RoomCount,predict(my.lda)[[1]],dnn=c("Nature","Classification"))
lda.classification

# misclassifications
412+76
# 488

qda.classification<-table(occupancy$RoomCount,predict(my.qda)[[1]],dnn=c("Nature","Classification"))
qda.classification

152+37
# 189

488-189
# 299, option 4.


Q2.2:
#Table 5.3 for accuracy:

# qda accuracy:

sum(diag(qda.classification))/sum(qda.classification)
#0.93

# lda accuracy:

sum(diag(lda.classification))/sum(lda.classification)
#0.88

#[0.93,0.88], option 5.

Q2.3:
# Test for additional information, theorem 5.19:

# U(p,k-1,N-k-q)
# 4 groups (k=4),
# testing that the last 4 doesn't contribute (q=8-4=4),
dim(occupancy)
# N=10129

# U(8,3,10121), option 3.

Q2.4:

#1: The data are not perfectly separable with a linear classifier
# We haven't applied a linear classifier.

#2: A perfectly separating hyperplane exists.
# No - a hyperplane divides data into two groups - we have 4.

#3: The data are perfectly separable using Quadratic Discrimination analysis
# We haven't appled QDA

#5: The data can be perfectly separated using the minimax principle
# We haven't applied any minimax principle

#6: Nothing, we need additional canonical variables 
#   before we can make any conclusions
# we cannot rule out the the combination of all canonical variables 
# may separate the data. The given information clearly don't.
# Thus, this is the right answer

Q3.1:

# 3rd row in the cumulative column is the first above 0.8; 
# 3, option 2.

Q3.2:
# Theorem 6.8:

(n<-dim(occupancy)[1])
#10129

#k=number of variables
k<-6
(m<-k-2)
#4

# 10124
lambda.5<-0.38144434
lambda.6<-0.14060819
lambda.star<-(lambda.5+lambda.6)/(k-m)
# test statistic:
-n*log(lambda.5*lambda.6/lambda.star^(k-m))

#2423.869, option 1.

Q3.3:

# option 5 fits.

Q3.4:

0.11449^2+0.96953^2
# 0.953

# V(X)=1 assumed; option 1.

#Q.3.5:

# Option 1: 0. Initial factors are always assumed uncorrelated
# in Factor anallysis

# Q3.6:

V1.1<-c(0.89348,0.55951,0.66549,0.57004,0.75198,0.78763)
V2.1<-c(0.64295,0.08579,0.93899,-0.01442,0.52105,0.63560)

My.V<-matrix(c(1,0.57948,0.65083,0.44771,0.50184,0.57781,
               0.57948,1,0.14861,0.21272,0.32454,0.22020,
               0.65083,0.14861,1,0.06922,0.39526,0.47344,
               0.44771,0.21272,0.06922,1,0.36837,0.42535,
               0.50184,0.32454,0.39526,0.36837,1,0.57113,
               0.57781,0.22020,0.47344,0.42535,0.57113,1),byrow=T,nrow=6)

V1.1%*%My.V%*%t(t(V2.1))





# 6.53 - option 3.


# Q4.1:

toxicity<-read.csv2("qsar_fish_toxicity3.csv")

names(toxicity)
names(toxicity)[1:6]<-paste("X",1:6,sep="")



analysis<-lm(LC50~X1+X2+X3+X4+X5+X6,data=toxicity)
summary(analysis)

# R^2=0.5772 - option 2.

Q4.2:
?rstudent
?leverage
# use hat()

X<-as.matrix(toxicity[,1:6])
sum((abs(rstudent(analysis))>=2)*(hat(X,intercept=T)>0.015))

# 8, option 2

#Q4.3:
temp<-dfbetas(analysis)
temp<-abs(temp)
apply(temp,2,max)

# X2,X4,X6 - option 3


# Q4.4:
#Tolerance of X1:
1-summary(lm(X1~X2+X3+X4+X5+X6,data=toxicity))$r.squared
# larger than 0.1

#Tolerance of X6:
1-summary(lm(X6~X1+X2+X3+X4+X5,data=toxicity))$r.squared
# larger than 0.1

# tolerance of X_2-X_5
1-summary(lm(X2~X1+X3+X4+X5+X6,data=toxicity))$r.squared
1-summary(lm(X3~X1+X2+X4+X5+X6,data=toxicity))$r.squared
1-summary(lm(X4~X1+X2+X3+X5+X6,data=toxicity))$r.squared
1-summary(lm(X5~X1+X2+X3+X4+X6,data=toxicity))$r.squared
# all bigger than 0.1

# no indication of colinearity - option 4

# Q4.5:

drop1(lm(toxicity$LC50~toxicity[,1]),test="F")
drop1(lm(toxicity$LC50~toxicity[,2]),test="F")
drop1(lm(toxicity$LC50~toxicity[,3]),test="F")
drop1(lm(toxicity$LC50~toxicity[,4]),test="F")
drop1(lm(toxicity$LC50~toxicity[,5]),test="F")
drop1(lm(toxicity$LC50~toxicity[,6]),test="F")

# largest F-value: X6.
# option 5 - X6.

# Q4.6:

my.mu<-predict(analysis)[1]
my.sd<-sqrt(summary(analysis)$sigma^2*0.01)

my.mu-1.96*my.sd
my.mu+1.96*my.sd

#[3.67;4.04], (nearly) option 6.

# Q4.7: 

my.mu<-predict(analysis)[6]
my.sd<-sqrt(2*summary(analysis)$sigma^2)

my.mu-1.96*my.sd
my.mu+1.96*my.sd

#[-0.15;5.12], (nearly) option 1.



# Q4.8:

analysis2<-lm(LC50~X2+X3+X4+X5,data=toxicity)
anova(analysis2,analysis)

#247.29 - option 1.



# Q5.1

X<-cbind(rep(1,5), c(1,1,0,2,0),c(1,0,1,0,2))
Y<-cbind(c(21,16,17,19,25),c(17,12,13,16,19))
 Sigma<-matrix(c(17/15,11/30,11/30,2/15),nrow=2)

# Doesn't work
#Sigma<-matrix(c(1.3333333,0.36666667,0.36666667,0.13333333),nrow=2)

(thetahat<-solve(t(X)%*%X)%*%t(X)%*%Y)

#(7.7,4.1,5.5) - option 3.

# Q5.2:

solve(t(X)%*%X)[1,1]*Sigma[1,2]

#0.856 - option 1.

# Q5.3:

solve(t(X)%*%X)[2,2]*Sigma[1,1]

#1.06 option 4

# Q5.4:

#A=(1 0 0),B=(1 0)

Delta<-thetahat[1,1]-11
R<-t(Y-X%*%thetahat)%*%(Y-X%*%thetahat)
(E<-R[1,1])
(H<-Delta*(1/solve(t(X)%*%X)[1,1])*Delta)
(Wilks<-E/(E+H))

# Wilks(1,5-3,1)
F<-2*(1-Wilks)/Wilks
1-pf(F,1,5-3)

# 0.857 -option 4 

# Q5.5:

# A=(0,1,0),B=I_2 so r=1,s=2. Since n-k=2, the solution is
# U(r,s,n-k)=U(2,1,2)
# option 4


# Q5.6:
#  A=I3,B=(1 0)
Delta<-thetahat[,1]-c(11,4,6)

E<-R[1,1]
H<-t(Delta)%*%(t(X)%*%X)%*%Delta

Wilks<-E/(E+H)

Wilks

#0.453 - option 1.

