# slide 11: R AS A CALCULATOR

2+2
(2*5)+(12/3)-(2^3)
exp(log(1))
sqrt(25)
log(2*2)
log(2)+log(2)

# slide 12: GETTING HELP

?plot

# Slide 13: ASSIGNING VALUES TO THE OBJECTS a AND A
a <- 2+5
A <- 10
ls()

# Slide 14:
#Make a sequence
#Here is a sequence from 0 up to 10
0:10

#Here is a sequence from 15 down to 5

15:5

#sequence in steps other than 1
#from, to, by
seq(from = 0, to = 1.2, by = 0.1)

# assigning sequence to object

x <- seq(from = 0, to = 1.5, length = 11); x

# slide 15: generating repeats

rep(8, 5)

rep(1:4, each = 2)

rep(1:4, each = 2, times = 3)

# slide 16: functions in R

# a simple function
f<-function(x){x^2}; f(2)

# a function of two variables
f<-function(x,pow){x^pow}; f(2,2)

# a function with a default value.
f<-function(x,pow=2){x^pow}; f(2,2); f(2);f(2,3)

# slide 17: Some functions previously applies

"+"(2,2)
sqrt(25)
log(2)
ls()
":"(0,10)
seq(from=0.1,to=1.2,by=0.1)
# look at
seq(0.1,1.2,0.1)
# and compare with the syntax from the help pages

rep(1:4,each=2,time=3)


# slide 18: Singles

TRUE
1==2
1
"5"
"abc"


# slide 19: Vectors


# vector of numbers
c(1,1.2,pi,exp(1))

# vector of logicals
c(TRUE,1==2)

# vector of characters
c("a","ab","abc")

# combination
c("a",5,1==2)

# Slide 20: Matrices

# matrix of numbers

matrix(c(1,2,3,4,5,6)+pi,nrow=2)

# matrix of logicals

matrix(c(1,2,3,4,5,6)+pi,nrow=2)<6


# slide 21: Data frames

data.frame(treatment=c("active","active","placebo"),
            bp=c(80,85,90))

# same data placed in a matrix

cbind(treatment=c("active","active","placebo"),bp=c(80,85,90))

# slide 22: Lists

list(a=1,b="abc",c=c(1,2,3),d=list(e=matrix(1:4,2), f=function(x){x^2}))

# Slide 24: Setting the working directory.

#setwd("C:/<your favorite directory using slashes for separation>")
# remove the hashtag for execution when you have entered your favorite directory
# example : setwd("C:/projects/my project name/R analysis number 3/messy data")
getwd()

# slide 25: Importing data to R

# reading with read.table:
Births.tab <- read.table("Data/Births.txt", header = TRUE, sep = "\t")

# reading tab separated data

Births.delim <-read.delim("Data/Births.txt")

# reading ; separated data
Births.csv2 <- read.csv2("Data/Births.csv")

# reading , separated data
Births.csv1 <- read.csv("Data/Births.csv1")


# slide 28: Improting data from other programs
install.packages("foreign")
library("foreign")

SPSS_Data <- read.spss("Data/SPSS_Data.sav", to.data.frame = TRUE)
Stata_Data <- read.dta("Data/string.dta")

# slide 29: LOOK AT THE DATA

#FIRST FEW OBSERVATIONS
head(Births.tab)

# slide 30: LAST FEW OBSERVATIONS

tail(Births.tab)

# variable names
names(Births.tab)

#VIEW THE DATA IN A NEW WINDOW
View(Births.tab)

#slide 33: ACCESS THE OBSERVATIONS

# A SINGLE CELL
Births.tab[345,4]

# LEAVING OUT A COLUMN NUMBER INDICATES THAT ALL COLUMS ARE CHOSEN
# HERE ALL COLUMNS IN ROW 224
Births.tab[224 , ]

#slide 34: ACCESS THE OBSERVATIONS

# LEAVING OUT A ROW NUMBER INDICATES THAT ALL ROWS ARE CHOSEN
# HERE ALL ROWS IN COLUMN 5
Births.tab[ ,5]

# slide 35: ACCESS THE OBSERVATIONS

# USE RANGES, ROWS 15 TO 18 COLUMNS 1 TO 4
Births.tab[15:18, 1:4]


# slide 36: ACCESS THE OBSERVATIONS

# GET THE BIRTH WEIGHT FOR CHILD 26 TO 36
Births.tab$bweight[26:36]


Births.tab[26:36, "bweight"]

Births.tab[26:36,2]


# slide 37: Subsetting using the c() function

# GET COLUMNS 2, 5, 7, 8, 9 FOR ROW 33
Births.tab[33, c(2, 5, 7:9)]

# GET bweight, preterm and sexalph FOR ROW 71
Births.tab[71, c("bweight", "preterm", "sexalph")]

# slide 38: Variable names

#NEW VARIABLE NAMES
names(Births.tab) <- c("ID", "Bweight", "LowBW", "GestWks",
                      "Preterm", "Matage", "Hyp", "Sex", "Sexalph")

#JUST THE FIRST NAME
names(Births.tab)[1] <- c("ID_new")

#CHECK HOW IT WENT
names(Births.tab)

# RESETTING:
names(Births.tab)[1] <- c("ID")

# slide 39: Saving/Exporting data

write.table(Births.tab, file = "Birth_new.txt", row.names= FALSE,
            sep = "\t", na = ".")

write.csv2(Births.tab, file = "Birth_new.csv")

# DESCRIPTION OF DATA
############################################

# slide 42: Description of data
# resetting to get lower case names again (not on slides):
Births.tab <- read.table("Data/Births.txt", header = TRUE, sep = "\t")


str(Births.tab)


# slide 44: CONVERT sex TO A FACTOR
Births.tab$sexalph <- as.factor(Births.tab$sexalph)
levels(Births.tab$sexalph)

str(Births.tab)

# resetting (not on slides):
Births.tab$sex <- as.numeric(Births.tab$sex)


# slide 45: CONVERT sex TO A FACTOR WITH LABELS
Births.tab$sex <- factor(Births.tab$sex,labels=c("Male","Female"))
levels(Births.tab$sex)

str(Births.tab)


# resetting (not on slides):
Births.tab$sex <- as.numeric(Births.tab$sex)



# slide 46: Summary statistics

#SUMMARY STATS FOR BIRTH WEIGHT
mean(Births.tab$bweight)
sd(Births.tab$bweight)
median(Births.tab$bweight)
max(Births.tab$bweight)
min(Births.tab[,2])

# slide 47: SUMMARY OF THE DATA FRAME
summary(Births.tab)

#SUMMARY OF A SUBSET
summary(Births.tab[Births.tab$bweight<2900,])

# slide 49: MEAN BIRTH WEIGHT FOR BOYS AND GIRLS

tapply(Births.tab$bweight, Births.tab$sexalph, mean)

# slide 50: Histogram

hist(Births.tab$bweight, main = "Title",
     xlab="Birth weight (g)")

# slide 51: Histogram with box

hist(Births.tab$bweight, main = "Title",
     xlab="Birth weight (g)")
box()


# slide 52: Boxplot

boxplot(Births.tab$bweight,
        xlab="Birth weight (g)")

###############################
# MODIFYING THE DATA
###############################

# slide 55: SORTING BY SEX AND BIRTH WEIGHT

Birth_sort <- Births.tab[order(Births.tab$sex, Births.tab$bweight), ]

head(Birth_sort)

# slide 56: Creating new variables and deleting old

Births.tab$log_bweight <- log(Births.tab$bweight)
Births.tab <- Births.tab[ , -10]
# alternative: Births.tab$log_bweight<-NULL

# OR AS A SEPARATE OBJECT
log_bweight <- log(Births.tab$bweight)
rm(log_bweight)


# Slide 57: USING CUT

Births.tab$agegrp <- cut(Births.tab$matage,
                         breaks = c(20,30,35,40,45) )
summary(Births.tab[ , c("matage", "agegrp")])


# slide 59: Creating new variables with rowSums

Births.tab$score <- rowSums(Births.tab[ ,c(3,5,7)], na.rm = FALSE)

#REMOVE MISSING
Births.tab$scoreRM <- rowSums(Births.tab[ ,c(3,5,7)], na.rm = TRUE)

head(Births.tab)

# slide 61: SPLIT
Births.Male <- subset(Births.tab, sex == 1)
Births.Female <- subset(Births.tab, sex == 2)

# with bracket operator:
Births.Male <- Births.tab[Births.tab$sex == 1,]
Births.Female <- Births.tab[Births.tab$sex == 2,]

# slide 62: Subset

#SELECT 3 VARIABLES
Births.new <- subset(Births.tab, select=c(id, bweight, sex))

# slide 63: Aggregate data

# A new dataframe with mean birthweight for
# combinations of sex and preterm

PreSex <- aggregate(Births.tab$bweight, by=list(Preterm=Births.tab$preterm, Sex=Births.tab$sexalph),
                               mean)
PreSex

# slide 64: rbind

#APPEND
Births.Both <- rbind(Births.Male, Births.Female)
dim(Births.Both)
dim(Births.Male)
dim(Births.Female)

# slide 66: MERGE
agesex <- read.delim("Data/agesex.txt")
agesex

bp <- read.delim("Data/bp.txt")
bp

# slide 68: merge
merge_small <- merge(agesex, bp, by = "id", all = FALSE)
merge_small

# slide 69: merge
merge_large <- merge(agesex, bp, by = "id", all = TRUE)
merge_large

# slide 70: merge
merge_x <- merge(agesex, bp, by = "id", all.x = TRUE)
merge_x

# slide 71: merge
merge_y <- merge(agesex, bp, by = "id", all.y = TRUE)
merge_y

# slide 72: COUNT MISSING
is.na(merge_y$sex)

#COUNT MISSING FOR ONE VARIABLE
sum(is.na(merge_y$sex))

#COUNT FOR DATA FRAME
colSums(is.na(merge_y))

###############################
# Loops and Flow Control
###############################

# slide 74: FOR LOOP

for(i in 1:3) {
cat(i, "+", i, "=", i+i, "\n")
}

# slide 75: IF AND ELSE STATEMENTS

# if statement:
for(i in 1:3){
if (i==2) cat("This index is even:","\n")
cat(i,"\n")
}


#if else statement:
for(i in 1:3){
if (i==2) cat("The index is 2","\n") else
cat("The index is not 2","\n")
}

# slide 79: WHILE LOOP EXAMPLE

set.seed(886)
k<-0 # number of big parts (>2)
y<-abs(rnorm(1000)) # simulated part size
i<-0 # index of parts
# loop:
while(k<3 & i<1000){
i<-i+1
temp<-y[i]
k<-k+(temp>2)
}
i


# Slide 80: REPEAT LOOP EXAMPLE

set.seed(371)
eye.colors<-c("brown","blue","green","yellow","grey")
eyecolor<-data.frame(personId=1:100,color=
                     sample(eye.colors,100,rep=T))
i<-0
list.of.ids<-numeric(0) # patient ID list
#loop:
repeat {
  i<-i+1
  if(eyecolor$color[i]=="yellow" |
     eyecolor$color[i]=="blue") next
  list.of.ids<-c(list.of.ids,eyecolor$personId[i])
  if(i==100 | length(list.of.ids)==20) break
  }
list.of.ids


# GRAPHICS

Births <- read.table("Data/Births.txt", header=TRUE, sep="\t")

str(Births)

# Slide 87: HISTOGRAM

hist(Births$bweight)

# Slide 88

hist(Births$bweight, xlab = "Birth weight (g)",
     main = "Histogram of birth weight")

# Slide 89

hist(Births$bweight, xlab = "Birth weight (g)", las = 1,
     main = "Histogram of birth weight", col = 2, density = 7)

# Slide 91:

pdf("Graphics/my.histogram.pdf")
hist(Births$bweight, xlab = "Birth weight (g)", las = 1,
     main = "Histogram of birth weight", col = 2, density = 7)
dev.off()

svg("Graphics/my.histogram.svg")
hist(Births$bweight, xlab = "Birth weight (g)", las = 1,
     main = "Histogram of birth weight", col = 2, density = 7)
dev.off()

png("Graphics/my.histogram.png")
hist(Births$bweight, xlab = "Birth weight (g)", las = 1,
     main = "Histogram of birth weight", col = 2, density = 7)
dev.off()



# Slide 94: HISTOGRAMS AND BOX PLOTS

par(mfrow = c(2,2))

hist(Births$bweight, xlab = "Birth weight (g)",
     main = " ")

hist(Births$bweight, xlab = "Birth weight (g)",
     main = " ", breaks = 4)

hist(Births$bweight, xlab = "Birth weight (g)",
     main = " ", breaks = 2)

boxplot(Births$bweight, xlab = "Birth weight (g)")

# resetting the graphics display (not on slides)
par(mfrow = c(1,1))



# Slide 95: BASIC BOX PLOT
boxplot(Births$bweight, xlab = "Birth weight (g)", horizontal = TRUE,
        col = 6)
rug(Births$bweight)


# Slide 96: BOX PLOT FOR BOYS AND GIRLS
boxplot(bweight ~ sexalph, data = Births, las = 1,
       ylab = "Birth weight (g)", col = 2:3)

# Slide 97: BOX PLOT WHERE WE WANT TO MAKE OUR OWN AXIS
boxplot(bweight ~ sexalph, data = Births, las = 1,
        ylab = "Birth weight (g)", col = c("red","blue"), xaxt = "n")
axis(1 ,at=c(1,2),labels=c('Girl','Boy'))


# SCATTER PLOT

#USING A DATA SET FROM R ABOUT CARS AND MILAGE
library(car)

data(mtcars)
mtcars

str(mtcars)

# Slide 100: PLOT OF MILES PER GALLON AND CAR WEIGHT

plot(mtcars$wt, mtcars$mpg)

# Slide 101: A NICER PLOT
plot(mtcars$wt, mtcars$mpg, xlab = "Car weight (lbs/1000)",
     ylab = "Miles per gallon", las = 1, pch = 19)
abline(lm(mtcars$mpg ~ mtcars$wt), lty = 1, col = 3)

# Slide 102: PLOT WITH REFERENCE LINES
plot(mtcars$wt, mtcars$mpg, xlab = "Car weight (lbs/1000)",
     ylab = "Miles per gallon", las = 1, pch = 19)
abline(h = c(25, 30), col = c("red", "magenta"), lty = 2)
abline(v = c(2, 5), col = 4:5, lty = 3:4)

# Slide 103: ADD A SMOOTHED LINE
plot(mtcars$wt, mtcars$mpg, xlab = "Car weight (lbs/1000)",
     ylab = "Miles per gallon", las = 1)
abline(lm(mtcars$mpg ~ mtcars$wt), lty = 2, col = 4)
lines(lowess(mtcars$wt, mtcars$mpg), lty = 1, col = 2)

#Slide 105: USING SCATTERPLOT FROM LIBRARY(CAR)
#install.packages("car")
#library(car)

scatterplot(mpg ~ wt | cyl, data = mtcars, ylim = c(0,40),
            xlab = "Car weight (lbs/1000)",
            ylab = "Miles per gallon", las = 1,
            legend=list(coords="topright"),
            id =list(method="identify"),
            boxplots = "xy",col=2:4)

#Slide 107: SCATTERPLOT MATRIX
# LOOK AT THE VARIABLES
# mpg: MILES PER GALLON
# disp: DISPLACEMENT
# hp: HORSEPOWER
# drat: REAR AXLE RATIO
# wt: WEIGHT (lbs/1000)

scatterplotMatrix(~mpg+hp+drat+wt,data=mtcars,
  smooth=list(col.smooth=2,col.spread=3,lty.smooth=1),
  regLine=FALSE,ellipse=T)

# Slide 108

scatterplotMatrix( ~ mpg + hp + drat + wt | cyl, diagonal=list(method="boxplot"),
                   smooth=list(lty.smooth=1),
                   regLine=FALSE,data = mtcars, col = 2:6)

# Slide 111: LINE CHART

TreeData <-subset(Orange, Tree==1 | Tree==2)

TreeA <- subset(TreeData, Tree == 2)
TreeB <- subset(TreeData, Tree == 1)
par(mfrow = c(1,2))
#SCATTTER PLOT
plot(TreeA$age, TreeA$circumference,
     xlab = "Age (days)",
     ylab = "Circumference (mm)", las = 1)

#LINE PLOT
plot(TreeA$age, TreeA$circumference, type = "b",
     xlab = "Age (days)",
     ylab = "Circumference (mm)", las = 1)
par(mfrow = c(1,1))

#Slide 112: PLOT TWO TREES TOGETHER
plot(TreeA$age, TreeA$circumference, type = "b", lty = 1,
    xlab = "Age (days)",
    ylab = "Circumference (mm)", las = 1, col= 2)

lines(TreeB$age, TreeB$circumference, type = "b", col = 3, lty = 2)

legend(locator(1),                   # we will place it with a mouse click
      legend = c("A","B"),
      title = "Tree",
      lty = 1:2,
      col= 2:3)



# Slide 113: PLOT OF GROWTH OF 5 TREES WITH A FOR LOOP

treedata<-Orange
plot(treedata$age,treedata$circumference,pch=" ",xlab = 'Age',
ylab='Circumference',las=1)
for(i in 1:5){lines(treedata$age[treedata$Tree==i],
treedata$circumference[treedata$Tree==i],type="b",col=i+1)}
legend(x="topleft",paste("Tree #",1:5),pch=1,lty=1,col=2:6)

# Slide 114: Layout options

par(mfrow = c(2,2))

hist(Births$bweight, xlab = "Birth weight (g)",
     main = " ")

hist(Births$bweight, xlab = "Birth weight (g)",
     main = " ", breaks = 4)

hist(Births$bweight, xlab = "Birth weight (g)",
     main = " ", breaks = 2)

boxplot(Births$bweight, xlab = "Birth weight (g)")

par(mfrow = c(1,1))


# Slide 115: Layout options
layout.matrix<-matrix(c(1:3,rep(4,3)),nrow=2,ncol=3,byrow=T)
layout.matrix


# Slide 116:
layout(layout.matrix)

hist(Births$bweight, xlab = "Birth weight (g)",
     main = " ")

hist(Births$bweight, xlab = "Birth weight (g)",
     main = " ", breaks = 4)

hist(Births$bweight, xlab = "Birth weight (g)",
     main = " ", breaks = 2)

boxplot(Births$bweight, xlab = "Birth weight (g)",horizontal=T)
