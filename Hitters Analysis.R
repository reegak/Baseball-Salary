library(ggplot2)
library(corrplot)
library(MASS)
library(effsize)
library(pROC)
library(ISLR)
library(gplots)
library(ROCR)
library(boot)
library(binom)
library(xtable)
library(rJava)
library(glmulti)
library(effsize)
library(tidyr)
library(glmnet)
library(foreach)
library(randomForest)
library(tidyverse)
library(car)

rm(list=ls(all=TRUE)) # remove all previous objects from memory
options(warn=-1)  # forces R to ignore all warning messages

nrow(Hitters)
head(Hitters)
?Hitters  # response: median value of homes (medv)
dim(Hitters)
sum(is.na(Hitters$Salary))
Hitters = na.omit(Hitters)
dim(Hitters)
attach(Hitters)

summary(Salary)

## EDA on Variables
# EDA on Salary Response Variable
bc = boxcox(Salary~1, data=Hitters)
bc = boxcox(Salary~., data=Hitters)
# optimal lambda
bc$x[bc$y==max(bc$y)]

ggplot(Hitters, aes(Salary)) + geom_histogram(bins = 50, color = "black", fill = "blue", alpha = 0.5) + theme_bw() +
  labs(title="Histogram for Salary", x="Salary", y="Count")

lSalary <- log(Salary)

ggplot(Hitters, aes(lSalary)) + geom_histogram(bins = 50, color = "black", fill = "blue", alpha = 0.5) + theme_bw() +
  labs(title="Histogram for Log Salary", x="Log-Salary", y="Count")
shapiro.test(lSalary)
shapiro.test(Salary)

#EDA on Predictor Variable ATBat
ggplot(Hitters, aes(AtBat)) + geom_histogram(bins = 50, color = "black", fill = "blue", alpha = 0.5) + theme_bw() +
  labs(title="Number of times at bat in 1986", x="AtBat", y="Count")

ggplot(Hitters, aes(x=AtBat, y=lSalary)) + geom_point(shape=1) + 
  geom_smooth(method=lm , color="red", se=FALSE) + labs(title="AtBat vs Log-Salary", x="AtBat", y="log-Salary")

#EDA on Predictor Variable Hits
ggplot(Hitters, aes(Hits)) + geom_histogram(bins = 50, color = "black", fill = "blue", alpha = 0.5) + theme_bw() +
  labs(title="Number of hits in 1986", x="Hits", y="Count")

ggplot(Hitters, aes(x=Hits, y=lSalary)) + geom_point(shape=1) + 
  geom_smooth(method=lm , color="red", se=FALSE) + labs(title="Hits vs Log-Salary", x="Hits", y="log-Salary")

#EDA on Predictor Variable HmRun
ggplot(Hitters, aes(HmRun)) + geom_histogram(bins = 50, color = "black", fill = "blue", alpha = 0.5) + theme_bw() +
  labs(title="Number of home runs in 1986", x="HmRun", y="Count")

lHmRun <- ifelse(HmRun>0,log(HmRun+0.15), -log(-HmRun+0.15))

ggplot(Hitters, aes(lHmRun)) + geom_histogram(bins = 20, color = "black", fill = "blue", alpha = 0.5) + theme_bw() +
  labs(title="Number of Log home runs in 1986", x="log-HmRun", y="Count")

ggplot(Hitters, aes(x=HmRun, y=lSalary)) + geom_point(shape=1) + 
  geom_smooth(method=lm , color="red", se=FALSE) + labs(title="HmRun vs Log-Salary", x="HmRun", y="log-Salary")

ggplot(Hitters, aes(x=lHmRun, y=lSalary)) + geom_point(shape=1) + 
  geom_smooth(method=lm , color="red", se=FALSE) + labs(title="Log-HmRun vs Log-Salary", x="log-HmRun", y="log-Salary")

#EDA on Predictor Variable Runs
ggplot(Hitters, aes(Runs)) + geom_histogram(bins = 50, color = "black", fill = "blue", alpha = 0.5) + theme_bw() +
  labs(title="Number of runs in 1986", x="Runs", y="Count")

ggplot(Hitters, aes(x=Runs, y=lSalary)) + geom_point(shape=1) + 
  geom_smooth(method=lm , color="red", se=FALSE) + labs(title="Runs vs Log-Salary", x="Runs", y="log-Salary")

#EDA on Predictor Variable RBI
ggplot(Hitters, aes(RBI)) + geom_histogram(bins = 50, color = "black", fill = "blue", alpha = 0.5) + theme_bw() +
  labs(title="Number of runs batted in in 1986", x="RBI", y="Count")

ggplot(Hitters, aes(x=RBI, y=lSalary)) + geom_point(shape=1) + 
  geom_smooth(method=lm , color="red", se=FALSE) + labs(title="RBI vs Log-Salary", x="RBI", y="log-Salary")

#EDA on Predictor Variable Walks
ggplot(Hitters, aes(Walks)) + geom_histogram(bins = 50, color = "black", fill = "blue", alpha = 0.5) + theme_bw() +
  labs(title="Number of walks in 1986", x="Walks", y="Count")

ggplot(Hitters, aes(x=Walks, y=lSalary)) + geom_point(shape=1) + 
  geom_smooth(method=lm , color="red", se=FALSE) + labs(title="Walks vs Log-Salary", x="Walks", y="log-Salary")

#EDA on Predictor Variable Years
ggplot(Hitters, aes(Years)) + geom_histogram(bins = 40, color = "black", fill = "blue", alpha = 0.5) + theme_bw() +
  labs(title="Number of years in the major leagues", x="Years", y="Count")

lYears <- log(Years)

ggplot(Hitters, aes(lYears)) + geom_histogram(bins = 5, color = "black", fill = "blue", alpha = 0.5) + theme_bw() +
  labs(title="Number of Log years in the major leagues", x="Log-Years", y="Count")

ggplot(Hitters, aes(x=Years, y=lSalary)) + geom_point(shape=1) + 
  geom_smooth(method=lm , color="red", se=FALSE) + labs(title="Years vs Log-Salary", x="Years", y="log-Salary")

ggplot(Hitters, aes(x=lYears, y=lSalary)) + geom_point(shape=1) + 
  geom_smooth(method=lm , color="red", se=FALSE) + labs(title="log-Years vs Log-Salary", x="log-Years", y="log-Salary")

#EDA on Predictor Variable CAtBat
ggplot(Hitters, aes(CAtBat)) + geom_histogram(bins = 50, color = "black", fill = "blue", alpha = 0.5) + theme_bw() +
  labs(title="Number of times at bat during his career", x="CAtBat", y="Count")

lCATBat <- log(CAtBat)

ggplot(Hitters, aes(lCATBat)) + geom_histogram(bins = 10, color = "black", fill = "blue", alpha = 0.5) + theme_bw() +
  labs(title="Number of Log times at bat during his career", x="Log-CAtBat", y="Count")

ggplot(Hitters, aes(x=CAtBat, y=lSalary)) + geom_point(shape=1) + 
  geom_smooth(method=lm , color="red", se=FALSE) + labs(title="CAtBat vs Log-Salary", x="CAtBat", y="log-Salary")

ggplot(Hitters, aes(x=lCATBat, y=lSalary)) + geom_point(shape=1) + 
  geom_smooth(method=lm , color="red", se=FALSE) + labs(title="log-CATBat vs Log-Salary", x="log-CATBat", y="log-Salary")

#EDA on Predictor Variable CHits
ggplot(Hitters, aes(CHits)) + geom_histogram(bins = 50, color = "black", fill = "blue", alpha = 0.5) + theme_bw() +
  labs(title="Number of hits during his career", x="CHits", y="Count")

lCHits <- log(CHits)

ggplot(Hitters, aes(lCHits)) + geom_histogram(bins = 35, color = "black", fill = "blue", alpha = 0.5) + theme_bw() +
  labs(title="Number of Log hits during his career", x="Log-CHits", y="Count")

ggplot(Hitters, aes(x=CHits, y=lSalary)) + geom_point(shape=1) + 
  geom_smooth(method=lm , color="red", se=FALSE) + labs(title="CHits vs Log-Salary", x="CHits", y="log-Salary")

ggplot(Hitters, aes(x=lCHits, y=lSalary)) + geom_point(shape=1) + 
  geom_smooth(method=lm , color="red", se=FALSE) + labs(title="log-CHits vs Log-Salary", x="log-CHits", y="log-Salary")

#EDA on Predictor Variable CHmRun
ggplot(Hitters, aes(CHmRun)) + geom_histogram(bins = 50, color = "black", fill = "blue", alpha = 0.5) + theme_bw() +
  labs(title="Number of home runs during his career", x="CHmRun", y="Count")

lCHmRun <- ifelse(CHmRun>0, log(CHmRun+0.15), -log(-CHmRun+0.15))

ggplot(Hitters, aes(lCHmRun)) + geom_histogram(bins = 20, color = "black", fill = "blue", alpha = 0.5) + theme_bw() +
  labs(title="Number of log home runs during his career", x="Log-CHmRun", y="Count")

ggplot(Hitters, aes(x=CHmRun, y=lSalary)) + geom_point(shape=1) + 
  geom_smooth(method=lm , color="red", se=FALSE) + labs(title="CHmRun vs Log-Salary", x="CHmRun", y="log-Salary")

ggplot(Hitters, aes(x=lCHmRun, y=lSalary)) + geom_point(shape=1) + 
  geom_smooth(method=lm , color="red", se=FALSE) + labs(title="log-CHmRun vs Log-Salary", x="log-CHmRun", y="log-Salary")

#EDA on Predictor Variable CRuns
ggplot(Hitters, aes(CRuns)) + geom_histogram(bins = 50, color = "black", fill = "blue", alpha = 0.5) + theme_bw() +
  labs(title="Number of runs during his career", x="CRuns", y="Count")

lCRuns <- log(CRuns)

ggplot(Hitters, aes(lCRuns)) + geom_histogram(bins = 30, color = "black", fill = "blue", alpha = 0.5) + theme_bw() +
  labs(title="Number of Log runs during his career", x="Log-CRuns", y="Count")

ggplot(Hitters, aes(x=CRuns, y=lSalary)) + geom_point(shape=1) + 
  geom_smooth(method=lm , color="red", se=FALSE) + labs(title="CRuns vs Log-Salary", x="CRuns", y="log-Salary")

ggplot(Hitters, aes(x=lCRuns, y=lSalary)) + geom_point(shape=1) + 
  geom_smooth(method=lm , color="red", se=FALSE) + labs(title="log-CRuns vs Log-Salary", x="log-CRuns", y="log-Salary")

#EDA on Predictor Variable CRBI
ggplot(Hitters, aes(CRBI)) + geom_histogram(bins = 50, color = "black", fill = "blue", alpha = 0.5) + theme_bw() +
  labs(title="Number of runs batted in during his career", x="CRBI", y="Count")

lCRBI <- log(CRBI)

ggplot(Hitters, aes(lCRBI)) + geom_histogram(bins = 40, color = "black", fill = "blue", alpha = 0.5) + theme_bw() +
  labs(title="Number of log runs batted in during his career", x="Log-CRBI", y="Count")

ggplot(Hitters, aes(x=CRBI, y=lSalary)) + geom_point(shape=1) + 
  geom_smooth(method=lm , color="red", se=FALSE) + labs(title="CRBI vs Log-Salary", x="CRBI", y="log-Salary")

ggplot(Hitters, aes(x=lCRBI, y=lSalary)) + geom_point(shape=1) + 
  geom_smooth(method=lm , color="red", se=FALSE) + labs(title="log-CRBI vs Log-Salary", x="log-CRBI", y="log-Salary")

#EDA on Predictor Variable CWalks
ggplot(Hitters, aes(CWalks)) + geom_histogram(bins = 50, color = "black", fill = "blue", alpha = 0.5) + theme_bw() +
  labs(title="Number of walks during his career", x="CWalks", y="Count")

lCWalks <- log(CWalks)

ggplot(Hitters, aes(lCWalks)) + geom_histogram(bins = 35, color = "black", fill = "blue", alpha = 0.5) + theme_bw() +
  labs(title="Number of log walks during his career", x="Log-CWalks", y="Count")

ggplot(Hitters, aes(x=CWalks, y=lSalary)) + geom_point(shape=1) + 
  geom_smooth(method=lm , color="red", se=FALSE) + labs(title="CWalks vs Log-Salary", x="CWalks", y="log-Salary")

ggplot(Hitters, aes(x=lCWalks, y=lSalary)) + geom_point(shape=1) + 
  geom_smooth(method=lm , color="red", se=FALSE) + labs(title="log-CWalks vs Log-Salary", x="log-CWalks", y="log-Salary")

#EDA on Predictor Variable PutOuts
ggplot(Hitters, aes(PutOuts)) + geom_histogram(bins = 50, color = "black", fill = "blue", alpha = 0.5) + theme_bw() +
  labs(title="Number of put outs in 1986", x="PutOuts", y="Count")

lPutOuts <- ifelse(PutOuts>0, log(PutOuts+0.15), -log(-PutOuts+0.15))

ggplot(Hitters, aes(lPutOuts)) + geom_histogram(bins = 30, color = "black", fill = "blue", alpha = 0.5) + theme_bw() +
  labs(title="Number of log put outs in 1986", x="Log-PutOuts", y="Count")

ggplot(Hitters, aes(x=PutOuts, y=lSalary)) + geom_point(shape=1) + 
  geom_smooth(method=lm , color="red", se=FALSE) + labs(title="PutOuts vs Log-Salary", x="PutOuts", y="log-Salary")

ggplot(Hitters, aes(x=lPutOuts, y=lSalary)) + geom_point(shape=1) + 
  geom_smooth(method=lm , color="red", se=FALSE) + labs(title="log-PutOuts vs Log-Salary", x="log-PutOuts", y="log-Salary")

#EDA on Predictor Variable Assists
ggplot(Hitters, aes(Assists)) + geom_histogram(bins = 50, color = "black", fill = "blue", alpha = 0.5) + theme_bw() +
  labs(title="Number of assists in 1986", x="Assists", y="Count")

lAssists <- ifelse(Assists>0, log(Assists+0.15), -log(-Assists+0.15))

ggplot(Hitters, aes(lAssists)) + geom_histogram(bins = 35, color = "black", fill = "blue", alpha = 0.5) + theme_bw() +
  labs(title="Number of Log assists in 1986", x="Log-Assists", y="Count")

ggplot(Hitters, aes(x=Assists, y=lSalary)) + geom_point(shape=1) + 
  geom_smooth(method=lm , color="red", se=FALSE) + labs(title="Assists vs Log-Salary", x="Assists", y="log-Salary")

ggplot(Hitters, aes(x=lAssists, y=lSalary)) + geom_point(shape=1) + 
  geom_smooth(method=lm , color="red", se=FALSE) + labs(title="log-Assists vs Log-Salary", x="log-Assists", y="log-Salary")

#EDA on Predictor Variable Errors
ggplot(Hitters, aes(Errors)) + geom_histogram(bins = 50, color = "black", fill = "blue", alpha = 0.5) + theme_bw() +
  labs(title="Number of errors in 1986", x="Errors", y="Count")

lErrors <- ifelse(Errors>0, log(Errors+0.15), -log(-Errors+0.15))

ggplot(Hitters, aes(lErrors)) + geom_histogram(bins = 20, color = "black", fill = "blue", alpha = 0.5) + theme_bw() +
  labs(title="Number of Log assists in 1986", x="Log-Errors", y="Count")

ggplot(Hitters, aes(x=Errors, y=lSalary)) + geom_point(shape=1) + 
  geom_smooth(method=lm , color="red", se=FALSE) + labs(title="Errors vs Log-Salary", x="Errors", y="log-Salary")

ggplot(Hitters, aes(x=lErrors, y=lSalary)) + geom_point(shape=1) + 
  geom_smooth(method=lm , color="red", se=FALSE) + labs(title="log-Errors vs Log-Salary", x="log-Errors", y="log-Salary")
##Probably not transform errors since the trend switches from neagative which makes sense to a positive one which does not.

#EDA on League Predictor variable
plot(factor(League), lSalary, col="cyan", 
        varwidth=T, ylab="lSalary",xlab="League" , main = "League vs Log-Salary")
ggplot(Hitters, aes(x=lSalary , fill = factor(League))) + geom_histogram(position="dodge", bins = 50) + theme_bw() +
  labs(title="Histogram for Log-Salary and League", x="Log-Salary", y="Count")

t.test(lSalary[League=="A"], lSalary[League=="N"])
# Cohen's d; standardized mean difference
# looking for a value above 0.2 (small), 0.5 (medium), and 0.8 (large)
cohen.d(lSalary, factor(League)) 

#EDA on NewLeague Predictor variable
plot(factor(NewLeague), lSalary, col="cyan", 
     varwidth=T, ylab="lSalary",xlab="NewLeague" , main = "NewLeague vs Log-Salary")
ggplot(Hitters, aes(x=lSalary , fill = factor(NewLeague))) + geom_histogram(position="dodge", bins = 50) + theme_bw() +
  labs(title="Histogram for Log-Salary and NewLeague", x="Log-Salary", y="Count")
t.test(lSalary[NewLeague=="A"], lSalary[NewLeague=="N"])
# Cohen's d; standardized mean difference
# looking for a value above 0.2 (small), 0.5 (medium), and 0.8 (large)
cohen.d(lSalary, factor(NewLeague)) 

#EDA on Division Predictor Variable
plot(factor(Division), lSalary, col="cyan", 
     varwidth=T, ylab="lSalary",xlab="Division" , main = "Division vs Log-Salary")
ggplot(Hitters, aes(x=lSalary , fill = factor(Division))) + geom_histogram(position="dodge", bins = 50) + theme_bw() +
  labs(title="Histogram for Log-Salary and Division", x="Log-Salary", y="Count")
t.test(lSalary[Division=="E"], lSalary[Division=="W"])
# Cohen's d; standardized mean difference
# looking for a value above 0.2 (small), 0.5 (medium), and 0.8 (large)
cohen.d(lSalary, factor(Division)) 

Hitvars = data.frame(lSalary, AtBat, Hits, lHmRun, Runs, RBI, Walks, lYears, lCATBat, lCHits, lCHmRun, lCRuns, lCRBI,
                     lCWalks, lPutOuts, lAssists, lErrors)
htv = cor(Hitvars)
corrplot(htv)

## lasso: Section 6.2, 6.6.2 of ISLR
# create design matrix without the intercept since that is required by glmnet
xs = model.matrix(lSalary ~ AtBat + Hits + lHmRun + Runs + RBI + Walks + lYears + lCATBat + lCHits + lCHmRun + lCRuns +
                    lCRBI + lCWalks + lPutOuts + lAssists + Division + League + NewLeague, Hitters)[,-1] 
grid = 10^seq(10,-2,length=100)
lasso.mod = glmnet(xs, lSalary, alpha=1, lambda=grid) # alpha=1 is L1 norm, lasso penalty
# x-axis is a measure of distance coeff is from 0, inversely related to penalty param
# so measures how much coefficients have been shrunk towards zero
plot(lasso.mod) 
#lasso.coef = predict(lasso.mod,type="coefficients")
# parameters to force lasso to perform linear regression (lm) 
# Dosen't work at the moment
#lasso.coef = predict(lasso.mod,type="coefficients", s=0, exact = T)  # s is penalty parameter lambda
#summary(lm(lSalary ~ AtBat + Hits + lHmRun + Runs + RBI + Walks + lYears + lCATBat + lCHits + lCHmRun + lCRuns +
#             lCRBI + lCWalks + lPutOuts + lAssists + lErrors, Hitters)) # same coeff as above lasso

# Finding an optimal lambda by training/testing, code from ISLR Section 6.6
grid=10^seq(10,-2,length=100)
set.seed(1)
x=model.matrix(lSalary ~ AtBat + Hits + lHmRun + Runs + RBI + Walks + lYears + lCATBat + lCHits + lCHmRun + lCRuns +
                 lCRBI + lCWalks + lPutOuts + lAssists + lErrors + Division + League + NewLeague, Hitters)[,-1]  
y=lSalary
train=sample(1:nrow(x), nrow(x)/2) # split data in half
test=(-train)
y.test=y[test]
lasso.mod=glmnet(x[train,],y[train],alpha=1,lambda=grid)
#plot(lasso.mod)
set.seed(1)
# minimizes squared-error loss (prediction error)
# run lasso on training set
cv.out=cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min  #0.006392416
bestlam
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])
mean((lasso.pred-y.test)^2) # prediction error for best lambda which is 0.2929137
out=glmnet(x,y,alpha=1,lambda=grid) # run lasso on whole data set
lasso.coef=predict(out,type="coefficients",s=bestlam)
lasso.coef
#lasso.coef[lasso.coef!=0]  # remove coeff shrunk to 0
#20 x 1 sparse Matrix of class "dgCMatrix"
#1
#(Intercept)  3.1437417615
#AtBat       -0.0001105722
#Hits         0.0033014102
#lHmRun       .           
#Runs         0.0022179599
#RBI          .           
#Walks        0.0038957180
#lYears       0.4435990464
#lCATBat      .           
#lCHits       .           
#lCHmRun      0.0753404155
#lCRuns       .           
#lCRBI        0.1386496438
#lCWalks      .           
#lPutOuts     0.0775119764
#lAssists     0.0086364972
#lErrors      .           
#DivisionW   -0.1214520302
#LeagueN      0.0944050444
#NewLeagueN   .        

FitFull_Lasso <- lm(lSalary ~ AtBat + Hits + Runs + Walks + lYears + lCHmRun + lCRBI +
                      lPutOuts + lAssists + Division + League, data = Hitters)
summary(FitFull_Lasso)

## Removed Runs
Fit_Lasso1 <- lm(lSalary ~ AtBat + Hits + Walks + lYears + lCHmRun + lCRBI +
                   lPutOuts + lAssists + Division + League, data = Hitters)
summary(Fit_Lasso1)

## Removed lAssists
Fit_Lasso2 <- lm(lSalary ~ AtBat + Hits + Walks + lYears + lCHmRun + lCRBI +
                   lPutOuts + Division + League, data = Hitters)
summary(Fit_Lasso2)

## Removed lCRBI
Fit_Lasso3 <- lm(lSalary ~ AtBat + Hits + Walks + lYears + lCHmRun +
                   lPutOuts + Division + League, data = Hitters)
summary(Fit_Lasso3)

## Removed League
Fit_Lasso4 <- lm(lSalary ~ AtBat + Hits + Walks + lYears + lCHmRun +
                   lPutOuts + Division, data = Hitters)
summary(Fit_Lasso4)

anova(Fit_Lasso4, FitFull_Lasso)

## Use Fit_Lasso4 is good P-value = 0.2242 means models are the same 

### Model Selection based off StepAIC

FitFull <- lm(lSalary ~ AtBat + Hits + lHmRun + Runs + RBI + Walks + lYears + lCATBat + lCHits + lCHmRun + lCRuns +
     lCRBI + lCWalks + lPutOuts + lAssists + lErrors + Division
   + League + NewLeague, data=Hitters)
step_model <- stepAIC(FitFull, direction="both") 
summary(step_model)

fit_step1 <- lm(lSalary ~ AtBat + Hits + RBI + Walks + lYears + lCATBat + lCRuns +
                  lCRBI + lCWalks + lPutOuts + Division + League, data=Hitters)
summary(fit_step1)

fit_step2 <- lm(lSalary ~ AtBat + Hits + Walks + lYears + lCATBat + lCRuns +
                  lCRBI + lCWalks + lPutOuts + Division + League, data=Hitters)
summary(fit_step2)

anova(fit_step2,FitFull)# P-value = 0.8192 means models are the same 

anova(fit_step2, Fit_Lasso4)


model = lm(lSalary ~ .*., data = Hitters)
summary(model)
#### Using Model from Fit_lasso4 on the different predictor to perform bagging and random forest trees
# Generate training and testing sets

Hitvarsl = data.frame(lSalary, AtBat, Hits, Walks, lYears, lCHmRun, 
                      lPutOuts, Division)

set.seed(1)
trainl = sample(1:nrow(Hitvarsl), nrow(Hitvarsl)/2)
hit.trainl = Hitvarsl[trainl,]
hit.testl = Hitvarsl[-trainl,]
# Perform regression #
fit_lml = lm(lSalary ~ AtBat + Hits + Walks + lYears + lCHmRun +
              lPutOuts + Division, data=hit.trainl) # linear model on all predictors
pred_lml = predict(fit_lml, hit.testl) # predicted test set
lm_MSEl = mean((pred_lml - hit.testl$lSalary)^2) # MSE ~ 0.2706304
lm_MSEl

# Bagging (bootstrap aggregration) a regression model fit 
set.seed(1)
iterations = 1000; n = nrow(hit.trainl)
predictions = foreach(m=1:iterations,.combine=cbind) %do% {
  # sample with replacement (bootstrap)
  training_positions = sample(nrow(hit.trainl), size=n, replace=TRUE)
  lm_fitl = lm(lSalary ~ AtBat + Hits + Walks + lYears + lCHmRun +
                lPutOuts + Division, data=hit.trainl[training_positions,])
  predict(lm_fitl, newdata=hit.testl)
}
# predictions has a 'iterations' number of predictions for each house in the testing data set
# so 253 x 1000
pred_bagl<-rowMeans(predictions)
bag_MSEl = sum((hit.testl$lSalary-pred_bagl)^2)/n # MSE ~ 0.2712446
bag_MSEl

# Bagging regression without bootstrap
#   randomly subset training data rather than bootstrap
set.seed(1)
bagging_lm = function(training, testing, length_divisor=4, iterations=1000)
{
  predictions<-foreach(m=1:iterations,.combine=cbind) %do% {
    training_positions = sample(nrow(training), size=floor((nrow(training)/length_divisor)))
    train_pos = 1:nrow(training) %in% training_positions
    # FUNCTION NOT AUTOMATED: must name response in following 'lm' call
    lm_fitl = lm(lSalary ~ AtBat + Hits + Walks + lYears + lCHmRun +
                  lPutOuts + Division, data=training[train_pos,])
    predict(lm_fitl,newdata=testing)
  }
  rowMeans(predictions)
}
# run the function
bagreg_predl = bagging_lm(hit.trainl, hit.testl)
bagreg_MSEl = sum((hit.testl$lSalary-bagreg_predl)^2)/n # MSE ~ 0.2607044
bagreg_MSEl

# Bagging a regression trees for kicks: 
#  show a learner with "high variance" that greatly benefits from bagging
set.seed(1)
# "tricking" random forest into building an overfit tree by setting mtry equal to number of variables
bag.hitl <- randomForest(lSalary ~ AtBat + Hits + Walks + lYears + lCHmRun +
                          lPutOuts + Division, data = hit.trainl, mtry = 19, ntree = 500)
yhat.bagl <- predict(bag.hitl, newdata = hit.testl)
yhat.bagl
bagRF_MSEl = mean((yhat.bagl - hit.testl$lSalary)^2) # MSE ~ 0.2740745
bagRF_MSEl

# Results
results = cbind(lm_MSEl, bag_MSEl, bagreg_MSEl, bagRF_MSEl)
colnames(results) = c("Regression", "Bagging", "No Bootstrap", "Bagging RF")
results
#      Regression  Bagging     No Bootstrap  Bagging RF
#      0.2706304   0.2712446   0.2607044     0.2740745


# Graphical comparison
truthl = hit.testl$lSalary
plot(y = pred_lml-truthl, x = truthl, xlab="Log Salary", ylab="Difference of Prediction from Truth")
lines(y = pred_bagl-truthl, x = truthl, type="p", col="blue")
lines(y = bagreg_predl-truthl, x = truthl, type="p", col="green")
lines(y = yhat.bagl-truthl, x = truthl, type="p", col="red")
abline(h=0, col="grey")
legend("bottomleft", 
       legend = c("LM 1", "Bootstrap aggr", "No Bootstrap", "Bagging trees"), 
       col = c("black", "blue", "green", "red"),
       text.col = "black",
       bty = "n",
       pch = c(1),
       horiz = F)



boxplot(pred_lm,pred_bag,bagreg_pred,yhat.bag, xaxt="n")
axis(1, at=1:4, labels=c("LM","Bagging","LM-Bag","RF"))


#### Using Model from SteAIC using the predictors lasso gave out on the different predictor to perform bagging and random forest trees
# Generate training and testing sets

Hitvars = data.frame(lSalary, AtBat, Hits, Walks, lYears, lCATBat, lCRuns, 
                       lCRBI, lCWalks, lPutOuts, Division, League)

set.seed(1)
train = sample(1:nrow(Hitvars), nrow(Hitvars)/2)
hit.train = Hitvars[train,]
hit.test = Hitvars[-train,]
# Perform regression #
fit_lm = lm(lSalary ~ AtBat + Hits + Walks + lYears + lCATBat + lCRuns +
              lCRBI + lCWalks + lPutOuts + Division + League, data=hit.train) # linear model on all predictors
pred_lm = predict(fit_lm, hit.test) # predicted test set
lm_MSE = mean((pred_lm - hit.test$lSalary)^2) # MSE ~ 0.3121002
lm_MSE

# Bagging (bootstrap aggregration) a regression model fit 
set.seed(1)
iterations = 1000; n = nrow(hit.train)
predictions = foreach(m=1:iterations,.combine=cbind) %do% {
  # sample with replacement (bootstrap)
  training_positions = sample(nrow(hit.train), size=n, replace=TRUE)
  lm_fit = lm(lSalary ~ AtBat + Hits + Walks + lYears + lCATBat + lCRuns +
                lCRBI + lCWalks + lPutOuts + Division + League, data=hit.train[training_positions,])
  predict(lm_fit, newdata=hit.test)
}
# predictions has a 'iterations' number of predictions for each house in the testing data set
# so 253 x 1000
pred_bag<-rowMeans(predictions)
bag_MSE = sum((hit.test$lSalary-pred_bag)^2)/n # MSE ~ 0.3020237
bag_MSE

# Bagging regression without bootstrap
#   randomly subset training data rather than bootstrap
set.seed(1)
bagging_lm = function(training, testing, length_divisor=4, iterations=1000)
{
  predictions<-foreach(m=1:iterations,.combine=cbind) %do% {
    training_positions = sample(nrow(training), size=floor((nrow(training)/length_divisor)))
    train_pos = 1:nrow(training) %in% training_positions
    # FUNCTION NOT AUTOMATED: must name response in following 'lm' call
    lm_fit = lm(lSalary ~ AtBat + Hits + Walks + lYears + lCATBat + lCRuns +
                  lCRBI + lCWalks + lPutOuts + Division + League, data=training[train_pos,])
    predict(lm_fit,newdata=testing)
  }
  rowMeans(predictions)
}
# run the function
bagreg_pred = bagging_lm(hit.train, hit.test)
bagreg_MSE = sum((hit.test$lSalary-bagreg_pred)^2)/n # MSE ~ 0.2723427
bagreg_MSE

# Bagging a regression trees for kicks: 
#  show a learner with "high variance" that greatly benefits from bagging
set.seed(1)
# "tricking" random forest into building an overfit tree by setting mtry equal to number of variables
bag.hit <- randomForest(lSalary ~ AtBat + Hits + Walks + lYears + lCATBat + lCRuns +
                          lCRBI + lCWalks + lPutOuts + Division + League, data = hit.train, mtry = 19, ntree = 500)
yhat.bag <- predict(bag.hit, newdata = hit.test)
yhat.bag
bagRF_MSE = mean((yhat.bag - hit.test$lSalary)^2) # MSE ~ 0.1959726
bagRF_MSE

# Results
results = cbind(lm_MSE, bag_MSE, bagreg_MSE, bagRF_MSE)
colnames(results) = c("Regression", "Bagging", "No Bootstrap", "Bagging RF")
results
#      Regression  Bagging     No Bootstrap  Bagging RF
#      0.3121002   0.2987299   0.2723427     0.1959726
xtable(results)

# Graphical comparison
truth = hit.test$lSalary
plot(y = pred_lm-truth, x = truth, xlab="Log Salary", ylab="Difference of Prediction from Truth")
lines(y = pred_bag-truth, x = truth, type="p", col="blue")
lines(y = bagreg_pred-truth, x = truth, type="p", col="green")
lines(y = yhat.bag-truth, x = truth, type="p", col="red")
abline(h=0, col="grey")
legend("bottomleft", 
       legend = c("LM 1", "Bootstrap aggr", "No Bootstrap", "Bagging trees"), 
       col = c("black", "blue", "green", "red"),
       text.col = "black",
       bty = "n",
       pch = c(1),
       horiz = F)



boxplot(pred_lm,pred_bag,bagreg_pred,yhat.bag, xaxt="n")
axis(1, at=1:4, labels=c("LM","Bagging","LM-Bag","RF"))




#Diagnostics
plot(predict(fit_step2), rstudent(fit_step2), ylab="Studentized Residuals", xlab="Predicted")
# Normality of Residuals
sresid <- studres(fit_step2)
hist(sresid, freq=FALSE,
     main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40)
yfit<-dnorm(xfit)
lines(xfit, yfit)
# Q-Q plot for studentized resid
qqPlot(fit_step2, main="QQ Plot", ylab="Studentized Residuals")
# Influential Observations
# Cook's D plot
# identify D values > 4/(n-k-1)
cutoff <- 4/((nrow(mtcars)-length(fit_step2$coefficients)-2))
plot(fit_step2, which=4, cook.levels=cutoff) # influence Plot

Hitters[173,]
Hitters[241,]

## Predicted values for certain players

exp(truth[70])
exp(pred_lm[70])
exp(pred_bag[70])
exp(bagreg_pred[70])
exp(yhat.bag[70])

exp(truth[115])
exp(pred_lm[115])
exp(pred_bag[115])
exp(bagreg_pred[115])
exp(yhat.bag[115])

exp(truth[20])
exp(pred_lm[20])
exp(pred_bag[20])
exp(bagreg_pred[20])
exp(yhat.bag[20])

hit.test[70,]
Hitters[131,]

hit.test[115,]
Hitters[223,]

hit.test[20,]
Hitters[33,]


sdfit=sd(fit_step2$resid)

John = data.frame(AtBat = 404, Hits = 92, Walks = 18, lYears = log(6), lCATBat = log(1354), lCRuns = log(188),
                  lCRBI = log(135), lCWalks = log(63), lPutOuts = log(222), League = "A", Division = "E")
JohnShelby=predict.lm(fit_step2,John); exp(JohnShelby+sdfit^2/2)
exp(predict(fit_step2, John, interval="prediction")+sdfit^2/2)

Steve = data.frame(AtBat = 461, Hits = 112, Walks = 35, lYears = log(2), lCATBat = log(680), lCRuns = log(76),
                   lCRBI = log(75), lCWalks = log(49), lPutOuts = log(111), League = "A", Division = "W")
SteveBuechele=predict.lm(fit_step2,Steve); exp(SteveBuechele+sdfit^2/2)
exp(predict(fit_step2, Steve, interval="prediction")+sdfit^2/2)

Bill = data.frame(AtBat = 217, Hits = 46, Walks = 9, lYears = log(4), lCATBat = log(694), lCRuns = log(86),
                   lCRBI = log(76), lCWalks = log(32), lPutOuts = log(307), League = "A", Division = "E")
BillSchroeder=predict.lm(fit_step2,Bill); exp(BillSchroeder+sdfit^2/2)
exp(predict(fit_step2, Bill, interval="prediction")+sdfit^2/2)