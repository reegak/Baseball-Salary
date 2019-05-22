library(ISLR)
library(MASS)
library(corrplot)
library(car)
library(KernSmooth)
library(leaps)
library(xtable)
library(foreach)
library(randomForest)
library(glmnet) 
library(tree)
sum(is.na(Hitters))

Hitters2<-na.omit(Hitters)

head(Hitters2)
names(Hitters2)

attach(Hitters2)

hist(AtBat)
hist(Salary)
hist(RBI)
hist(AtBat)
hist(CHits)
hist(CWalks)
hist(Assists)
hist(Hits)
hist(Walks)
hist(CHmRun)
hist(Errors)
hist(HmRun)
hist(Years)
hist(CRuns)
hist(Runs)
hist(CAtBat)
hist(CRBI)
hist(PutOuts)


###########################################
#### Checking for Log Transformation #######
############################################

bc1 <- boxcox(Salary~., data = Hitters2)
bc1$x[bc1$y==max(bc1$y)]


lSalary <- log(Salary)

hist(lSalary, breaks = 20)
hist(Salary)

########################################################
###LOWESS TO FIND FUNCTIONAL FORM OF VARIABLES###########
########################################################

plot(RBI,lSalary)
lines(lowess(RBI,lSalary), col="blue")  

plot(AtBat,lSalary)
lines(lowess(AtBat,lSalary), col="blue")  

plot(CWalks,lSalary)
lines(lowess(CWalks,lSalary), col="blue")  

plot(log(CWalks),lSalary)
lines(lowess(log(CWalks),lSalary), col="blue")  

plot(CHits,lSalary)
lines(lowess(CHits,lSalary), col="blue")  

plot(log(CHits),lSalary)
lines(lowess(log(CHits),lSalary), col="blue")  

plot(Assists,lSalary)
lines(lowess(Assists,lSalary), col="blue")  

plot(Hits,lSalary)
lines(lowess(Hits,lSalary), col="blue")  

plot(Walks,lSalary)
lines(lowess(Walks,lSalary), col="blue")  

plot(CHmRun,lSalary)
lines(lowess(CHmRun,lSalary), col="blue")  

plot(log(CHmRun),lSalary)
lines(lowess(log(CHmRun),lSalary), col="blue")  

plot(Years,lSalary)
lines(lowess(Years,lSalary), col="blue")  

plot(log(Years),lSalary)
lines(lowess(log(Years),lSalary), col="blue")  

plot(CRuns,lSalary)
lines(lowess(CRuns,lSalary), col="blue")  

plot(log(CRuns),lSalary)
lines(lowess(log(CRuns),lSalary), col="blue")  

plot(Runs,lSalary)
lines(lowess(Runs, lSalary), col="blue")  

plot(log(CAtBat),lSalary)
lines(lowess(log(CAtBat),lSalary), col="blue")  

plot(CRBI,lSalary)
lines(lowess(CRBI,lSalary), col="blue")  

plot(log(CRBI),lSalary)
lines(lowess(log(CRBI),lSalary), col="blue")  

plot(PutOuts,lSalary)
lines(lowess(PutOuts,lSalary), col="blue")  

plot(HmRun,lSalary)
lines(lowess(HmRun,lSalary), col="blue")  

plot(Errors,lSalary)
lines(lowess(Errors,lSalary), col="blue")  


boxplot(lSalary~League)
boxplot(lSalary~Division)

#Log transform below variables
lCWalks<-log(CWalks)
lCHits <-log(CHits)
lCRuns <-log(CRuns)
lCAtBat <-log(CAtBat)
lCRBI <- log(CRBI)
lYears <-log(Years)
lCHmRun <-log(CHmRun)

##########################
###CORRELATION MATRIX######
###########################

Hitters2vars = data.frame(as.numeric(Salary), as.numeric(Hits), as.numeric(RBI), as.numeric(Walks), 
                          as.numeric(Years), as.numeric(CAtBat), as.numeric(CRuns), as.numeric(CRBI), 
                          as.numeric(CWalks), as.numeric(League), as.numeric(Division), as.numeric(PutOuts),
                          as.numeric(AtBat), as.numeric(HmRun), as.numeric(Runs), as.numeric(Assists), 
                          as.numeric(CHits), as.numeric(CHmRun), as.numeric(Errors), as.numeric(NewLeague))
Hit2var = cor(Hitters2vars)
corrplot(Hit2var)

###################################
#### Univariate Analysis ##########
###################################

fit.Years<-lm(lSalary~Years, data = Hitters2) #2e-16
summary(fit.Years)
fit.CAtBat<-lm(lSalary~CAtBat, data = Hitters2) #2e-16
summary(fit.CAtBat)
fit.CRuns<-lm(lSalary~CRuns, data = Hitters2) #2e-16
summary(fit.CRuns)
fit.CRBI<-lm(lSalary~CRBI, data = Hitters2) #2e-16
summary(fit.CRBI)
fit.CWalks<-lm(lSalary~CWalks, data = Hitters2) #2e-16
summary(fit.CWalks)
fit.CHits<-lm(lSalary~CHits, data = Hitters2) #2e-16
summary(fit.CHits)
fit.CHmRun<-lm(lSalary~CHmRun, data = Hitters2) #2e-16
summary(fit.CHmRun)
# Keep CHits, remove other variables #

fit.AtBat<-lm(lSalary~AtBat, data = Hitters2)
summary(fit.AtBat)
fit.Hits<-lm(lSalary~Hits, data = Hitters2)
summary(fit.Hits)
# Keep AtBat, remove Hits #

Hitters2.b.vars = data.frame(as.numeric(Salary), as.numeric(RBI), as.numeric(Walks), 
                             as.numeric(League), as.numeric(Division), as.numeric(PutOuts),
                             as.numeric(AtBat), as.numeric(HmRun), as.numeric(Runs), as.numeric(Assists), 
                             as.numeric(Errors), as.numeric(NewLeague))
Hit2var.b = cor(Hitters2.b.vars)
corrplot(Hit2var.b)


###########################################
#### Model Building no/Interaction##########
###########################################


fit.cor<-lm(lSalary~AtBat+HmRun+Runs+RBI+Walks+lCHits+
              League+Division+PutOuts+Assists+Errors+NewLeague, data = Hitters2)
summary(fit.cor)

stepAIC(fit.cor, direction="both")  ##AIC = -285


fit.cor.1<-lm(lSalary ~ AtBat + Runs + RBI + Walks + lCHits + 
                League + Division + PutOuts + Assists + Errors, data = Hitters2)
summary(fit.cor.1)


#Remove Walks

fit.cor.2<-lm(lSalary ~ AtBat + Runs + RBI + lCHits + 
                League + Division + PutOuts + Assists + Errors, data = Hitters2)
summary(fit.cor.2)

#Remove Assists

fit.cor.3<-lm(lSalary ~ AtBat + Runs + RBI + lCHits + 
                League + Division + PutOuts + Errors, data = Hitters2)
summary(fit.cor.3)

#Remove Errors

fit.cor.4<-lm(lSalary ~ AtBat + Runs + RBI + lCHits + 
                League + Division + PutOuts, data = Hitters2)
summary(fit.cor.4)

#Remove RBI

fit.cor.5<-lm(lSalary ~ AtBat + Runs + lCHits + 
                League + Division + PutOuts, data = Hitters2)
summary(fit.cor.5)

#Remove AtBat

fit.cor.6<-lm(lSalary ~ Runs + lCHits + 
                League + Division + PutOuts, data = Hitters2)
summary(fit.cor.6)

#Remove League

fit.cor.7<-lm(lSalary ~ Runs + lCHits + 
                Division + PutOuts, data = Hitters2)
summary(fit.cor.7)

vif(fit.cor.7)

########################################
#### Model Building w/interactions #####
#######################################

logtransforms = data.frame(AtBat, HmRun, RBI, Walks, 
                           lCHits, lCRuns, League,
                           Division, PutOuts, Assists, Errors, NewLeague)


fit.interaction<-lm(lSalary~.*., data = logtransforms)
summary(fit.interaction)

stepAIC(fit.interaction, direction="both")  ##AIC = -977.24

fit.with.interactions<-lm(lSalary ~ AtBat + HmRun + RBI + Walks + lCHits + 
                            lCRuns + League + Division + PutOuts + Assists + Errors + 
                            NewLeague + AtBat:Walks + AtBat:lCHits + HmRun:lCHits + HmRun:League + 
                            HmRun:NewLeague + RBI:League + RBI:Errors + Walks:lCHits + 
                            Walks:lCRuns + Walks:Division + lCHits:lCRuns + lCHits:League + 
                            lCHits:Division + lCHits:Assists + lCHits:Errors + lCHits:NewLeague + 
                            lCRuns:League + lCRuns:Division + lCRuns:NewLeague + League:Division + 
                            League:NewLeague + Division:PutOuts + Division:Assists + 
                            PutOuts:Assists + League:PutOuts, data = logtransforms)
summary(fit.with.interactions)

#Remove Walks:Division

fit.with.interactions1<-lm(lSalary ~ AtBat + HmRun + RBI + Walks + lCHits + 
                             lCRuns + League + Division + PutOuts + Assists + Errors + 
                             NewLeague + AtBat:Walks + AtBat:lCHits + HmRun:lCHits + HmRun:League + 
                             HmRun:NewLeague + RBI:League + RBI:Errors + Walks:lCHits + 
                             Walks:lCRuns + lCHits:lCRuns + lCHits:League + 
                             lCHits:Division + lCHits:Assists + lCHits:Errors + lCHits:NewLeague + 
                             lCRuns:League + lCRuns:Division + lCRuns:NewLeague + League:Division + 
                             League:NewLeague + Division:PutOuts + Division:Assists + 
                             PutOuts:Assists + League:PutOuts, data = logtransforms)
summary(fit.with.interactions1)

#Remove PutOut:Division

fit.with.interactions2<-lm(lSalary ~ AtBat + HmRun + RBI + Walks + lCHits + 
                             lCRuns + League + Division + PutOuts + Assists + Errors + 
                             NewLeague + AtBat:Walks + AtBat:lCHits + HmRun:lCHits + HmRun:League + 
                             HmRun:NewLeague + RBI:League + RBI:Errors + Walks:lCHits + 
                             Walks:lCRuns + lCHits:lCRuns + lCHits:League + 
                             lCHits:Division + lCHits:Assists + lCHits:Errors + lCHits:NewLeague + 
                             lCRuns:League + lCRuns:Division + lCRuns:NewLeague + League:Division + 
                             League:NewLeague + Division:Assists + 
                             PutOuts:Assists + League:PutOuts, data = logtransforms)
summary(fit.with.interactions2)


#Remove CHits:Division

fit.with.interactions3<-lm(lSalary ~ AtBat + HmRun + RBI + Walks + lCHits + 
                             lCRuns + League + Division + PutOuts + Assists + Errors + 
                             NewLeague + AtBat:Walks + AtBat:lCHits + HmRun:lCHits + HmRun:League + 
                             HmRun:NewLeague + RBI:League + RBI:Errors + Walks:lCHits + 
                             Walks:lCRuns + lCHits:lCRuns + lCHits:League + 
                             lCHits:Assists + lCHits:Errors + lCHits:NewLeague + 
                             lCRuns:League + lCRuns:Division + lCRuns:NewLeague + League:Division + 
                             League:NewLeague + Division:Assists + 
                             PutOuts:Assists + League:PutOuts, data = logtransforms)
summary(fit.with.interactions3)

#Remove CRuns:Division

fit.with.interactions4<-lm(lSalary ~ AtBat + HmRun + RBI + Walks + lCHits + 
                             lCRuns + League + Division + PutOuts + Assists + Errors + 
                             NewLeague + AtBat:Walks + AtBat:lCHits + HmRun:lCHits + HmRun:League + 
                             HmRun:NewLeague + RBI:League + RBI:Errors + Walks:lCHits + 
                             Walks:lCRuns + lCHits:lCRuns + lCHits:League + 
                             lCHits:Assists + lCHits:Errors + lCHits:NewLeague + 
                             lCRuns:League + lCRuns:NewLeague + League:Division + 
                             League:NewLeague + Division:Assists + 
                             PutOuts:Assists + League:PutOuts, data = logtransforms)
summary(fit.with.interactions4)

#Remove NewLeague

fit.with.interactions5<-lm(lSalary ~ AtBat + HmRun + RBI + Walks + lCHits + 
                             lCRuns + League + Division + PutOuts + Assists + Errors + 
                             AtBat:Walks + AtBat:lCHits + HmRun:lCHits + HmRun:League + 
                             HmRun:NewLeague + RBI:League + RBI:Errors + Walks:lCHits + 
                             Walks:lCRuns + lCHits:lCRuns + lCHits:League + 
                             lCHits:Assists + lCHits:Errors + lCHits:NewLeague + 
                             lCRuns:League + lCRuns:NewLeague + League:Division + 
                             League:NewLeague + Division:Assists + 
                             PutOuts:Assists + League:PutOuts, data = logtransforms)
summary(fit.with.interactions5)

#Remove Division

fit.with.interactions6<-lm(lSalary ~ AtBat + HmRun + RBI + Walks + lCHits + 
                             lCRuns + League + PutOuts + Assists + Errors + 
                             AtBat:Walks + AtBat:lCHits + HmRun:lCHits + HmRun:League + 
                             HmRun:NewLeague + RBI:League + RBI:Errors + Walks:lCHits + 
                             Walks:lCRuns + lCHits:lCRuns + lCHits:League + 
                             lCHits:Assists + lCHits:Errors + lCHits:NewLeague + 
                             lCRuns:League + lCRuns:NewLeague + League:Division + 
                             League:NewLeague + Division:Assists + 
                             PutOuts:Assists + League:PutOuts, data = logtransforms)
summary(fit.with.interactions6)

#Remove CHits

fit.with.interactions7<-lm(lSalary ~ AtBat + HmRun + RBI + Walks + 
                             lCRuns + League + PutOuts + Assists + Errors + 
                             AtBat:Walks + AtBat:lCHits + HmRun:lCHits + HmRun:League + 
                             HmRun:NewLeague + RBI:League + RBI:Errors + Walks:lCHits + 
                             Walks:lCRuns + lCHits:lCRuns + lCHits:League + 
                             lCHits:Assists + lCHits:Errors + lCHits:NewLeague + 
                             lCRuns:League + lCRuns:NewLeague + League:Division + 
                             League:NewLeague + Division:Assists + 
                             PutOuts:Assists + League:PutOuts, data = logtransforms)
summary(fit.with.interactions7)

#Remove League:Division

fit.with.interactions8<-lm(lSalary ~ AtBat + HmRun + RBI + Walks + 
                             lCRuns + League + PutOuts + Assists + Errors + 
                             AtBat:Walks + AtBat:lCHits + HmRun:lCHits + HmRun:League + 
                             HmRun:NewLeague + RBI:League + RBI:Errors + Walks:lCHits + 
                             Walks:lCRuns + lCHits:lCRuns + lCHits:League + 
                             lCHits:Assists + lCHits:Errors + lCHits:NewLeague + 
                             lCRuns:League + lCRuns:NewLeague + 
                             League:NewLeague + Division:Assists + 
                             PutOuts:Assists + League:PutOuts, data = logtransforms)
summary(fit.with.interactions8)

#Remove Walks
fit.with.interactions9<-lm(lSalary ~ AtBat + HmRun + RBI  + 
                             lCRuns + League + PutOuts + Assists + Errors + 
                             AtBat:Walks + AtBat:lCHits + HmRun:lCHits + HmRun:League + 
                             HmRun:NewLeague + RBI:League + RBI:Errors + Walks:lCHits + 
                             Walks:lCRuns + lCHits:lCRuns + lCHits:League + 
                             lCHits:Assists + lCHits:Errors + lCHits:NewLeague + 
                             lCRuns:League + lCRuns:NewLeague + 
                             League:NewLeague + Division:Assists + 
                             PutOuts:Assists + League:PutOuts, data = logtransforms)
summary(fit.with.interactions9)

#Remove Walks:Hits
fit.with.interactions10<-lm(lSalary ~ AtBat + HmRun + RBI  + 
                              lCRuns + League + PutOuts + Assists + Errors + 
                              AtBat:Walks + AtBat:lCHits + HmRun:lCHits + HmRun:League + 
                              HmRun:NewLeague + RBI:League + RBI:Errors + 
                              Walks:lCRuns + lCHits:lCRuns + lCHits:League + 
                              lCHits:Assists + lCHits:Errors + lCHits:NewLeague + 
                              lCRuns:League + lCRuns:NewLeague + 
                              League:NewLeague + Division:Assists + 
                              PutOuts:Assists + League:PutOuts, data = logtransforms)
summary(fit.with.interactions10)

#Remove CRuns:Walks
fit.with.interactions11<-lm(lSalary ~ AtBat + HmRun + RBI  + 
                              lCRuns + League + PutOuts + Assists + Errors + 
                              AtBat:Walks + AtBat:lCHits + HmRun:lCHits + HmRun:League + 
                              HmRun:NewLeague + RBI:League + RBI:Errors + 
                              lCHits:lCRuns + lCHits:League + 
                              lCHits:Assists + lCHits:Errors + lCHits:NewLeague + 
                              lCRuns:League + lCRuns:NewLeague + 
                              League:NewLeague + Division:Assists + 
                              PutOuts:Assists + League:PutOuts, data = logtransforms)
summary(fit.with.interactions11)

#Remove HmRun:NewLeague
fit.with.interactions12<-lm(lSalary ~ AtBat + HmRun + RBI  + 
                              lCRuns + League + PutOuts + Assists + Errors + 
                              AtBat:Walks + AtBat:lCHits + HmRun:lCHits + HmRun:League + 
                              RBI:League + RBI:Errors + 
                              lCHits:lCRuns + lCHits:League + 
                              lCHits:Assists + lCHits:Errors + lCHits:NewLeague + 
                              lCRuns:League + lCRuns:NewLeague + 
                              League:NewLeague + Division:Assists + 
                              PutOuts:Assists + League:PutOuts, data = logtransforms)
summary(fit.with.interactions12)

#Remove League
fit.with.interactions13<-lm(lSalary ~ AtBat + HmRun + RBI  + 
                              lCRuns + PutOuts + Assists + Errors + 
                              AtBat:Walks + AtBat:lCHits + HmRun:lCHits + HmRun:League + 
                              RBI:League + RBI:Errors + 
                              lCHits:lCRuns + lCHits:League + 
                              lCHits:Assists + lCHits:Errors + lCHits:NewLeague + 
                              lCRuns:League + lCRuns:NewLeague + 
                              League:NewLeague + Division:Assists + 
                              PutOuts:Assists + League:PutOuts, data = logtransforms)
summary(fit.with.interactions13)

#Remove League:NewLeague
fit.with.interactions14<-lm(lSalary ~ AtBat + HmRun + RBI  + 
                              lCRuns + PutOuts + Assists + Errors + 
                              AtBat:Walks + AtBat:lCHits + HmRun:lCHits + HmRun:League + 
                              RBI:League + RBI:Errors + 
                              lCHits:lCRuns + lCHits:League + 
                              lCHits:Assists + lCHits:Errors + lCHits:NewLeague + 
                              lCRuns:League + lCRuns:NewLeague + 
                              Division:Assists + 
                              PutOuts:Assists + League:PutOuts, data = logtransforms)
summary(fit.with.interactions14)

vif(fit.with.interactions14)

###########################################
#### MODEL DIAGNOSTICS no Interactions ######
###########################################

plot(predict(fit.cor.7), rstudent(fit.cor.7), ylab="Studentized Residuals", xlab="Predicted")
identify(predict(fit.cor.7), rstudent(fit.cor.7), labels=row.names(Hitters2)) # 'escape to finish'
predict(fit.cor.7)[rstudent(fit.cor.7)==min(rstudent(fit.cor.7))]


sresid <- studres(fit.cor.7)
hist(sresid, freq=FALSE, xlab = "Residuals", main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40)
yfit<-dnorm(xfit)
lines(xfit, yfit, col = "blue")


qqPlot(fit.cor.7, main="QQ Plot", ylab="Studentized Residuals")


cutoff <- 4/((nrow(set2)-length(fit.cor.7$coefficients)-2))
plot(fit.cor.7, which=4, cook.levels=cutoff) # influence Plot


influencePlot(fit.cor.7, id.method="identify", 
              main="Influence Plot", sub="Circle size is proportial to Cook's Distance")

varif = vif(fit.cor.7)
varif 



###########################################
#### MODEL DIAGNOSTICS w/Interactions ######
###########################################

plot(predict(fit.with.interactions14), rstudent(fit.with.interactions14), ylab="Studentized Residuals", xlab="Predicted")
identify(predict(fit.with.interactions14), rstudent(fit.with.interactions14), labels=row.names(Hitters2)) # 'escape to finish'
predict(fit.with.interactions14)[rstudent(fit.with.interactions14)==min(rstudent(fit.with.interactions14))]


sresid <- studres(fit.with.interactions14)
hist(sresid, freq=FALSE, xlab = "Residuals", main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40)
yfit<-dnorm(xfit)
lines(xfit, yfit, col = "blue")


qqPlot(fit.with.interactions14, main="QQ Plot", ylab="Studentized Residuals")


cutoff <- 4/((nrow(set2)-length(fit.with.interactions14$coefficients)-2))
plot(fit.cor.7, which=4, cook.levels=cutoff) # influence Plot


influencePlot(fit.with.interactions14, id.method="identify", 
              main="Influence Plot", sub="Circle size is proportial to Cook's Distance")
vif(fit.with.interactions14)
#########################
######## Lasso  #########
#########################

x=model.matrix(Salary~., Hitters2)[,-1] 
y=Salary

grid=10^seq(10,-2,length=100)
lasso.mod = glmnet(x, Salary, alpha=1, lambda=grid) # alpha=1 is L1 norm, lasso penalty
plot(lasso.mod) 
lasso.coef = predict(lasso.mod,type="coefficients")
lasso.coef = predict(lasso.mod,type="coefficients", s=0)  # s is penalty parameter lambda
summary(lm(Salary~., Hitters2)) # same coeff as above lasso

set.seed(1)
train=sample(1:nrow(x), nrow(x)/2) # split data in half
test=(-train)
y.test=y[test]
lasso.mod=glmnet(x[train,],y[train],alpha=1,lambda=grid)
plot(lasso.mod)
set.seed(1)
# minimizes squared-error loss (prediction error)
# run lasso on training set

test=(-train)
y.test=y[test]
cv.out=cv.glmnet(x[train,],y[train],alpha=1)

plot(cv.out)
bestlam=cv.out$lambda.min  #16.78016
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])
mean((lasso.pred-y.test)^2) # prediction error for best lambda
out=glmnet(x,y,alpha=1,lambda=grid) # run lasso on whole data set
lasso.coef=predict(out,type="coefficients",s=bestlam)
lasso.coef
lasso.coef[lasso.coef!=0]  

mean((lasso.pred-y.test)^2) #100743.4

set.seed (1)
train = sample(1:nrow(Hitters2), nrow(Hitters2)/2)
tree.Hitters=tree(Salary~ . ,Hitters2 ,subset=train)
summary(tree.Hitters)

plot(tree.Hitters)
text(tree.Hitters ,pretty=0)

bag.Hitters=randomForest(Salary~ .,data=Hitters2,subset=train,
                         mtry=19,importance =TRUE)
bag.Hitters

yhat.bag = predict(bag.Hitters ,newdata=Hitters2[-train ,])
plot(yhat.bag, Hitters.test)
abline(0,1)
mean((yhat.bag-Hitters.test)^2)


rf.Hitters=randomForest(Salary~ .,data=Hitters2,subset=train,
                        mtry=7,importance =TRUE)
yhat.rf = predict(rf.Hitters ,newdata=Hitters2[-train ,])

importance(rf.Hitters)

varImpPlot (rf.Hitters)

###########################
#### Finding best MSE #####
###########################


#create a new dataframe with hitters for the logsalary

Hitters3<-data.frame(lSalary, Hits, RBI, Walks, 
                     Years, CAtBat, CRuns, CRBI, 
                     CWalks, League, Division, PutOuts,
                     AtBat, HmRun, Runs, Assists, 
                     CHits, CHmRun, Errors, NewLeague)

# Generate training and testing sets

set.seed(1)
train = sample(1:nrow(Hitters3), nrow(Hitters3)/2)
Hitters.train = Hitters3[train,]
Hitters.test = Hitters3[-train,]

# Perform regression
fit_lm = lm(lSalary~., data=Hitters.train) 
pred_lm = predict(fit_lm, Hitters.test) # predicted test set
lm_MSE = mean((pred_lm - Hitters.test$lSalary)^2) # MSE ~ 0.378902

# Bagging (bootstrap aggregration) a regression model fit 
set.seed(1)
iterations = 1000; n = nrow(Hitters.train)
predictions = foreach(m=1:iterations,.combine=cbind) %do% {
  # sample with replacement (bootstrap)
  training_positions = sample(nrow(Hitters.train), size=n, replace=TRUE)
  lm_fit = lm(lSalary ~ ., data=Hitters.train[training_positions,])
  predict(lm_fit, newdata=Hitters.test)
}

pred_bag<-rowMeans(predictions)
bag_MSE = sum((Hitters.test$lSalary-pred_bag)^2)/n # MSE ~ 0.3672615
plot(pred_bag)
# Bagging regression without bootstrap
# randomly subset training data rather than bootstrap
set.seed(1)
bagging_lm = function(training, testing, length_divisor=4, iterations=1000)
{
  predictions<-foreach(m=1:iterations,.combine=cbind) %do% {
    training_positions = sample(nrow(training), size=floor((nrow(training)/length_divisor)))
    train_pos = 1:nrow(training) %in% training_positions
    # FUNCTION NOT AUTOMATED: must name response in following 'lm' call
    lm_fit = lm(lSalary ~ ., data=training[train_pos,])
    predict(lm_fit,newdata=testing)
  }
  rowMeans(predictions)
}
bagreg_pred = bagging_lm(Hitters.train, Hitters.test)
bagreg_MSE = sum((Hitters.test$lSalary-bagreg_pred)^2)/n # MSE ~ 0.3568651
# Results
results = cbind(lm_MSE, bag_MSE, bagreg_MSE)
colnames(results) = c("Regression", "Bagging", "No Bootstrap")
results
