library(ISLR)
library(corrplot)
library(MASS)
head(Hitters)
hitter = na.omit(Hitters)

summary(hitter) 
sum(is.na(hitter))
names(hitter)
attach(hitter)
hist(Salary, main ="Salaries of the MLB")

####
corrsalary = data.frame(AtBat, Hits, HmRun, Runs, RBI, Walks, Years, CAtBat,   
                     CHits, CHmRun, CRuns, CRBI, CWalks, as.numeric(League), as.numeric(Division), PutOuts,  
                    Assists, Errors, Salary, as.numeric(NewLeague))
scorr = cor(corrsalary)
corrplot(scorr)
names(hitter)
summary(hitter)

money = data.frame(AtBat, Hits, HmRun, Runs, RBI, Walks, Years, CAtBat,   
                   CHits, CHmRun, CRuns, CRBI, CWalks, PutOuts, as.numeric(Division), Salary)
summary(money)
mcorr = cor(money)
corrplot(mcorr)

boxplot(Salary~as.numeric(Division))
plot(Salary~PutOuts)
plot(Salary~CWalks)
plot(Salary~CRBI)
plot(Salary~CRuns)
plot(Salary~CHmRun)
plot(Salary~CHits)
plot(Salary~CAtBat)
boxplot(Salary~Years)
plot(Salary~Walks)
plot(Salary~RBI)
plot(Salary~Runs)
boxplot(Salary~HmRun)
plot(Salary~Hits)
plot(Salary~AtBat)

### Bad Residuals Plot
fit = glm(Salary~AtBat+ Hits+ HmRun+ Runs+ RBI+ Walks+ Years+ CAtBat+   
         CHits+ CHmRun+ CRuns+ CRBI+ CWalks+ PutOuts+ as.numeric(Division))
summary(fit)
stepAIC(fit)
fit2 = glm(formula = Salary ~ AtBat + Hits + Walks + CAtBat + CRuns + 
      CRBI + CWalks + PutOuts + as.numeric(Division)) #AIC = 3781
stepAIC(fit2) 
summary(fit2) #with career at bats
catbats = data.frame(Salary , AtBat , Hits , Walks , CAtBat , CRuns , CRBI , CWalks , 
                       PutOuts , as.numeric(Division))
ccorr = cor(catbats)
corrplot(ccorr)

fit3 = glm(Salary~AtBat+ Hits+ HmRun+ Runs+ RBI+ Walks+ Years+    
            CHits+ CHmRun+ CRuns+ CRBI+ CWalks+ PutOuts+ as.numeric(Division))
summary(fit3)#dropping the CAtBats severly changes your model.
stepAIC(fit3)
fit4 = glm(formula = Salary ~ AtBat + Hits + Walks + CRuns + CRBI + 
             CWalks + PutOuts + as.numeric(Division))
summary(fit4) #without career at bats
nocatbats = data.frame(Salary , AtBat , Hits , Walks , CRuns , CRBI , 
                         CWalks , PutOuts , as.numeric(Division))
ncorr = cor(nocatbats)
corrplot(ncorr)

#regression model includes salary, atbat, hits, walks, cruns, crbi, cwalks, putouts, and Division
plot(fit4)
#terrible diagnostics

###EDA
bc1 <- boxcox(Salary~., data = Hitters2) #to see if log transformation of the response is necessary. 
hist(Salary)
hist(log(Salary)) # hist(log(log(Salary))) hist(log(log(log(Salary))))
hist(RBI) #hist(log(RBI)) hist(log(RBI))
hist(AtBat) #hist(log(AtBat)) #hist(log(log(AtBat)))
hist(CHits)
hist(log(CHits)) #hist(log(log(CHits)))
hist(CWalks)
hist(log(CWalks))
hist(Assists)
hist(log(Assists)) #hist(log(log(Assists)))
hist(Hits) #hist(log(Hits))
hist(Walks) #hist(log(Walks))
hist(CHmRun)
hist(log(CHmRun))
hist(Errors)
hist(log(Errors))
hist(HmRun)
hist(log(HmRun)) #hist(log(log(HmRun)))
hist(Years)
hist(log(Years)) #hist(log(log(Years)))
hist(CRuns)
hist(log(CRuns)) #hist(log(log(CRuns)))
hist(Runs)
hist(log(Runs)) #hist(log(log(Runs)))
hist(CAtBat)
hist(log(CAtBat)) #hist(log(log(CAtBat)))
hist(CRBI)
hist(log(CRBI)) #hist(log(log(CRBI)))
hist(PutOuts)
hist(log(PutOuts)) #hist(log(log(PutOuts)))

atbat = AtBat
lsalary = log(Salary)
rbi = RBI
lchits = log(CHits)
lcwalks = log(CWalks)
lassists = log(Assists)
hits = Hits
walk = Walks
chmrun = CHmRun
years = Years
lyears = log(Years)
lcruns = log(CRuns)
lruns = log(Runs)
lcatbat = log(CAtBat)
lcrbi = log(CRBI)
lputouts = log(PutOuts)

logcorr = data.frame(AtBat, Hits, log(HmRun), log(Runs), log(RBI), Walks, log(Years), log(CAtBat),   
                               log(CHits), log(CHmRun), log(CRuns), log(CRBI), log(CWalks),
                     as.numeric(League), as.numeric(Division), log(PutOuts),  
                               log(Assists), log(Errors), log(Salary), as.numeric(NewLeague))
lcorr = cor(logcorr)
corrplot(lcorr)
