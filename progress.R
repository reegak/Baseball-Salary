library(ISLR)
library(corrplot)
library(MASS)
head(Hitters)
hitter = na.omit(Hitters)

summary(hitter) 
sum(is.na(hitter))
names(hitter)
attach(hitter)

####
corrsalary = data.frame(AtBat, Hits, HmRun, Runs, RBI, Walks, Years, CAtBat,   
                        CHits, CHmRun, CRuns, CRBI, CWalks, as.numeric(League), as.numeric(Division), PutOuts,  
                        Assists, Errors, Salary, as.numeric(NewLeague))
scorr = cor(corrsalary)
corrplot(scorr)

hist(Salary, main ="Salaries of the MLB")
bc1 <- boxcox(Salary~., data = Hitters) #to see if log transformation of the response is necessary. 
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

lSalary = log(Salary)
lCHits = log(CHits)
lCWalks = log(CWalks)
lAssists = log(Assists)
lCHmRun = log(CHmRun)
lErrors = log(Errors)
lHmRun = log(HmRun)
lYears = log(Years)
lCRuns = log(CRuns)
lRuns = log(Runs)
lCAtBat = log(CAtBat)
lCRBI = log(CRBI)
lPutOuts = log(PutOuts)
lAtBat = log(AtBat)
lHits = log(Hits)
lRBI = log(RBI)
lWalks = log(Walks)
### 20 variables ###
fit = lm(lSalary~ RBI+ AtBat+ CHits+ CWalks+ Assists+ Hits+ Walks+ CHmRun+ Errors+ HmRun+
         Years+ CRuns+ Runs+ CAtBat+CRBI+ PutOuts+ NewLeague+ Division+
           League, data = hitter)
summary(fit)
stepAIC(fit, direction = "both")
###After AIC###
fit2 = lm(lSalary ~ AtBat + CWalks + Assists + Hits + Walks + Errors + 
            HmRun + Years + CRuns + PutOuts + Division + League, data = hitter)
summary(fit2)
###without Errors###
fit3 = lm(lSalary ~ AtBat + CWalks + Assists + Hits + Walks +  
  HmRun + Years + CRuns + PutOuts + Division + League, data = hitter)
summary(fit3)

###without Assists###
fit4 = lm(formula = lSalary ~ AtBat + CWalks +  Hits + Walks + 
            HmRun + Years + CRuns + PutOuts + Division + League, data = hitter)
summary(fit4)
###without HomeRuns ###
fit5 = lm(formula = lSalary ~ AtBat + CWalks + Hits + Walks +  
           Years + CRuns + PutOuts + Division + League, data = hitter)
summary(fit5)
###without League###
fit6 = lm(formula = lSalary ~ AtBat + CWalks + Hits + Walks + Years + 
            CRuns + PutOuts + Division , data = hitter)
summary(fit6)
par(mfrow = c(2,2))
plot(fit6)
par(mfrow = c(1,1))
##logtransform = data.frame( lSalary, RBI, AtBat, lCHits, lCWalks, lAssists, Hits, Walks, lCHmRun, lErrors, lHmRun,
#             lYears, lCRuns, lRuns, lCAtBat,lCRBI, lPutOuts, NewLeague, Division,
#             League)
#model = lm(lSalary~ .*., data = logtransform)

boxplot(lSalary~League)
boxplot(lSalary~Division)
model = lm(lSalary~ RBI+ AtBat+ lCHits+ lCWalks+ lAssists+ Hits+ Walks+ lCHmRun+ lErrors+ lHmRun+
                      lYears+ lCRuns+ lRuns+ lCAtBat+lCRBI, data = hitter)
sum(is.na(lCHmRun))
