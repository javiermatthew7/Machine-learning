################################################################################
#
# final project
#
# Young, Matthew
#
################################################################################
# External Functions
################################################################################
library(smallstuff)
library(data.table)
library(MASS)          #boxcox, LDA, QDA
library(sur)
library(readtext)
library(tm)
library(leaps)
library(glmnet)
################################################################################
# Internal Functions
unknownToNA=function(x){
  x[x=='unknown']=NA
  return(x)
}
################################################################################
source("smallstuff2.R")
################################################################################
# Save the environment 
################################################################################
parSave=par(no.readonly = TRUE)
#par(parSave)
################################################################################
# Processing 
################################################################################


# load data and transform into a data table ##################################
bankdt=fread('../data_final/bank.csv')
head(bankdt,3)

# variable description ########################################################
rt=readtext("../data_final/bank_names.txt")
text=unlist(strsplit(rt$text,'\n'))
cat(text[(grep('Input variables:',text)-1):grep('Output',text)+1],sep = '\n')

# recreate dataset with relevant variables (variables that will be used) #####
bank=copy(bankdt)
names(bank)
vars=names(bank)[c(10,11,16)] # removing variables: day, month, poutcome, 
bank[,(vars):=NULL]
bank[1:3]

# convert categorical variable to factor #####################################
bank[,lapply(.SD,class)]
vars=names(bank)[c(2,3,4,5,7,8,9,14)]
bank[,(vars):=lapply(.SD,factor),.SDcols = vars]
bank[,lapply(.SD,class)]
summary(bank)

# convert unknowns to NA #####################################################
bank[,lapply(.SD,class)]
(vars=names(bank)[c(1:13)])
bank=bank[,(vars):=lapply(.SD,unknownToNA),.SDcols = vars]
colSums(is.na(bank)) # job, education, and contact have missing values

# deal with missing values ###################################################
# job variable
levels(bank$job) # 11 levels + NA
bank[,mean(is.na(job))] # 0.841% NA
# imputing missing job by mode
(job_mode=bank[!is.na(job),.N,by=job][order(-N)])
bank[is.na(job),job:=job_mode[1,1]] # missing job replaced with management
bank[,job:=droplevels(job)]

# education variable
levels(bank$education) # 3 levels + NA
bank[,mean(is.na(education))] # 4.13% NA
# observations with missing education are assigned the most common education 
# level among others with the same job
bank[is.na(education),.N, by=job] # missing education in each job group
edu_mode=bank[!is.na(education),.N, by=.(job,education)][order(job,-N)]
(edu_mode=edu_mode[,.SD[1],by=job])
# most frequent education level in each job
bank[is.na(education),education:=edu_mode[.SD,on='job',education]]
bank[,education:=droplevels(education)]

# contact variable
levels(bank$contact) # cellular, telephone, NA
bank[,mean(is.na(contact))] # 29.3% NA
# treat missing value as third category
bank[,contact:=as.character(contact)]
bank[is.na(contact),contact:='missing']
bank[,contact:=factor(contact)]
levels(bank$contact) # new level: cellular, telephone, missing

# aggregate categorical variables ##########################################
# convert jobs into 2-level categorical variable to make simpler job-related
# interpretation: employed vs. unemployed
employed=bank[,unique(job)][c(2,3,4,5,6,7,8,10)] 
# admin, blue-collar, entrepreneur, housemaid, management, self-employed,
# services, technician
unemployed=bank[,unique(job)][c(1,9,11)] # retired, student, unemployed
bank[,job_group:= ifelse(job %in% employed, 'employed',
                         ifelse(job %in% unemployed, 'unemployed',NA))]
bank[,job_group:=factor(job_group)]
bank[,.N,keyby=job_group]
bank[,job:=NULL][] # job variable replaced by job_group 

# convert pdays to factor variable ###########################################
# while pdays tells how many days passed after the client was last contacted 
# from a previous campaign, -1 indicates the client was never contacted 
# this is factor + scale variable
# converting into factor with levels: no & yes
bank[,pdays:=ifelse(pdays==-1,'no','yes')]
bank[,pdays:=factor(pdays)]
bank[,.N,by=pdays]

# aggregate contact variable ###################################################
bank[,.N,by=contact]
# to make comparison between customers who had been contacted and who had not 
# been contacted, convert contact into 2-level categorical variable with levels:
# known vs. missing
bank[,contact_method:=ifelse(contact=='missing','missing','known')]
bank[,contact_method:=factor(contact_method)]
bank[,contact:=NULL]
bank[,.N,by=contact_method] # contact variable replaced by contact_method

summary(bank)
sum(is.na(bank))

# split dataset into training and testing set ################################
# training: 80%, testing: 20%
(n=nrow(bank))
set.seed(24L)
tr=sample(n,.8*n)
bankTrain=bank[tr]
bankTest=bank[-tr]

# candidate models ###########################################################
# step 1: logistic regression using all variables
gmod=glm(y~.,family = binomial,bankTrain)
summary(gmod)
# age, marital, default, balance, previous are not significant

# step 2: best subset selection
X1=model.matrix(y~.,bankTrain)[,-1]
X1t=model.matrix(y~.,bankTest)[,-1]
colnames(X1)
subs=regsubsets(y~.,bankTrain,nvmax = 15)
subsum=summary(subs)

# step 3: plot and compare BIC, CP, and adjusted R2
par(mfrow=c(2,2))
plot(subsum$rsq,type='b',xlab='Number of Variables',ylab='Rsq',
     main='R2 vs. Number of Variables')
# adjusted R2
plot(subsum$adjr2,type='b',xlab='Number of Variables',ylab='Adjs Rsq',
     main='Adjusted R2 vs. Number of Variables')
(idx1=which.max(subsum$adjr2)) # 10 predictors
points(idx1,subsum$adjr2[idx1],pch='X',col='red',cex=2)
# BIC
plot(subsum$bic,type='b',xlab='Number of Variables',ylab='BIC',
     main='BIC vs. Number of Variables')
(idx2=which.min(subsum$bic)) # 6 predictors
points(idx2,subsum$bic[idx2],pch='X',col='red',cex=2)
# CP
plot(subsum$cp,type='b',xlab='Number of Variables',ylab='Cp',
     main='CP vs. Number of Variables')
(idx3=which.min(subsum$cp) )# 10 predictors
points(idx3,subsum$cp[idx3],pch='X',col='red',cex=2)
par(mfrow=c(1,1))

# step 4: recreate logistic regression based on selected predictors
# according to min BIC
coef(subs,idx2)
gmod6=glm(y~housing+loan+duration+pdays+job_group+contact_method,
          family = binomial,bankTrain)
summary(gmod6)
# all variables are significant

# making models with 7 and 8 variables
coef(subs,7L)
gmod7=glm(y~education+housing+loan+duration+pdays+job_group+contact_method,
          family = binomial,bankTrain)
summary(gmod7)

coef(subs,8)
gmod8=glm(y~education+housing+loan+duration+campaign+pdays+job_group+
            contact_method,family = binomial,bankTrain)
summary(gmod8)
# comparing all 3 logistic regression models
BIC(gmod6,gmod7,gmod8)

# step 5: cross validation on selected model
# perform CV on gmod6
set.seed(24L);CVerror(gmod6,10) # 11.09%
set.seed(24L);CVerror(gmod6,5) # 11.14%
# test error rate
(gmod6err=logistErrorRate(gmod6,bankTest)$errorRate) # 8.84%

# perform CV on gmod7
set.seed(24L);CVerror(gmod7,10) # 11.17%
set.seed(24L);CVerror(gmod7,5) # 11.14%
# test error rate
(gmod7err=logistErrorRate(gmod7,bankTest)$errorRate) # 9.17%

# perform CV on gmod8
set.seed(24L);CVerror(gmod8,10) # 11.23%
set.seed(24L);CVerror(gmod8,5) # 11.23%
# test error rate
(gmod8err=logistErrorRate(gmod8,bankTest)$errorRate) # 9.39%

(gmodTable=data.table(model=rep('logistic regression',3),
                      numVar=c(6,7,8),
                      errorRate=round2(c(gmod6err,gmod7err,gmod8err),2)))
# logistic regression with 6 variables is the best

# step 6: LDA model on selected variables
# LDA on gmod6 (6 variables)
ldamod=lda(y~housing+loan+duration+pdays+job_group+contact_method,bankTrain)

# step 7: cross validation on LDA models
set.seed(24L);CVerror(ldamod,10) # 11.20%
set.seed(24L);CVerror(ldamod,5) # 11.09%
# LDA model testing error rate 
(ldamoderr=mean(predict(ldamod,bankTest)$class!=bankTest$y)*100) # 10.06%
# not as good as logistic regression models

# step 8: KNN model on selected variables
# KNN on gmod6 (6 variables)
Ks=c(1,seq(5,40,by=5),seq(50,200,by=10),seq(250,1000,by=50))
kmod=KNN(y~housing+loan+duration+pdays+job_group+contact_method,bankTrain)

# step 9: cross validation on KNN models
# 5-fold and 10-fold CV
knnerr=NULL;knnerr2=NULL
for (i in seq_along(Ks)){
  set.seed(24L);knnerr[i]=CVerror(kmod,5,K=i)
  set.seed(24L);knnerr2[i]=CVerror(kmod,10,K=i)
}
par(mfrow=c(2,1))
# 5-fold CV
plot(knnerr~Ks,type='b',main='KNN 5-fold CV',xlab='K',ylab='Error Rate (%)')
(k1=which.min(knnerr))
points(Ks[k1],min(knnerr),pch='X',col='red',cex=2)
# 10-fold CV
plot(knnerr2~Ks,type='b',main='KNN 10-fold CV',xlab='K',ylab='Error Rate (%)')
(k2=which.min(knnerr2))
points(Ks[k2],min(knnerr2),pch='X',col='red',cex=2)
par(mfrow=c(1,1))

# test error rate for KNN model with 6 variables
# 5-fold CV
(kmoderr5=mean(predict(kmod,bankTest,K=k1)$class!=bankTest$y)*100) # 10.06%
# 10-fold CV
(kmoderr10=mean(predict(kmod,bankTest,K=k2)$class!=bankTest$y)*100) # 9.83

# step 10: lasso using all variables
lambdas=c(seq(1000,2,by=-1),seq(1,0,by=-.001))
lassomod=glmnet(X1,bankTrain$y,'binomial',lambda = lambdas)

# step 11: cross validation on lasso model
# 5-fold
set.seed(24L)
lassocv5=cv.glmnet(X1,bankTrain$y,lambda = lambdas,family='binomial',
                   nfolds = 5)
(lamlasso5=lassocv5$lambda.min) # 0.002
# 10-fold
set.seed(24L)
lassocv10=cv.glmnet(X1,bankTrain$y,lambda = lambdas,family='binomial',
                    nfolds = 10)
(lamlasso10=lassocv10$lambda.min) # 0.001

# test error rate for lasso regression
# 5-fold CV
pred1=predict(lassomod, X1t, s=lamlasso5,type='response')
yhat=rep(levels(bankTest$y)[1],nrow(bankTest))
yhat[pred1[,1]>.5]=levels(bankTest$y)[2]
(lassomodcv5err=mean(bankTest$y!=yhat)*100) # 9.28%
# 10-fold CV
pred2=predict(lassomod, X1t, s=lamlasso10,type='response')
yhat1=rep(levels(bankTest$y)[1],nrow(bankTest))
yhat1[pred2[,1]>.5]=levels(bankTest$y)[2]
(lassomodcv10err=mean(bankTest$y!=yhat1)*100) # 9.39%

coef(lassomod,s=lamlasso5) # 5-fold, 11 variables
coef(lassomod,s=lamlasso10) # 10-fold, 11 variables

# step 12: compare all method
modTable=
    data.table(model=
                 c(rep('logistic regression',3),'LDA','KNN 5-CV','KNN 10-CV',
                   'LASSO 5-CV','LASSO 10-CV'),
               numVar=c(6,7,8,rep(6,3),11,11),
               errorRate=
                 round2(c(gmod6err,gmod7err,gmod8err,ldamoderr,kmoderr5,
                          kmoderr10,lassomodcv5err,lassomodcv10err),2))
(modTable=modTable[order(errorRate)])

col1=rep('grey',nrow(modTable))
col1[order((modTable$errorRate))[1:2]]=c('navy','skyblue')
barplot(modTable$errorRate,names.arg=modTable$model,space=c(.5,.5),col=col1, 
        ylim=c(0,12),cex.names = .8,border = NA,
        main='Test Error Rates Comparison',ylab='Error Rate (%)')

# dummy model
bankTrain[,summary(y)/.N]
# all customers are predicted not to open up a new savings account
bankTest[,mean(y!='no')]*100
# 9.28% error rate, only logistic regression models with 6 and 7 variables have
# lower error rates

# we will work with logistic regression models
par(mfrow=c(1,2))
ROCcurve(gmod6,bankTest) 
ROCcurve(gmod7,bankTest) 
par(mfrow=c(1,1))

# work with gmod6 (6 variables)
round2(coef(gmod6),5) # best model
# y = -3.08 - 0.795housingYes - 0.813loanYes + 0.00432duration + 1.02pdaysYes 
#     + 0.402jobUnemployed - 1.01contactMissing + eps
plot(duration~y,bankTrain,col=c('lightcoral','steelblue'),ylab='Duration',
     xlab='Customer Response',main='Duration vs. Customer Response')

# sensitivity, specificity, precision, false positive, false negative
(lr6=logistErrorRate(gmod6,bankTest)$result)

lr6[2,2]/lr6[3,2]*100 # sensitivity: 32.1% (true positive)
# among customers who opened a savings account, 32.1% were correctly classified
# as likely to subscribe

lr6[1,1]/lr6[3,1]*100 # specificity: 97.2% (true negative)
# model correctly identified 97.2% of customers who did not subscribe

lr6[2,2]/lr6[2,3]*100 # precision: 54.0%
# out of all customers predicted to subscribe, 54.0% actually subscribed
# according to this model, over half of the customers flagged by the model as
# likely to subscribe actually followed through

lr6[2,1]/lr6[3,1]*100 # false positive: 2.80%
# out of all customers who did not subscribe, 2.80% were incorrectly predicted
#as likely to subscribe

lr6[1,2]/lr6[3,2]*100 # false negative: 67.9%
# our of all customers who actually subscribed, 67.9% were missed by the model
# failed to identify 67.9% of potential subscribers

100-logistErrorRate(gmod6,bankTest)$errorRate # accuracy: 91.2%
# the model correctly classified 91.2% of all customer outcomes, whether
# subscribing or not


# step 13: using best model, make prediction on randomly created data
# new dataset: mimic original data distribution
# obtain probabilities of each variable
coef(gmod6)
housingProb=table(bankTrain$housing)/nrow(bankTrain)
loanProb=table(bankTrain$loan)/nrow(bankTrain)
pdaysProb=table(bankTrain$pdays)/nrow(bankTrain)
jobProb=table(bankTrain$job_group)/nrow(bankTrain)
contactProb=table(bankTrain$contact_method)/nrow(bankTrain)

# sample factor variables
nPred = 450

set.seed(24)
housingNew=sample(levels(bankTrain$housing),nPred,replace=T,prob=housingProb)
loanNew=sample(levels(bankTrain$loan),nPred,replace=T,prob=loanProb)
pdaysNew=sample(levels(bankTrain$pdays),nPred,replace=T,prob=pdaysProb)
jobNew=sample(levels(bankTrain$job_group),nPred,replace=T,prob=jobProb)
contactNew=sample(levels(bankTrain$contact_method),nPred,replace=T,
                  prob=contactProb)

# new data table with 6 variables
dt1=data.table(
  housing=factor(housingNew,levels = levels(bankTrain$housing)),
  loan=factor(loanNew,levels = levels(bankTrain$loan)),
  pdays=factor(pdaysNew,levels = levels(bankTrain$pdays)),
  job_group=factor(jobNew,levels = levels(bankTrain$job_group)),
  contact_method=factor(contactNew,levels = levels(bankTrain$contact_method)),
  duration=sample(bankTrain$duration,nPred,replace = T)
)
summary(dt1)
dt2=copy(dt1) # for predictions on other models

# making prediction using the best model (gmod6)
set.seed(24)
yProb=predict(gmod6,dt1,type = 'response')
yhat=ifelse(yProb > .5, 'yes','no')
dt1[,y:=factor(yhat)]
summary(dt1)

mean(yhat=='yes')*100 # 3.11% predicted to subscribe 
