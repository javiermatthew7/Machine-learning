################################################################################
#
# Final_Project Shaz,Matthew
#
################################################################################
# External Functions
################################################################################
library(faraway)
library(smallstuff)
library(ISLR)
library(MASS)
library(broom)
################################################################################
# Internal Functions
################################################################################
source("smallstuff2.R")
################################################################################
# Save the environment
################################################################################
parSave = par(no.readonly = TRUE)
#par(parSave)
################################################################################


# Preprocessing dataset
data("birthwt")
? birthwt
head(birthwt)
summary(birthwt)
dim(birthwt) # 189 x 10


#######
# Question 1
# What it is: Risk Factors Associated with Low Infant Birth Weights

# This dataset from the MASS package contains information on 189 babies
# and their mothers, collected at Baystate Medical Center, Springfield, In 1986




# How it was obtained:
# Data was collected directly from the hospital, including maternal details like
# age, weight and smoking status, It aims to identify factors increasing the risk
# of low birth weight

# Variables:
# 1. Low = Indicator of birth weight < 2.5 kg
# 2. Age = Mother's age in years
# 3. Lwt = Mother's weight at last menstrual period (lbs)
# 4. Race = Mother's race (1 = White, 2 = Black, 3 = Other)
# 5. Smoke = Smoking Status during pregnancy
# 6. Ptl = Number of previous premature labors
# 7. Ht = History of hypertension
# 8. UI = Presence of uterine irritability
# 9. Ftv = number of physician visits during the first trimester
# 10. Bwt = Birth weight in grams.


# Why did you choose this dataset:
# 1. Includes bith categorical and numerical variables for regression analysis.
# 2. Explore maternal health, socioeconomic factors, and birth outcomes.
# 3. Based on real hospital data, providing real-world healthcare insights.



#Question 2: Check data for obvious errors and fix it.


sum(is.na(birthwt)) # No missing values

str(birthwt)



#Question 3
birthwt$low = as.factor(birthwt$low)
birthwt$smoke = as.factor(birthwt$smoke)
birthwt$ht = as.factor(birthwt$ht)
birthwt$ui = as.factor(birthwt$ui)



#question 4 check for identifiability

testmodel <- lm(age ~ ., birthwt)
summary(testmodel)
length(coef(testmodel)) # 10
testmodel$rank #10
# It is identifiable since the rank is the same.



#question 5: performing linear
# Full model

lmodf <- lm(bwt ~ ., birthwt)
summary(lmodf) #R2 = 0.6632

#lmoda (2nd model)

lmoda <- lm(bwt ~ low + age + smoke, birthwt)
summary(lmoda)

#lmodb 3rd model

lmodb <- lm(bwt ~ smoke + ptl + ht, birthwt)
summary(lmodb)

#lmod c ( 4th model )

lmodc <- lm(bwt ~ low + smoke + lwt, birthwt)
summary(lmodc)


#Possibly interaction

lmodl = lm(bwt ~ lwt * smoke, birthwt)
summary(lmodl)
# bwt = 2350.578 + 5.387lwt + 41.384smoke1 - 2.422lwt:smoke1 + eps



#Question 6

#Test: H0: lmoda is better vs H1: lmodf is better.
anova(lmoda, lmodf)

#The p-value is 0.00133 < 0.05, so we have enough evidence to reject H0
# and conclude that the full model(lmodf) is statistically significantly better
# than lmoda with low, age and smoke predictors.

#Test: H0: lmodb is better vs H1: lmodf is better
anova(lmodb, lmodf) #p value = 2.2e-16

# The p value is 2.2e-16 < 0.05, so we have enough evidence to reject
# H0 and conclude that the full model(lmodf) is statistically significantly better
# than lmodb with smoke, ptl and ht predictors


#Test: H0: lmodc is better vs H1: lmodf is better
anova(lmodc, lmodf) # p value = 0.002275

# The p-value is 0.002275 < 0.05, so we have enough evidence to reject
#H0 and conclude that the full model(lmodf) is statistically significantly better
#than lmodc with low,smoke and lwt predictors

#Test: H0: lmodl is better vs H1: lmodf is better
anova(lmodl, lmodf) # p = 2.2e-16

#The p-value = 2.2e-16 < 0.05, so we have enough evidence to reject H0 and conlude that
# the full model(lmodf) is statistically significantly better than lmodl


# Question 10 (We did this first to get find the high leverage values) we cannot do
# question 7 ,residual plots and etc, without finding if there are high leverage.


hatv <- hatvalues(lmodf)
length(hatv[hatv > 2 * mean(hatv)]) # 18 high leverage points
halfnorm(hatv)
qqlineHalf(hatv)
# Looks like we have 18 high leverage points so for the rest of the
# analysis we will be using standard residual plots.
# to confirm if theres a funnel shape and check normality using the standard residuals.

#Finding outliers
summary(lmodf) #R2 = 0.6632
rstudent(lmodf)[abs(rstudent(lmodf)) > 3] #226 index
range(rstudent(lmodf)) # min-2.408658  max = 4.182269
#we confirmed that there is an outlier 4.182269 because exceeding the range of the outlier 3

#model without outlier

out <- as.numeric(rstudent(lmodf)[abs(rstudent(lmodf)) > 3])
lmodff <- update(lmodf, subset = -out)
summary(lmodff)
#After removing the outlier we found negligible difference in R2 lmodff =0.6643, lmodf = 0.6632

#Finding the influential points.
cook = cooks.distance(lmodff)
cook[cook > .5]
plot(lmodff, 5)
# There are no influential points so we can conclude that there are unusual observation
# but not big significant enough to affect the model.



#Question 7

plot(rstandard(lmodff) ~ fitted(lmodff))
#Since there's a gap we will do the var.test to find out
#if the variance is constant or not.
var.test(rstandard(lmodff)[fitted(lmodff) > 2500], rstandard(lmodff)[fitted(lmodff) < 2800])
#Since the p value = 0.09237 > 0.05, so we do not have enough evidence to reject
#H0, so we retain it and conclude that the model is homoscedascity.

# Just to make sure we did another test lecture 11 slide 18 to further verify it.
summary(lm(sqrt(abs(residuals(
  lmodff
))) ~ fitted(lmodff))) # p-value: 0.1698
# p-value: 0.1698 > 0.05 so we do not have enough evidence to reject H0, so we retain it
# and conclude that the model is homoscedascity


#Check for normality

qqnorm(rstandard(lmodff))
qqline(rstandard(lmodff))

#It looks like its normally distributed.
# but we need to do a verification by hypothesis test

shapiro.test(rstandard(lmodff)) #p-value = 0.07886

#Since the p- value 0.078 > 0.05, we retain the null hypothesis of normality
# and conclude that the residuals are normally distributed.

#as of now we have to create a model with significance predictors
summary(lmodff)

lmods <- lm(bwt ~ low + race + smoke + ui, birthwt)
summary(lmods) # R2 0.6557
#Now adding more predictors to get more accurate results

lmodss <- update(lmods,  ~ low * age + ui)
summary(lmodss) # R2 = 0.6617


lmods3 <- update(lmodss, sqrt(bwt) ~ .)
summary(lmods3) # R2 = 0.6855 :)
plot(rstandard(lmods3) ~ fitted(lmods3))

var.test(rstandard(lmods3)[fitted(lmods3) > 53], rstandard(lmods3)[fitted(lmods3) <
                                                                     54]) #p value = 0.9833
?rstandard
#The p-value is 0.9833 >0.05 so we do not have enough evidence to reject H0, so we retain it.
# and conclude that the model is homoscedasticity


#Question 8

#boxplot
levels(birthwt$smoke) = c('No', 'Yes')
plot(age ~ smoke,
     birthwt,
     col = c('cornflowerblue', 'salmon'),
     main = 'Age vs smoking status')

#barplot

birthwt$race <- as.factor(birthwt$race)
levels(birthwt$race) = c('White', 'Black', 'Other')
ct = table(birthwt$ht, birthwt$race)



barplot(
  ct,
  beside = T,
  xlab = 'RACE',
  ylab = 'Count',
  main = 'Hypertension by Race',
  col = 2:3,
  ylim = c(0, 100)
)
legend('topright', levels(birthwt$ht), fill = 2:3)


birthwt$race <- as.numeric(birthwt$race)

plot(
  birthwt$lwt,
  col = 4,
  pch = 19,
  main = 'Mothers weight(lbs) at last menstrual period ',
  ylab = 'Weight in lb'
)
abline(h = mean(birthwt$lwt),
       lwd = 3,
       col = 'red')


#Question 12

#step1
#Collinearity
X_1 = model.matrix(lmods3)[, -1]
head(X_1)
summary(X_1)



#step 2
#Now doing pairwise correlation
round2(cor(X_1), 2)
#all 1's are diagonal which they are orthogonal


#step 3
#vif #variance inflation factor

vif(lmods3) #lecture 13 slide 10
# low predictor is giving 23.817 vif value which is greater than 10, and since its over 10
# we have a problem and we will try to remove it to best fit the model. and get rid of the
# collinearity between predictors.

lmods4 <- update(lmods3,  ~ . - low)
summary(lmods4) #R2 = 0.6853 #R2 went down for this but it is very small and we can neglect it.
summary(lmods3) #R2 = 0.6855

# To best fit the model we removed the predictor low because it was
# giving a higher vif value (23). after removing it the R2 went from 0.6855 to 0.6853
# we can say that this new model is still the best model we can get from this dataset analayis.



#Question 9 # prediction and explanation

range(birthwt$age)
(birthwt$low)

coef(lmods4)

(df = data.frame(
  low = c(1, 0),
  age = c(18, 45),
  ui = c(0, 1)
))
df$low = as.factor(df$low)
df$ui = as.factor(df$ui)

predict(lmods4, df, interval = "pred") #
# 95% CI (40.34981, 56.19909)
# 95% CI 2nd observation (47.54240, 64.31121)

?birthwt
#First observation
# We are 95% confident that if the baby's weight is more than 2.5 kg, and the mother's age is 18
# years old, and there's no presence of uterine irritability, the average weight of the baby will be between
# 40.349 grams and 56.199 grams

#Second observation

# We are 95% confident that if the baby's weight is less than 2.5 kg, and the mother's age is 45
# years old, and there is a presence of uterine irritability, the average weight of the baby will be between
# 47.542 grams and 64.311 grams




#question 11 partial regression and or partial residual plots.

#partial residual plot

termplot(
  lmods4,
  partial.resid = T,
  terms = 'age',
  main = 'Partial Residual Plot',
  pch = 19
)

# No sign of non-linearity. so partial graphs shows its linear. # its spread out


# Question 15



#Question 13 reporting on the steps and interpretation in context

# Step 1: was us deciding which data set to use and we landed on
# Risk Factors Associated with Low Infant Birth Weight(birthwt) since it has integers and factors that we could use

# step 2: Was checking for factors and missing data (No missing data), and changed the variables to the right data type which is a factor

# step 3: we needed to check if it was identifiable first and since the rank was the same and also it satisfied the normal equation, it was identifiable

# step 4: Was choosing for the response and predictors. We chose birth weight in grams(bwt)
# and initially made a full model with all the predictors and made 3-4 models and did an anova test on them
# to see which model has the improve R2. and while doing this the Null hypotheses was always rejected
# and the Alternative one (fullmodel) was found to be the best at that point.


# step 5: Since the full model was chosen we needed check if there were high leverage values and there was 18 high leverage points therefore we will be using standard residual plots.
# to check if it has a funnel shape and normality using standard residuals.


# step 6: was to find if there was outliers, we confirmed that there is an outlier 4.182269 because exceeding the range of the outlier 3
# and then updated the model and removed the outlier, it did not affect the R2 that much but we still removed it.


# step 7: we checked for influential points using cooks, and found that there are no influential points so we can conclude that there are unusual observation
# but not  significant enough to affect the model. since it was below the cook's distance (0.5)


#step 8: Now we needed to check for the assumptions we were making that is multivariate normal assumptions (constant variance and normality)
# For the residual plot we found a gap and needed to do a var.test to see if the variance was constant or not. p = 0.09237 > 0.05 and there was homoscedascity

# now we needed to check for normality assumption by creating halfnormal resid plot, it looked normally distributed to confirm our assumptions
# we did a shapiro hypothesis check, Since the p- value 0.078 > 0.05 we conclude that the residuals are indeed normally distributed.



#step 9: Since all of our assumptions was correct now we needed to create a model with more significant predictors to get a higher R2 after experimenting
# we landed on lmods3 that has low*age + ui interaction with an R2 of = 0.6855 we did a var.test on this and conluded that there was homoscedacity


#step 10: now we have the best fitted model we needed to check for collinearity and after experimenting we found it that they were orthogonal as all the 1's are in diag

# we did vif to see which predicor was giving a greater than 10 and found that the low predic was giving the value 23 and removed it to best fit the model.
#R2 went down for this but it was very small and we can neglect it.



##Question 14 conclusion

# We can say that the better  model was lmods4 with an R2 = 0.68531 compared to the fullmodel R2 = 0.66318
# For every 10 years old in age increase if every other predictors are held constant the we predict that
# the newborn baby's weight increases by 0.6388 grams
summary(lmodf)$r.sq # 0.6631813
summary(lmods4)$r.sq # 0.6853195

birthwt$low
