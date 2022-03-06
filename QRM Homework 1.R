#Linear Model Replication and Assumptions 
#Potter and Tavits, Impact of Campaign Finance Laws on Party Competition 
-----------------------------------------------------------------------------

#Codebook 
# cnty \ country name
# year \ year of current election
# rulelaw \ from World Bank
# polity \ Polity IV score
# thresh \ legal vote threshold 
# postenp \ ENP after current election
# preenp \ ENP prior to current election
# fundparity4 \ Metric presented in paper
# fundparity 3 \ Alternate drops direct funding
# directelig \ Direct funding eligibility
# demin \ Year first democratic
# demyear \ Number of democratic years
# fed \ Whether or not federal
# pres \ Whether or not presidential
# avemag \ Average district magnitude
# smd \ Whether or not SMD system
# fract \ Ethnolinguistic fractionalization
# donorlimit \ Whether limits on donations
# eligmedia \ Free media eligibility
# partyspend \ Whether limits on spending

#install.packages("olsrr") 
library(foreign)
raw.data <- read.dta("/Users/isiktopcu/Downloads/potter_tavits_data.dta")
head(raw.data, n=2)


#Raw Data OLS to see the outliers 
raw.data.ols <-lm(postenp ~ fundparity4
                  + demyears
                  + fed 
                  + pres 
                  + log(avemag) 
                  + fract 
                  + log(avemag):fract, 
                  data=raw.data)
summary(raw.data.ols)
confint(raw.data.ols, level = 0.95)

#plotting raw.data just to see how we're doing
par(mfrow=c(2,2))
plot(raw.data.ols)
#small heteroskedasticity problem (residuals vs fitted has a triangle shape and also from the scale-location graph which should've been horizontal 
#not normally distributed, q-q plot is u shaped.
#install.package("texdreg")
#removing the three outliers as mentioned in the paper, Potter & Tavits, p.84.
#the outliers discussed in the paper are 1,31 and 113 are Albania (2003), Brazil(2003) and Liberia(2012)
campaigns = subset(raw.data, postenp < 9.2) 

#running OLS on removed outliers data
full<-lm(postenp ~ fundparity4
         + demyears
         + fed 
         + pres 
         + log(avemag) 
         + fract 
         + log(avemag):fract, 
         data=campaigns)
summary(full)
#N = 90, given that 40 observations deleted due to NA and 3 were outliers. 
library(texreg)
rep.list <- list(full)
texreg(rep.list)
par(mfrow=c(2,2))
plot(full)
#creating a subset to check endogeneity using only newer democracies (post-1973). 
#is there inverse direction of correlation?
later1974<-subset(campaigns, demin>1973) # demin \ Year first democratic

#Density plots 
par(mfrow=c(3,3))
vars <- c("postenp","fundparity4", "demyears", "avemag","fract")
for(var in vars) plot(density(campaigns[,var],na.rm=TRUE), main=var)
#all seem pretty normal except avemag, which they already applied log. 
#let's see how log transformation normalized the average magnitude's long tail. 
plot(density(log(campaigns$avemag), na.rm=TRUE), main="log(avemag)") #better

----------------------------------------------------------
#OLS Assumptions
#Linear in parameters 
par(mar=c(1,1,1,1))
plot(full)
#Q-Q plot the tails don't seem so problematic. Although, we can apply Box-Cox

#one box-cox to rule them all 

boxcox(full) #lambda is O so take the log 
fullbc  = lm(log(postenp) ~ fundparity4
            + demyears
            + fed 
            + pres 
            + log(avemag) 
            + fract 
            + log(avemag):fract, 
            data=campaigns)

campaigns$y = log(campaigns$postenp)#create new variable 
par(mfrow=c(2,2))
plot(fullbc) #homoskedasticity seems better?

#Normality 
#Shapiro Wilk is usually used in sample sizes above 50,unfortunately the p values don't make sense. 
#Shapiro-Wilk Test 
vars <- c("postenp","fundparity4", "demyears", "avemag","fract")
shap <- for(var in vars)  {
  print(var)
  print(shapiro.test(campaigns[,var]))
}

#Lilliefors (Kolmogorov-Smirnov) Test
install.packages("tseries")
library(tseries)
vars <- c("postenp","fundparity4", "demyears", "avemag","fract")
shap <- for(var in vars)  {
  print(var)
  print(lillie.test(campaigns[,var]))
}
#Lilliefors p values also don't seem to make sense.Perhaps one should change it's parameters? 
#Anderson Darling 
library(nortest)
vars <- c("postenp","fundparity4", "demyears", "avemag","fract")
shap <- for(var in vars)  {
  print(var)
  print(ad.test(campaigns[,var]))
}

#No endogeneity   
#Testing if there is a similar correlation between fund parity and ENP in newer democracies
post1974<-lm(postenp ~ fundparity4
             + demyears
             + fed 
             + pres 
             + log(avemag) 
             + fract 
             + log(avemag):fract, 
             data=later1974)	
summary(post1974) #similar significance, evidence for exogeneity. 
pst74list.list <- list(post1974)
texreg(pst74list.list)


#construct the endogeneity plot in Figure 1, p. 87.
plot(campaigns$fundparity4 
     ~ campaigns$preenp, 
     pch=20, col="grey20", cex=1.5, 
     xlab="Previous ENP", 
     ylab="Current Fund Parity Value")
summary(lm(fundparity4 ~ preenp, data=campaigns))
abline(a=0.82, b=-0.04, lwd=2)
#second argument for endogeneity was if "pre" effective number of parties(preenp) and fund parity are linear. the causal relationship might be inverse.
#in this case, there is no significant correlation. 


#No autocorrelation in the errors 
durbinWatsonTest(full)
#lag Autocorrelation D-W Statistic p-value
#1       0.0982805      1.755332   0.136
#very high p value that is not significant and no evidence of autocorrelation 
#D-W = 1.5 - 2.5
bgtest(full)
#LM test = 0.92467, df = 1, p-value = 0.3363 
#very high p value that is not significant and no evidence of autocorrelation 
  
#No multicolliniearity 
#install.packages("car")
vif(full) #log(avemag) > 5, acceptable 

#No Heteroskecasticity
#Breusch-Pagan
# (H0): Homoscedasticity is present (the residuals are distributed with equal variance)
#if p_val > 0.05, we fail to reject the null and conclude there may not be heteroskedasticity.
#We can't fail to reject the null, no heteroscedasticity
bptest(full)
#BP = 7.449, df = 7, p-value = 0.3837
#Golfield - Quandt 
#install.packages("lmtest)
#Typically one chooses to remove around 20% of the total observations.(18)
gqtest(full, order.by = ~ fundparity4
       + demyears
       + fed 
       + pres 
       + log(avemag) 
       + fract 
       + log(avemag):fract, data = campaigns, fraction = 18)

#GQ = 1.1946, df1 = 28, df2 = 28, p-value = 0.3206
#(H0): Homoscedasticity is present.
#(HA): Heteroscedasticity is present.
#Since the p-value is not less than 0.05, we fail to reject the null hypothesis. 
#We do not have sufficient evidence to say that heteroscedasticity is present in the regression model.

#Outliers
outlierTest(full)
#    rstudent unadjusted p-value Bonferroni p
#113 4.425194         2.8737e-05    0.0026726
raw.out = lm(postenp ~ fundparity4
                 + demyears
                 + fed 
                 + pres 
                 + log(avemag) 
                 + fract 
                 + log(avemag):fract, 
                 data=raw.data[-113,])
summary(raw.out)
plot(raw.out) 


#Prediction seems impossible with so many NA values and lack of observations but it's below 
#install.packages('caTools')
#library(caTools)
#set.seed(123)
#split = sample.split(campaigns$postenp, SplitRatio = 0.8)
#training_set = subset(campaigns, split == TRUE)
#test_set = subset(campaigns, split == FALSE)

# Fitting Multiple Linear Regression to the Training set
#regressor = lm(postenp ~ fundparity4
#                     + demyears
#                     + fed 
#                     + pres 
#                     + log(avemag) 
#                     + fract 
#                     + log(avemag):fract, 
#                     data=training_set)
# Predicting the Test set results
#prediction = predict(regressor, newdata = test_set)
#data.frame(
  #RMSE = RMSE(prediction, test_set$postenp),
  #R2 = R2(prediction, test_set$postenp)
#)

# model to ensure that all fund parity metric components are exerting similarly-signed influences (mentioned in footnote 44, p.86)
components<-lm(postenp ~ directelig 
               + partyspend
               + donorlimit
               + eligmedia
               + demyears
               + fed 
               + pres 
               + log(avemag) 
               + fract 
               + log(avemag):fract, 
               data=campaigns)
summary(components)
comp <- list(components)
texreg(comp)


# model to ensure that differences between legal rules and actual empirical practice in a country are not driving their results (fn. 45)
rules.practice<-lm(postenp ~ fundparity4
                   + rulelaw
                   + fundparity4*rulelaw
                   + demyears
                   + fed 
                   + pres 
                   + log(avemag) 
                   + fract 
                   + log(avemag):fract, 
                   data=campaigns)
summary(rules.practice)
rules <- list(rules.practice)
texreg(rules)

# model to ensure that including legal threshold (which eliminates a large number of our observations due to data availability) does not undercut our results (fn. 56) 
threshold<-lm(postenp ~ fundparity4
              + thresh
              + demyears
              + fed 
              + pres 
              + log(avemag) 
              + fract 
              + log(avemag):fract,
              data=campaigns) 
summary(threshold)
thres <- list(threshold)
texreg(thres)