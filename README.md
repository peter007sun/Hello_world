# Hello_world
First project on Github 

#***************************************************************************************************
#  Yang Sun 
#  
#  Stat 101A Project. 
#
#  Descriptions: 
#
#  These consist of finishing times for a 10-mile race held in Washington D.C. this past Spring.
#  (cherryblossom.org) The times are in 'seconds'. Net.Time is the time it took from when the 
#  running crossed the starting line to when he or she crossed the finish line.
#   
#***************************************************************************************************




####### import the data ##########

data1<- read.csv("final.exam.csv", header = T)
data1<- data1[,-1]

# Run a simple regression, and find the confidence interval by controlling the age factor. 
LM<- lm(Net.Time ~ Age + factor(sex),  data= data1)
summary(LM)
conf_M <- confint(LM, level = .9) 
conf_M
## interval is (-726.15089, -679.72093)
## When we control for the age and female runner, then we are 90% confidence that the male runner is generally 679 to 726 sec faster than female.  


# Aging effect might be different for man and women, so lets take a look at it. 
LM_compare1 <- lm(Net.Time ~ Age + Age:sex, data = data1)
summary(LM_compare1)
LM_compare2<- lm(Net.Time ~ Age, data = data1)
summary(LM_compare2)
anova(LM_compare2, LM_compare1)
## Ho: effect of age is the same, Net.Time= 5095.8821 + 12.3172 Age
## Ha: effect of age is different, Net.Time = 4887.7302 +25.1299 Age-17.6348 Age:sexM

## The P value (2.2e-16) for the model 2 (interaction model) is sigfinant. If the age effect on male and female is the same, the interaction term should not be any significant. Therefore, we reject the Ho, and conclude that the age effect on male and female is different. 

# How much difference between man and women runner with respect to age factor ? 
summary(LM_compare1)
LM_compare1$coefficients
## Generally, when we control for the age variable, and the age increase by one for both male and female, we expect a 17.6248 increasing on finishing time of male than female. 

# comparing two models (adding Bib variable)
LM_elite1<- lm(Net.Time ~ Age + sex, data = data1)
LM_elite2<- lm(Net.Time ~ Age + sex + Bib, data = data1)
summary(LM_elite1)
summary(LM_elite2)
anova(LM_elite1, LM_elite2)


## a)
## model 1: Net.Time = 5171.5589 +  17.5825 Age - 702.9359 sexM
## model 2: Net.Time = 4.186e+03 + 1.543e+01 Age - 3.971e+02 sexM +8.399e-02 Bib
## By using anova to compare two model, the P-value for model 2 (with Bib) is significant (2.2e-16), which suggest Bib is signifiant predictor. Therefore, our model is model 2: Net.Time = 4.186e+03 + 1.543e+01 Age - 3.971e+02 sexM +8.399e-02 Bib

# To see whether the newer model (second model) is valid or not. 
par(mfrow=c(2,2))
library(alr3)
mmps(LM_elite2)
plot(LM_elite2)
2*(3+1)/nrow(data1)
## Based on the diagnostic plot, the residual plot and the scale-locatin seem fine, which suggest that constant variance is not a problem, and based on the leverage plot, there are lots of leverage points, but these leverage points are good, so i will not influence our model that much. However, based on the Normal QQ plot, the normality seems to a problem. 

# Will the new added varible is highly collinear with the existing variables ?  
vif(LM_elite2)
## Collinearity is not a problem, since no value of variable is greater than 5

## in the marginal model of age plot, the regression line and the loess line is about the same, which suggest that if our data is not linear, age variable is not the cause of it. 
## in the marginal model of Bib, the regression line and the loess line differ a lot, which sugest that if our data is not linear, Bib variable might be the cause of the nonlinearity. 

## The marginal model plot suggest a potential problem on Bib variable for linearity, therefore, and it seems to suggest a log transformation on Bib variable.
LM_elite2_improve <- lm(Net.Time ~ Age + sex + Bib+ I(Bib^2), data = data1)
summary(LM_elite2_improve)
## Net.Time =  -185.6476   +  15.0818  Age -  370.0044 sexM + 589.3161tBib


# Lets take a look at the transformed model. (validity)
par(mfrow=c(2,2))
mmps(LM_elite2_improve)
plot(LM_elite2)
plot(LM_elite2_improve)


# Try another transformation of the model. 
par(mfrow=c(1,2))
library(lattice)
xyplot(Bib ~ Age|sex, data = data1)
## According to the scatterplot of Bib and Age (control for sex), the shape of our data is not elliptical, which suggest that the joint distribution of our predictor is not multivariate normal. Therefore, in order to fix this problem, we should use Box-Cox transformation, which is primarily used for solving multivariate normality problem. 

#  What transformations does the boxcox method suggest?
library(alr3)
summary(powerTransform(with(data1, cbind(Age, Bib) ~1)))
t2data1<- transform(data1, t2Bib=Bib^0.67, t2Age=Age^-0.13)
LM_elite2_improve1 <- lm(Net.Time ~ t2Age + sex + t2Bib, data = t2data1)
mmps(LM_elite2_improve1)  
plot(LM_elite2_improve1)

## Based on the summary of Powertransformation, when lambda =0, the p value is significant, which suggest that no log transform. When lambda= 1, the p value is significant, which suggest that need for transform on the X variable. 

# Lets determine the best model out of all the variables. 
n<- dim(data1)[1]
set.seed(4321)
i.testing<- sample(1:n, size = (17881/3), replace = F)
data1.testing<- data1[i.testing, ]
data1.training<- data1[-i.testing, ]
library(leaps)
output<- regsubsets(Net.Time ~ Bib+Age+sex+sex:Bib+sex:Age+I(Bib^2) + I(Age^2), data = data1.training, method = "forward", nvmax=7)
summary(output, scale= "bic")
par(mfrow=c(1,1))
nvar<- 7
plot(1:nvar, summary(output)$bic, xlab = "Number of vars", ylab = "BIC")
lines(1:nvar, summary(output)$bic)


M4<- lm(Net.Time ~ Age+Bib+I(Bib^2)+sex:Bib, data = data1.training)
M5<- update(M4, .~.+Age:sex)

pred.4<- predict(M4, newdata = data1.testing)
rss.4<- sum((pred.4-data1.testing$Net.Time)^2)

summary(M4)

pred.5<- predict(M5, newdata = data1.testing)
rss.5<- sum((pred.5-data1.testing$Net.Time)^2)

summary(M5)
M5_predict<- lm(Net.Time ~ Age+Bib+I(Bib^2)+sex:Bib+Age:sex, data = data1)
summary(M5_predict)
# Best model 
## The model is Net.Time = Age+Bib+I(Bib^2)+factor(sex):Bib+Age:factor(sex)
## Based on the BIC plot, model with 4 or 5 variables seem reasonable, and also based on the rss, model with 5 vars has smallest rss (2893802027). Therefore, we choose model with 5 variables. 

# Make a small prediction based on this model with 20year-old female with Bib number 1000, to see her running time. 
predict(M5_predict,data.frame(Bib=1000,sex="F",Age=20),interval="prediction",level=.95)
## fit= 4057.826,  prediction interval: (2682.442, 5396.444)

# Overall critics based on the final model: 
# The prediction interval that we made in part c is (2682.442, 5396.444), and this interval has a large variability. If we use this model to predict individual runner’s time, we will end up with the prediction interval with very large variability, and since large variability means large RSS (error). Therefore, this prediction interval we made to predict individual runner is not useful.  

