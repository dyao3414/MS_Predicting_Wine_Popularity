
#call libraries
library(ggplot2) # For graphical tools
library(MASS) # For some advanced statistics
library(pscl) # For "counting" models (e.g., Poisson and Negative Binomial)
library(dplyr) # For general needs and functions
library(readr)
library(corrplot)
library("mice")
install.packages("leaps")
install.packages(dplyr)
install.packages(broom)
install.packages(car)
install.packages(MASS)
install.packages(dvmisc)
install.packages(leaps)
install.packages(ISLR)
install.packages(knitr)
install.packages(printr)
library(ISLR)
library(knitr)
library(printr)
library(dplyr)
library(broom)
library(car)
library(MASS)
library(dvmisc)
library(leaps)

#import data
setwd("~/Desktop/Assignment 3")
wine = read.csv("wine_imputed.csv")
wine_test = read.csv("TEST1.csv")

hist(wine$TARGET,  main="histogram of TARGET")

barplot(wine$TARGET)
barplot(table(age),
        main="Age Count of 10 Students",
        xlab="Age",
        ylab="Count",
        border="red",
        col="blue",
        density=10
)
## factor variable
#wine$Star_Imputed <- as.factor(wine$Star_Imputed)
#wine$Target_Flag <- as.factor(wine$Target_Flag)

## create train set and validation set
set.seed(1000)
nobs <- nrow(wine)
trainprop<-0.8
validateprop<-0.2
train <- sample(1:nobs, 0.8 *nobs)
validate <- sample(setdiff(1:nobs, train), 0.2 *nobs) 

train.set <- wine[train, ]
validate.set <- wine[validate, ]
nrow(wine)
nrow(train.set)
nrow(validate.set)
train.x <- train.set[setdiff(names(train.set), 'TARGET')]
validate.x <- validate.set[setdiff(names(validate.set), 'TARGET')]
train.y <- train.set$TARGET
validate.y <- validate.set$TARGET

#####################################################
############# FIRST SET OF MODEL #####################
#####################################################

## FIRST MODEL: REGULAR LINEAR REGRESSION MODEL ##
lm_fit1 <- lm(TARGET~ FixedAcidity
             +VolatileAcidity+CitricAcid+ResidualSugar+Chlorides+FreeSulfurDioxide
             +TotalSulfurDioxide+Density+pH+Sulphates+Alcohol+LabelAppeal
             +AcidIndex+STARS , data = train.set)

summary(lm_fit1)
validate.set$lm_fit1 <- predict(lm_fit1,validate.set)
lm_fit1.MSE <- mean((validate.set$lm_fit1-validate.y)^2)
lm_fit1.MSE
AIC(lm_fit1)
BIC(lm_fit1)

## SECOND MODEL: REGULAR LINEAR REGRESSION MODEL USING STEPWISE VARIABLE SELECTION (AIC) ##
stepwise_lm1 <- stepAIC(lm_fit1, direction="both")
stepwise_lm$anova


lm_fit_stepwise1 <- lm(TARGET~ FixedAcidity + VolatileAcidity + CitricAcid + ResidualSugar + 
                        Chlorides + FreeSulfurDioxide + TotalSulfurDioxide + Density + 
                        pH + Sulphates + Alcohol + LabelAppeal + AcidIndex + STARS, data=train.set)

summary(lm_fit_stepwise1)

validate.set$lm_fit_stepwise1 <- predict(lm_fit_stepwise1,validate.set)
lm_fit_stepwise1.MSE <- mean((validate.set$lm_fit_stepwise1-validate.y)^2)
lm_fit_stepwise1.MSE
AIC(lm_fit_stepwise1)
BIC(lm_fit_stepwise1)

## THIRD MODEL: POISSON ##
poisson_model1 <- glm(TARGET ~ FixedAcidity + VolatileAcidity + CitricAcid + ResidualSugar + 
                       Chlorides + FreeSulfurDioxide + TotalSulfurDioxide + Density + 
                       pH + Sulphates + Alcohol + LabelAppeal + AcidIndex + STARS, 
                      family="poisson"(link="log"), data=train.set)

summary(poisson_model1)
str(poisson_model1)
validate.set$poisson_model1 <- predict(poisson_model1,validate.set)
poisson_model1.MSE <- mean((validate.set$poisson_model1 - validate.y)^2)
poisson_model1.MSE
AIC(poisson_model1)
BIC(poisson_model1)


## FOURTH MODEL: NEGATIVE BINOMIAL DISTRIBUTION##
NBR_Model1<-glm.nb(TARGET ~ FixedAcidity + VolatileAcidity + CitricAcid + ResidualSugar + 
                    Chlorides + FreeSulfurDioxide + TotalSulfurDioxide + Density + 
                    pH + Sulphates + Alcohol + LabelAppeal + AcidIndex + STARS, data=train.set)

summary(NBR_Model1)

validate.set$NBR_Model1 <- predict(NBR_Model1,validate.set)
NBR_Model1.MSE <- mean((validate.set$NBR_Model1 -validate.y)^2)
NBR_Model1.MSE
AIC(NBR_Model1)
BIC(NBR_Model1)


## FIFTH MODEL: ZERO INFLATED POISSON (ZIP)##
ZIP_Model1<-zeroinfl(TARGET ~ FixedAcidity + VolatileAcidity + CitricAcid + ResidualSugar + 
                      Chlorides + FreeSulfurDioxide + TotalSulfurDioxide + Density + 
                      pH + Sulphates + Alcohol + LabelAppeal + AcidIndex + STARS, data=train.set,link = "logit", dist = "poisson")

summary(ZIP_Model1)

validate.set$ZIP_Model1 <- predict(ZIP_Model1,validate.set)
ZIP_Model1.MSE <- mean((validate.set$ZIP_Model1 -validate.y)^2)
ZIP_Model1.MSE
AIC(ZIP_Model1)
BIC(ZIP_Model1)


## SIXTH MODEL: ZERO INFLATED NEGATIVE BINOMIAL REGRESSION (ZINB)###

ZINB_Model1<-zeroinfl(TARGET ~ FixedAcidity + VolatileAcidity + CitricAcid + ResidualSugar + 
                       Chlorides + FreeSulfurDioxide + TotalSulfurDioxide + Density + 
                       pH + Sulphates + Alcohol + LabelAppeal + AcidIndex + STARS, data=train.set, link = "logit", dist = "negbin")
summary(ZINB_Model1)

validate.set$ZINB_Model1 <- predict(ZINB_Model1,validate.set)
ZINB_Model1.MSE <- mean((validate.set$ZINB_Model1-validate.y)^2)
ZINB_Model1.MSE
AIC(ZINB_Model1)
BIC(ZINB_Model1)

### SEVENTH MODEL: HURDLE###
Hurdle_Model1 <- hurdle(TARGET ~ FixedAcidity + VolatileAcidity + CitricAcid + ResidualSugar + 
                        Chlorides + FreeSulfurDioxide + TotalSulfurDioxide + Density + 
                        pH + Sulphates + Alcohol + LabelAppeal + AcidIndex + STARS, 
                        link = "logit", dist = "negbin", data=train.set)

summary(Hurdle_Model1)
str(Hurdle_Model1)
validate.set$Hurdle_Model1 <- predict(Hurdle_Model1,validate.set)
Hurdle_Model1.MSE <- mean((validate.set$Hurdle_Model1 - validate.y)^2)
Hurdle_Model1.MSE
AIC(Hurdle_Model1)
BIC(Hurdle_Model1)

###########################################
########## SECOND SET OF MODEL #############
############################################

## FIRST MODEL: REGULAR LINEAR REGRESSION MODEL ##
lm_fit2 <- lm(TARGET~ VolatileAcidity
              +ResidualSugar+Chlorides+FreeSulfurDioxide
              +TotalSulfurDioxide+Density+pH+Sulphates+Alcohol+LabelAppeal
              +AcidIndex+STARS+ Star_Imputed , data = train.set)

summary(lm_fit2)
train.set$lm_fit2 <- fitted(lm_fit2)
validate.set$lm_fit2 <- predict(lm_fit2,validate.set)
lm_fit2.MSE <- mean((validate.set$lm_fit2-validate.y)^2)
lm_fit2.MSE
AIC(lm_fit2)
BIC(lm_fit2)


## SECOND MODEL: POISSON ##
poisson_model2 <- hurdle(TARGET ~  VolatileAcidity + ResidualSugar + 
                        Chlorides + FreeSulfurDioxide + TotalSulfurDioxide + Density + 
                        pH + Sulphates + Alcohol + LabelAppeal + AcidIndex + STARS +
                        Star_Imputed , family="poisson",link = "logit", data=train.set)

summary(poisson_model2)

validate.set$poisson_model2 <- predict(poisson_model2,validate.set)
poisson_model2.MSE <- mean((validate.set$poisson_model2 -validate.y)^2)
poisson_model2.MSE
AIC(poisson_model2)
BIC(poisson_model2)


## THIRD MODEL: NEGATIVE BINOMIAL DISTRIBUTION##
NBR_Model2<-hurdle(TARGET ~ FixedAcidity + VolatileAcidity + CitricAcid + ResidualSugar + 
                     Chlorides + FreeSulfurDioxide + TotalSulfurDioxide + Density + 
                     pH + Sulphates + Alcohol + LabelAppeal + AcidIndex + STARS + Star_Imputed, 
                   data=train.set, link = "logit", dist = "negbin")
                   

summary(NBR_Model2)

validate.set$NBR_Model2 <- predict(NBR_Model2,validate.set)
NBR_Model2.MSE <- mean((validate.set$NBR_Model2 -validate.y)^2)
NBR_Model2.MSE
AIC(NBR_Model2)
BIC(NBR_Model2)


## FOURTH MODEL: ZERO INFLATED POISSON (ZIP)##
ZIP_Model2<-zeroinfl(TARGET ~  VolatileAcidity +  ResidualSugar + 
                       Chlorides + FreeSulfurDioxide + TotalSulfurDioxide + Density + 
                       pH + Sulphates + Alcohol + LabelAppeal + AcidIndex + STARS 
                      + Star_Imputed , data=train.set)

summary(ZIP_Model2)

validate.set$ZIP_Model2 <- predict(ZIP_Model2,validate.set)
ZIP_Model2.MSE <- mean((validate.set$ZIP_Model2 -validate.y)^2)
ZIP_Model2.MSE
AIC(ZIP_Model2)
BIC(ZIP_Model2)


## FIFTH MODEL: ZERO INFLATED NEGATIVE BINOMIAL REGRESSION (ZINB)###

ZINB_Model2<-zeroinfl(TARGET ~ VolatileAcidity + ResidualSugar + 
                        Chlorides + FreeSulfurDioxide + TotalSulfurDioxide + Density + 
                        pH + Sulphates + Alcohol + LabelAppeal + AcidIndex + STARS 
                       + Star_Imputed , data=train.set, link = "logit", dist = "negbin")
summary(ZINB_Model2)

validate.set$ZINB_Model2 <- predict(ZINB_Model2,validate.set)
ZINB_Model2.MSE <- mean((validate.set$ZINB_Model2-validate.y)^2)
ZINB_Model2.MSE
AIC(ZINB_Model2)
BIC(ZINB_Model2)

### SIXTH MODEL: HURDLE###
Huddle_Model2 <- hurdle(TARGET ~ FixedAcidity + VolatileAcidity + CitricAcid + ResidualSugar + 
                          Chlorides + FreeSulfurDioxide + TotalSulfurDioxide + Density + 
                          pH + Sulphates + Alcohol + LabelAppeal + AcidIndex + STARS, 
                        link = "logit", dist = "negbin", data=train.set)

summary(Huddle_Model2)
str(Huddle_Model2)
validate.set$Huddle_Model2 <- predict(Huddle_Model2,validate.set)
Huddle_Model2.MSE <- mean((validate.set$Huddle_Model2 - validate.y)^2)
Huddle_Model2.MSE
AIC(Huddle_Model2)
BIC(Huddle_Model2)
###########################################
########## THIRD SET OF MODEL #############
############################################

## FIRST MODEL: REGULAR LINEAR REGRESSION MODEL ##
lm_fit3 <- lm(TARGET~ VolatileAcidity
              +ResidualSugar+Chlorides+FreeSulfurDioxide
              +TotalSulfurDioxide+Density+pH+Sulphates+Alcohol+LabelAppeal
              +AcidIndex+ STARS + Star_Imputed 
               + TotalSulfurDioxide*pH + VolatileAcidity*Alcohol
              + VolatileAcidity*AcidIndex +Star_Imputed*STARS
              + Density*ResidualSugar + LabelAppeal*STARS + Alcohol*pH 
              +ResidualSugar*Density +AcidIndex*Chlorides+ Density*pH
              +LabelAppeal*Alcohol + LabelAppeal*Density + LabelAppeal*STARS, data = train.set)

summary(lm_fit3)
train.set$lm_fit3 <- fitted(lm_fit3)
validate.set$lm_fit3 <- predict(lm_fit3,validate.set)
lm_fit3.MSE <- mean((validate.set$lm_fit3-validate.y)^2)
lm_fit3.MSE
AIC(lm_fit3)
BIC(lm_fit3)


## SECOND MODEL: POISSON ##
poisson_model3 <- glm(TARGET ~  VolatileAcidity + ResidualSugar + 
                        Chlorides + FreeSulfurDioxide + TotalSulfurDioxide + Density + 
                        pH + Sulphates + Alcohol + LabelAppeal + AcidIndex + STARS +
                        Star_Imputed ++ TotalSulfurDioxide*pH + VolatileAcidity*Alcohol
                      + VolatileAcidity*AcidIndex +Star_Imputed*STARS
                      + Density*ResidualSugar + LabelAppeal*STARS + Alcohol*pH 
                      +ResidualSugar*Density +AcidIndex*Chlorides+ Density*pH
                      +LabelAppeal*Alcohol + LabelAppeal*Density + LabelAppeal*STARS, family="poisson"(link="log"), data=train.set)

summary(poisson_model3)

validate.set$poisson_model3 <- predict(poisson_model3,validate.set)
poisson_model3.MSE <- mean((validate.set$poisson_model3 -validate.y)^2)
poisson_model3.MSE
AIC(poisson_model3)
BIC(poisson_model3)


## THIRD MODEL: NEGATIVE BINOMIAL DISTRIBUTION##
NBR_Model3<-glm.nb(TARGET ~ FixedAcidity + VolatileAcidity + CitricAcid + ResidualSugar + 
                     Chlorides + FreeSulfurDioxide + TotalSulfurDioxide + Density + 
                     pH + Sulphates + Alcohol + LabelAppeal + AcidIndex + STARS + Star_Imputed
                   + TotalSulfurDioxide*pH + VolatileAcidity*Alcohol
                   + VolatileAcidity*AcidIndex +Star_Imputed*STARS
                   + Density*ResidualSugar + LabelAppeal*STARS + Alcohol*pH 
                   +ResidualSugar*Density +AcidIndex*Chlorides+ Density*pH
                   +LabelAppeal*Alcohol + LabelAppeal*Density + LabelAppeal*STARS, data=train.set)

summary(NBR_Model3)

validate.set$NBR_Model3 <- predict(NBR_Model3,validate.set)
NBR_Model3.MSE <- mean((validate.set$NBR_Model3 -validate.y)^2)
NBR_Model3.MSE
AIC(NBR_Model3)
BIC(NBR_Model3)


## FOURTH MODEL: ZERO INFLATED POISSON (ZIP)##
ZIP_Model3<-zeroinfl(TARGET ~  VolatileAcidity +  ResidualSugar + 
                       Chlorides + FreeSulfurDioxide + TotalSulfurDioxide + Density + 
                       pH + Sulphates + Alcohol + LabelAppeal + AcidIndex + STARS 
                     + Star_Imputed + + TotalSulfurDioxide*pH + VolatileAcidity*Alcohol
                     + VolatileAcidity*AcidIndex +Star_Imputed*STARS
                     + Density*ResidualSugar + LabelAppeal*STARS + Alcohol*pH 
                     +ResidualSugar*Density +AcidIndex*Chlorides+ Density*pH
                     +LabelAppeal*Alcohol + LabelAppeal*Density + LabelAppeal*STARS, data=train.set)

summary(ZIP_Model3)

validate.set$ZIP_Model3 <- predict(ZIP_Model3,validate.set)
ZIP_Model3.MSE <- mean((validate.set$ZIP_Model3 -validate.y)^2)
ZIP_Model3.MSE
AIC(ZIP_Model3)
BIC(ZIP_Model3)


## FIFTH MODEL: ZERO INFLATED NEGATIVE BINOMIAL REGRESSION (ZINB)###

ZINB_Model3<-zeroinfl(TARGET ~ VolatileAcidity + ResidualSugar + 
                        Chlorides + FreeSulfurDioxide + TotalSulfurDioxide + Density + 
                        pH + Sulphates + Alcohol + LabelAppeal + AcidIndex + STARS 
                      + Star_Imputed + + TotalSulfurDioxide*pH + VolatileAcidity*Alcohol
                      + VolatileAcidity*AcidIndex +Star_Imputed*STARS
                      + Density*ResidualSugar + LabelAppeal*STARS + Alcohol*pH 
                      +ResidualSugar*Density +AcidIndex*Chlorides+ Density*pH
                      +LabelAppeal*Alcohol + LabelAppeal*Density + LabelAppeal*STARS, link = "logit", dist = "negbin", data = train.set)
summary(ZINB_Model3)

validate.set$ZINB_Model3 <- predict(ZINB_Model3,validate.set)
ZINB_Model3.MSE <- mean((validate.set$ZINB_Model3-validate.y)^2)
ZINB_Model3.MSE
AIC(ZINB_Model3)
BIC(ZINB_Model3)

#### Hurdle Model #########
Hurdle_Model3 <- hurdle(TARGET ~ VolatileAcidity + ResidualSugar + 
                          Chlorides + FreeSulfurDioxide + TotalSulfurDioxide + Density + 
                          pH + Sulphates + Alcohol + LabelAppeal + AcidIndex + STARS 
                        + Star_Imputed + + TotalSulfurDioxide*pH + VolatileAcidity*Alcohol
                        + VolatileAcidity*AcidIndex +Star_Imputed*STARS
                        + Density*ResidualSugar + LabelAppeal*STARS + Alcohol*pH 
                        +ResidualSugar*Density +AcidIndex*Chlorides+ Density*pH
                        +LabelAppeal*Alcohol + LabelAppeal*Density + LabelAppeal*STARS, 
                        link = "logit", dist = "negbin", data = train.set)

summary(Hurdle_Model3)
str(Hurdle_Model3)
validate.set$Hurdle_Model3 <- predict(Hurdle_Model3,validate.set)
Hurdle_Model3.MSE <- mean((validate.set$Hurdle_Model3 - validate.y)^2)
Hurdle_Model3.MSE
AIC(Hurdle_Model3)
BIC(Hurdle_Model3)
####### testing ########
###### Champion model#######
ZIP_Model3<-zeroinfl(TARGET ~  VolatileAcidity +  ResidualSugar + 
                       Chlorides + FreeSulfurDioxide + TotalSulfurDioxide + Density + 
                       pH + Sulphates + Alcohol + LabelAppeal + AcidIndex + STARS 
                     + Star_Imputed + + TotalSulfurDioxide*pH + VolatileAcidity*Alcohol
                     + VolatileAcidity*AcidIndex +Star_Imputed*STARS
                     + Density*ResidualSugar + LabelAppeal*STARS + Alcohol*pH 
                     +ResidualSugar*Density +AcidIndex*Chlorides+ Density*pH
                     +LabelAppeal*Alcohol + LabelAppeal*Density + LabelAppeal*STARS, data=train.set)

wine_test$TARGET <- predict(ZIP_Model3, newdata = wine_test, type = "response")

summary(wine_test)

select <- dplyr::select

# Scored Data File
scores <- wine_test[c("INDEX","TARGET")]
write.csv(scores, file = "write.csv")
