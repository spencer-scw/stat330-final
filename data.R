library(tidyverse)
library(ggfortify)
library(corrplot)
library(car)
library(patchwork)
library(bestglm)
library(glmnet)
set.seed(12345)


#importing and changing variable types
paris <- read.csv('ParisHousing.csv')
summary(paris)
paris$hasYard <- as.factor(paris$hasYard)
paris$hasPool <- as.factor(paris$hasPool)
paris$hasStorageRoom <- as.factor(paris$hasStorageRoom)
paris$isNewBuilt <- as.factor(paris$isNewBuilt)
paris$hasStormProtector <- as.factor(paris$hasStormProtector)
paris$cityCode <- NULL
#paris$squareMeters <- NULL
paris <- as.data.frame(paris)

#pairs(paris, pch = 19, lower.panel = NULL)

#paris %>%
  #select(-hasYard,-hasPool,-hasStormProtector,-hasStorageRoom,-isNewBuilt)%>%
  #cor(paris)

# checking Normality
paris_lm <- lm(price~., data = paris)
parismeters_lm <- lm(price ~ squareMeters, data = paris)
summary(parismeters_lm)
summary(paris_lm)
autoplot(paris_lm)
plot(paris_lm)
vifs <- vif(paris_lm)
vifs
mean(vifs)

ggplot(data = paris)+
  geom_line(aes(x= squareMeters, y = price))
cor(paris$squareMeters, paris$price)


#looking at data
ggplot(data = paris) +
  geom_boxplot(aes(x = hasYard, y = price))
ggplot(data = paris) +
  geom_boxplot(aes(x = hasPool, y = price))
ggplot(data = paris) +
  geom_boxplot(aes(x = hasStormProtector, y = price))
ggplot(data = paris) +
  geom_boxplot(aes(x = hasStorageRoom, y = price))
ggplot(data = paris) +
  geom_boxplot(aes(x = isNewBuilt, y = price))
#ggplot(data = paris) +
  #geom_boxplot(aes(x = squareMeters))
ggplot(data = paris) +
  geom_boxplot(aes(x = numberOfRooms))
ggplot(data = paris) +
  geom_boxplot(aes(x = garage))







#best subsets
best_subsets_bic <- bestglm(paris,
                            IC = "BIC",
                            method = "exhaustive")
summary(best_subsets_bic$BestModel)

# forward selection
base_mod <- lm(price ~ 1, data = paris) 
full_mod <- lm(price ~ ., data = paris)

forw_BIC <- step(base_mod,
                 direction = "forward", 
                 scope=list(lower= base_mod, upper= full_mod),
                 k = log(nrow(paris)))
summary(forw_BIC)

# backward selection
back_BIC <- step(full_mod,
                 direction = "backward", 
                 scope=list(lower= base_mod, upper= full_mod),
                 k = log(nrow(paris)))
summary(back_BIC)

# Sequential
full_seq_BIC <- step(full_mod,
                     direction = "both", 
                     scope=list(lower= base_mod, upper= full_mod),
                     k=log(nrow(paris)))

base_seq_BIC <- step(base_mod,
                     direction = "both", 
                     scope=list(lower= base_mod, upper= full_mod),
                     k=log(nrow(paris)))

summary(base_seq_BIC)
summary(full_seq_BIC)


# LASSO
body_x <- as.matrix(paris[, 1:15])
body_y <- paris[, 16] 
body_lasso_cv <- cv.glmnet(x = body_x, 
                           y = body_y, 
                           type.measure = "mse", 
                           alpha = 1)

autoplot(body_lasso_cv, label = FALSE) +
  theme_bw() +
  theme(aspect.ratio = 1)

coef(body_lasso_cv, s = "lambda.1se")

# Elastic Net
body_elastic_cv <- cv.glmnet(x = body_x, 
                             y = body_y, 
                             type.measure = "mse", 
                             alpha = 0.5)

coef(body_elastic_cv, s = "lambda.1se")


avPlots(paris_lm)
