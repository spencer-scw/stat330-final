library(tidyverse)
library(ggfortify)
library(corrplot)
library(car)
library(patchwork)
library(bestglm)
library(glmnet)
library(readxl)
set.seed(12345)

# import data
energy <- read_xlsx('ENB2012_data.xlsx')
energy <- as.data.frame(energy)


# change col names
colnames(energy) = c('Compact', 'SurfaceA', 'WallA', 'RoofA', 'twoStories', 'Orient', 'GlazingA', 'GlazingADist', 'Heat', 'Cool')


# remove data we are not using
energy$Orient <- NULL
energy$GlazingADist <- NULL


# change building height to categorical
energy$twoStories <- ifelse(energy$twoStories == 7, 1, 0)
energy$twoStories <- as.factor(energy$twoStories)
summary(energy)


# some visualization of the data
ggplot(data = energy) +
  geom_point(aes(x = Heat, y = Cool, color = twoStories))
ggplot(data = energy) +
  geom_boxplot(aes(x = twoStories, y = Heat))
ggplot(data = energy) +
  geom_point(aes(x = Compact, y = Heat, color = twoStories), position = 'jitter')
ggplot(data = energy) +
  geom_point(aes(x = SurfaceA, y = Heat, color = twoStories), position = 'jitter')
ggplot(data = energy) +
  geom_point(aes(x = WallA, y = Heat, color = twoStories), position = 'jitter')
ggplot(data = energy) +
  geom_point(aes(x = RoofA, y = Heat, color = twoStories), position = 'jitter')

ggplot(data = energy) +
  geom_point(aes(x = GlazingA, y = Heat, color = twoStories), position = 'jitter')
# it looks like there will be some interaction terms with number of stories and 
    # some of the other variables. Heating and cooling also look to be very 
    # closely correlated


# check for multicollinearity between continuous variables
continuous <- energy %>%
  select(-twoStories, -RoofA, -SurfaceA)
corrplot(cor(continuous), type = 'upper')
round(cor(continuous),2)
# it looks like there is some multicollinearity between the variables

# create a full linear model for both heating and cooling
heating_lm <- lm(Heat ~ Compact + SurfaceA + WallA + RoofA + twoStories + 
                   GlazingA, data = energy)
cooling_lm <- lm(Cool ~ Compact + SurfaceA + WallA + RoofA + twoStories + 
                   GlazingA, data = energy)
summary(heating_lm)
summary(cooling_lm)
avPlots(heating_lm)
avPlots(cooling_lm)
#vif(heating_lm)
# both models removed Roof Area, i think because it is perfectly multicollinear
# with number of stories


# check diagnostics on base models
autoplot(heating_lm)
autoplot(heating_lm, which = 4)
autoplot(cooling_lm)
autoplot(cooling_lm, which = 4)
shapiro.test(heating_lm$residuals)
hist(heating_lm$residuals)
boxplot(heating_lm$residuals)
shapiro.test(cooling_lm$residuals)
hist(cooling_lm$residuals)
boxplot(cooling_lm$residuals)
# neither model has normally distributed errors and there could some problems
# equality of variance

# Interactions
anova <- aov(Heat ~ Compact*twoStories + SurfaceA*twoStories + WallA*twoStories
             + GlazingA, data = energy)
summary(anova)
interaction.plot(x.factor = energy$Compact, 
                 trace.factor = energy$twoStories, 
                 response = energy$Heat)
interaction.plot(x.factor = energy$SurfaceA, 
                 trace.factor = energy$twoStories, 
                 response = energy$Heat)
interaction.plot(x.factor = energy$WallA, 
                 trace.factor = energy$twoStories, 
                 response = energy$Heat)
interaction.plot(x.factor = energy$RoofA, 
                 trace.factor = energy$twoStories, 
                 response = energy$Heat)
interaction.plot(x.factor = energy$GlazingA, 
                 trace.factor = energy$twoStories, 
                 response = energy$Heat)

interaction.plot(x.factor = energy$GlazingA, 
                 trace.factor = energy$Compact, 
                 response = energy$Heat)

# looks like number of stories has an interaction with  compact, surface area,
# and wall area

# fit a new model with the interaction terms
heat_interax_lm <- lm(Heat ~ Compact + SurfaceA + WallA + twoStories + 
                        GlazingA + WallA*twoStories + Compact*twoStories + 
                        SurfaceA*twoStories, data = energy)
cool_interax_lm <- lm(Cool ~ Compact + SurfaceA + WallA + twoStories + 
                        GlazingA + WallA*twoStories + Compact*twoStories + 
                        SurfaceA*twoStories, data = energy)
summary(heat_interax_lm)
summary(cool_interax_lm)
autoplot(heat_interax_lm)
autoplot(cool_interax_lm)
shapiro.test(heat_interax_lm$residuals)
shapiro.test(cool_interax_lm$residuals)
hist(heat_interax_lm$residuals)
hist(cool_interax_lm$residuals)
# a model with all the interactions still has the same problems


# Variable Selection/Shrinkage
heat <- energy %>%
  select(Compact, SurfaceA, WallA, RoofA, twoStories, GlazingA, Heat)
cool <- energy %>%
  select(Compact, SurfaceA, WallA, RoofA, twoStories, GlazingA, Cool)
heat <- as.data.frame(heat)
cool <- as.data.frame(cool)
heat_x <- as.matrix(heat[, 1:6]) 
heat_y <- heat[, 7]
cool_x <- as.matrix(cool[, 1:6]) 
cool_y <- cool[, 7]
## Ridge
heat_ridge_cv <- cv.glmnet(x = heat_x,
                          y = heat_y, 
                          type.measure = "mse", 
                          alpha = 0)
autoplot(heat_ridge_cv, label = FALSE) +
  theme_bw() +
  theme(aspect.ratio = 1)
heat_ridge_cv$lambda.1se
coef(heat_ridge_cv, s = "lambda.1se")

cool_ridge_cv <- cv.glmnet(x = cool_x,
                           y = cool_y, 
                           type.measure = "mse", 
                           alpha = 0)
autoplot(cool_ridge_cv, label = FALSE) +
  theme_bw() +
  theme(aspect.ratio = 1)
cool_ridge_cv$lambda.1se
coef(cool_ridge_cv, s = "lambda.1se")
## LASSO
heat_lasso_cv <- cv.glmnet(x = heat_x,
                           y = heat_y, 
                           type.measure = "mse", 
                           alpha = 1)
autoplot(heat_lasso_cv, label = FALSE) +
  theme_bw() +
  theme(aspect.ratio = 1)
heat_lasso_cv$lambda.1se
coef(heat_lasso_cv, s = "lambda.1se")


cool_lasso_cv <- cv.glmnet(x = cool_x,
                           y = cool_y, 
                           type.measure = "mse", 
                           alpha = 1)
autoplot(cool_lasso_cv, label = FALSE) +
  theme_bw() +
  theme(aspect.ratio = 1)
cool_lasso_cv$lambda.1se
coef(cool_lasso_cv, s = "lambda.1se")
## Elastic Net
heat_net_cv <- cv.glmnet(x = heat_x,
                           y = heat_y, 
                           type.measure = "mse", 
                           alpha = 0.5)
autoplot(heat_net_cv, label = FALSE) +
  theme_bw() +
  theme(aspect.ratio = 1)
heat_net_cv$lambda.1se
coef(heat_net_cv, s = "lambda.1se")


cool_net_cv <- cv.glmnet(x = cool_x,
                         y = cool_y, 
                         type.measure = "mse", 
                         alpha = 0.5)
autoplot(cool_net_cv, label = FALSE) +
  theme_bw() +
  theme(aspect.ratio = 1)
cool_net_cv$lambda.1se
coef(cool_net_cv, s = "lambda.1se")
## Forward
base_mod <- lm(Heat ~ 1, data = heat) 
full_mod <- lm(Heat ~ ., data = heat) 
base_mod_cool <- lm(Cool ~ 1, data = cool) 
full_mod_cool <- lm(Cool ~ ., data = cool)
forw_BIC <- step(base_mod,
                 direction = "forward", 
                 scope=list(lower= base_mod, upper= full_mod),
                 k = log(nrow(heat)))
summary(forw_BIC)


forw_BIC_cool <- step(base_mod_cool,
                 direction = "forward", 
                 scope=list(lower= base_mod_cool, upper= full_mod_cool),
                 k = log(nrow(cool)))
summary(forw_BIC_cool)
## Backward
back_BIC <- step(full_mod,
                 direction = "backward", 
                 scope=list(lower= base_mod, upper= full_mod),
                 k = log(nrow(heat)))
summary(back_BIC)


back_BIC_cool <- step(full_mod_cool,
                 direction = "backward", 
                 scope=list(lower= base_mod_cool, upper= full_mod_cool),
                 k = log(nrow(cool)))
summary(back_BIC_cool)
## Sequential
full_seq_BIC <- step(full_mod,
                     direction = "both", 
                     scope=list(lower= base_mod, upper= full_mod),
                     k=log(nrow(heat)))
summary(full_seq_BIC)


full_seq_BIC_cool <- step(full_mod_cool,
                     direction = "both", 
                     scope=list(lower= base_mod_cool, upper= full_mod_cool),
                     k=log(nrow(cool)))
summary(full_seq_BIC_cool)

# all the models for both explanatory variables agree to remove roof area and 
    # keep compact, wall area, stories, and glazing area. LASSO and Net say
    # remove surface area, forward and backward say keep it



invTranPlot(log(Heat) ~ Compact , data = heat, lambda = c(-1,0,1,2))
invTranPlot(log(Heat) ~ WallA , data = heat, lambda = c(-1,0,1,2))
invTranPlot(log(Heat) ~ GlazingA, data = heat, lambda = c(-1,0,1,2))


invTranPlot(log(Cool) ~ Compact , data = cool, lambda = c(-1,0,1,2))
invTranPlot(log(Cool) ~ WallA , data = cool, lambda = c(-1,0,1,2))

bc = boxCox(heating_lm)
lambda.opt = bc$x[which.max(bc$y)]
lambda.opt
heat_trans_lm <- lm(log(Heat) ~ 1/(WallA**2) + twoStories + 
                      GlazingA + twoStories*GlazingA, data = heat)

cool_trans_lm <- lm(log(Cool) ~ log(Compact) + log(WallA) + twoStories + 
                      GlazingA + twoStories*GlazingA, data = cool)

#######




cool_trans_lm_small <- lm(log(Cool) ~ log(WallA) + twoStories + 
                      GlazingA + twoStories*GlazingA, data = cool) # best model so far



######



anova(cool_trans_lm, cool_trans_lm_small)
anova(cool_trans_lm_small, cool_trans_lm)



confint(cool_trans_lm)

dat <- data.frame(Compact = .75, WallA = 345, twoStories = 1, GlazingA = 0.1)
dat$twoStories <- as.factor(dat$twoStories)

predict(cool_trans_lm, newdata = dat, interval = 'confidence', level = .9)


predict(wind_lm, newdata = data.frame(RSpd = 8), interval = 'prediction',level=.9)

summary(cool_trans_lm)
autoplot(cool_trans_lm)
shapiro.test(cool_trans_lm$residuals)
hist(cool_trans_lm$residuals)
boxplot(cool_trans_lm$residuals)

summary(heat_trans_lm)
autoplot(heat_trans_lm)
shapiro.test(heat_trans_lm$residuals)
hist(heat_trans_lm$residuals)
boxplot(heat_trans_lm$residuals)

ggplot(data = heat)+
  geom_point(aes(x = log(Compact), y = log(Heat), color = twoStories))+
  geom_line(mapping = aes(x = log(Compact),
                          y = predict(heat_trans_lm), 
                          color = twoStories))
ggplot(data = heat)+
  geom_point(aes(x = 1/(WallA**2), y = log(Heat), color = twoStories))+
  geom_line(mapping = aes(x = 1/(WallA**2),
                          y = predict(heat_trans_lm), 
                          color = twoStories))
ggplot(data = heat)+
  geom_point(aes(x = GlazingA, y = log(Heat), color = twoStories))+
  geom_line(mapping = aes(x = GlazingA,
                          y = predict(heat_trans_lm), 
                          color = twoStories))


cool_reduced_log_model <- lm(Cool ~ sqrt(WallA) + twoStories + sqrt(GlazingA) + twoStories*WallA, data = energy)
cool_reduced <- lm(sqrt(Cool) ~ sqrt(WallA) + twoStories + sqrt(GlazingA), data = energy)
heat_reduced_log_model <- lm(sqrt(Heat) ~ Compact + sqrt(WallA) + twoStories + sqrt(GlazingA), data = energy)
heat$residuals <- heat_reduced_log_model$residuals
cor(energy)

ggplot(data = heat) +
  geom_point(mapping = aes(x = Compact, y = residuals)) +
  theme(aspect.ratio = 1)

ggplot(data = heat) +
  geom_point(mapping = aes(x = sqrt(WallA), y = residuals)) +
  theme(aspect.ratio = 1)
ggplot(data = heat) +
  geom_point(mapping = aes(x = sqrt(GlazingA), y = residuals)) +
  theme(aspect.ratio = 1)



autoplot(heat_reduced_log_model)
autoplot(heat_reduced_log_model, which = 4)
autoplot(cool_reduced)

autoplot(cool_reduced_log_model)
autoplot(cool_reduced_log_model, which = 4)

shapiro.test(heat_reduced_log_model$residuals)
hist(heat_reduced_log_model$residuals)
boxplot(heat_reduced_log_model$residuals)

shapiro.test(cool_reduced_log_model$residuals)
hist(cool_reduced_log_model$residuals)
boxplot(cool_reduced_log_model$residuals)
