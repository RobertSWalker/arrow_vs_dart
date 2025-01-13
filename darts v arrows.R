library(readxl)
setwd("C:/Users/walkerro/OneDrive - University of Missouri/Desktop/r scripts/briggs/darts and arrows")
df <- read_xlsx("ethnodarts.xlsx")
df$Southwest <- ifelse(df$Region == "Southwest", 1, 0)

colSums(is.na(df))
table(df$type)
df$isdart <- ifelse(df$type=="dart",1,0)
df$type_factor <- factor(df$type)
levels(df$type_factor)

df <- df[df$Continent == "North America",] #remove 2 rows outside of N Am
table(df$Region, df$type)

#boxplots
library(ggplot2);library(cowplot);theme_set(theme_cowplot());library(ggpubr)
a <- ggplot(df, aes(x = type, y = Width, color = type, group = type)) +
  geom_boxplot(outliers = F) +
  geom_jitter(height=0, width=.1) +
  xlab("") +
  geom_rug() +
  theme(legend.position="none") +
  coord_flip()
b <- ggplot(df, aes(x = type, y = Length, color = type, group = type)) +
  geom_boxplot(outliers = F) +
  geom_jitter(height=0, width=.1) +
  xlab("") +
  geom_rug() +
  theme(legend.position="none") +
  coord_flip()
c <- ggplot(df, aes(x = type, y = Width*Length, color = type, group = type)) +
  geom_boxplot(outliers = F) +
  geom_jitter(height=0, width=.1) +
  xlab("") +
  geom_rug() +
  theme(legend.position="none") +
  coord_flip()
d <- ggplot(df, aes(x = type, y = Neckwidth, color = type, group = type)) +
  geom_boxplot(outliers = F) +
  geom_jitter(height=0, width=.1) +
  xlab("") +
  geom_rug() +
  theme(legend.position="none") +
  coord_flip()
ggarrange(a, b, c, d, ncol = 2, nrow = 2)
#ggsave("4panels.pdf", width=6, height=6)

#make interaction terms
df$lw <- df$Length * df$Width
df$lt <- df$Length * df$Thickness
df$ln <- df$Length * df$Neckwidth
df$wt <- df$Thickness * df$Width
df$wt <- df$Thickness * df$Neckwidth
df$wn <- df$Width * df$Neckwidth
df$lwn <- df$Width * df$Length * df$Neckwidth
names(df)

#impute Weight from volume
summary(mm <- lm(Weight ~ volume , df))
df$Weight[is.na(df$Weight)] <- predict(mm, newdata = df[is.na(df$Weight),])

#create dummy variables for region
library(caret)
dmy <- dummyVars(" ~ Region", data = df)
trsf <- predict(dmy, newdata = df)
trsf <- trsf[, !colnames(trsf) %in% c("RegionMissouri","RegionNorth America", "RegionWisconsin",
                                      "RegionTexas")]
#elasticnet
library(doParallel)

#make predictor matrix
x <- scale(df[,c(6:10,12,17:22)])
x <- cbind(x, trsf)
library(glmnet) #https://www.r-bloggers.com/2017/09/variable-selection-with-elastic-net/
library(foreach)

#find optimal alpha (ELASTIC NET WITH 0 < ALPHA < 1) with leave one out cross val
a <- seq(0, 1, 0.1)
set.seed(1)
search <- foreach(i = a, .combine = rbind) %dopar% {
  cv <- cv.glmnet(x, df$type_factor, relax=F, #weights=model_weights,
                  family = "binomial", nfold = nrow(df), #type.measure = "auc", 
                  parallel = TRUE, alpha = i)
  data.frame(cvm = cv$cvm[cv$lambda == cv$lambda.1se], lambda.1se = cv$lambda.1se, alpha = i)
}
(cv3 <- search[search$cvm == min(search$cvm), ]) #alpha = .9
cv3 <- cv3[1,]
m.cv <- glmnet(x, df$type_factor, family = "binomial", relax = F,
               #type.measure = "auc", 
               lambda = cv3$lambda.1se, alpha = cv3$alpha)
coef(m.cv) #length, width, lw, and SW included

co <- coef(m.cv,s1 = "lambda.1se")
inds <- which(co!=0)
variables <- row.names(co)[inds]
variables[!(variables %in% '(Intercept)')]  #optimal set of variables according to elasticnet

#this is glmnet with known alpha=0.9
set.seed(0)
m.cv <- cv.glmnet(x, df$type_factor, family = "binomial", #default alpha=1 is lasso, alpha=0 is ridge
                 folds = nrow(df), alpha=.9,
                 relax=F) #weights=model_weights, type.measure = "class") #auc or class or deviance
plot(m.cv)
co <- coef(m.cv,s1 = "lambda.1se")
inds <- which(co!=0)
variables <- row.names(co)[inds]
variables[!(variables %in% '(Intercept)')];

#run explanatory models; compare algorithms with caret https://topepo.github.io/caret/available-models.html
set.seed(0)
control <- trainControl(method="repeatedcv", #seeds=seeds,
                        number = 10, repeats=5,
                        search = 'random') #random search through tuning parameters

fit.lda <- train(type_factor ~ Length + Width + Southwest , 
                 data=df, 
                 method="lda",
                 seed=1,
                 family="binomial",
                 trControl=control) #nothing to tune
fit.lda #kappa .67
fit.nb <- train(type_factor ~ Length + Width + Southwest, 
                 data=df, 
                 method="nb",
                 seed=1,
                 #family="binomial",
                 trControl=control) #nothing to tune
fit.nb #kappa .67
fit.gam <- train(type_factor ~ Length + Width + Southwest , 
                 data=df, 
                 method="gam", 
                 family="binomial", 
                 tuneLength=10,
                 trControl=control) 
fit.gam #kappa .75
fit.knn <- train(type_factor ~ Length + Width + Southwest, 
                 data=df, 
                 method="knn", 
                 tuneLength=20,
                 trControl=control) 
fit.knn #.65
fit.logistic <- train(type_factor ~ Length + Width + Southwest, 
                      data=df, 
                      method="glm", 
                      family="binomial", 
                      trControl=control) #nothing to tune
fit.logistic #.67
fit.cart <- train(type_factor ~ Length + Width + lw + Southwest, 
                  data=df, 
                  method="rpart", 
                  trControl=control,
                  tuneLength = 10)
fit.cart #kappa .75
fit.nn <- train(type_factor ~ Length + Width + lw + Southwest,
                data=df, 
                method="nnet",
                tuneLength = 10,
                trControl=control)
fit.nn #.67
fit.rf <- train(type_factor ~ Length + Width + lw + Southwest, 
                data=df, 
                method="rf", 
                trControl=control,
                tuneLength = 10,
                ntree = 1000)
fit.rf #.69
fit.svm <- train(type_factor ~ Length + Width + lw + Southwest,
                data=df, 
                method="svmLinear",
                tuneLength = 10,
                trControl=control)
fit.svm #.69
fit.svmRadial <- train(type_factor ~ Length + Width + lw + Southwest,
                 data=df, 
                 method="svmRadialSigma",
                 tuneLength = 10,
                 trControl=control)
fit.svmRadial #.75
fit.xgb <- train(type_factor ~ Length + Width + lw + Southwest,
                       data=df, 
                       method="xgbTree",
                       tuneLength = 10,
                       trControl=control)
fit.xgb
fit.lvq <- train(type_factor ~ Length + Width + lw + Southwest,
                 data=df, 
                 method="lvq",
                 tuneLength = 10,
                 trControl=control)
fit.lvq 
fit.gp <- train(type_factor ~ Length + Width + lw + Southwest,
                 data=df, 
                 method="gaussprRadial",
                 tuneLength = 10,
                 trControl=control)
fit.gp

# Compare models
results <- resamples(list(logistic = fit.logistic,
                          gam = fit.gam,
                          knn = fit.knn,
                          lda = fit.lda,
                          cart = fit.cart, 
                          logistic = fit.logistic, 
                          nnet = fit.nn,
                          svm = fit.svm,
                          svm2 = fit.svmRadial,
                          xgb = fit.xgb,
                          lvq = fit.lvq,
                          gp = fit.gp,
                          naivebayes = fit.nb,
                          rf = fit.rf))
summary(results)
dotplot(results) #best algorithms are cart (decision tree), Gaussian process, Support Vector Machine, and GAM 


# Examine GAM
fit.gam
varImp(fit.gam, scale = FALSE)
ggplot(fit.gam)


#add in additional data and rerun machine learning models
df <- read_xlsx("Additional data.xlsx")
df$Southwest <- ifelse(df$Region == "Southwest", 1, 0)
df$type_factor <- factor(df$type)
levels(df$type_factor)
df <- df[df$Continent == "North America",]
table(df$Region)
set.seed(0)
df$lw <- df$Width * df$Length

control <- trainControl(method="repeatedcv", #seeds=seeds,
                        number = 10, repeats=5,
                        search = 'random') #random search through tuning parameters

fit.lda <- train(type_factor ~ Length + Width + Southwest , 
                 data=df, 
                 method="lda2",
                 seed=1,
                 family="binomial",
                 trControl=control) #nothing to tune
fit.lda #.68
fit.nb <- train(type_factor ~ Length + Width + Southwest, 
                data=df, 
                method="nb",
                #family="binomial",
                trControl=control) #nothing to tune
fit.nb #.73
fit.gam <- train(type_factor ~ Length + Width + Southwest, 
                 data=df, 
                 method="gam", 
                 family="binomial", 
                 tuneLength=10,
                 trControl=control) 
fit.gam #.73
fit.knn <- train(type_factor ~ Length + Width + Southwest, 
                 data=df, 
                 method="knn", 
                 #family="binomial", 
                 tuneLength=20,
                 trControl=control) 
fit.knn #.61
fit.logistic <- train(type_factor ~ Length + Width + Southwest, 
                      data=df, 
                      method="glm", 
                      family="binomial", 
                      trControl=control) #nothing to tune
fit.logistic #.68
fit.cart <- train(type_factor ~ Length + Width + lw + Southwest, 
                  data=df, 
                  method="rpart", 
                  trControl=control,
                  tuneLength = 10)
fit.cart #.74
fit.nn <- train(type_factor ~ Length + Width + lw + Southwest,
                data=df, 
                method="nnet",
                tuneLength = 10,
                trControl=control)
fit.nn
fit.rf <- train(type_factor ~ Length + Width + lw + Southwest, 
                data=df, 
                method="rf", 
                trControl=control,
                tuneLength = 10,
                ntree = 1000)
fit.rf
fit.svm <- train(type_factor ~ Length + Width + lw + Southwest,
                 data=df, 
                 method="svmLinear",
                 tuneLength = 10,
                 trControl=control)
fit.svm 
fit.svmRadial <- train(type_factor ~ Length + Width + lw + Southwest,
                       data=df, 
                       method="svmRadialSigma",
                       tuneLength = 10,
                       trControl=control)
fit.svmRadial
fit.xgb <- train(type_factor ~ Length + Width + lw + Southwest,
                 data=df, 
                 method="xgbTree",
                 tuneLength = 10,
                 trControl=control)
fit.gp <- train(type_factor ~ Length + Width + lw + Southwest,
                data=df, 
                method="gaussprRadial",
                tuneLength = 10,
                trControl=control)

# Compare models
results <- resamples(list(logistic = fit.logistic,
                          gam = fit.gam,
                          knn = fit.knn,
                          lda = fit.lda,
                          cart = fit.cart, 
                          logistic = fit.logistic, 
                          nnet = fit.nn,
                          svm = fit.svm,
                          svm2 = fit.svmRadial,
                          xgb = fit.xgb,
                          gp = fit.gp,
                          naivebayes = fit.nb,
                          rf = fit.rf))
summary(results) #similar winners as before
dotplot(results)


library(brms)

#run bayesian logistic model
get_prior(type_factor~ s(Length) + s(Width) + Southwest, data=df, family=bernoulli(link='logit'))

#model probability of being dart
m <- brm(type_factor ~ s(Length, k=3) + 
             s(Width, k=3) +
             Southwest, 
             data = df, family=bernoulli(link='logit'),
             prior = c(prior(normal(0, 2), class = b),
                       prior("exponential(1)", class = sds)),
             chains=4, cores=4, iter=1e4, sample_prior = T,
             control = list(adapt_delta = .99)#, backend = "cmdstanr"
          )
prior_summary(m)
m
pl <- plot(m, ask = FALSE) #Trace and Density Plots for MCMC Samples
posterior_summary(m) #bayes_R2(m) #.62
conditional_effects(m)

#saveRDS(m,"m.Rds")
m <- readRDS("m.Rds") #plot(hypothesis(m, "Length:Width = 0"))
pp_check(m, ndraws=100)

library(DHARMa);library(DHARMa.helpers)
simres <- dh_check_brms(m,integer=F)

library(sjPlot)
tab_model(m, digits=3, file="m.html")


#to predict a single point for shiny app
Length=10;Width=20;Southwest=1
t = data.frame(Length, Width, Southwest)
t$Southwest <- as.numeric(t$Southwest)
preds <- data.frame(fitted(m, type='response', t))
preds
t$type_est <- preds$Estimate
t$type <- ifelse(t$type_est > .15, "dart", 'arrow')
t$type_est2 <- ifelse(t$type_est >= .15 * 2, .15 * 2 - .001, t$type_est) 
t$odds1 <- t$type_est / (.15 * 2 - t$type_est2)
t$odds2 <- 1/t$odds1
t$odds <- ifelse(t$odds1 > t$odds2, t$odds1, t$odds2)

#add prediction colors for graph
dfpred <- expand.grid(Width = seq(8,35,by=.2), Length = seq(10,90,by=.5))
dfpred$Southwest <- 0
dfpred$fit <- data.frame(fitted(m, type='response', dfpred))$Estimate
dfpred2 <- expand.grid(Width = seq(8,22,by=.3), Length = seq(10,90,by=.5))
dfpred2$Southwest <- 1
dfpred2$fit <- data.frame(fitted(m, type='response', dfpred2))$Estimate
#write.csv(dfpred, "dfpred.csv")

df$Southwest2 <- ifelse(df$Southwest == 1, "Southwest", "Other region")

#for shiny app and paper figure
library(viridis)
ggplot(df, aes(x=Width, y=Length)) + 
  geom_tile(data = dfpred, mapping = aes(x = Width, y = Length, fill=fit), 
            alpha = .7, size = 0, color = NA) + 
  geom_smooth(data = dfpred[round(dfpred$fit,2) == .15,], mapping = aes(x = Width, y = Length), 
            size = .1, color="black", se=F) + 
  geom_smooth(data = dfpred2[round(dfpred2$fit,2) == .15,], mapping = aes(x = Width, y = Length), 
              size = .1, color="black", se=F, linetype="dashed") + 
  geom_point(aes(color=type, shape = factor(Southwest2)), size=1, alpha=.6 ) +
  scale_color_discrete(type=c("darkblue","darkred"), name=NULL) +
  scale_fill_viridis(discrete = F, breaks = c(0.01, .5, .97),
                     labels = c("arrow", "", "dart"), name="Prediction") +
  theme(legend.title=element_blank(),
        text=element_text(size=8),
        axis.text=element_text(size=8)) +
  xlab("Width (mm)") + ylab("Length (mm)") 
ggsave("fig.jpg")
  
  
#compare models https://en.wikipedia.org/wiki/Bayes_factor
#bayes_factor(m, m2)
#m <- add_criterion(m, criterion = c("loo"))
#m2 <- add_criterion(m2, criterion = c("loo"))
#loo_compare(m, m2) #expected log pointwise predictive density, best model on top

#get predictions from brms model
preds <- data.frame(fitted(m, df, type='response'))
df$prob_brms <- preds$Estimate

#find optimal cutoff that equalizes sensitivity and specificity
library(caret)
df$brms_predictions <- ifelse(df$prob_brms > .15, "dart", 'arrow')
confusionMatrix(as.factor(df$brms_predictions), as.factor(df$type))

#check into problematic groups
#table(df$grp)
#df1 <- df[df$grp == 8,] #3and5 are split
#df1 <- df[is.na(df$grp),] #3and5 are split
#table(df1$brms_predictions, df1$type)
ggplot(df, aes(x=Width, y=Length, color=factor(grp))) + geom_point()

#arch point data
apd <- read_xlsx("Arch dart and arrow data.xlsx")
colSums(is.na(apd))

#get predictions from brms model
preds <- data.frame(predict(m, apd, type='response'))
fits <- data.frame(fitted(m, apd, type='response'))
apd$prob_brms <- fits$Estimate
apd$prob_brmsSD <- preds$Est.Error
cutoff = .15
apd$brms_prediction <- ifelse(apd$prob_brms > cutoff , "dart", 'arrow')

#write.csv(apd, "apd.csv", row.names = F)
table(apd$brms_prediction, apd$type)

ggplot(apd, aes(x=Width, y=Length, color=brms_prediction)) + 
  #geom_tile(data = dfpred, mapping = aes(x = Width, y = Length, fill=fit), 
  #          alpha = .4, size = 0, color = NA) + 
  geom_point(aes(color=brms_prediction)) +
  guides(fill="none", color=guide_legend(override.aes=list(fill=NA))) +
  scale_fill_gradient(low="blue", high="darkred")

#smith data
sm <- read_xlsx("Smith et al 2013 data.xlsx")
colSums(is.na(sm))
preds <- data.frame(predict(m, sm, type='response'))
fits <- data.frame(fitted(m, sm, type='response'))
sm$prob_brms <- fits$Estimate
sm$prob_brmsSD <- preds$Est.Error
cutoff = .15
sm$brms_prediction <- ifelse(sm$prob_brms > cutoff , "dart", 'arrow')
write.csv(sm, "sm.csv", row.names = F)

ggplot(sm, aes(x=Width, y=Length, color=brms_prediction)) + 
  #geom_tile(data = dfpred, mapping = aes(x = Width, y = Length, fill=fit), 
  #          alpha = .4, size = 0, color = NA) + 
  geom_point(aes(color=brms_prediction)) +
  guides(fill="none", color=guide_legend(override.aes=list(fill=NA))) +
  scale_fill_gradient(low="blue", high="darkred")

#get predictions for folsom data
f1 <- read_xlsx("Folsom points.xlsx")
f1$Southwest <- 0
preds <- data.frame(fitted(m, f1, type='response'))
f1$prob_brms <- preds$Estimate
f1$prob_brmsSD <- preds$Est.Error
cutoff = .15
f1$brms_prediction <- ifelse(f1$prob_brms > cutoff , "dart", 'arrow')
table(f1$brms_prediction)
#write.csv(f1, "f1.csv", row.names=F )

ggplot(df, aes(x=Width, y=Length, color=type)) + 
  #geom_tile(data = dfpred, mapping = aes(x = Width, y = Length, fill=fit), 
  #          alpha = .4, size = 0, color = NA) + 
  #geom_point(aes(color=type)) +
  geom_smooth(method=lm, se=F) + #scale_x_continuous(trans="log10") + scale_y_continuous(trans="log10") +
  #geom_point(data = t, aes(color=type), size = 10) +
  scale_color_discrete(type=c("darkblue","darkred")) +
  geom_point(data = f1, size = 2, aes(color=brms_prediction)) +
  guides(fill="none", color=guide_legend(override.aes=list(fill=NA))) +
  scale_fill_gradient(low="blue", high="darkred") +
  geom_segment(aes(x = 5, y = 30, xend = 3.5, yend = 25),
               arrow = arrow(length = unit(0.5, "cm"))) 

#get predictions for great basin
g1 <- read_xlsx("GB arrow dart.xlsx")
g1$Southwest <- 0
preds <- data.frame(fitted(m, g1, type='response'))
g1$prob_brms <- preds$Estimate
g1$prob_brmsSD <- preds$Est.Error
cutoff = .15
g1$brms_prediction <- ifelse(g1$prob_brms > cutoff , "dart", 'arrow')
table(g1$brms_prediction)
g1
write.csv(g1, "g1.csv", row.names=F )

ggplot(df, aes(x=Width, y=Length, color=type)) + 
  geom_tile(data = dfpred, mapping = aes(x = Width, y = Length, fill=fit), 
            alpha = .4, size = 0, color = NA) + 
  geom_point(aes(color=type)) +
  geom_smooth(method=lm, se=F) + #scale_x_continuous(trans="log10") + scale_y_continuous(trans="log10") +
  #geom_point(data = t, aes(color=type), size = 10) +
  scale_color_discrete(type=c("darkblue","darkred")) +
  geom_point(data = g1, size = 2, color='black') +
  guides(fill="none", color=guide_legend(override.aes=list(fill=NA))) +
  scale_fill_gradient(low="blue", high="darkred") +
  geom_segment(aes(x = 5, y = 30, xend = 3.5, yend = 25),
               arrow = arrow(length = unit(0.5, "cm"))) 

#get predictions for paleoindian data
p1 <- read_xlsx("Paleoindian points.xlsx")
p1$Southwest <- 0
preds <- data.frame(fitted(m, p1, type='response'))
p1$prob_brms <- preds$Estimate
p1$prob_brmsSD <- preds$Est.Error
cutoff = .15
p1$brms_prediction <- ifelse(p1$prob_brms > cutoff , "dart", 'arrow')
table(p1$brms_prediction)
p1
write.csv(p1, "p1.csv", row.names=F )
table(p1$brms_prediction)
ggplot(df, aes(x=Width, y=Length, color=type)) + 
  geom_tile(data = dfpred, mapping = aes(x = Width, y = Length, fill=fit), 
            alpha = .4, size = 0, color = NA) + 
  geom_point(aes(color=type)) +
  geom_smooth(method=lm, se=F) + #scale_x_continuous(trans="log10") + scale_y_continuous(trans="log10") +
  #geom_point(data = t, aes(color=type), size = 10) +
  scale_color_discrete(type=c("darkblue","darkred")) +
  geom_point(data = p1, size = 2, color='black') +
  guides(fill="none", color=guide_legend(override.aes=list(fill=NA))) +
  scale_fill_gradient(low="blue", high="darkred") +
  geom_segment(aes(x = 5, y = 30, xend = 3.5, yend = 25),
               arrow = arrow(length = unit(0.5, "cm"))) 

#what if length or width is missing
library(dplyr)
wl <- df |> select(Width, Length)
wl2 <- apd |> select(Width, Length)
wl <- rbind(wl, wl2)
summary(m_length <- lm(Length~Width, wl))
summary(m_width <- lm(Width~Length, wl))

