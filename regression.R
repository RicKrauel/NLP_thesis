setwd("~/Library/CloudStorage/OneDrive-Persönlich/Documents/PyCharm Projects/NLP_thesis")

load("/Users/richardkrauel/Library/CloudStorage/OneDrive-Persönlich/Documents/PyCharm Projects/NLP_thesis/data/predict_small.RData")

library(dplyr)
library(glmnet)
library(foreign)
library(nnet)
library(MASS)
library(Hmisc)
library(reshape2)
library(fastDummies)
library(ggplot2)
library(stargazer)
library(olsrr)
library(lmtest)
library(car)
library(sandwich)
library(margins)
library(DescTools)
########
# pre processing
########
df <- predict

df$top2vec_0 <- factor(df$top2vec_0)
df$top2vec_1 <- factor(df$top2vec_1)
df$date <- substr(df$date, 1, 6)
df$date <- as.numeric(df$date)
#df$date <- as.Date(df$date, "%Y%m%d")
df <- df[df$date >= "2020-01-01",]
df <- df[df$date <= "2022-01-01",]
df <- df[, -c(which(colnames(df)=='bert_label'))]
df <- df[, -c(which(colnames(df)=='count_adj'))]
df <- df[, -c(which(colnames(df)=='new_cases_smoothed'))]
df <- df[, -c(which(colnames(df)=='top2vec_1'))]
df <- df[, -c(which(colnames(df)=='icu_patients'))]
#df$Month_Yr <- format(as.Date(df$date), "%Y-%m")
#df <- dummy_cols(df, remove_selected_columns = TRUE)

is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

df[is.nan(df)] <- 0

######
# average freq per newspaper / topic
######
freq_labels <- df_log %>% group_by(newspaper, top2vec_0, bert_label) %>% 
  dplyr::summarise(n = n())

freq_tag <- freq_labels[freq_labels$newspaper == 'tagespiegel',]
freq_zeit <- freq_labels[freq_labels$newspaper == 'zeit',]
freq_welt <- freq_labels[freq_labels$newspaper == 'welt',]

#freq_tag$n <- freq_tag$n / sum(freq_tag$n)
#freq_zeit$n <- freq_zeit$n / sum(freq_zeit$n)
#freq_welt$n <- freq_welt$n / sum(freq_welt$n)

freq_labels <- rbind(freq_tag, freq_zeit, freq_welt)
aggregate(freq_labels$n, list(freq_labels$top2vec_0), sum)

ggplot(freq_labels, aes(fill=newspaper, y=n, x=top2vec_0)) + ylab("Frequency of topic")  + geom_bar(position="dodge", stat="identity", show.legend = FALSE) + theme_bw(base_size = 8) + scale_fill_manual(name='Newspaper',labels = c("Tagesspiegel", "WELT", "Zeit Online"), values=c('#bebebe', '#71deef', '#dedd96')) + theme(legend.position = 'bottom') + theme(axis.title.x=element_blank())
ggsave(path = 'figures', filename= 'topic_freq.png', width=8, height=1.5,dpi = 300)

ggplot(freq_labels, aes(fill=newspaper, y=n, x=top2vec_0)) + ylab("Frequency of topic") + xlab("Topic") + geom_bar(position="dodge", stat="identity") + facet_wrap(.~factor(freq_labels$bert_label, levels=c('negative','neutral','positive')), ncol = 1, scales = "free") + theme_bw(base_size = 8) + scale_fill_manual(name='Newspaper',labels = c("Tagesspiegel", "WELT", "Zeit Online"), values=c('#bebebe', '#71deef', '#dedd96')) + theme(legend.position = 'bottom')

ggsave(path = 'figures', filename= 'sent_per_topic.png', width=8, height=5,dpi = 300)



df_tagesspiegel = df[df$newspaper == 'tagespiegel',]
df_tagesspiegel <- df_tagesspiegel[, -c(which(colnames(df_tagesspiegel)=='newspaper'))]
df_zeit = df[df$newspaper == 'zeit',]
df_zeit <- df_zeit[, -c(which(colnames(df_zeit)=='newspaper'))]
df_welt = df[df$newspaper == 'welt',]
df_welt <- df_welt[, -c(which(colnames(df_welt)=='newspaper'))]

t_tagesspiegel <- aggregate(predict_tagesspiegel$bert_score, list(predict_tagesspiegel$top2vec_0), mean)
t_zeit <- aggregate(predict_zeit$bert_score, list(predict_zeit$top2vec_0), mean)
t_welt <- aggregate(predict_welt$bert_score, list(predict_welt$top2vec_0), mean)

t_tagesspiegel$x <- t_tagesspiegel$x - mean(predict_tagesspiegel$bert_score)
t_zeit$x <- t_zeit$x - mean(predict_zeit$bert_score)
t_welt$x <- t_welt$x - mean(predict_welt$bert_score)

ggplot(aes(y = x, x=Group.1), data = t_tagesspiegel) + geom_bar(position="dodge", stat="identity")
ggplot(aes(y = x, x=Group.1), data = t_zeit) + geom_bar(position="dodge", stat="identity")
ggplot(aes(y = x, x=Group.1), data = t_welt) + geom_bar(position="dodge", stat="identity")

ggplot(t_tagesspiegel, aes(Group.1)) +
  geom_col(aes(y = x, fill = "Tagesspiegel"),
           width = 0.2, position = position_nudge(0.22)) +
  geom_col(aes(y = x, fill = "Zeit Online"), data = t_zeit,
           width = 0.2) +
  geom_col(aes(y = x, fill = "WELT"), data = t_welt,
           width = 0.2, position = position_nudge(-0.22))

###################
# linear regression
###################
######
# assumption checks
######
model_lin <- lm(bert_score ~ date + stringency_index + new_deaths_smoothed + new_vaccinations_smoothed + new_tests_smoothed + readability + newspaper*top2vec_0, data = df)
model_lin_tag <- lm(bert_score ~ date + stringency_index + new_deaths_smoothed + new_vaccinations_smoothed + new_tests_smoothed + readability + top2vec_0, data = df_tagesspiegel)
model_lin_zeit <- lm(bert_score ~ date + stringency_index + new_deaths_smoothed + new_vaccinations_smoothed + new_tests_smoothed + readability + top2vec_0, data = df_zeit)
model_lin_welt <- lm(bert_score ~ date + stringency_index + new_deaths_smoothed + new_vaccinations_smoothed + new_tests_smoothed + readability + top2vec_0, data = df_welt)

#1: check linearity assumption -> linearity assumed, no visual pattern in the data/ HOWEVER Test says no linear assumption; maybe gam would be a better choice for this (sqrt and ^2 transformations showed no effect)
par(mfrow=c(2,2))
plot(model_lin_tag)
raintest(model_lin_tag)
plot(model_lin_zeit)
raintest(model_lin_zeit)
plot(model_lin_welt)
raintest(model_lin_welt)

#2: Homogeneity of variance -> heteroscadisticity seems to be true, taking robust se as a precaution
lmtest::bptest(model_lin)
seWhite <- sqrt(diag(vcovHC(model_lin,type = 'HC0')))
lmtest::bptest(model_lin_tag)
seWhite_tag <- sqrt(diag(vcovHC(model_lin_tag,type = 'HC0')))
lmtest::bptest(model_lin_zeit)
seWhite_zeit <- sqrt(diag(vcovHC(model_lin_zeit,type = 'HC0')))
lmtest::bptest(model_lin_welt)
seWhite_welt <- sqrt(diag(vcovHC(model_lin_welt,type = 'HC0')))

#3: Normality of residuals -> NOT MET
hist(model_lin_tag$residuals, main = "Residual Histogram")
hist(model_lin_zeit$residuals, main = "Residual Histogram")
hist(model_lin_welt$residuals, main = "Residual Histogram")

#4: Test for multicollinearity -> remove c(count_adj, new_cases_smoothed, icu_patients)
vif <- vif(model_lin_tag)
stargazer(vif,type='text')

stargazer(model_lin_tag,model_lin_zeit,model_lin_welt, se=list(seWhite_tag,seWhite_zeit,seWhite_welt),column.labels = c('Tagesspiegel', 'Zeit Online', 'WELT'), font.size = 'footnotesize', no.space=TRUE, digits=2, type = 'text')

stargazer(model_lin, se=list(seWhite), font.size = "tiny", no.space=TRUE, digits=2, type = 'text')

###################
# ordinal regression
###################
df_org <- predict

df_org$top2vec_0 <- factor(df_org$top2vec_0)
df_org$date <- substr(df_org$date, 1, 6)
df_org$date <- as.numeric(df_org$date)/100
df_org$bert_label <- as.ordered(df_org$bert_label)
df_org$new_deaths_smoothed <- df_org$new_deaths_smoothed/100
df_org$new_tests_smoothed <- df_org$new_tests_smoothed/100
df_org$new_vaccinations_smoothed <- df_org$new_vaccinations_smoothed/1000
#df$date <- as.Date(df$date, "%Y%m%d")
df_org <- df_org[df_org$date >= "2020-01-01",]
df_org <- df_org[df_org$date <= "2022-01-01",]
df_org <- df_org[, -c(which(colnames(df_org)=='bert_score'))]
df_org <- df_org[, -c(which(colnames(df_org)=='count_adj'))]
df_org <- df_org[, -c(which(colnames(df_org)=='new_cases_smoothed'))]
df_org <- df_org[, -c(which(colnames(df_org)=='top2vec_1'))]
df_org <- df_org[, -c(which(colnames(df_org)=='icu_patients'))]
#df <- dummy_cols(df, remove_selected_columns = TRUE)

is.nan.data.frame <- function(x)
do.call(cbind, lapply(x, is.nan))

df_org[is.nan(df_org)] <- 0

table(df_org$bert_label)

df_tagesspiegel = df_org[df_org$newspaper == 'tagespiegel',]
df_tagesspiegel <- df_tagesspiegel[, -c(which(colnames(df_tagesspiegel)=='newspaper'))]
df_zeit = df_org[df_org$newspaper == 'zeit',]
df_zeit <- df_zeit[, -c(which(colnames(df_zeit)=='newspaper'))]
df_welt = df_org[df_org$newspaper == 'welt',]
df_welt <- df_welt[, -c(which(colnames(df_welt)=='newspaper'))]
######
# assumption checks
######

######
# model
######
model_ord_null = polr(bert_label ~ 1, data = df_org, method = "logistic")
model_ord_no_topic = polr(bert_label ~ date + stringency_index + new_deaths_smoothed + new_vaccinations_smoothed + new_tests_smoothed + readability, data = df_org, method = "logistic")
model_ord_no_interaction = polr(bert_label ~ date + stringency_index + new_deaths_smoothed + new_vaccinations_smoothed + new_tests_smoothed + readability + newspaper + top2vec_0, data = df_org, method = "logistic")

model_ord_log = polr(bert_label ~ date + stringency_index + new_deaths_smoothed + new_vaccinations_smoothed + new_tests_smoothed + readability + newspaper*top2vec_0, data = df_org, method = "logistic")
model_ord_prob = polr(bert_label ~ date + stringency_index + new_deaths_smoothed + new_vaccinations_smoothed + new_tests_smoothed + readability + newspaper*top2vec_0, data = df_org, method = "probit")

add.lnL <- c("lnL", round(logLik(model_ord_log),3))
add.Aic <- c("AIC", round(AIC(model_ord_log),3))

est.Logit  <- summary(model_ord_log)$coefficients

add.mu1.est <- c("Intercept negative/neutral",
                 round(est.Logit[nrow(est.Logit)-1,  "Value"], 3))
add.mu1.std <- c("",
                 round(est.Logit[nrow(est.Logit)-1,  "Std. Error"], 3))
add.mu2.est <- c("Intercept neutral/positive",
                 round(est.Logit[nrow(est.Logit),  "Value"], 3))
add.mu2.std <- c("",
                 round(est.Logit[nrow(est.Logit),  "Std. Error"], 3))

# Make the table
stargazer(model_ord_log, model_lin,se=list(seWhite), no.space = TRUE,
          add.lines = list(add.mu1.est, add.mu1.std,
                           add.mu2.est, add.mu2.std,
                           add.lnL, add.Aic), font.size = "tiny", digits=2, )

PseudoR2(model_ord_log)

######
# effects / plot
######
predict(model_ord_log, type = "probs")
predict(model_ord_log, type = "class")

tmpWVS <- ddply(df_org, .(newspaper), summarise,
                top2vec_0 = seq(0,16, length.out = 17))
tmpWVS$top2vec_0   <- as.factor(tmpWVS$top2vec_0)
tmpWVS$date   <- mean(df_org$date)
tmpWVS$stringency_index <- mean(df_org$stringency_index)
tmpWVS$new_deaths_smoothed   <- mean(df_org$new_deaths_smoothed)
tmpWVS$new_vaccinations_smoothed   <- mean(df_org$new_vaccinations_smoothed)
tmpWVS$new_tests_smoothed <- mean(df_org$new_tests_smoothed)
tmpWVS$readability <- mean(df_org$readability)

tmpWVS <- cbind(
  tmpWVS,
  predict(model_ord_log, newdata = tmpWVS, type = "prob"))

tmpWVS <- tmpWVS[, -c(3:8)]
tmpWVS_long <- melt(tmpWVS, id.vars = c("newspaper", "top2vec_0"))

ggplot(tmpWVS_long, aes(fill=newspaper, y=value, x=top2vec_0)) + ylab("Probabilities of class membership") + xlab("Topic") + geom_bar(position="dodge", stat="identity") + facet_wrap(~variable, ncol = 1, scales = "free") + theme_bw(base_size = 8) + scale_fill_manual(name='Newspaper',labels = c("Tagesspiegel", "WELT", "Zeit Online"), values=c('#bebebe', '#71deef', '#dedd96')) + theme(legend.position = 'bottom')
ggsave(path = 'figures', filename= 'probs_per_topic.png', width=8, height=5,dpi = 300)

#----------------------------------------------------------
# Determine marginal effects (average partial effects)
#----------------------------------------------------------
# Determine the average partial effects for each explanatory
# variable, for each rating, for the logit and probit model
# separately
prb.Logit  <- as.data.frame(predict(model_ord_log,  type="probs"))
prb.Probit <- as.data.frame(predict(model_ord_prob, type="probs"))

# Calculate the cumulative probabilities (as an intermediate
# step, for code transparency)
cdf.Logit.1 <- prb.Logit[, 1]
cdf.Logit.2 <- prb.Logit[, 1] + prb.Logit[, 2]
cdf.Logit.3 <- prb.Logit[, 1] + prb.Logit[, 2] + prb.Logit[, 3]

cdf.Probit.1 <- prb.Probit[, 1]
cdf.Probit.2 <- prb.Probit[, 1] + prb.Probit[, 2]
cdf.Probit.3 <- prb.Probit[, 1] + prb.Probit[, 2] + prb.Probit[, 3]
# ... cdf.Logit.3 and cdf.Probit.3 should be equal to 1

# Calculate density parts of the effects (Greene, p.910)
prb.Logit$pdf.1 <- 
  -dlogis(qlogis(cdf.Logit.1))
prb.Logit$pdf.2 <- 
  dlogis(qlogis(cdf.Logit.1)) - dlogis(qlogis(cdf.Logit.2))
prb.Logit$pdf.3 <- 
  dlogis(qlogis(cdf.Logit.2))

prb.Probit$pdf.1 <- 
  -dnorm(qnorm(cdf.Probit.1))
prb.Probit$pdf.2 <- 
  dnorm(qnorm(cdf.Probit.1)) - dnorm(qnorm(cdf.Probit.2))
prb.Probit$pdf.3 <- 
  dnorm(qnorm(cdf.Probit.2))

# Determine the average effects (apart from multiplication by 
# the parameter estimates)
avgAPE.Logit  <- colMeans(prb.Logit[c("pdf.1", "pdf.2", "pdf.3")])
avgAPE.Probit <- colMeans(prb.Probit[c("pdf.1", "pdf.2", "pdf.3")])

# Extract the estimated effects from the logit
# and probit objects
est.Logit  <- coef(model_ord_log)
est.Probit <- coef(model_ord_prob)

# Determine the APE
dfAPE.Logit  <- as.data.frame(round(avgAPE.Logit  %*% t(est.Logit), 3))
dfAPE.Probit <- as.data.frame(round(avgAPE.Probit %*% t(est.Probit), 3))

rownames(dfAPE.Logit) <- c("P(y=negative)", "P(y=neutral)", "P(y=positive)")
  
rownames(dfAPE.Probit) <- c("P(y=negative)", "P(y=neutral)", "P(y=positive)")

dfAPE.Logit <- as.data.frame(t(dfAPE.Logit))
dfAPE.Probit <- as.data.frame(t(dfAPE.Probit))

# Make the table
stargazer(dfAPE.Logit, summary = FALSE, no.space = TRUE)
stargazer(dfAPE.Probit, summary = FALSE,
          align = TRUE, no.space = TRUE, type = 'text')


#----------------------------------------------------------
# Perform likelihood ratio test for joint effects
#----------------------------------------------------------
lrtest(model_ord_null, model_ord_log)
lrtest(model_ord_no_topic, model_ord_log)
lrtest(model_ord_no_interaction, model_ord_log)


















lasso_fit <- glmnet(x = as.matrix(df[, -c(which(colnames(df)=='bert_score'))]), y = df$bert_score, alpha = 1)
coef(lasso_fit,s=0.00593)
plot_glmnet(lasso_fit, label=5, xvar ="norm")
cv_lasso_fit <- cv.glmnet(x = as.matrix(df[, -c(which(colnames(df)=='bert_score'))]), y = df$bert_score, alpha = 1, nfolds = 5)
plot(cv_lasso_fit)
cv_lasso_fit$lambda.min
coef(lasso_fit,s=cv_lasso_fit$lambda.min)


#Load the libraries
library(mlbench)
library(elasticnet)
library(caret)

#Initialize cross validation and train LASSO
cv_5 <- trainControl(method="cv", number=10)
lasso <- train(bert_score ~., data=df, method='lasso',  trControl=cv_5)

#Filter out the variables whose coefficients have squeezed to 0
drop <-predict.enet(lasso$finalModel, type='coefficients', s=lasso$bestTune$fraction, mode='fraction')$coefficients  
drop<-drop[drop==0]%>%names()
My_Data_Frame<- df%>%select(-drop) 




model_tagesspiegel <- lm(bert_score ~ date + count + new_cases_smoothed + new_deaths_smoothed + icu_patients + new_tests_smoothed + new_vaccinations_smoothed + stringency_index + avg_length + count_adj + percent_adj + count_nn + percent_nn + drosten + kekulé + lauterbach + streeck + wodarg + top2vec_0, data=predict_tagesspiegel)

model_zeit <- lm(bert_score ~ date + count + new_cases_smoothed + new_deaths_smoothed + icu_patients + new_tests_smoothed + new_vaccinations_smoothed + stringency_index + avg_length + count_adj + percent_adj + count_nn + percent_nn + drosten + kekulé + lauterbach + streeck + wodarg + top2vec_0, data=predict_zeit)

model_welt <- lm(bert_score ~ date + count + new_cases_smoothed + new_deaths_smoothed + icu_patients + new_tests_smoothed + new_vaccinations_smoothed + stringency_index + avg_length + count_adj + percent_adj + count_nn + percent_nn + drosten + kekulé + lauterbach + streeck + wodarg + top2vec_0, data=predict_welt)

model_total <- lm(bert_score ~ ., data=My_Data_Frame)
stargazer::stargazer(model_total, type = 'text')
library(stargazer)

stargazer(model_tagesspiegel, model_zeit, model_welt, column.labels = c('tagesspiegel', 'zeit', 'welt'), font.size = 'footnotesize', no.space=TRUE, digits=2)

model_1 <- lm(value ~ newspaper*top2vec_0, data = predict)

bartlett.test(bert_score ~ newspaper, data = df)

library(car)
# Levene's test with multiple independent variables
leveneTest(bert_score ~ newspaper, data = df)

library(ggpubr)
ggqqplot(predict_filtered$calc)

predict_filtered <- predict[predict$newspaper != 'tagespiegel',]

shapiro.test(df$bert_score)
