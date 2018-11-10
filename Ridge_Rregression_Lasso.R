install.packages("ElemStatLearn")
library(ElemStatLearn)
install.packages("car")
library(car) #분산팽창인수
install.packages("corrplot")
library(corrplot) #상관간계 도표.1
install.packages("corrr")
library(corrr) #상관간계 도표.2
install.packages("PerformanceAnalytics")
library(PerformanceAnalytics) #상관간계 도표.3
install.packages("dplyr")
library(dplyr)
install.packages("leaps")
library(leaps) #최량부분집합회귀
install.packages("glmnet")
library(glmnet) #능형회귀분석, lASSO
ininstall.packages("ggplot2")
library(ggplot2)

str(prostate)
plot(prostate)
par(mfrow=c(1,1))
plot(prostate$gleason)
table(prostate$gleason)
ggplot(data = prostate, aes(x = gleason, y = prostate$lpsa, color=gleason)) + geom_boxplot(aes(group = gleason))
prostate$gleason <- ifelse(prostate$gleason == 6, 0, 1)  #지시 변수로 변형
table(prostate$gleason)
prostate %>%  #상관관계
  #focus(mpg:drat, mirror = TRUE) %>% 
  correlate() %>% #상관객체만들기
  network_plot()
  #rplot()
chart.Correlation(prostate, histogram = TRUE, pch = 19)

train <- subset(prostate, train == TRUE)[,1:9]
str(train)
test <- subset(prostate, train == FALSE)[,1:9]
str(test)


##최량부분집합##
################
subfit <- regsubsets(lpsa ~ ., data = train)
b.sum <- summary(subfit)
which.min(b.sum$bic) #세 가지 피처를 사용한 모형
plot(b.sum$bic, type = "l", xlab = "# of Features", ylab = "BIC",
     main = "BIC score by Feature Inclusion")
plot(subfit, scale = "bic", main = "Best Subset Features") #최량부분집합으로 피처선택

ols <- lm(lpsa ~ lcavol + lweight + gleason, data = train)
tmp_df <- as.data.frame(cbind(pv = ols$fitted.values, rv = train$lpsa))  
ggplot(data = tmp_df, aes(x = tmp_df$pv, y = tmp_df$rv)) + geom_point() + 
  labs(title = 'Predicted vs Actual',
  x = 'Predicted', y = 'Actual')

pred.subfit <- predict(ols, newdata = test) #test 데이터로 확인
tmp_df_2 <- as.data.frame(cbind(pv = pred.subfit, rv = test$lpsa))  
ggplot(data = tmp_df_2, aes(x = pv, y = rv)) + geom_point() + 
  labs(title = 'Predicted vs Actual',
       x = 'Predicted', y = 'Actual')

#MSE
resid.subfit <- tmp_df_2$pv - tmp_df_2$rv 
mean(resid.subfit^2)
install.packages("Metrics")
library(Metrics)
Metrics::mse(tmp_df_2$rv, tmp_df_2$pv)


##능형회귀분석##
################
#능형 회귀 분석_입력 피처는 행렬로!
x <- as.matrix(train[, 1:8])
y <- train[, 9]

#반응 변수값이 연속이기 때문에 반응 변수의 분포는 가우시안 분포, 능형 회귀 alpha는 0으로
ridge <- glmnet(x, y, family = "gaussian", alpha = 0)

#람다값을 바꿨을 때 편차의 백분율이 크게 나아지지 않으면, 그 전에 멈출 수 있음
#사용하는 피처의 수는 8개
print(ridge)

plot(ridge, xvar = "lambda", label = TRUE)

#특정 람다 사용 
ridge.coef <- coef.glmnet(ridge, s = 0.1); ridge.coef

#편차와 계수의 관계도
plot(ridge, xvar = "dev", label = TRUE)

#Test
newx <- as.matrix(test[,1:8])
ridge.y <- predict(ridge, newx = newx, type = "response", s = 0.1)
plot(ridge.y, test$lpsa, xlab = "Predicted", ylab = "Actual", main = "Ridge Regression")
#MSE
ridge.resid <- ridge.y - test$lpsa 
mean(resid.subfit^2)


##LASSO##
#########
#람다 값이 줄어드는 데도 편차가 더 이상 나아지지 않아 69번째만에 멈춤
lasso <- glmnet(x, y, family = "gaussian", alpha = 1)
print(lasso)
plot(lasso, xvar = "lambda", label = TRUE)

#lcp 계수를 0으로 만듦
lasso.coef <- coef(lasso, s = 0.045); lasso.coef

#예측 및 그래프 확인
lasso.y <- predict(lasso, newx = newx, type = "response", s = 0.045)
plot(lasso.y, test$lpsa, xlab = "Predicted", ylab = "Actual", main = "LASSO")

#MSE
lasso.resid <- lasso.y - test$lpsa
mean(lasso.resid^2)


##LASSO_교창검증##
##################
set.seed(317)
lasso.cv = cv.glmnet(x, y, nfolds = 3)
plot(lasso.cv)
lasso.cv$lambda.min #최소
lasso.cv$lambda.min #최소에서 표준 오차만큼 떨어져 있는 곳

coef(lasso.cv, s = "lambda.1se")
lasoo.y.cv = predict(lasso.cv, newx = newx, type = "response", s = "lambda.1se")
lasso.cv.read = lasoo.y.cv - test$lpsa
mean(lasso.cv.read^2)


##LASSO/람다_0.125/교차검증##
#############################
set.seed(317)
lasso.cv = cv.glmnet(x, y, nfolds = 10)
lasso.coef.cv <- coef(lasso.cv, s = 0.125); lasso.coef.cv
plot(lasso.coef.cv)
lasso.coef.cv$lambda.min #최소
lasso.coef.cv$lambda.min #최소에서 표준 오차만큼 떨어져 있는 곳

lasoo.coef.y.cv = predict(lasso.cv, newx = newx, type = "response", s = 0.125)
lasoo.coef.cv.read = lasoo.coef.y.cv - test$lpsa
mean(lasoo.coef.cv.read^2)


##로지스틱 회귀애 적용##
########################
library(MASS)
biopsy$ID = NULL
names(biopsy) = c("thick", "u.size", "u.shape", "adhsn", "s.size", "nucl", 
                  "chrom", "n.nuc", "mit", "class")
biopsy.v2 <- na.omit(biopsy)
set.seed(123)
ind <- sample(2, nrow(biopsy.v2), replace = TRUE, prob = c(0.7, 0.3))
train <- biopsy.v2[ind==1, ]
test <- biopsy.v2[ind==2, ]

x <- as.matrix(train[, 1:9])
y <- train[, 10]

set.seed(3)
fitCV <- cv.glmnet(x, y, family = "binomial", type.measure = "auc", nfolds = 10)
plot(fitCV)
fitCV$lambda.1se
coef(fitCV, s = "lambda.1se")

#AUC
install.packages("InformationValue")
library(InformationValue)

predCV <- predict(fitCV, newx = as.matrix(test[, 1:9]),
                  s = "lambda.1se", type = "response")
actuals <- ifelse(test$class == "malignant", 1, 0)
misClassError(actuals, predCV)
plotROC(actuals, predCV)

predCV.min <- predict(fitCV, newx = as.matrix(test[, 1:9]),
                      s = "lambda.min", type = "response")
misClassError(actuals, predCV.min)
