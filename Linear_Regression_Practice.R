data("anscombe")
attach(anscombe)
head(anscombe)

cor(x1, y1) #상관계수

par(mfrow=c(2,2))
plot(x1, y1, main="plot 1")
plot(x2, y2, main="plot 2")
plot(x3, y3, main="plot 3")
plot(x4, y4, main="plot 4") #상관관계 0

install.packages("alr3")
library(alr3)
data("snake")
head(snake)
names(snake) <- c("content", "yield")
attach(snake)
head(snake)
par(mfrow=c(1,1))
plot(content, yield, xlab="water content of snow", ylab = "water yield")

yield.fit <- lm(yield ~ content)
summary(yield.fit)

par(mfrow=c(2,2))
plot(yield.fit) #Residuals vs Fitted_이분산성오류항, Normal Q-Q_잔차의정규분포, Resudyals vs Leverage_특이점

install.packages("car")
library(car)
par(mfrow=c(1,1))
qqPlot(yield.fit) #신뢰구간 정규분포여부 확인

#다변량
library(alr3)
data("water")
str(water)
socal.water <- water[, -1]
head(socal.water) #모든 피처 정량. 상관관계 통계량. 산포토행렬.
  #1.상관분석 그래프
install.packages("corrplot")
library(corrplot)
water.cor <- cor(socal.water); water.cor
corrplot(water.cor, method = "ellipse")
pairs(~., data =socal.water)
  #2.상관분석 그래프
install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
chart.Correlation(socal.water, histogram = TRUE, pch = 19)
  #3.상관분석 그래프
install.packages("corrr")
library(corrr)
install.packages("dplyr")
library(dplyr)
socal.water %>% 
  #focus(mpg:drat, mirror = TRUE) %>% 
  correlate() %>% 
    #network_plot()
    rplot()
  #
install.packages("leaps") 
library(leaps)
fit <- lm(BSAAM ~ ., data = socal.water)
summary(fit)
sub.fit <- leaps::regsubsets(BSAAM ~ ., data = socal.water) #최량부분집합
best.summary <- summary(sub.fit)
names(best.summary)
which.min(best.summary$rss) #최대 피처의 수
which.max(best.summary$rss) #AIC(과대피팅) <(반비례)> Cp/BIC(과소피팅)_작을수록좋은모델
par(mfrow = c(1,2))
plot(best.summary$cp, xlab = "number of features", ylab = "cp")
plot(sub.fit, scale = "Cp") #가장 낮은 cp인 피처 확인 
which.min(best.summary$bic)
which.max(best.summary$adjr2)
best.fit <- lm(BSAAM ~ APSLAKE + OPRC + OPSLAKE, data = socal.water) #성능이 가장 좋은 피처들
summary(best.fit)
par(mfrow=c(2,2))
plot(best.fit)
install.packages("car")
library(car)
car::vif(best.fit) #공분산성 확인 
par(mfrow = c(1,1))
plot(socal.water$OPRC, socal.water$OPSLAKE, xlab = "OPRC", ylab = "OPSLAKE") #선형성 확인 
best.summary$adjr2 #R-squard로 설명력으로 피처 개수 선택
fit.2 <- lm(BSAAM ~ APSLAKE + OPSLAKE, data = socal.water)
summary(fit.2)
par(mfrow=c(2,2))
plot(fit.2)
car::vif(fit.2)
install.packages("lmtest") #오차항의 등분산성, 브루시-페이건
library(lmtest)
lmtest::bptest(fit.2)
socal.water["Actual"] = water$BSAAM #벡터 Actual 생성
socal.water$Forecst = predict(fit.2) #예측값 대입 
install.packages("ggplot2")
library(ggplot2)
ggplot(socal.water, aes(x=Forecst, y=Actual)) + geom_point() + geom_smooth(method = lm)
+ labs(title = "Forcast versus Actuals")
  #train_validation_test 데이터 나누기
sample_num = sample(1:nrow(socal.water), size = round(0.2 * nrow(socal.water)), replace = F)
test_water = socal.water[sample_num, ]
train_water = socal.water[-sample_num, ]
val_water = train_water[sample_num,]
train_water = train_water[-sample_num, ]
install.packages("MPV")
library(MPV)
MPV::PRESS(best.fit) #loocv_예측오차 제곱합 #작은걸선택
MPV::PRESS(fit.2) 

#상호작용 
install.packages("MASS")
library(MASS)
data(Boston)
str(Boston)
value.fit <- lm(medv ~ lstat*age, data = Boston) #lm() 함수에 feat1*feat2 두 피처의 상호작용
summary(value.fit)