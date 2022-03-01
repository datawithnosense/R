### Chapter 5. 상관분석과 회귀분석

## Chapter5-1. 병아리의 성장(체중)에 영향을 미치는 인자는 무엇일까? (상관분석)

# 데이터 불러오기
w <- read.csv("ch5-1.csv", header = TRUE)

head(w)
str(w)

# w 데이터 셋에서 2~5열 데이터만 가져오기(첫열은 factor이므로)
w_n <- w[,2:5]
head(w_n)

# 위와 동일
w_n <- subset(w, select = -c(chick_nm))

head(w_n)

w_cor <- cor(w_n)  # w_n 데이터 셋으로 상관분석한 결과를 w_cor변수에 넣음
w_cor  # w_cor 상관분석 결과 확인

plot(w_n)  # w_n 데이터 셋 산점도로 표현

# 상관분석을 보다 잘 표현할 수 있는 패키지 설치
install.packages("corrplot")   # 패키지 설치
library(corrplot)  # corrplot 패키지 불러오기

# 그냥 한번 실행해보기(주의할 점은 데이터셋이 아닌 상관분석결과를 넣어야함)
corrplot(w_cor)  # 상관분석 결과인 w_cor을 corrplot 패키지로 실행해보기

# 원을 타원으로 표시하고, 하단에만 표시하고, 상관계수 표시
corrplot(w_cor, method = "ellipse", 
         type = "lower", addCoef.col = "white")


## Chapter5-2. 병아리의 무게를 예측할 수 있을까? (회귀분석)

# 단순선형 회귀분석 실시
w_lm <- lm(weight ~ egg_weight, data = w_n)

# 회귀모델 결과 확인
summary(w_lm)

# 산점도에 회귀직선을 표시해 모델이 데이터를 잘 대표하는지 확인
plot(w_n$egg_weight, w_n$weight)  # 산점도 그리기
lines(w_n$egg_weight, w_lm$fitted.values, col = "blue")  # 회귀직선 추가
text(x = 66, y = 132, label = 'Y = 2.3371X - 14.5475')  # 회귀직선 라벨로 표시

names(w_lm)  # w_lm 변수에 어떤 항목들이 있는지 확인

w_lm

w_lm$coefficients
w_lm$model

hist(w_lm$residuals, col = "skyblue", xlab = "residuals",
       main = "병아리 무게 잔차 히스토그램")


# 다중회귀분석 실시
w_mlm <- lm(weight ~ egg_weight + movement + food, data = w_n)

summary(w_mlm)

# p값이 높은 movement 변수를 제외한 열만 다시 회귀분석 실시 
w_mlm2 <- lm(weight ~ egg_weight + food, data = w_n)

summary(w_mlm2)

# 다중공선성(Multicollinearity) 확인을 위한 패키지 설치
install.packages("car")
library(car)

# 분산팽창요인(Variation Inflation Factor, VIF)
# 10이상이면 문제있다고 보고, 30보다 크면 심각
vif(w_mlm2)

# 잔차 히스토그램
hist(w_mlm2$residuals, col = "skyblue", xlab = "residuals",
     main = "병아리 무게 잔차 히스토그램(다중 회귀)")

# (참고)후진소거법을 적용해 자동으로 실행
step_mlm <- step(w_mlm, direction = "backward")

# (참고)회귀분석 결과 그래프로 확인
plot(w_mlm2)

# 비선형 회귀분석용 두번째 데이터셋 불러오기
w2 <- read.csv("ch5-2.csv", header = TRUE)

head(w2)

str(w2)

plot(w2)  # 데이터 형태 산점도로 확인


# 성장기간에 따른 병아리 무게 변화 선형 회귀분석 실시
w2_lm <- lm(weight ~ day, data = w2)

summary(w2_lm)

# 산점도 위에 회귀직선 표시
lines(w2$day, w2_lm$fitted.values, col = "blue")

# 성장기간에 따른 병아리 무게 변화 비선형 회귀분석 실시
w2_lm2 <- lm(weight ~ I(day^3) + I(day^2) + day, data = w2)

summary(w2_lm2)

plot(w2)

# 산점도 위에 회귀곡선 표시
lines(w2$day, w2_lm2$fitted.values, col = "blue")

# w2_lm2 회귀분석 결과에서 계수 확인
w2_lm2$coefficients

# 산점도 위에 수식 표시
text(25, 3000, "weight = -0.025*day^3 + 2.624*day^2 - 15.298*day + 117.014")

