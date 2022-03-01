### Chapter 6. 분류 및 군집분석

## Chapter 6-1. 병아리의 성별을 구분할 수 있을까? (로지스틱 회귀)

# 데이터 불러오기
g <- read.csv("ch6-1.csv", header = TRUE)

head(g)

str(g)

# 로지스틱 회귀 실시
g_glm <- glm(gender ~ wing_length + tail_length,
             data = g, family = binomial)

summary(g_glm)

# g_glm 변수에 포함된 객체(Object) 확인
names(g_glm)

# 기존 독립변수 데이터를 이용해 모델로 계산해 본 결과
g_glm$fitted.values

# g 데이터 셋에 pred열을 만들어 계산된 값을 넣음
g$pred <- g_glm$fitted.values
head(g)

# pred가 0.5보다 크면 m, 아니면 f로 판정하고 그 결과를 gender_pred라는 열에 넣음
g$gender_pred <- ifelse(g$pred > 0.5, 'm', 'f')
head(g)

# 간단한 정오분류표 그려보기
table(g$gender_pred, g$gender)

install.packages("caret")  # 정오분류표를 그리기 위한 패키지 설치
library(caret)
install.packages("e1071")  # 정오분류표를 그리기 위한 패키지 설치
library(e1071)

# caret 패키지 활용을 위해 g$gender_pred 열 데이터 유형 변경
g$gender_pred <- as.factor(g$gender_pred)

confusionMatrix(g$gender_pred, g$gender)

install.packages("Epi")  # ROC 커브를 그리기 위한 패키지 설치
library(Epi)

# ROC 커브 그리기
ROC(g$pred, g$gender, main = "ROC Curve")


## Chapter 6-2. 병아리 품종을 구분할 수 있을까? (분류 알고리즘)

# Naive Bayes
# 위에서 설치했지만 혹시나 몰라 넣었음
install.packages("e1071")  # Naive Bayes 수행을 위한 패키지 설치
library(e1071)

c_train <- read.csv("ch6-2_train.csv", header = TRUE)
c_test <- read.csv("ch6-2_test.csv", header = TRUE)
str(c_train)
str(c_test)

# 병아리 품종을 종속변수로 나머지 변수를 독립변수로한 학습 실시
c_nb <- naiveBayes(breeds ~., data = c_train)

c_nb

names(c_nb)

# 나이브 베이즈 모델에 테스트용 데이터셋을 이용해 품종 분류 실시
c_test$pred <- predict(c_nb, newdata = c_test, type = "class")

head(c_test)

library(caret)
confusionMatrix(c_test$pred, c_test$breeds)


# k-NN

install.packages("DMwR2")  # k-NN 수행을 위한 패키지 설치
library(DMwR2)

c_train <- read.csv("ch6-2_train.csv", header = TRUE)
c_test <- read.csv("ch6-2_test.csv", header = TRUE)

c_knn3 <- kNN(breeds ~., c_train, c_test, k = 3)
c_test$pred3 <- c_knn3  # 예측값을 c_test 데이터셋에 pred3열을 만들어서 입력
head(c_test)  # 데이터 확인


library(caret)
confusionMatrix(c_test$pred3, c_test$breeds)

# c_test 데이터 셋에 pred3열을 만들었기 때문에 1~4열까지만 선택해서 테스트 실시
c_knn5 <- kNN(breeds ~., c_train, c_test[,1:4], k = 5)
c_test$pred5 <- c_knn5
confusionMatrix(c_test$pred5, c_test$breeds)


# Decision Tree

install.packages("rpart")  # CART 수행을 위한 패키지 설치
library(rpart)

c_train <- read.csv("ch6-2_train.csv", header = TRUE)
c_test <- read.csv("ch6-2_test.csv", header = TRUE)


# 병아리 품종을 종속변수로 나머지 변수를 독립변수로한 학습 실시
c_rpart <- rpart(breeds ~., data = c_train)

c_rpart

# 의사결정트리 보기 편한 그래프를 그리기 위한  패키지 설치
install.packages("rpart.plot")
library(rpart.plot)
# 의사결정트리 그래프 그리기 type과 extra는 도움말(?rpr)을 통해 검색 참조
prp(c_rpart, type = 4, extra = 2, main = "Decision Tree")

# 자체 평가용 만들어봤음  
c_train$pred <- predict(c_rpart, data = c_train, type = "class")
table(c_train$pred, c_train$breeds)

# CART 모델에 테스트용 데이터 셋을 이용해 품종 분류 실시
c_test$pred <- predict(c_rpart, newdata = c_test, type = "class")
library(caret)
confusionMatrix(c_test$pred, c_test$breeds)


# Bagging

install.packages("adabag")  # 배깅 및 부스팅 수행을 위한 패키지 설치
library(adabag)

c_train <- read.csv("ch6-2_train.csv", header = TRUE)  # 훈련용 데이터셋 불러오기
c_test <- read.csv("ch6-2_test.csv", header = TRUE)  # 테스트용 데이터셋 불러오기

c_bag <- bagging(breeds ~., data = c_train, type = "class")

names(c_bag)  # 모델 객체 확인
c_bag$importance  # 모델 객체 중 importance 확인

c_bag$trees

# c_bag 모델의 trees 객체의 100번째 트리 그래프로 그리고, 텍스트 추가하기
# margin의 경우 그래프에서 텍스트가 잘리는 문제가 발생해 부여함
plot(c_bag$trees[[100]], main = "Bagging", margin=0.1)
text(c_bag$trees[[100]])

# 배깅 모델에 테스트용 데이터 셋을 이용해 품종 분류 실시
pred <- predict(c_bag, newdata = c_test, type = "class")

str(pred)  # 예측값(class) 외에도 다양한 결과가 입력된 리스트 형태임

# 모델의 예측값(class)만 c_test에 pred열을 만들어 입력(단, 형태는 factor로 변경)
c_test$pred <- as.factor(pred$class)

confusionMatrix(c_test$pred, c_test$breeds)


# (Ada)Boosting

library(adabag)

?adabag
c_train <- read.csv("ch6-2_train.csv", header = TRUE)
c_test <- read.csv("ch6-2_test.csv", header = TRUE)

c_boost <- boosting(breeds ~., data = c_train, type = "class")
c_boost$importance

# 그래프 글자 잘림 방지를 위해 margin 지정
plot(c_boost$trees[[100]], main = "Boosting-Adaboost", margin = 0.1)
text(c_boost$trees[[100]])

pred <- predict(c_boost, newdata = c_test, type = "class")

c_test$pred <- as.factor(pred$class)
library(caret)  # confusionMatrix() 함수 실행을 위한 라이브러리 불러오기
confusionMatrix(c_test$pred, c_test$breeds)


# Random Forest

install.packages("randomForest")  # 랜덤포레스트 수행을 위한 패키지 설치
library(randomForest)

c_train <- read.csv("ch6-2_train.csv", header = TRUE)
c_test <- read.csv("ch6-2_test.csv", header = TRUE)

c_rf <- randomForest(breeds ~., data = c_train, type = "class")

names(c_rf)

c_rf$importance  # 모델 객체 중 importance 확인
varImpPlot(c_rf)

c_test$pred <- predict(c_rf, newdata = c_test, type = "class")
library(caret)  # confusionMatrix() 함수 실행을 위한 라이브러리 불러오기
confusionMatrix(c_test$pred, c_test$breeds)


# SVM

library(e1071)  # 나이브 베이즈에서 사용했던 e1071 패키지 불러오기

c_train <- read.csv("ch6-2_train.csv", header = TRUE)  # 훈련용 데이터셋 불러오기
c_test <- read.csv("ch6-2_test.csv", header = TRUE)  # 테스트용 데이터셋 불러오기

# 병아리 품종을 종속변수로 나머지 변수를 독립변수로한 학습 실시
c_svm <- svm(breeds ~., data = c_train)

# svm 분류 내역 그래프로 확인
plot(c_svm, c_train, wing_length ~ tail_length,
     slice = list(comb_height = 34))

c_test$pred <- predict(c_svm, newdata = c_test, type = "class")
library(caret)  # confusionMatrix() 함수 실행을 위한 라이브러리 불러오기
confusionMatrix(c_test$pred, c_test$breeds)


# XGBoost, 데이터 타입을 바꿔야하고 학습이 있어야함

install.packages("xgboost")
library(xgboost)

c_train <- read.csv("ch6-2_train.csv", header = TRUE)
c_test <- read.csv("ch6-2_test.csv", header = TRUE)

c_x_train <- data.matrix(c_train[,1:3])  # 훈련용 데이터 셋 matrix 타입으로 만들기
c_y_train <- c_train[,4]  # 훈련용 라벨 만들기, vector 타입
c_x_test <- data.matrix(c_test[,1:3])  # 테스트용 데이터 셋 matrix 타입으로 만들기
c_y_test <- c_test[,4]  # 테스트용 라벨 만들기, vector 타입

?xgboost
c_xgb <- xgboost(data = c_x_train, label = as.numeric(c_y_train)-1,
                 num_class = 3, nrounds = 20, eta = 0.1,
                 objective = "multi:softprob")

# 모델에 테스트용 데이터 셋을 넣어 예측한 후 예측값(확률)을 matrix 타입으로 변환
c_y_test_pred <- predict(c_xgb, c_x_test, reshape = TRUE)

# 모델의 예측값(확률) 중 가장 큰 값에 대응되는 라벨로 매핑
c_y_test_pred_label <- levels(c_y_test)[max.col(c_y_test_pred)]

class(c_y_test_pred_label)

# character 속성인 예측결과 라벨을 factor 속성으로 변환(factor의 경우 level이 존재)
c_y_test_pred_label <- as.factor(c_y_test_pred_label)

library(caret)  # confusionMatrix() 함수 실행을 위한 라이브러리 불러오기
confusionMatrix(c_y_test_pred_label, c_y_test)

# nrounds 20 -> 50 조정
c_xgb2 <- xgboost(data = c_x_train, label = as.numeric(c_y_train)-1,
                  num_class = 3, nrounds = 50, eta = 0.1,
                  objective = "multi:softprob")

# 모델에 테스트용 데이터 셋을 넣어 예측한 후 예측값(확률)을 matrix 타입으로 변환
c_y_test_pred2 <- predict(c_xgb2, c_x_test, reshape = TRUE)

# 모델의 예측값(확률) 중 가장 큰 값에 대응되는 라벨로 매핑
c_y_test_pred_label2 <- levels(c_y_test)[max.col(c_y_test_pred2)]

# character 속성인 예측결과 라벨을 factor 속성으로 변환
c_y_test_pred_label2 <- as.factor(c_y_test_pred_label2)

confusionMatrix(c_y_test_pred_label2, c_y_test)

# eta 0.1 -> 0.3 조정
c_xgb3 <- xgboost(data = c_x_train, label = as.numeric(c_y_train)-1,
                  num_class = 3, nrounds = 50, eta = 0.3,
                  objective = "multi:softprob")

# 모델에 테스트용 데이터 셋을 넣어 예측한 후 예측값(확률)을 matrix 타입으로 변환
c_y_test_pred3 <- predict(c_xgb3, c_x_test, reshape = TRUE)

# 모델의 예측값(확률) 중 가장 큰 값에 대응되는 라벨로 매핑
c_y_test_pred_label3 <- levels(c_y_test)[max.col(c_y_test_pred3)]

# character 속성인 예측결과 라벨을 factor 속성으로 변환
c_y_test_pred_label3 <- as.factor(c_y_test_pred_label3)

confusionMatrix(c_y_test_pred_label3, c_y_test)


## 효과적인 사육을 위해 사육환경을 분리해보자 (군집 알고리즘)

# K-means Clustering

cl <- read.csv("ch6-3.csv", header = TRUE)  # 데이터 셋 불러오기

str(cl)  # 데이터 셋 형태 확인
summary(cl)

plot(cl$food, cl$weight)  # 산점도, x축 food, y축 weight

cl_kmc <- kmeans(cl[,2:3], 3)  # k-means 군집 실시, k=3

cl_kmc

cl$cluster <- cl_kmc$cluster  # 군집결과 기존 데이터 셋에 입력
head(cl)  # 데이터 확인

# 산점도를 이용해 군집결과 확인, cluster에 따라 3가지 색상 부여
plot(cl$food, cl$weight, col = c("red", "blue", "green")[cl$cluster])

install.packages("cluster")  # clusplot 수행을 위한 패키지 설치
library(cluster)

# clusplot 함수를 이용해 더 보기 쉽게 군집 표현, col.p 옵션을 통해 군집에 따른 색상지정
clusplot(cl[,2:3], cl$cluster, col.p = cl$cluster)
