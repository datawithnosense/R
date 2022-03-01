### Chapter 7. 인공신경망과 딥 러닝

## Chapter 7-1. 닭의 무게를 예측할 수 있을까? (회귀)

# 종란무게와 누적 사료량으로 닭의 무게 예측을 위한 데이터 불러오기
w <- read.csv("ch7-1.csv", header = TRUE)
head(w)
str(w)

# ind라는 인덱스를 무작위로 만들어 8:2로 훈련, 테스트 셋 분할
ind <- sample(1:nrow(w), nrow(w)*0.8, replace =F) 
r_train <- w[ind,] # 80%의 데이터를 훈련 셋으로 분할
r_test <- w[-ind,] # 나머지 데이터를 테스트 셋으로 분할
head(r_train)

cor(r_train)  # 상관분석을 통해 유의한 인자 인지 확인
# 훈련용 데이터셋 산점도 그리기
plot(r_train)

install.packages("nnet")  # 간단한 신경망 구현을 위한 패키지 설치
library(nnet)

?nnet
# nnet 함수활용 은닉층 하나의 간단한 신경망 구현
r_nn <- nnet(weight~., data = r_train, size = 3 , decay = 5e-4,
             ,rang = 0.1, maxit = 500, linout = TRUE)

summary(r_nn)  # 모델 확인

# r_test 데이터 셋을 이후에 쓸 예정으로 성능평가를 위한 test 데이터 셋 만들기
test <- r_test

# 신경망 모델을 이용해 예측값 생성
test$pred <- predict(r_nn, newdata = r_test)

head(test)  # 데이터 확인

# 산점도 확인
plot(test)

# 회귀 예측값 성능평가
library(caret)
R2(test$pred, test$weight)
RMSE(test$pred, test$weight)
MAE(test$pred, test$weight)


# Data Normalization
# caret 패키지에서 preProcess() 함수로 스케일링을 제공하기 때문에 불러옴
library(caret)

# preProcess() 함수에서 method를 range로 지정하면 Normalization 가능
nor <- preProcess(w[,1:2], method="range")

# predict() 함수를 이용해 r_train 데이터의 독립변수를 Normalization 실시
r_x_train <- predict(nor, r_train[,1:2])
summary(r_x_train)
# predict() 함수를 이용해 r_test 데이터의 독립변수를 Normalization 실시
r_x_test <- predict(nor, r_test[,1:2])
summary(r_x_test)

# Normalization 시킨 r_x_train 데이터 셋과 기존 종속변수를 열병합 실시
r_n_train <- cbind(r_x_train, r_train[,3])
names(r_n_train)[3] <- "weight"  # 합쳐진 데이터 셋의 3번째 열 이름을 weight로 변경

# Normalization 시킨 r_x_test 데이터 셋과 기존 종속변수를 열병합 실시
r_n_test <- cbind(r_x_test, r_test[,3])
names(r_n_test)[3] <- "weight"  # 합쳐진 데이터 셋의 3번째 열 이름을 weight로 변경

head(r_n_train)
head(r_n_test)

# nnet 함수활용 간단한 신경망 구현
r_nn_s <- nnet(weight~., data = r_n_train, size = 3, decay = 5e-4,
               ,rang = 0.1, maxit = 500, linout = TRUE)

summary(r_nn_s)


# 신경망 모델을 이용해 예측값 생성
r_n_test$pred <- predict(r_nn_s, newdata = r_n_test)
head(r_n_test)


# 회귀모델 성능 평가
library(caret)
R2(r_n_test$pred, r_n_test$weight)   # R2
RMSE(r_n_test$pred, r_n_test$weight)  # RMSE
MAE(r_n_test$pred, r_n_test$weight)  # MAE


# H2O 활용 딥러닝 실시(회귀)
install.packages("h2o")  # h2o 패키지 설치
library(h2o)  # h2o 라이브러리 불러오기
h2o.init()  # h2o 라이브러리 기동

# 데이터 불러오기
head(r_train)

# H2O 전용 데이터프레임으로 변환
hf_r_train <- as.h2o(r_train)
hf_r_test <- as.h2o(r_test)

head(hf_r_train)
str(hf_r_train)

# 설정된 심층 신경망과 역전파 알고리즘을 통하여 예측
# hf.r_train 데이터 셋 활용 노드가 3개씩인 2개의 은닉층을 가진 DNN 구축
fit <- h2o.deeplearning(x = 1:2, y = 3, training_frame = hf_r_train,
                        hidden = c(3, 3), epochs = 200,
                        standardize = TRUE)

summary(fit)

# hf.r_test 데이터셋과 fit 모델을 이용해 만든 예측값을 r_pred에 입력
r_pred <- h2o.predict(fit, newdata = hf_r_test)
class(r_pred)

# r_pred를 데이터프레임으로 변환
df_r_pred <- as.data.frame(r_pred)
class(df_r_pred)

library(caret)  # 성능평가를 위해 caret 라이브러리 불러오기
R2(df_r_pred$predict, r_test$weight)   # R2
RMSE(df_r_pred$predict, r_test$weight)  # RMSE
MAE(df_r_pred$predict, r_test$weight)  # MAE


## Chapter 7-2. 딥 러닝을 이용해 병아리 품종을 다시 구분 해보자 (분류)

# Keras 활용 딥러닝

install.packages("keras")  # keras 패키지 설치
install.packages("tensorflow")  # tensorflow 패키지 설치
library(keras)  # keras 라이브러리 불러오기
install_keras()  # keras 설치
library(tensorflow)  # tensorflow 라이브러리 불러오기
install_tensorflow()  # tensorlfow 설치

# 데이터 불러오기
c_train <- read.csv("ch6-2_train.csv", header = TRUE)
c_test <- read.csv("ch6-2_test.csv", header = TRUE)

c_x_train <- c_train[,1:3]  # 훈련용 데이터 셋 만들기
c_y_train <- c_train[,4]  # 훈련용 라벨 만들기, vector 타입
c_x_test <- c_test[,1:3]  # 테스트용 데이터 셋 만들기
c_y_test <- c_test[,4]  # 테스트용 라벨 만들기, vector 타입


str(c_x_train)

# caret 패키지에서 preProcess() 함수로 스케일링을 제공하기 때문에 불러옴
library(caret)

# preProcess() 함수에서 method를 range로 지정하면 Normalization 가능
nor <- preProcess(c_x_train[,1:3], method="range")

# predict() 함수를 이용해 c_x_train 데이터를 Normalization 실시
n_c_x_train <- predict(nor, c_x_train)
summary(n_c_x_train)  # Normalization 결과 확인

# predict() 함수를 이용해 c_x_test 데이터를 Normalization 실시
n_c_x_test <- predict(nor, c_x_test)
summary(n_c_x_test)  # Normalization 결과 확인

# Matrix 형태로 변환
n_c_x_train <- as.matrix(n_c_x_train)
n_c_x_test <- as.matrix(n_c_x_test)

# 종속변수 a, b, c를 각각 0, 1, 2 숫자로 변환(첫 시작을 0으로 두기 위해 1을 뺐음)
nu_c_y_train <- as.numeric(c_y_train) -1
nu_c_y_test <- as.numeric(c_y_test) -1

class(n_c_x_train)  # 데이터 형태 확인
class(nu_c_y_train)  # 데이터 형태 확인
head(nu_c_y_train)  # 문자가 숫자로 잘 변환되었는지 확인

str(nu_c_y_train)

# One-hot encoding 위해 필요
# (keras라이브러리) 라벨 One-hot encoding
library(keras)
o_c_y_train <- to_categorical(nu_c_y_train)
o_c_y_test <- to_categorical(nu_c_y_test)

head(o_c_y_train)
tail(o_c_y_train)

# 모델(model) 생성
model <- keras_model_sequential()

#모델에 계층 추가
model %>% 
  layer_dense(units = 16, activation = 'relu', input_shape = 3) %>% 
  layer_dense(units = 16, activation = 'relu') %>%
  layer_dense(units = 3, activation = 'softmax')

# 모델 살펴보기
summary(model)

?compile

# 모델 학습설정(compile)
model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = 'adam',
  metrics = 'accuracy'
)

# 모델 학습 실시 
history <- model %>% fit(
  n_c_x_train, 
  o_c_y_train, 
  epochs = 300,
  batch_size = 16,
  validation_split = 0.2
)

# 학습과정 그래프 표시
plot(history)

# 테스트 데이터셋을 이용한 분류성능 평가
pred_mat <- model %>% predict(n_c_x_test)

head(pred_mat)  # 데이터 확인

# 확률값에 따라 a, b, c로 결과 매핑
pred_mat_label <- levels(c_y_test)[max.col(pred_mat)]

head(pred_mat_label)  # 데이터 확인

pred <- as.factor(pred_mat_label)  # 예측값 factor로 타입 변경

act <- as.factor(c_y_test)  # 실제값 factor로 타입 변경

# 정오분류표(Confusion Matrix) 생성
library(caret)
confusionMatrix(pred, act)


# (참고)Dropout용 모델(model) 생성
model_d <- keras_model_sequential()

# 모델에 계층 추가(Dropout 40% 적용)
model_d %>% 
  layer_dense(units = 16, activation = 'relu', input_shape = 3) %>% 
  layer_dropout(rate = 0.4) %>% 
  layer_dense(units = 16, activation = 'relu') %>%
  layer_dropout(rate = 0.4) %>% 
  layer_dense(units = 3, activation = 'softmax')

# 모델 살펴보기
summary(model_d)

