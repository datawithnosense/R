## Chapter 7. 연습문제(iris 데이터 셋을 활용한 딥 러닝)

# 1번 정답
# c_iris 데이터 셋 만들기
c_iris <- iris

# caret 패키지에서 preProcess() 함수로 스케일링을 제공하기 때문에 불러옴
library(caret)

# preProcess() 함수에서 method를 range로 지정하면 Normalization 가능
nor <- preProcess(c_iris[,-5], method="range")

# predict() 함수를 이용해 c_iris 데이터 Normalization 실시
n.c_iris <- predict(nor, c_iris)

summary(n.c_iris)  # Normalization 결과 확인

# 종속변수를 각각 0, 1, 2 숫자로 변환(첫 시작을 0으로 두기 위해 1을 뺐음)
n.c_iris$Species <- as.numeric(n.c_iris$Species) -1

library(keras)
# (keras라이브러리) 라벨 One-hot encoding
n.c_iris$Species <- to_categorical(n.c_iris$Species)

# 데이터 확인
head(n.c_iris)

# 2번 정답
# ind라는 인덱스를 무작위로 만들어 8:2로 훈련, 테스트 셋 분할
ind <- sample(1:nrow(n.c_iris), nrow(n.c_iris)*0.8, replace =F)
c_train <- n.c_iris[ind,] # 80%의 데이터를 훈련 셋으로 분할
c_test <- n.c_iris[-ind,] # 나머지 데이터를 테스트 셋으로 분할

c_x_train <- c_train[,1:4]  # 훈련용 데이터 셋 만들기
c_y_train <- c_train[,5]  # 훈련용 라벨 만들기
c_x_test <- c_test[,1:4]  # 테스트용 데이터 셋 만들기
c_y_test <- c_test[,5]  # 테스트용 라벨 만들기

# Matrix 형태로 변환
c_x_train <- as.matrix(c_x_train)
c_x_test <- as.matrix(c_x_test)

# 3번 정답
# 모델(model) 생성
model <- keras_model_sequential()

#모델에 계층 추가
model %>% 
  layer_dense(units = 16, activation = 'relu', input_shape = 4) %>% 
  layer_dense(units = 16, activation = 'relu') %>%
  layer_dense(units = 16, activation = 'relu') %>%
  layer_dense(units = 3, activation = 'softmax')

# 모델 살펴보기
summary(model)

# 모델 학습설정(compile)
model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = 'adam',
  metrics = 'accuracy'
)

# 모델 학습 실시 
history <- model %>% fit(
  c_x_train, 
  c_y_train, 
  epochs = 200,
  batch_size = 16,
  validation_split = 0.2
)

# 학습과정 그래프 표시
plot(history)

# 테스트 데이터셋을 이용한 분류성능 평가
pred_mat <- model %>% predict(c_x_test)

head(pred_mat)  # 데이터 확인

# 확률값에 따라 a, b, c로 결과 매핑
pred_mat_label <- levels(c_iris$Species)[max.col(pred_mat)]

head(pred_mat_label)  # 데이터 확인

pred <- as.factor(pred_mat_label)  # 예측값 factor로 타입 변경

act <- as.factor(c_iris[-ind,]$Species)  # 실제값 factor로 타입 변경

# 정오분류표(Confusion Matrix) 생성
library(caret)
confusionMatrix(pred, act)


