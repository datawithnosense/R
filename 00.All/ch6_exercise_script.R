## Chapter 6. 연습문제

# 1번 정답
iris_cl <- as.data.frame(iris$Species)
names(iris_cl) <- c("act")
head(iris_cl)

# 2번 정답
# 나이브베이즈
library(e1071)
model_nb <- naiveBayes(Species~., data = iris, type = "class")

# AdaBoost
library(adabag)
model_adab <- boosting(Species~., data = iris, type = "class")

# 랜덤 포레스트
library(randomForest)
model_rf <- randomForest(Species~., data = iris, type = "class")

# SVM
library(e1071)
model_svm <- svm(Species~., data = iris)

# 3번 정답
iris_cl$pred_nb <- predict(model_nb, newdata = iris)

iris_pred <- predict(model_adab, newdata = iris)
iris_cl$pred_adab <- as.factor(iris_pred$class)

iris_cl$pred_rf <- predict(model_rf, newdata = iris)

iris_cl$pred_svm <- predict(model_svm, newdata = iris)

head(iris_cl)

# 4번 정답 : AdaBoost, 랜덤 포레스트 각각 정확도 100%
library(caret)
confusionMatrix(iris_cl$pred_nb, iris_cl$act)
confusionMatrix(iris_cl$pred_adab, iris_cl$act)
confusionMatrix(iris_cl$pred_rf, iris_cl$act)
confusionMatrix(iris_cl$pred_svm, iris_cl$act)