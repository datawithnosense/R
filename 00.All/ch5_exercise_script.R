## Chapter 5. 연습문제

# 1번 정답
test <- subset(iris[,1:4], iris$Species == 'virginica')

# 2번 정답 : Petal.Length
cor(test)

# 3번 정답 : 0.7469
test_lm <- lm(Sepal.Length ~ Petal.Length, data = test)

summary(test_lm)

# 4번 정답 : Sepal.Width와 Petal.Width는 통계적으로 유의하지 않음
test_mlm <- lm(Sepal.Length ~ ., data = test)

summary(test_mlm)

# 5번 정답 : 다중공선성 문제 없음
library(car)
vif(test_mlm)