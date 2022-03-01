## Chapter 4. 연습문제

# 1번 정답 : 5개의 열, 150개의 행, 4개의 Numeric, 1개의 Factor 구조
str(iris)

# 2번 정답
head(iris, 10)

# 3번 정답 : 평균 - 3.057333, 표준편차 - 0.4358663, 3사분위수 3.300
mean(iris$Sepal.Width)
sd(iris$Sepal.Width)
summary(iris$Sepal.Width)

# 4번 정답
hist(iris$Sepal.Width)

# 5번 정답 : setosa
boxplot(Sepal.Width ~ Species, data = iris)

# 6번 정답 
s <- subset(iris$Sepal.Width, iris$Species == 'setosa')
v <- subset(iris$Sepal.Width, iris$Species == "versicolor")

# 7번 정답 : 95% 신뢰수준에서 두 집단 모두 정규분포함
shapiro.test(s)
shapiro.test(v)

# 8번 정답: 95% 신뢰수준에서 두 집단의 평균은 서로 같지 않음
t.test(s, v)