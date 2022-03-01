### Chapter 4. 통계분석과 기본 그래프

## Chapter4-1. 어제까지 몇 마리의 병아리가 부화했을까? (기초통계량)

# ch4-1.csv 파일의 데이터 불러오기
hat <- read.csv("ch4-1.csv", header = TRUE)

hat

# 데이터 확인하기, head는 가장 위 6행만 보여줌
head(hat)

# 데이터 확인하기, tail은 가장 아래 6행만 보여줌
tail(hat)

# head는 아래와 같이 df옆에 숫자를 입력하면 해당 숫자만큼 행이 출력됨
head(hat,3)

# 합계 구하기
sum(hat$chick)

# 평균 구하기
mean(hat$chick)

# 표준편차 구하기
sd(hat$chick)

# 중앙값 구하기
median(hat$chick)

# 최소값 구하기
min(hat$chick)

# 최대값 구하기
max(hat$chick)

# 데이터 정렬하기
hat_asc <- hat[order(hat$chick),] # chick 열을 기준으로 오름차순 정렬

hat_asc

# 간단한 그래프를 그려서 보자
# 막대그래프
barplot(hat$chick)

# 다양한 옵션을 통해 막대그래프 정보를 추가하자
barplot(hat$chick, names.arg = hat$hatchery,
        col = c("red","orange","yellow","green", "blue", "navy", "violet"), 
        main = "부화장별 병아리 부화현황", xlab = "부화장", ylab = "병아리수",
        ylim = c(0,35))

?barplot

install.packages("RColorBrewer") # RColorBrewer 이라는 색상 팔레트 패키지 설치
library(RColorBrewer) # RColorBrewer 패키지 현재 작업 환경으로 불러오기

display.brewer.all()

col7 <- brewer.pal(7, "Pastel2")  # col7이라는 변수에 "Pastel2"라는 팔레트에서 7개의 색상을 집어넣음

barplot(hat$chick, names.arg = hat$hatchery,
        col = col7, 
        main = "부화장별 병아리 부화현황", xlab = "부화장", ylab = "병아리수",
        ylim = c(0,35))


bar_x <- barplot(hat$chick)  # bar_x 변수에 barplot의 x좌표 집어넣음

# 위에 bar_chart라는 변수를 만들어주는 이유는 x좌표를 알아내기 위함임
bar_x

# 다시 예쁜 그래프 기리기
barplot(hat$chick, names.arg = hat$hatchery,
        col = col7, 
        main = "부화장별 병아리 부화현황", xlab = "부화장", ylab = "병아리수",
        ylim = c(0,35))


# 막대그래프에 text 추가, 라벨에 2가지 이상 넣을때는 paste를 써야함, pos는 라벨의 위치
text(x = bar_x, y = hat$chick, labels = hat$chick, pos = 3)

# 막대그래프에 30기준으로 빨간색 점선 추가
abline(h = 30, col = "red", lty = 2, lwd = 1)

# 파이차트 그리기

# 파이차트 그리기에 앞서 Percentage 열 만들어줌
hat$pct <- round(hat$chick/sum(hat$chick)*100, 1)
hat

# 파이차트 그리기
?pie
pie(hat$chick, labels = paste(hat$hatchery, hat$pct, "%"), 
    col = col7, clockwise = TRUE, 
    main = "부화장별 병아리 부화 비율")


## Chapter4-2. 부화한 병아리들의 체중은 얼마일까? (정규분포와 중심극한정리)

# ch4-2.csv 파일의 데이터 불러오기
b <- read.csv("ch4-2.csv", header = TRUE)

# 데이터가 정상적으로 불러와졌는지 확인하기, head는 가장 위 6행만 보여줌
head(b)

# 데이터의 형태와 변수 개수, 데이터 길이 확인 함수
str(b)

# 대략적인 데이터의 분포 확인
summary(b)

# B 부화장 병아리 무게 표준편차
sd(b$weight)

# (참고)정규분포 그래프 설명용 그리기
x <- seq(-5, 5, length = 500)
y1 <- dnorm(x, mean = 0, sd = 1)
y2 <- dnorm(x, mean = 0, sd = 2)

plot(x, y1, type = "l", col = "blue", ylabel = NULL, xlabel = NULL, main = "표준편차(1, 2)에 따른 정규분포 비교")
lines(x, y2, type = "l", col = "red")
legend("topright", c("X~N(0,1)","X~N(0,4)"), text.col = c("blue", "red"))

# Histogram으로 분포 확인
hist(b$weight, col = "sky blue", xlab = "병아리 무게(g)", main = "B 부화장 병아리 무게 분포 현황")

# Box-Plot으로 분포 확인
boxplot(b$weight, col = "sky blue", main = "B 부화장 병아리 무게 상자그림")

# 히스토그램과 Box-Plot을 같이 그리기
par(mfrow=c(2,1))  # 행 2개, 열 1개
hist(b$weight, col = "sky blue", xlab = "병아리 무게(g)", , main = "B 부화장 병아리 무게 분포 현황")
boxplot(b$weight, horizontal = TRUE, col = "sky blue")


## Chapter4-3. 사료 제조사별 성능차이가 있을까? (가설검정)

# ch4-3.csv 파일의 데이터 불러오기
test <- read.csv("ch4-3.csv", header = TRUE)
test

# 상자그림을 2개 그려서 비교해봄
boxplot(weight ~ hatchery, data = test, 
        horizontal = TRUE, col = c("light green", "sky blue"),
        ylab= "부화장", xlab = "몸무게 분포",
        main = "부화장 A vs. B 몸무게 분포 비교")

# 두 집단이 우선 정규분포를 따르고 있는지 샤피로 월크 검정 실시
a <- subset(test$weight, test$hatchery == 'A')
b <- subset(test$weight, test$hatchery == 'B')
shapiro.test(a)
shapiro.test(b)

# 2-표본사례(정규분포따름) : 부화 후 일주일 뒤 각각 다른 사료를 먹고 키운 병아리의 성장에 차이가 있을까?
# 두 집단의 평균이 다르다고 할 수 있을까?

t.test(data = test, weight ~ hatchery) 

# 결과해석 : p값이 0.01094로 0.05보다 작으므로 대립가설을 채택함, 즉 두 집단의 평균은 다름

# (참고)엑셀파일 불러오기 참고(test.xlsx 파일이 있어야함)
install.packages("openxlsx")
library(openxlsx)

?read.xlsx

# test.xlsx 파일의 두 번째 시트를 불러옴
excel_test <- read.xlsx("test.xlsx", sheet = 2)

head(excel_test)

class(excel_test)


