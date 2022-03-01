### Chapter 3. 데이터 다루기

## Chapter3-2. 데이터 프레임 다루기

# 데이터 프레임 열 이름 변경
names(iris) # names 함수를 이용해 열이름만 확인

df <- iris  # iris 데이터 셋을 df라는 변수에 집어넣음

names(df)
names(df) <- c("sl", "sw", "pl", "pw", "sp")  # names 함수를 이용해 이름 변경
names(df)

names(df)[5] <- "s"  # 5번째 열이름을 "s"로 변경
names(df)


# 데이터 프레임 특정 데이터만 추출하기
?subset

str(df)

# df 데이터 셋에서 s가 'versicolor'인 대상만 추출해 df_1에 넣음
df_1 <- subset(df, s == 'versicolor')

# df 데이터 셋에서 sl이 6보다 크고, s가 'versicolor'인 대상만 추출해 df_2에 넣음
df_2 <- subset(df, sl > 6 & s == 'versicolor')

# df 데이터 셋에서 sl, sw, s 열만 추출해 df_3에 넣음
df_3 <- subset(df, s == 'setosa', select = c(sl, sw, s))

# df 데이터 셋에서 s 열만 제외하고 추출해 df_4에 넣음
df_4 <- subset(df, select = -c(s))

df_1
df_2
df_3
df_4

str(df)

v <- c(1, 2, 3, 4, 5, 6, 7, 8, 9)  # 벡터 생성 예시

m <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), nrow=3)  # 행렬 생성 예시

d <- data.frame(x=c(1, 2, 3), y=c(4, 5, 6), z=c('a', 'b', 'c'))  # 데이터 프레임 생성 예시

a <- array(1:18, dim=c(2,3,3))  # 배열 생성 예시

l <- list(name = 'Jon Snow', family = c('Stark','Targaryen'))  # 리스트 생성 예시

class(v)  # v 데이터 타입 확인, 벡터는 별도 표시 안됨
class(m)  # m 데이터 타입 확인
class(d)  # d 데이터 타입 확인
class(a)  # a 데이터 타입 확인
class(l)  # l 데이터 타입 확인


## Chapter3-3. 데이터 정제

# 실제 데이터 불러와서 결측치 제거
air <- read.csv("ch3-1.csv", header = TRUE)

str(air)

air

# 결측치(NA) 제거
air_d <- na.omit(air)

air_d

air_m <- air

# 결측치 중앙값 대체를 위한 DMwR2 패키지 설치 및 라이브러리 불러오기
install.packages("DMwR2")
library(DMwR2)

median(air_m$PM10, na.rm = TRUE) # 결측치를 제외한 PM10의 중앙값 계산
median(air_m$PM25, na.rm = TRUE) # 결측치를 제외한 PM25의 중앙값 계산

air_m[14:16,]  # 14~16시에만 결측치가 존재하므로 해당 행번호를 이용해 대상만 확인
air_m

air_c <- centralImputation(air_m)
air_m[14:16,]  # air_m 데이터 셋의 14~16 번째 행의 값만 표시
air_c[14:16,]  # air_c 데이터 셋의 14~16 번째 행의 값만 표시


str(air_m)

air_k <- air_m[,-1]  # 첫번 째 열을 제외하고, 별도 데이터 셋 구성
air_knn <- knnImputation(air_k)  # 인접값 가중 평균으로 대체
air_knn[14:16,]  # air_knn 데이터 셋의 14~16 번째 행의 값만 표시
air_c[14:16,-1]  # 앞서 계산한 중앙값 대체 데이터와 비교


