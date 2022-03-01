## DB에서 데이터 불러오는 법

# Posgresql DB 연결을 위한 패키지
install.packages("RPostgres")
library(RPostgres)

install.packages("RPostgreSQL")
library(RPostgreSQL)


# R에서 SQL을 사용할 수 있게 해주는 패키지
install.packages("sqldf")
library(sqldf)


# DB 연결 만들기
con <- dbConnect(dbDriver("Postgres"), 
                 dbname = "postgres", 
                 host = "localhost", 
                 port = 5432,
                 user = "postgres", 
                 password = "1234")

con

# DB iris 테이블에 test 데이터 조회하기(select)
qry_s = "select * from datawithr.iris"
sqldf(qry_s, connection = con)

# DB iris 테이블 불러와서 db_iris 변수에 데이터 프레임으로 넣기
db_iris <- sqldf(qry_s, connection = con)
head(db_iris)

# DB iris 테이블에 test 데이터 삽입하기(insert)
qry_i = "insert into datawithr.iris (sl, sw, pl, pw, species) values (1, 2, 3, 4, 'test')"
sqldf(qry_i, connection = con)

# 마지막부터 6행만 데이터 확인
tail(sqldf(qry_s, connection = con))

# DB iris 테이블에 test 데이터 삭제하기(delete)
qry_d = "delete from datawithr.iris where species = 'test'"
sqldf(qry_d, connection = con)

# 마지막부터 6행만 데이터 확인
tail(sqldf(qry_s, connection = con))

# 데이터베이스 연결 해제
dbDisconnect(con)
con


## 주성분분석

# USArrests 데이터 셋 구조 확인(범죄 통계 데이터)
str(USArrests)
head(USArrests)

# 주성분분석 실시
pca_usa <-princomp(USArrests, cor = TRUE)

# 주성분결과 확인
summary(pca_usa)

# 주성분 선형결합 확인
loadings(pca_usa)

# scree plot 통한 적정 주성분 개수 확인
plot(pca_usa, type = "l", main = "USArrests PCA Scree Plot")

# biplot 통한 주성분 영향 시각화
biplot(pca_usa)


## 데이터 재구조화

# 데이터 재구조화 패키지 설치 및 라이브러리 불러오기
install.packages("reshape2")
library(reshape2)

?airquality

# airquality 데이터 셋 구조 확인
str(airquality)
head(airquality)

# airqualtiy 데이터 셋을 melt로 데이터 구조 변환
m <- melt(id=c("Month", "Day"), airquality)

head(m)

subset(m, Month == 5 & Day == 1)

# 변경했던 데이터 셋을 복원
x <- dcast(m, Month + Day ~ variable)

head(x)


## R 내장 데이터셋 소개
?mtcars
str(mtcars)
mtcars

?Titanic
str(Titanic)
Titanic


## 유명한 데이터셋 소개

# mnist
library(keras)
mnist_lst <- dataset_mnist()
str(mnist_lst)

data()
