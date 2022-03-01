## Chapter8. 연습문제

# 1번 정답
# 미리 크롤링 해놓은 martin_luther_king.txt 파일을 프로젝트 폴더에 옮기고, mlk에 저장함
mlk <- readLines("martin_luther_king.txt", encoding = "UTF-8")

# 2번 정답
library(KoNLP)

# 한글처리에 필요한 세종 사전 수행, 최초 실행 시 1을 입력해 설치 실시
useSejongDic()

# mlk에서 명사만 뽑아서 n에 저장
mlk_n <- extractNoun(mlk)

# 텍스트 수정을 위해 mlk_n의 내용을 unlist해서 m에 저장함
m <- unlist(mlk_n)

# gsub 함수를 통해 텍스트 수정 실시
m2 <- gsub("미국에는","미국", m)
m2 <- gsub("워싱턴으로","워싱턴", m2)

# 3번 정답 : 
# m2에 저장된 명사 중 두 글자 이상이 되는 것만 필터링
m3 <- Filter(function(x) {nchar(x) >=2}, m2)

# m3를 table 함수를 이용해 단어별 빈도수가 나오게 만들고, w에 저장
w <- table(m3)

# 내림차순으로 정렬해서 어떤 단어가 많이 나왔는지 확인
sort(w, decreasing = TRUE)

# 다양한 색상을 적용하기 위해 RColorBrewer 패키지 설치(Chapter 4에서 이미 설치)
library(RColorBrewer)

# 팔레트 확인
display.brewer.all()

# 팔레트 지정
Dark2 <- brewer.pal(8, "Dark2")

# 워드 클라우드 라이브러리 불러오기
library(wordcloud)

# 워드 클라우드로 표현, 옵션은 도움말로 검색해보세요!
wordcloud(names(w), freq=w, scale=c(5, 0.5), 
          rot.per=0.25, min.freq=1, random.order=F,
          random.color=T, colors=Dark2)