### Chapter 8. 텍스트 마이닝

## Chapter8-1. 고객 리뷰에서 어떻게 핵심을 파악할 수 있을까? (워드클라우드)

# 한글 자연어 분석 패키지 KoNLP(Korea Natural Language Processing)
# KoNLP 패키지가 더 이상 업데이트 되지 않아 기본 명령어로 설치되지 않음
# 따라서 의존성 패키지를 먼저 설치하고, github에 올려진 패키지를 수동 설치
install.packages("hash")
install.packages("rJava")
install.packages("tau")
install.packages("Sejong")
install.packages("RSQLite")
install.packages("devtools")

# Java 실행 문제 제거를 위해 설치
install.packages("multilinguer")
library(multilinguer)
install_jdk()

# github에 올려져있는 KoNLP 패키지 원격으로 설치
install.packages("remotes")
library(remotes)
install_github('haven-jeon/KoNLP', upgrade = "never", 
                        INSTALL_opts=c("--no-multiarch"))

library(KoNLP)


# 한글처리에 필요한 세종 사전 수행, 최초 실행 시 1을 입력해 설치 실시
useSejongDic()

# 미리 크롤링 해놓은 ch8.txt 파일을 txt에 저장함
txt <- readLines("ch8.txt", encoding = "UTF-8")
head(txt)

# txt에서 명사만 뽑아서 n에 저장
n <- extractNoun(txt)
head(n)  # 데이터 확인

# 텍스트 수정을 위해 n의 내용을 unlist해서 c에 저장함
c <- unlist(n)

# gsub 함수를 통해 텍스트 수정 실시
c2 <- gsub("육","육질", c)  # "육"은 "육질"로 변경
c2 <- gsub("재구","재구매", c2)  # "재구"는 "재구매"로 변경
c2 <- gsub("에서","", c2)  # "에서"는 제거
head(c2,30)

# c2에 저장된 명사 중 두 글자 이상이 되는 것만 필터링
c3 <- Filter(function(x) {nchar(x) >=2}, c2)
head(c3,30)

# c3를 table 함수를 이용해 단어별 빈도수가 나오게 만들고, wordcnt에 저장
wordcnt <- table(c3)
# 내림차순으로 정렬해서 어떤 단어가 많이 나왔는지 확인
sort(wordcnt, decreasing = TRUE)

# 다양한 색상을 적용하기 위해 RColorBrewer 패키지 설치(Chapter 4에서 이미 설치)
library(RColorBrewer)

# 팔레트 확인
display.brewer.all()

# 팔레트 지정
Dark2 <- brewer.pal(8, "Dark2")

# 워드 클라우드 패키지 설치 및 라이브러리 불러오기
install.packages("wordcloud")
library(wordcloud)

# 워드 클라우드로 표현, 옵션은 도움말로 검색해보세요!
wordcloud(names(wordcnt), freq=wordcnt, scale=c(4, 0.5), 
          rot.per=0.25, min.freq=1, random.order=F,
          random.color=T, colors=Dark2)


## Chapter8-2.고객들은 정말로 만족하였을까? (감성 분석)

# 사전 파일을 불러오기 위한 패키지 설치 및 라이브러리 불러오기
install.packages("readr")
library(readr)
# 이미 만들어진 리뷰 감성분석 전용 사전을 불러옴
# 필자가 실습을 위해 만들었으며 여러분도 시간만 투자하면 쉽게 만들 수 있습니다.
rev <- read_delim("review_dict.txt", delim="\t", col_names=c("word", "score"))

head(rev, 10)

# 감성분석을 위한 패키지 설치
install.packages("SentimentAnalysis")
library(SentimentAnalysis)

?SentimentDictionaryWeighted

# sd에 감성분석 전용 사전의 단어에 따른 점수로 가중치를 매길 수 있게 기준 설정
sentdic <- SentimentDictionaryWeighted(words = rev$word,
                                  scores = rev$score)

# 점수가 0보다 크면 긍정어(positive), 0보다 작으면 부정어(negative)로 기준 설정
sentdic <- SentimentDictionary(rev$word[rev$score>0],
                          rev$word[rev$score<0])

# 감성사전 기준 확인
summary(sentdic)

# 미리 크롤링 해놓은 ch8.txt 파일을 txt에 저장함
txt <- readLines("ch8.txt", encoding = "UTF-8")
head(txt)

# gsub 함수 대신 이번엔 stringr 패키지를 통해 텍스트 대체 실시
install.packages("stringr")
library(stringr)

# 마침표, 쉼표, 느낌표, 물음표를 다 없앰
txt_2 <- str_replace_all(txt, "([.,!?])","")
head(txt_2)

# txt_2 데이터 타입 확인
class(txt_2)

# 문서형 데이터 형태 변환을 위해 tm 패키지 설치 및 라이브러리 불러오기
install.packages("tm")
library(tm)

# txt_2 데이터를 Corpus 형태로 변환
co_txt <- Corpus(VectorSource(txt_2))
class(co_txt)  # 데이터 타입 확인

inspect(co_txt)  # co_txt 형태 살펴보기

# Corpus 형태에서 DocumentTermMatrix 형태로 변환
dtm_txt <- DocumentTermMatrix(co_txt)

# DocumentTermMatrix 형태 살펴보기
inspect(dtm_txt)

# (참고)특정 문서와 특정 단어순서들을 지정해서 검색할 수도 있음
inspect(dtm_txt[2,1:9])

# dtm_txt를 위에서 만든 감성 기준 사전 sentdic을 이용해 분석
res <- analyzeSentiment(dtm_txt, language="korean", 
                      rules=list("sentiment"=list(ruleSentiment, sentdic)))
# 결과확인
head(res)

# sentiment가 0보다 크면 긍정, 0이면 중립, 0보다 작으면 부정으로 표시
res$pn <- ifelse(res$sentiment>0,"Positive",
                 ifelse(res$sentiment==0,"Neutral","Negative"))

head(res)

# 결과 요약해서 보기
table(res$pn)

# 결과 별도 저장하되 데이터 프레임 형태로 변환
df_res <- as.data.frame(table(res$pn))

# 데이터 프레임 열이름 별도 지정
names(df_res) <- c("res","freq")

# 파이차트에 퍼센트 표시를 위해 pct 열 생성
df_res$pct <- round(df_res$freq/sum(df_res$freq)*100, 1)

df_res

# 파이차트로 감성 분석 결과 확인하기
pie(df_res$freq, labels = paste(df_res$res, df_res$pct, "%"),
    main = "생닭 판매 고객 리뷰 감성 분석 결과")
