---
title: "Untitled"
output:
  html_document: 
    keep_md: yes
---

***

## < 분석 개요 >  

**1. 데이터**  
: 가온차트 사이트 순위 추출
  
**2. 목적**  
- 웹 사이트 데이터를 크롤링, 정제 단계까지 진행하면서 전반적인 데이터 수집에 대한 프로세스를 연습한다.
- 대형 음원사들의 데이터를 집계한 가온차트를 통해 최장 차트 점유 곡, 예능별 다운로드 횟수 등을 알아본다.

***

```r
library(XML)
library(stringr)
```

```
## Warning: package 'stringr' was built under R version 3.6.3
```

```r
library(plyr)
```

```
## Warning: package 'plyr' was built under R version 3.6.3
```

```r
library(reshape)
```

```
## Warning: package 'reshape' was built under R version 3.6.3
```

```r
library(ggplot2)
```

## 1. 데이터 수집
### 1) 가온차트 주소 불러오기

```r
url <- "http://www.gaonchart.co.kr/main/section/chart/online.gaon?nationGbn=T&serviceGbn=S1020&targetTime=1&hitYear=2016&termGbn=week" #URL 불러오기
start <- as.Date(rep("2016-01-03",100)) #2016년 첫째 주 시작일
end <- as.Date(rep("2016-01-09",100)) #2016년 첫째 주 종료일
data <- readLines(url,encoding="UTF-8")
```

### 2) 음원 제목 정제

```r
title <- data[str_detect(data,"<p title=")]
title <- gsub("<>/|","",title)
title <- gsub("</p>","",title)
title <- gsub("\t\t\t\t\t<p title=","",title)
title <- gsub("\"","",title)
title <- gsub(">","",title)
for(i in 1:100){
  num <- nchar(title[i])/2
  title[i] <- substr(title[i],1,num)
}

head(title)
```

```
## [1] "또 하루 (Feat. 개코)" "어땠을까"             "매일 그대와"         
## [4] "함께"                 "소녀"                 "걱정말아요 그대"
```

### 3) 가수, 앨범명 정제

```r
singer_album <- data[str_detect(data,"<p class=\"singer\" title=")]
singer_album <-  gsub("\t\t\t\t\t<p class=","",singer_album)
singer_album <-  gsub(" title=","",singer_album)
singer_album <- gsub("\"singer/","",singer_album)
singer_album <- gsub("<span class=\"bar\\>","",singer_album)
singer_album <- gsub("</span>","",singer_album)
singer_album <- gsub("</p>","",singer_album)
singer_album <- gsub("singer","",singer_album)
singer_album <- gsub(">","",singer_album)
singer_album <- gsub('\"\"\"',"",singer_album)
singer_album <- gsub('"',"",singer_album)
singer_album <- data.frame(do.call("rbind", strsplit(as.character(singer_album), split = "|", fixed = T)))

head(singer_album)
```

```
##                 X1                                       X2
## 1     Gary (개리)                        또 하루Gary (개리)
## 2          김나영                            어땠을까김나영
## 3 소진 (걸스데이)   응답하라 1988 OST Part 8소진 (걸스데이)
## 4            노을              응답하라 1988 OST Part 7노을
## 5            오혁              응답하라 1988 OST Part 3오혁
## 6            이적              응답하라 1988 OST Part 2이적
##                         X3
## 1                  또 하루
## 2                 어땠을까
## 3 응답하라 1988 OST Part 8
## 4 응답하라 1988 OST Part 7
## 5 응답하라 1988 OST Part 3
## 6 응답하라 1988 OST Part 2
```

```r
singer <- as.character(singer_album$X1)
album <- as.character(singer_album$X3)
```

### 4) 다운로드 횟수 정제

```r
count <- which(str_detect(data, "class=\"count\">"))
count <- data[count+1]
count <- gsub("\t\t\t\t\t<p>","",count)
count <- gsub("</p>","",count)
count <- gsub(",","",count)
count <- gsub("<!--","",count)
count <- gsub("히트수-->","",count)
count <- as.numeric(count)

head(count)
```

```
## [1] 170898 145589  85124  81875  75382  74835
```

### 5) 제작사 정제

```r
pro <- data[str_detect(data,"<p class=\"pro\"")]
pro <- gsub("\t\t\t\t\t","",pro)
pro <- gsub("<p class=\"pro\" title=","",pro)
pro <- gsub("</p>","",pro)
pro <- gsub("\"","",pro)
pro <- gsub(">","",pro)
for(i in 1:100){
  num <- nchar(pro[i])/2
  pro[i] <- substr(pro[i],1,num)
}

head(pro)
```

```
## [1] "리쌍컴퍼니"                               
## [2] "네버랜드엔터테인먼트, 로엔엔터테인먼트"   
## [3] "Stone Music Entertainment, 쿵엔터테인먼트"
## [4] "Stone Music Entertainment, 쿵엔터테인먼트"
## [5] "Stone Music Entertainment, 쿵엔터테인먼트"
## [6] "Stone Music Entertainment, 쿵엔터테인먼트"
```

### 6) 유통사 정제

```r
dist <- data[str_detect(data,"<p class=\"dist\" title=")]
dist <- gsub("\"","",dist)
dist <- gsub("\t\t\t\t\t<p class=dist title=","",dist)
dist <- gsub("</p>","",dist)
dist <- gsub(">","",dist)
for(i in 1:100){
  num <- nchar(dist[i])/2
  dist[i] <- substr(dist[i],1,num)
}

head(dist)
```

```
## [1] "카카오 M"                  "카카오 M"                 
## [3] "Stone Music Entertainment" "Stone Music Entertainment"
## [5] "Stone Music Entertainment" "Stone Music Entertainment"
```

### 7) 순위

```r
rank <- 1:100
```

### 8) 데이터 병합

```r
gaon <- data.frame(start,end,rank,title,singer,album,count,pro,dist)
head(gaon)
```

```
##        start        end rank                title           singer
## 1 2016-01-03 2016-01-09    1 또 하루 (Feat. 개코)     Gary (개리) 
## 2 2016-01-03 2016-01-09    2             어땠을까          김나영 
## 3 2016-01-03 2016-01-09    3          매일 그대와 소진 (걸스데이) 
## 4 2016-01-03 2016-01-09    4                 함께            노을 
## 5 2016-01-03 2016-01-09    5                 소녀            오혁 
## 6 2016-01-03 2016-01-09    6      걱정말아요 그대            이적 
##                      album  count                                       pro
## 1                  또 하루 170898                                리쌍컴퍼니
## 2                 어땠을까 145589    네버랜드엔터테인먼트, 로엔엔터테인먼트
## 3 응답하라 1988 OST Part 8  85124 Stone Music Entertainment, 쿵엔터테인먼트
## 4 응답하라 1988 OST Part 7  81875 Stone Music Entertainment, 쿵엔터테인먼트
## 5 응답하라 1988 OST Part 3  75382 Stone Music Entertainment, 쿵엔터테인먼트
## 6 응답하라 1988 OST Part 2  74835 Stone Music Entertainment, 쿵엔터테인먼트
##                        dist
## 1                  카카오 M
## 2                  카카오 M
## 3 Stone Music Entertainment
## 4 Stone Music Entertainment
## 5 Stone Music Entertainment
## 6 Stone Music Entertainment
```


```r
write.csv(gaon, "gaonchart.csv")
```

## 2. 데이터 로드

```r
# 데이터 로드
raw <- read.csv("C:/Users/Windows10/Desktop/Project/data/gaonchart.csv")

# 데이터 확인
str(raw)
```

```
## 'data.frame':	4600 obs. of  9 variables:
##  $ start : Factor w/ 46 levels "2015-12-27","2016-01-03",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ end   : Factor w/ 46 levels "2016-01-02","2016-01-09",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ rank  : int  1 2 3 4 5 6 7 8 9 10 ...
##  $ title : Factor w/ 1036 levels "#떨려","..IS YOU",..: 625 848 643 1016 782 413 742 87 592 973 ...
##  $ singer: Factor w/ 459 levels "10cm","2LSON (투엘슨)",..: 26 82 240 102 311 349 333 266 421 92 ...
##  $ album : Factor w/ 631 levels "(아수라) X 비와이 (BewhY)",..: 400 499 546 545 541 540 457 579 36 537 ...
##  $ count : int  170898 145589 85124 81875 75382 74835 61750 60105 57808 56110 ...
##  $ pro   : Factor w/ 198 levels "$exy $treet & Yello Music",..: 100 80 20 20 20 20 26 72 83 20 ...
##  $ dist  : Factor w/ 19 levels "CJ E&M","Kt music",..: 10 10 1 1 1 1 10 2 14 1 ...
```

```r
attach(raw)
```

## 3. 데이터 분석
### 1) 차트 점유 기간

```r
title_u <- unique(raw$title) #모든 노래 제목 파악
temp <- c()

for(i in 1:length(title_u)){
  temp[i] <- sum(title_u[i]==raw[,"title"])
}

continue_week <- data.frame(title_u,temp)
names(continue_week)[1:2] <- c("title","week") #변수 이름 설정
continue_week <- continue_week[order(continue_week$week,decreasing = T),] #지속 주가 긴 노래 순서대로 배열
weektop10 <- head(continue_week,10) # 그중 10개를 뽑음

ggplot(weektop10, aes(x=title, y=week, fill=title)) +
  geom_bar(stat="identity", width=0.7) +
  #coord_fixed(ratio = 0.08)+
  geom_text(size=3.5, aes(label=round(week, 0), vjust=-0.3)) +
  scale_fill_brewer(palette = "Spectral", name="예능") +
  scale_y_continuous(breaks=c(0,10,20,30,40,50)) +
  ylab("지속 주") +
  xlab("노래 제목") +
  theme(axis.title = element_text(face = 'bold', size = 14),
        axis.text.x = element_text(angle=30),
        axis.ticks.length.x = unit(3, "mm"),
        text = element_text(size=13),
        legend.position = "top",
        legend.title =  element_text(face=2, size = 14),
        legend.key.size = unit(3, 'mm'),
        legend.text =  element_text(size=8))
```

![](gaon_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

### 2) 예능별 음원 다운로드 횟수

```r
enter <- raw[grep("투유|무한도전|보컬전쟁|판타스틱|언니들의|쇼미더머니|언프리티|복면가왕|슈퍼스타", raw$album),]
head(enter)
```

```
##          start        end rank           title
## 58  2015-12-27 2016-01-02   58     아마도 그건
## 59  2015-12-27 2016-01-02   59 겁 (Feat. 태양)
## 70  2015-12-27 2016-01-02   70 가질 수 없는 너
## 90  2015-12-27 2016-01-02   90      Okey Dokey
## 94  2015-12-27 2016-01-02   94          Heaven
## 100 2015-12-27 2016-01-02  100            레옹
##                                       singer                         album
## 58                 크러쉬(CRUSH), 로꼬(LOCO) 투유 프로젝트 - 슈가맨 Part 3
## 59                                    송민호        쇼미더머니 4 Episode 5
## 70                                      거미 투유 프로젝트 - 슈가맨 Part.7
## 90                        송민호, 지코(ZICO)        쇼미더머니 4 Episode 6
## 94                                      조권 투유 프로젝트 - 슈가맨 Part 9
## 100 이유 갓지(GOD G)않은이유(박명수, 아이유)  무한도전 영동고속도로 가요제
##     count    pro     dist
## 58  22233   JTBC 인터파크
## 59  21843 CJ E&M   CJ E&M
## 70  20583   JTBC 인터파크
## 90  16725 CJ E&M   CJ E&M
## 94  15670   JTBC 인터파크
## 100 14957    MBC     벅스
```

```r
enter$enter_title <- ifelse(grepl("투유", enter$album), "투유프로젝트",
                     ifelse(grepl("무한도전", enter$album), "무한도전",
                     ifelse(grepl("보컬전쟁", enter$album), "신의목소리",
                     ifelse(grepl("판타스틱", enter$album), "판타스틱듀오",
                     ifelse(grepl("언니들의", enter$album), "언니들의 슬램덩크",
                     ifelse(grepl("쇼미더머니", enter$album), "쇼미더머니",
                     ifelse(grepl("언프리티", enter$album), "언프리티랩스타",
                     ifelse(grepl("복면가왕", enter$album), "복면가왕",
                     ifelse(grepl("슈퍼스타", enter$album), "슈퍼스타K", "")))))))))

enter_d <- ddply(enter, .(enter_title), summarise, download=sum(count))
enter_d$download <- enter_d$download*0.001

ggplot(enter_d, aes(x=enter_title, y=download, fill=enter_title)) +
  geom_bar(stat="identity") +
  coord_fixed(ratio = 0.0004)+
  geom_text(size=3.5, aes(label=round(download, 0), vjust=-0.3)) +
  scale_fill_brewer(palette = "Spectral", name="예능") +
  ylab("다운로드 횟수(K)") +
  xlab("노래 제목") +
  theme(axis.title = element_text(face = 'bold', size = 14),
        axis.text.x = element_text(angle=30),
        axis.ticks.length.x = unit(3, "mm"),
        legend.position = "top",
        legend.title =  element_text(face=2, size = 14),
        legend.key.size = unit(4, 'mm'))
```

![](gaon_files/figure-html/unnamed-chunk-12-1.png)<!-- -->


































