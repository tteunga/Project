---
title: "Untitled"
output: 
  html_document: 
    keep_md: yes
---

***

## < 분석 개요 >  

**1. 데이터**  
: 2016년 상반기 영화 데이터
  
**2. 목적**  
- 영화별 기초분석과 변수간의 관계를 분석
- 영화제목길이에 따른 매출 평균과 관객 평점 분석

***

```r
library(lubridate)
```

```
## Warning: package 'lubridate' was built under R version 3.6.3
```

```r
library(reshape)
```

```
## Warning: package 'reshape' was built under R version 3.6.3
```

```r
library(plyr)
```

```
## Warning: package 'plyr' was built under R version 3.6.3
```

```r
library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 3.6.3
```

```r
library(psych)
```

```
## Warning: package 'psych' was built under R version 3.6.3
```

## 1. 데이터 로드

```r
# 데이터 로드
raw <- read.csv("C:/Users/Windows10/Desktop/Project/data/2016_1_movie.csv")

# 데이터 확인
str(raw)
```

```
## 'data.frame':	17494 obs. of  23 variables:
##  $ 측정일시    : Factor w/ 265 levels "","1988-06-22",..: 1 90 90 90 90 90 90 90 90 90 ...
##  $ 순위        : int  NA 1 2 3 4 5 6 7 8 9 ...
##  $ 영화명      : Factor w/ 2089 levels "","[SIAFF2016] 국제단편경쟁 1",..: 1 185 2084 323 264 1019 1922 1709 675 2026 ...
##  $ 개봉일      : Factor w/ 574 levels "","1958-04-20",..: 1 434 423 429 434 430 434 434 426 434 ...
##  $ 매출액      : Factor w/ 6944 levels "","(27000)","0 ",..: 1 4976 4851 4844 4735 3769 2833 547 198 6538 ...
##  $ 매출액.1    : Factor w/ 364 levels "0.00%","0.10%",..: 364 79 73 72 68 27 322 200 196 125 ...
##  $ 매출액증감  : Factor w/ 7788 levels "-1,000","-1,001,000",..: 3814 6701 3541 3570 6505 738 5509 4532 3742 7536 ...
##  $ 매출액증감율: Factor w/ 370 levels "-10.00%","-100.00%",..: 11 14 1 1 319 4 276 31 6 310 ...
##  $ 누적매출액  : num  NA 5.34e+08 5.20e+10 9.33e+09 5.47e+08 ...
##  $ 관객수      : Factor w/ 3435 levels "","(3)","0 ",..: 1 3001 2861 2776 2748 2283 1790 779 636 202 ...
##  $ 관객수증감  : Factor w/ 3363 levels "-1","-1,001",..: 1747 3164 289 293 3041 438 2609 2144 391 2020 ...
##  $ 관객수증감율: Factor w/ 359 levels "-10.00%","-100.00%",..: 11 14 1 3 317 4 265 67 6 291 ...
##  $ 누적관객수  : num  NA 75762 6708901 1145624 70697 ...
##  $ 스크린수    : num  NA 585 614 730 503 563 352 300 296 153 ...
##  $ 상영횟수    : num  NA 1668 2571 2099 2317 ...
##  $ 대표국적    : Factor w/ 54 levels "","그리스","기타",..: 1 14 51 51 51 28 14 36 14 14 ...
##  $ 국적        : Factor w/ 210 levels "","그리스","그리스,독일,카타르",..: 1 37 195 195 195 95 37 127 37 37 ...
##  $ 제작사      : Factor w/ 320 levels "","'탁주'조합",..: 1 1 86 4 13 1 1 1 1 1 ...
##  $ 배급사      : Factor w/ 260 levels "","(사)필레마,필름포럼,홀리가든",..: 1 209 187 53 187 161 63 11 211 13 ...
##  $ 등급        : Factor w/ 35 levels "","12세 미만인 자는 관람할 수 없는 등급",..: 1 30 4 32 11 4 11 30 30 32 ...
##  $ 장르        : Factor w/ 294 levels "","SF","SF,드라마",..: 1 187 41 131 97 131 140 170 170 222 ...
##  $ 감독        : Factor w/ 1408 levels "","D.W. 그리피스",..: 1 1324 884 821 912 212 758 1349 1040 1195 ...
##  $ 배우        : Factor w/ 1588 levels "","G-드래곤,태양,최승현,대성,승리",..: 1 1 1570 689 1076 517 298 1 703 559 ...
```

```r
attach(raw)
```

## 2. 데이터 파악
### 1) 영화별 누적매출액

```r
sales_m <- melt(raw, id = "영화명", measure = "누적매출액", na.rm = T)
sales_c <- cast(sales_m, 영화명~..., max)
sales <- sales_c[order(sales_c$누적매출액,decreasing = T),]
head(sales)
```

```
##          영화명   누적매출액
## 658        명량 135753322310
## 180    국제시장 110933990730
## 824      베테랑 105169264250
## 1206       암살  98463522781
## 1538 인터스텔라  82613173500
## 131    검사외전  77316673964
```

### 2) 영화별 누적관객수

```r
audi_m <- melt(raw, id = "영화명", measure = "누적관객수", na.rm = T)
audi_c <- cast(audi_m, 영화명~..., max)
audi <- audi_c[order(audi_c$누적관객수, decreasing = T),]
head(audi)
```

```
##          영화명 누적관객수
## 658        명량   17615057
## 180    국제시장   14262199
## 824      베테랑   13414200
## 1206       암살   12705783
## 1538 인터스텔라   10304503
## 131    검사외전    9706697
```

### 3) 영화별 스크린수, 상영횟수

```r
screen_m <- melt(raw, id = "영화명", measure = c("스크린수","상영횟수"), na.rm = T)
screen_c <- cast(screen_m, 영화명~..., max)
plot(screen_c$상영횟수~screen_c$스크린수)
```

![](2016_movie_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
screen_s <- screen_c[order(screen_c$스크린수, decreasing = T),]
head(screen_s)
```

```
##                                 영화명 스크린수 상영횟수
## 1798            캡틴 아메리카: 시빌 워     1990    10336
## 131                           검사외전     1812     9451
## 785  배트맨 대 슈퍼맨: 저스티스의 시작     1708     8300
## 160                               곡성     1485     6355
## 1815                         쿵푸팬더3     1365     8061
## 1288                엑스맨: 아포칼립스     1258     5991
```

## 3. 데이터 분석
### 1) 스크린수와 누적관객수의 관계

```r
audi_screen_m <- melt(raw, id = "영화명", measure = c("누적관객수","스크린수"), na.rm = T)
audi_screen_c <- cast(audi_screen_m, 영화명~..., max)
audi_screen <- audi_screen_c[order(audi_screen_c$스크린수, decreasing = T),]
head(audi_screen)
```

```
##                                 영화명 누적관객수 스크린수
## 1798            캡틴 아메리카: 시빌 워    8676103     1990
## 131                           검사외전    9706697     1812
## 785  배트맨 대 슈퍼맨: 저스티스의 시작    2256680     1708
## 160                               곡성    6862271     1485
## 1815                         쿵푸팬더3    3984796     1365
## 1288                엑스맨: 아포칼립스    2934682     1258
```

```r
plot(audi_screen$스크린수~audi_screen$누적관객수)
```

![](2016_movie_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
cor(audi_screen)
```

```
##            누적관객수  스크린수
## 누적관객수  1.0000000 0.3339975
## 스크린수    0.3339975 1.0000000
```

### 2) 영화 제목 길이에 따른 누적 매출액, 관객수

```r
# 전체 데이터에서 영화 제목, 누적매출액,누적관객수만 추출
df <- raw[ ,c(3,9,13)]

# 영화별 최고누적매출액 추출
top_sales <- ddply(df, "영화명", summarize, 누적매출액=max(누적매출액))

#영화별 최고누적관객수 추출
top_audi <- ddply(df, "영화명", summarize, 누적관객수=max(누적관객수))

# 영화 제목 공백 제거 후 길이 추출
top_sales$제목길이 <- nchar(gsub("[[:space:]]", "", top_sales$영화명))
top_audi$제목길이 <- nchar(gsub("[[:space:]]", "", top_audi$영화명))
```


```r
# 평균 계산
mean_sales <- ddply(top_sales, "제목길이", summarize,평균누적매출액=mean(누적매출액))
mean_audi <- ddply(top_audi, "제목길이", summarize,평균누적관객수=mean(누적관객수))

# join
mean_sa <- left_join(mean_sales, mean_audi, by = "제목길이")
head(mean_sa)
```

```
##   제목길이 평균누적매출액 평균누적관객수
## 1        0             NA             NA
## 2        1      341164359        45416.4
## 3        2     4284470551       560670.9
## 4        3     1611954043       205985.5
## 5        4     2373201865       313123.0
## 6        5     1575697967       205227.9
```



```r
pairs.panels(mean_sa[c("제목길이","평균누적관객수")])
```

![](2016_movie_files/figure-html/unnamed-chunk-8-1.png)<!-- -->



