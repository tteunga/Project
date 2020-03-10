
# < 교통사고 기술통계분석 및 GPS 좌표 시각화 >

> ## 분석 개요
**1. 데이터**
- 2012 ~ 2014 교통사망사고 정보  
- 평택시 CCTV 현황  
- 화성시 CCTV 현황  
  
**2. 목적**  
**- 기술통계분석**  
: 요약 통계량으로 교통사고사상자 증감 추이와 보험사기 연관성 파악  
**- GPS 좌표 시각화**  
: 경기도 내 최다 사고 지역과 GPS 좌표를 이용한 사고발생 현황 및 CCTV 취약지 시각화  

***
  
### # 패키지 로드

```r
library(plyr)
library(ggplot2)
library(dplyr)
library(stringr)
library(lubridate)
library(ggmap)
library(reshape)
```

## 1. 데이터 로드

```r
# 경로 지정
# setwd("C:/Users/Windows10/Desktop/Project/data")

# 데이터 로드
raw <- read.csv("C:/Users/Windows10/Desktop/Project/data/2012_2014_교통사망사고정보.csv")

# 데이터 확인
str(raw)
```

```
## 'data.frame':	14624 obs. of  28 variables:
##  $ 발생년              : int  2012 2012 2012 2012 2012 2012 2012 2012 2012 2012 ...
##  $ 발생월              : int  201201 201201 201201 201201 201201 201201 201201 201201 201201 201201 ...
##  $ 발생일              : int  20120101 20120101 20120101 20120101 20120101 20120101 20120101 20120101 20120101 20120101 ...
##  $ 발생시간            : Factor w/ 24 levels "00시","01시",..: 2 2 3 3 4 4 5 5 5 6 ...
##  $ 주야                : Factor w/ 2 levels "야","주": 1 1 1 1 1 1 1 1 1 1 ...
##  $ 요일                : Factor w/ 7 levels "금","목","수",..: 5 5 5 5 5 5 5 5 5 5 ...
##  $ 사고유형            : Factor w/ 17 levels "경보기 무시",..: 9 15 16 17 6 17 2 2 15 3 ...
##  $ 사망자수            : int  1 1 1 1 1 1 1 1 1 1 ...
##  $ 부상자수            : int  5 0 3 0 0 0 0 1 0 0 ...
##  $ 중상자수            : int  5 0 0 0 0 0 0 1 0 0 ...
##  $ 경상자수            : int  0 0 3 0 0 0 0 0 0 0 ...
##  $ 부상신고자수        : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ 발생지_시도         : Factor w/ 17 levels "강원","경기",..: 14 9 9 9 4 5 4 16 2 8 ...
##  $ 발생지_시군구       : Factor w/ 208 levels "가평군","강남구",..: 163 148 167 92 135 87 11 90 205 6 ...
##  $ 사고유형_대분류     : Factor w/ 4 levels "건널목","차대사람",..: 3 2 3 2 4 2 4 4 2 3 ...
##  $ 사고유형_중분류     : Factor w/ 15 levels "경보기무시","공작물충돌",..: 8 12 14 15 5 15 2 2 12 3 ...
##  $ 법규위반1당_대분류  : Factor w/ 3 levels "보행자과실","운전자법규위반",..: 2 2 2 2 2 2 2 2 2 2 ...
##  $ 법규위반1당_중분류  : Factor w/ 19 levels "과속","교차로 통행방법 위반",..: 14 10 8 10 10 10 10 10 10 10 ...
##  $ 도로형태_대분류     : Factor w/ 6 levels "건널목","고가도로위",..: 5 5 3 3 5 4 5 5 5 5 ...
##  $ 도로형태            : Factor w/ 11 levels "건널목","고가도로위",..: 7 7 4 5 7 6 7 7 10 7 ...
##  $ 당사자종별1당_대분류: Factor w/ 10 levels "건설기계","농기계",..: 4 4 5 4 4 4 6 4 4 4 ...
##  $ 당사자종별1당       : Factor w/ 10 levels "건설기계","경형",..: 10 10 10 10 10 10 8 10 7 10 ...
##  $ 당사자종별2당_대분류: Factor w/ 13 levels "0","건설기계",..: 6 4 6 4 1 4 1 1 4 12 ...
##  $ 당사자종별2당       : Factor w/ 13 levels "0","건설기계",..: 9 6 13 6 1 6 1 1 6 5 ...
##  $ 발생위치X..NO_005_X.: int  946526 949859 955169 958876 1070286 939330 1114053 911131 938839 1133679 ...
##  $ 발생위치Y..NO_005_Y.: int  1737709 1957198 1952212 1942819 1834622 1684433 1761943 1861851 1912203 1691741 ...
##  $ X.Grd.경도.         : num  127 127 127 127 128 ...
##  $ Y.Grd.위도.         : num  35.6 37.6 37.6 37.5 36.5 ...
```

## 2. 데이터 전처리
### 1) 'time' 변수 생성 (시간 데이터 정제)

```r
raw$발생일 <- as.character(raw$발생일)
raw$발생시간 <- as.character(raw$발생시간)

# 각 자리에서 년, 월, 일, 시간을 분리
hour <- str_sub(raw$발생시간, 1, 2) %>% paste('00', sep=':')

year <- str_sub(raw$발생일, 1, 4)
month <- str_sub(raw$발생일, 5, 6)
day <- str_sub(raw$발생일, 7, 8)

# 새로운 time 변수
time <- paste(year, month, day, sep='-') %>% paste(hour, sep=' ')
time <- as.POSIXct(time)
```

### 2) 'site' 변수 생성

```r
# ex) 경기도 수원시
site <- paste(raw$발생지_시도, raw$발생지_시군구, sep='')
```

### 3) 'total'(총 사상자) 변수 생성

```r
# 사망자 수 + 중상자 수 + 경상자 수 + 부상신고자 수 = 총 사상자 수
total <- c()
for (i in 1:nrow(raw)){
  total[i] <- raw[i,8] + raw[i,9] + raw[i,10] + raw[i,11] + raw[i,12]
}
```



```r
# 더미 변수 생성
freq <- rep(1, nrow(raw))

# 새 변수들 병합
data <- cbind(raw, time, site, freq, total)
head(data)
```

```
##   발생년 발생월   발생일 발생시간 주야 요일      사고유형 사망자수 부상자수
## 1   2012 201201 20120101     01시   야   일      정면충돌        1        5
## 2   2012 201201 20120101     01시   야   일    차도통행중        1        0
## 3   2012 201201 20120101     02시   야   일  측면직각충돌        1        3
## 4   2012 201201 20120101     02시   야   일        횡단중        1        0
## 5   2012 201201 20120101     03시   야   일 도로이탈 추락        1        0
## 6   2012 201201 20120101     03시   야   일        횡단중        1        0
##   중상자수 경상자수 부상신고자수 발생지_시도 발생지_시군구 사고유형_대분류
## 1        5        0            0        전북        정읍시          차대차
## 2        0        0            0        서울        은평구        차대사람
## 3        0        3            0        서울          중구          차대차
## 4        0        0            0        서울        서초구        차대사람
## 5        0        0            0        경북        예천군        차량단독
## 6        0        0            0        광주          서구        차대사람
##   사고유형_중분류 법규위반1당_대분류   법규위반1당_중분류 도로형태_대분류
## 1        정면충돌     운전자법규위반          중앙선 침범          단일로
## 2      차도통행중     운전자법규위반 안전운전 의무 불이행          단일로
## 3    측면직각충돌     운전자법규위반             신호위반          교차로
## 4          횡단중     운전자법규위반 안전운전 의무 불이행          교차로
## 5        도로이탈     운전자법규위반 안전운전 의무 불이행          단일로
## 6          횡단중     운전자법규위반 안전운전 의무 불이행       기타/불명
##     도로형태 당사자종별1당_대분류 당사자종별1당 당사자종별2당_대분류
## 1 기타단일로               승용차          중형               승용차
## 2 기타단일로               승용차          중형               보행자
## 3   교차로내               승합차          중형               승용차
## 4 교차로부근               승용차          중형               보행자
## 5 기타단일로               승용차          중형                    0
## 6  기타/불명               승용차          중형               보행자
##   당사자종별2당 발생위치X..NO_005_X. 발생위치Y..NO_005_Y. X.Grd.경도.
## 1          소형               946526              1737709    126.9094
## 2        보행자               949859              1957198    126.9319
## 3          중형               955169              1952212    126.9923
## 4        보행자               958876              1942819    127.0348
## 5             0              1070286              1834622    128.2849
## 6        보행자               939330              1684433    126.8339
##   Y.Grd.위도.                time       site freq total
## 1    35.63408 2012-01-01 01:00:00 전북정읍시    1    11
## 2    37.61285 2012-01-01 01:00:00 서울은평구    1     1
## 3    37.56818 2012-01-01 02:00:00   서울중구    1     7
## 4    37.48370 2012-01-01 02:00:00 서울서초구    1     1
## 5    36.50669 2012-01-01 03:00:00 경북예천군    1     1
## 6    35.15331 2012-01-01 03:00:00   광주서구    1     1
```

## 3. 교통사고사상자 증감 추이 분석
### 1) 년도별 추세 확인
##### *부상신고자수: 교통사고 발생시 본인이 사고를 당하였다고 신고한 사람의 수

```r
# 년도별 사상자 수, 발생 빈도 요약 통계
sum_by_year <- ddply(data, .(발생년), summarise, dead=sum(사망자수), hard_hit=sum(중상자수), simple_hit=sum(경상자수), 
                     report_hit=sum(부상신고자수), freq=sum(freq)) %>% data.frame()

# 열 이름 변경
colnames(sum_by_year) <- c("발생년도", "사망자수", "중상자수", "경상자수", "부상신고자수", "교통사고발생수")

sum_by_year
```

```
##   발생년도 사망자수 중상자수 경상자수 부상신고자수 교통사고발생수
## 1     2012     5392     1747     1347          179           5165
## 2     2013     5092     1612     1236          128           4876
## 3     2014     4762     1271     1013          176           4583
```

### 2) 분기별 추세 확인

```r
# 분기별 분류
year2012 <- filter(data, 발생년 == "2012")
year2013 <- filter(data, 발생년 == "2013")
year2014 <- filter(data, 발생년 == "2014")

# 년도/분기별 합계
sum_by_quarter_2012 <- ddply(year2012, .(quarter(time)), summarise, dead=sum(사망자수), hard_hit=sum(중상자수),
                             simple_hit=sum(경상자수), report_hit=sum(부상신고자수), freq=sum(freq)) %>% data.frame()
sum_by_quarter_2013 <- ddply(year2013, .(quarter(time)), summarise, dead=sum(사망자수), hard_hit=sum(중상자수),
                             simple_hit=sum(경상자수), report_hit=sum(부상신고자수), freq=sum(freq)) %>% data.frame()
sum_by_quarter_2014 <- ddply(year2014, .(quarter(time)), summarise, dead=sum(사망자수), hard_hit=sum(중상자수),
                             simple_hit=sum(경상자수), report_hit=sum(부상신고자수), freq=sum(freq)) %>% data.frame()

sum_by_quarter <- rbind(sum_by_quarter_2012, sum_by_quarter_2013, sum_by_quarter_2014)
head(sum_by_quarter)
```

```
##   quarter.time. dead hard_hit simple_hit report_hit freq
## 1             1 1214      405        277         84 1152
## 2             2 1403      491        393         47 1340
## 3             3 1311      367        336         18 1263
## 4             4 1464      484        341         30 1410
## 5             1 1133      405        262         25 1094
## 6             2 1236      382        307         26 1176
```

```r
#필요 없는 변수 제거
sum_by_quarter <- sum_by_quarter[,-1]

# 년도 변수 생성
year_quarter <- c(2012.1, 2012.2, 2012.3, 2012.4, 
                  2013.1, 2013.2, 2013.3, 2013.4, 
                  2014.1, 2014.2, 2014.3, 2014.4)
# 병합
sum_by_quarter <- cbind(year_quarter, sum_by_quarter)

colnames(sum_by_quarter) <- c("발생년도", "사망자수", "중상자수", "경상자수", "부상신고자수", "교통사고발생수")
head(sum_by_quarter)
```

```
##   발생년도 사망자수 중상자수 경상자수 부상신고자수 교통사고발생수
## 1   2012.1     1214      405      277           84           1152
## 2   2012.2     1403      491      393           47           1340
## 3   2012.3     1311      367      336           18           1263
## 4   2012.4     1464      484      341           30           1410
## 5   2013.1     1133      405      262           25           1094
## 6   2013.2     1236      382      307           26           1176
```

### 3) 시각화

```r
# 발생년도 기준으로 melt
sum_by_quarter_melt <- melt(sum_by_quarter, id.vars = "발생년도")

# ggplot
ggplot() + geom_line(data=sum_by_quarter_melt, aes(x=발생년도, y=value, group=variable, color=variable), size=1.5)
```
![1](https://user-images.githubusercontent.com/33209479/76287297-7948c100-62e7-11ea-8477-733279d25daf.PNG)

- 전체 교통사고 수, 사상자 수는 매년 감소하지만, 부상신고자수는 증가하고 있음
  50만원에서 150만원으로 상향된 대물할증 기분금액과 급증하고 있는 보험사기율과 연관성이 있는 것으로 보임
  
## 4. 교통사망사고가 높은 지역과 CCTV 분석
### 1) 경기도 내 사망사고가 가장 많이 발생한 시군구

```r
# 경기도 데이터 추출
kyonggi <- filter(data, 발생지_시도 == "경기")

# 경기도 시군구 별 교통사고 발생횟수
acc_freq <- tapply(kyonggi$freq, kyonggi$발생지_시군구, sum, na.rm=T)

# NA(결측값) 제외
acc_freq_frame <- data.frame(acc_freq[-which(is.na(acc_freq))])
colnames(acc_freq_frame) <- c("사망사고")
acc_freq_frame
```

```
##          사망사고
## 가평군         52
## 고양시        164
## 과천시          8
## 광명시         40
## 광주시         83
## 구리시         27
## 군포시         27
## 김포시         90
## 남양주시      144
## 동두천시       34
## 부천시         88
## 성남시        137
## 수원시        169
## 시흥시        132
## 안산시        132
## 안성시        102
## 안양시         94
## 양주시         77
## 양평군         58
## 여주시         78
## 연천군         29
## 오산시         32
## 용인시        173
## 의왕시         25
## 의정부시       63
## 이천시        101
## 파주시         99
## 평택시        183
## 포천시         98
## 하남시         33
## 화성시        205
```

- 화성시가 205회로 사망사고가 가장 높았고
  평택, 용인, 수원 순서로 2012 ~ 2014년 동안 교통사고가 가장 많이 발생함

### 2) 화성, 평택시의 교통사망사고와 교통단속 CCTV의 상관성

```r
# 화성, 평택시 데이터 추출
target_h <- filter(data, 발생지_시군구 == "화성시")
target_p <- filter(data, 발생지_시군구 == "평택시")

# 화성, 평택시 CCTV 데이터 로드
# 화성시
hwaseong <- read.csv("C:/Users/Windows10/Desktop/Project/data/경기도+화성시_CCTV_20160908.csv")
str(hwaseong)
```

```
## 'data.frame':	1373 obs. of  13 variables:
##  $ 관리기관명      : Factor w/ 1 level "경기도 화성시청": 1 1 1 1 1 1 1 1 1 1 ...
##  $ 소재지도로명주소: Factor w/ 76 levels "","경기도 화성시  삼성전자로 1",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ 소재지지번주소  : Factor w/ 1261 levels "경기도 화성시    동탄반송3길 51",..: 751 779 366 605 490 680 288 538 310 750 ...
##  $ 설치목적구분    : Factor w/ 6 levels "교통단속","다목적",..: 3 3 3 3 3 3 3 3 3 3 ...
##  $ 카메라대수      : int  3 4 3 3 4 3 3 3 3 4 ...
##  $ 카메라화소수    : int  200 200 200 200 200 200 200 200 200 200 ...
##  $ 촬영방면정보    : Factor w/ 18 levels "","360도 전방면",..: 3 3 3 3 3 3 3 3 3 3 ...
##  $ 보관일수        : int  30 30 30 30 30 30 30 30 30 30 ...
##  $ 설치년월        : Factor w/ 55 levels "","1998-10","2002-01",..: 52 52 52 52 52 52 52 52 52 52 ...
##  $ 관리기관전화번호: Factor w/ 4 levels "02-2243-4011",..: 4 4 4 4 4 4 4 4 4 4 ...
##  $ 위도            : num  37.1 37.2 37.2 37.2 37.2 ...
##  $ 경도            : num  127 127 127 127 127 ...
##  $ 데이터기준일자  : Factor w/ 1 level "2016-09-08": 1 1 1 1 1 1 1 1 1 1 ...
```

```r
h_ac <- filter(hwaseong, 설치목적구분 %in% c("교통단속", "다목적", "어린이보호"))

# 평택시
pyeongtaek <- read.csv("C:/Users/Windows10/Desktop/Project/data/경기도+평택시_CCTV_20160905.csv")
str(pyeongtaek)
```

```
## 'data.frame':	1089 obs. of  13 variables:
##  $ 관리기관명      : Factor w/ 2 levels "경기도 평택시",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ 소재지도로명주소: Factor w/ 114 levels "","경기도 평택시 경기대로 1347",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ 소재지지번주소  : Factor w/ 1048 levels "경기도 평택시 가재동 181-27",..: 774 119 685 124 508 512 99 311 67 695 ...
##  $ 설치목적구분    : Factor w/ 6 levels "공원조성","교통단속",..: 3 3 3 3 3 3 3 3 3 3 ...
##  $ 카메라대수      : int  1 1 1 1 1 1 1 1 1 3 ...
##  $ 카메라화소수    : int  200 200 200 200 200 200 200 200 200 200 ...
##  $ 촬영방면정보    : Factor w/ 1 level "360도전방면": 1 1 1 1 1 1 1 1 1 1 ...
##  $ 보관일수        : int  30 30 30 30 30 30 30 30 30 30 ...
##  $ 설치년월        : Factor w/ 47 levels "2005-09","2006-09",..: 45 45 45 45 45 45 45 45 45 45 ...
##  $ 관리기관전화번호: Factor w/ 3 levels "031-790-6456",..: 2 2 2 2 2 2 2 2 2 2 ...
##  $ 위도            : num  37 37 37 37 37 ...
##  $ 경도            : num  127 127 127 127 127 ...
##  $ 데이터기준일자  : Factor w/ 2 levels "2015-08-17","2016-09-05": 2 2 2 2 2 2 2 2 2 2 ...
```

```r
p_ac <- filter(pyeongtaek, 설치목적구분 %in% c("교통단속", "어린이보호"))
```

- 설치목적상 시설물관리, 생활방범, 차량방범, 공원관리와 같은 목적이 확실한 CCTV를 제외하고
  교통단속의 목적을 수행할 가능성이 있는  CCTV 추출

### 3) 4분위수를 통해 위험지역 판단

```r
summary(data$total)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   1.000   1.000   1.000   2.234   3.000 196.000
```

```r
# 화성시
low_h <- filter(target_h, total <= 1)
mid_h <- filter(target_h, total > 1 & total <= 3)
high_h <- filter(target_h, total > 3)

# 평택시
low_p <- filter(target_p, total <= 1)
mid_p <- filter(target_p, total > 1 & total <= 3)
high_p <- filter(target_p, total > 3)
```

### 4) 사고발생위치의 중심점 도출 및 시각화

```r
#library(devtools)
#devtools::install_github("dkahle/ggmap")
#register_google(key = "AIzaSyBq-g9JUyBgbOZ2nF4VnFRjhlh6_fNBtp4")
#qmap()

# GPS 좌표 위도, 경도와 평균을 구해서 중심점 도출
#cent_h <- c(mean(target_h$X.Grd.경도.), mean(target_h$Y.Grd.위도.))
#cent_p <- c(mean(target_p$Y.Grd.위도.), mean(target_p$Y.Grd.위도.))

# 화성시 시각화
#h_map <- get_map(location=cent_h, color="color", source="google", maptype="roadmap", zoom=11)

#ggmap(h_map) + 
#  geom_point(data=high_h, aes(x=X.Grd.경도., y=Y.Grd.위도.), size=6, alpha=1, color="#FF00FF") +
#  geom_point(data=mid_h, aes(x=X.Grd.경도., y=Y.Grd.위도.), alpha=1, col="red", size=5) +
#  geom_point(data=low_h, aes(x=X.Grd.경도., y=Y.Grd.위도.), alpha=1, col="#FF7518", size=4) +
#  geom_point(data=h_ac, aes(x=경도, y=위도), alpha=1, col="green", size=3)
```
