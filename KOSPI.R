#load library
library(data.table)
library(tidyverse)  
library(caret)
library(randomForest)
library(magrittr)
library(xts)
library(quantmod)
library(pROC)
library(nnet)
library(MLmetrics)
library(e1071)
library(TTR)
library(dispRity)

# Ⅰ. Data Collection
## 1. Technical Indicator
###kopsi 지수 데이터를 불러옵니다
kospi <- fread('C:/Users/USER/Desktop/미래에셋 금융페스티벌/2019빅데이터페스티벌데이터/KOSPI_국면분석.csv') 

###사용할 변수만 남겨줍니다
kospi2 <- kospi %>% select(날짜, Y, 지수종가, 지수시가, 지수고가, 지수저가)
x <- as.data.frame(kospi2[,3]) 
x <- x[,1]

### 5일후 종가와 비교하여 상승 폭을 계산합니다. 이의 절댓값이 1.5보다 작으면 보합으로 간주하여 재범주화합니다
y <- c(x[6:4449],rep(NA,5))
kospi2 %<>% mutate(change = y-x)
kospi3 <- kospi2 %>% mutate(changerate = change / 지수종가 * 100) %>% 
  mutate(Y= ifelse(changerate>1.5, '상승',
                   ifelse(changerate<=1.5&changerate>0,'상승보합',
                          ifelse(changerate<=0&changerate>-1.5,'하락보합','하락'))))
### Table을 보면 각 class의 비율이 유사하다는 것을 알 수 있습니다.
table(kospi3$Y)

### 기술적 변수를 계산하기 위하여 고가,저가,종가를 모은 데이터를 따로 만듭니다.
kospiHLC <- kospi %>% select(High=지수고가,Low=지수저가,Close=지수종가)

### Stochastic Fast %K, Fast %D, slow %D
#ROC (rate of change)
#moment (momentum)
#cci (Commodity channel index)
#disparity : 이격도
#pctB : 볼린저 밴드의 표준편차

stc <- stoch(kospiHLC,nFastK = 5, nFastD = 3, nSlowD = 3)
roc <- kospiHLC %>% select(Close) %>% ROC(1) %>% as.data.frame()
moment <- kospiHLC %>% select(Close) %>% momentum() %>% as.data.frame()
cci <- CCI(kospiHLC) %>% as.data.frame()
colnames(roc) <- 'roc'
colnames(moment) <- 'momentum'
colnames(cci) <- 'cci'


kospidata <- kospi3 %>% cbind(stc,roc,moment,cci) %>%
  cbind(BBands(kospi[,c('지수시가','지수저가','지수종가')],n=5)) %>%
  mutate(disparity=지수종가/mavg*100,Y=as.factor(Y)) %>%
  filter(날짜>=2011&날짜<2017) %>% 
  select(날짜,Y,fastK:cci,pctB,disparity)

kospitest <- kospi2 %>% cbind(stc,roc,moment,cci) %>%
  cbind(BBands(kospi[,c('지수시가','지수저가','지수종가')],n=5)) %>%
  mutate(disparity=지수종가/mavg*100,Y=as.factor(Y)) %>%
  filter(날짜>=2017) %>% 
  select(날짜,Y,fastK:cci,pctB,disparity)

## 2. News Text
### 뉴스 링크와 주소 가져오기
library(rvest)
library(dplyr) ; library(magrittr)
url <- 'http://www.koreaherald.com/search/index.php?q=japan&sort=1&mode=list&np='
page <- 1:3000
news_url <- c(); news_date <- c()
for(i in page){
  herald_url <- paste0(url,i)
  html <- read_html(herald_url)
  temp1 <- html_nodes(html,css='.wrapDiv')%>%
    html_nodes(css='.contentsWrap')%>%
    html_nodes(css='.contents.sub')%>%
    html_nodes(css='.overHidden')%>%
    html_nodes(css='.subContents.w776')%>%
    html_nodes(css= '.listDiv.l1.mt58.bb1.pb17')%>%
    html_nodes(css='.mb24.pl55.w721')%>%
    html_nodes(css='.contentsArea')%>%
    html_nodes('a')%>%
    html_attr('href')%>%unique()
  news_url <- c(news_url,temp1)
  temp2 <- html_nodes(html,css='.wrapDiv')%>%
    html_nodes(css='.contentsWrap')%>%
    html_nodes(css='.contents.sub')%>%
    html_nodes(css='.overHidden')%>%
    html_nodes(css='.subContents.w776')%>%
    html_nodes(css= '.listDiv.l1.mt58.bb1.pb17')%>%
    html_nodes(css='.mb24.pl55.w721')%>%
    html_nodes(css='.dateDiv')%>%
    html_nodes('p')%>%
    html_text()
  news_date <- c(news_date,temp2)
}

### 뉴스 내용 가져오기
news_content <- c()
for(i in 1:length(news_url)){
  html2 <- read_html(paste0('http://www.koreaherald.com',news_url[i]))
  temp3 <- html_text(html_nodes(html2,'#articleText.content_view'))
  news_content <- c(news_content,temp3)
}

### 뉴스 날짜, 내용 합치기
japan_news <- tibble(date=news_date,content=news_content)

### 뉴스 링크와 주소 가져오기(중국)
library(rvest)
library(dplyr) ; library(magrittr)
url <- 'http://www.koreaherald.com/search/index.php?q=china&sort=1&mode=list&np='
page <- 1:3000
news_url <- c(); news_date <- c()
for(i in page){
  herald_url <- paste0(url,i)
  html <- read_html(herald_url)
  temp1 <- html_nodes(html,css='.wrapDiv')%>%
    html_nodes(css='.contentsWrap')%>%
    html_nodes(css='.contents.sub')%>%
    html_nodes(css='.overHidden')%>%
    html_nodes(css='.subContents.w776')%>%
    html_nodes(css= '.listDiv.l1.mt58.bb1.pb17')%>%
    html_nodes(css='.mb24.pl55.w721')%>%
    html_nodes(css='.contentsArea')%>%
    html_nodes('a')%>%
    html_attr('href')%>%unique()
  news_url <- c(news_url,temp1)
  temp2 <- html_nodes(html,css='.wrapDiv')%>%
    html_nodes(css='.contentsWrap')%>%
    html_nodes(css='.contents.sub')%>%
    html_nodes(css='.overHidden')%>%
    html_nodes(css='.subContents.w776')%>%
    html_nodes(css= '.listDiv.l1.mt58.bb1.pb17')%>%
    html_nodes(css='.mb24.pl55.w721')%>%
    html_nodes(css='.dateDiv')%>%
    html_nodes('p')%>%
    html_text()
  news_date <- c(news_date,temp2)
}

### 뉴스 내용 가져오기
news_content <- c()
for(i in 1:length(news_url)){
  html2 <- read_html(paste0('http://www.koreaherald.com',news_url[i]))
  temp3 <- html_text(html_nodes(html2,'#articleText.content_view'))
  news_content <- c(news_content,temp3)
}

### 뉴스 날짜, 내용 합치기
china_news <- tibble(date=news_date,content=news_content)

### 뉴스 링크와 주소 가져오기(북한)
library(rvest)
library(dplyr) ; library(magrittr)
url <- 'http://www.koreaherald.com/search/index.php?q=north_korea&sort=1&mode=list&np='
page <- 1:3000
news_url <- c(); news_date <- c()
for(i in page){
  herald_url <- paste0(url,i)
  html <- read_html(herald_url)
  temp1 <- html_nodes(html,css='.wrapDiv')%>%
    html_nodes(css='.contentsWrap')%>%
    html_nodes(css='.contents.sub')%>%
    html_nodes(css='.overHidden')%>%
    html_nodes(css='.subContents.w776')%>%
    html_nodes(css= '.listDiv.l1.mt58.bb1.pb17')%>%
    html_nodes(css='.mb24.pl55.w721')%>%
    html_nodes(css='.contentsArea')%>%
    html_nodes('a')%>%
    html_attr('href')%>%unique()
  news_url <- c(news_url,temp1)
  temp2 <- html_nodes(html,css='.wrapDiv')%>%
    html_nodes(css='.contentsWrap')%>%
    html_nodes(css='.contents.sub')%>%
    html_nodes(css='.overHidden')%>%
    html_nodes(css='.subContents.w776')%>%
    html_nodes(css= '.listDiv.l1.mt58.bb1.pb17')%>%
    html_nodes(css='.mb24.pl55.w721')%>%
    html_nodes(css='.dateDiv')%>%
    html_nodes('p')%>%
    html_text()
  news_date <- c(news_date,temp2)
}

### 뉴스 내용 가져오기
news_content <- c()
for(i in 1:length(news_url)){
  html2 <- read_html(paste0('http://www.koreaherald.com',news_url[i]))
  temp3 <- html_text(html_nodes(html2,'#articleText.content_view'))
  news_content <- c(news_content,temp3)
}

### 뉴스 날짜, 내용 합치기
northkorea_news <- tibble(date=news_date,content=news_content)

### 뉴스 링크와 주소 가져오기(미국)
library(rvest)
library(dplyr) ; library(magrittr)
url <- 'http://www.koreaherald.com/search/index.php?q=us&sort=1&mode=list&np='
page <- 1:3000
news_url <- c(); news_date <- c()
for(i in page){
  herald_url <- paste0(url,i)
  html <- read_html(herald_url)
  temp1 <- html_nodes(html,css='.wrapDiv')%>%
    html_nodes(css='.contentsWrap')%>%
    html_nodes(css='.contents.sub')%>%
    html_nodes(css='.overHidden')%>%
    html_nodes(css='.subContents.w776')%>%
    html_nodes(css= '.listDiv.l1.mt58.bb1.pb17')%>%
    html_nodes(css='.mb24.pl55.w721')%>%
    html_nodes(css='.contentsArea')%>%
    html_nodes('a')%>%
    html_attr('href')%>%unique()
  news_url <- c(news_url,temp1)
  temp2 <- html_nodes(html,css='.wrapDiv')%>%
    html_nodes(css='.contentsWrap')%>%
    html_nodes(css='.contents.sub')%>%
    html_nodes(css='.overHidden')%>%
    html_nodes(css='.subContents.w776')%>%
    html_nodes(css= '.listDiv.l1.mt58.bb1.pb17')%>%
    html_nodes(css='.mb24.pl55.w721')%>%
    html_nodes(css='.dateDiv')%>%
    html_nodes('p')%>%
    html_text()
  news_date <- c(news_date,temp2)
}

### 뉴스 내용 가져오기
news_content <- c()
for(i in 1:length(news_url)){
  html2 <- read_html(paste0('http://www.koreaherald.com',news_url[i]))
  temp3 <- html_text(html_nodes(html2,'#articleText.content_view'))
  news_content <- c(news_content,temp3)
}

### 뉴스 날짜, 내용 합치기
us_news <- tibble(date=news_date,content=news_content)

# Ⅱ. Data Preprocessing
## Text mining, Sentimental Analysis
### text처리
library(tidytext)
library(tidyr)
### 날짜 정리(일본)
t1 <- japan_news%>%unnest_tokens(word,content)
t1%<>%anti_join(stop_words) # 관사 제거
t1%<>%mutate(word = gsub("'s", "", word)) # 's 없애기
c<- t1%>%count(word, sort=T)

### 감성분석
emotion <- t1 %>% group_by(date)%>%
  left_join(get_sentiments("bing")) %>% 
  count(word, sentiment)
emotion%<>%na.omit
emotion%<>%mutate(posneg = ifelse(sentiment=='positive', 1, -1))

emotion%<>%separate(date,into=c('month','day','year'),sep=' ')
emotion%<>%na.omit
emotion%<>%mutate(day = gsub(',', '',day))
emotion%<>%mutate(month = recode(month, 'Jan'=1,'Feb'=2,'Mar'=3,'Apr'=4,'May'=5,'Jun'=6,'Jul'=7,'Aug'=8,'Sep'=9,'Oct'=10,'Nov'=11,'Dec'=12))
emotion%<>%unite(col=date,year,month,day,sep='-')
emotion%<>%arrange(date)

emotion_japan<- emotion%>%
  group_by(date)%>%
  summarise(sentiment_score=sum(posneg*n))

### 날짜 정리(중국)
t2 <- china_news%>%unnest_tokens(word,content)
t2%<>%anti_join(stop_words) # 관사 제거
t2%<>%mutate(word = gsub("'s", "", word)) # 's 없애기
c<- t2%>%count(word, sort=T)

### 감성분석
emotion <- t2 %>% group_by(date)%>%
  left_join(get_sentiments("bing")) %>% 
  count(word, sentiment)
emotion%<>%na.omit
emotion%<>%mutate(posneg = ifelse(sentiment=='positive', 1, -1))

emotion%<>%separate(date,into=c('month','day','year'),sep=' ')
emotion%<>%na.omit
emotion%<>%mutate(day = gsub(',', '',day))
emotion%<>%mutate(month = recode(month, 'Jan'=1,'Feb'=2,'Mar'=3,'Apr'=4,'May'=5,'Jun'=6,'Jul'=7,'Aug'=8,'Sep'=9,'Oct'=10,'Nov'=11,'Dec'=12))
emotion%<>%unite(col=date,year,month,day,sep='-')
emotion%<>%arrange(date)

emotion_china<- emotion%>%
  group_by(date)%>%
  summarise(sentiment_score=sum(posneg*n))

### 날짜 정리(북한)
t3 <- northkorea_news%>%unnest_tokens(word,content)
t3%<>%anti_join(stop_words) # 관사 제거
t3%<>%mutate(word = gsub("'s", "", word)) # 's 없애기
c<- t3%>%count(word, sort=T)

### 감성분석
emotion <- t3 %>% group_by(date)%>%
  left_join(get_sentiments("bing")) %>% 
  count(word, sentiment)
emotion%<>%na.omit
emotion%<>%mutate(posneg = ifelse(sentiment=='positive', 1, -1))

emotion%<>%separate(date,into=c('month','day','year'),sep=' ')
emotion%<>%na.omit
emotion%<>%mutate(day = gsub(',', '',day))
emotion%<>%mutate(month = recode(month, 'Jan'=1,'Feb'=2,'Mar'=3,'Apr'=4,'May'=5,'Jun'=6,'Jul'=7,'Aug'=8,'Sep'=9,'Oct'=10,'Nov'=11,'Dec'=12))
emotion%<>%unite(col=date,year,month,day,sep='-')
emotion%<>%arrange(date)

emotion_northkorea<- emotion%>%
  group_by(date)%>%
  summarise(sentiment_score=sum(posneg*n))

### 날짜 정리(미국)
t4 <- us_news%>%unnest_tokens(word,content)
t4%<>%anti_join(stop_words) # 관사 제거
t4%<>%mutate(word = gsub("'s", "", word)) # 's 없애기
c<- t4%>%count(word, sort=T)

### 감성분석
emotion <- t4 %>% group_by(date)%>%
  left_join(get_sentiments("bing")) %>% 
  count(word, sentiment)
emotion%<>%na.omit
emotion%<>%mutate(posneg = ifelse(sentiment=='positive', 1, -1))

emotion%<>%separate(date,into=c('month','day','year'),sep=' ')
emotion%<>%na.omit
emotion%<>%mutate(day = gsub(',', '',day))
emotion%<>%mutate(month = recode(month, 'Jan'=1,'Feb'=2,'Mar'=3,'Apr'=4,'May'=5,'Jun'=6,'Jul'=7,'Aug'=8,'Sep'=9,'Oct'=10,'Nov'=11,'Dec'=12))
emotion%<>%unite(col=date,year,month,day,sep='-')
emotion%<>%arrange(date)

emotion_us<- emotion%>%
  group_by(date)%>%
  summarise(sentiment_score=sum(posneg*n))

# Ⅲ. Feature Selection & Validation
logdata <- kospidata
logemotion <- kospiemotion
control <- trainControl(method = 'cv', number = 5,summaryFunction = multiClassSummary, classProbs = TRUE, search = 'random')
test_y <- kospiemotiontest$Y

## multinom
set.seed(2019)
logmodel <- train(Y~., data=logdata, trControl= control, method = 'multinom', metric = 'Mean_F1', tuneLength = 10, verbose=F)
summary(logmodel)
y_pred <- predict(logmodel,kospitest[-1], type='raw')
y_pred_log <- as.factor(ifelse(y_pred %in% c('상승','상승보합'), '상승','하락'))
confusionMatrix(y_pred_log, test_y)

##  randomforest
set.seed(2019)
grid1 = expand.grid(.mtry = 2:12)
rfmodel <- train(Y~., data=logdata, trControl= control, method = 'rf', metric = 'Mean_F1', tuneGrid = grid1, verbose=F)
rfmodel
imp<- varImp(rfmodel,scale = F)
plot(imp)
y_pred <- predict(rfmodel,kospitest[-1], type='raw')
y_pred_rf <- as.factor(ifelse(y_pred %in% c('상승','상승보합'), '상승','하락'))
confusionMatrix(y_pred_rf, test_y)

##  svm
set.seed(2019)
grid = expand.grid(.sigma = 2^seq(-13,13,2), .C = 2^seq(-13,13,2))
svmmodel <- train(Y~., data=logdata, trControl= control, method = 'svmRadial', metric = 'Mean_F1', verbose=F, tuneGrid = grid)
svmmodel
y_pred <- predict(svmmodel,kospitest[-1], type='raw')
y_pred_svm <- as.factor(ifelse(y_pred %in% c('상승','상승보합'), '상승','하락'))
confusionMatrix(y_pred_svm, test_y)

########################### with emotion########################
logemotion <- kospiemotion
control <- trainControl(method = 'cv', number = 5,summaryFunction = multiClassSummary, classProbs = TRUE, search = 'random')

##  multinom2
set.seed(2019)
logmodel2 <- train(Y~., data=logemotion, trControl= control, method = 'multinom', metric = 'Mean_F1', tuneLength = 10, verbose=F)
summary(logmodel2)
y_pred2 <- predict(logmodel2,kospiemotiontest[-1], type='raw')
y_pred_log2 <- as.factor(ifelse(y_pred2 %in% c('상승','상승보합'), '상승','하락'))

confusionMatrix(y_pred_log2, test_y)

##  randomforest2
set.seed(2019)
control <- trainControl(method = 'cv', number = 5,summaryFunction = multiClassSummary, classProbs = TRUE, search = 'random')
rfmodel2 <- train(Y~., data=logemotion, trControl= control, method = 'rf', metric = 'Mean_F1', tuneGrid = grid1, verbose=F)
rfmodel2
imp<- varImp(rfmodel2,scale = F)
plot(imp)
y_pred2 <- predict(rfmodel2,kospiemotiontest[-1], type='raw')
y_pred_rf2 <- as.factor(ifelse(y_pred2 %in% c('상승','상승보합'), '상승','하락'))
confusionMatrix(y_pred_rf2, test_y)

##  svm2
set.seed(2019)
grid = expand.grid(.sigma = 2^seq(-13,13,2), .C = 2^seq(-13,13,2))
control <- trainControl(method = 'cv', number = 5,summaryFunction = multiClassSummary, classProbs = TRUE, search = 'random')
svmmodel2 <- train(Y~., data=logemotion, trControl= control, method = 'svmRadial', metric = 'Mean_F1', tuneLength = 3, verbose=F, tuneGrid = grid)
svmmodel2
y_pred2 <- predict(svmmodel2,kospiemotiontest[-1], type='raw')
y_pred_svm2 <- as.factor(ifelse(y_pred2 %in% c('상승','상승보합'), '상승','하락'))
confusionMatrix(y_pred_svm2, test_y)

# Ⅳ. Best Model Selection
Accuracy(y_pred_log, test_y); F1_Score(y_pred_log, test_y) # accuracy :0.5503 , f1-score : 0.6812
Accuracy(y_pred_rf, test_y); F1_Score(y_pred_rf, test_y) # accuracy : 0.5318, f1-score : 0.625
Accuracy(y_pred_svm, test_y); F1_Score(y_pred_svm, test_y) # accuracy : 0.5503, f1-score : 0.7076
Accuracy(y_pred_log2, test_y); F1_Score(y_pred_log2, test_y) # accuracy : 0.5646, f1-score : 0.6778
Accuracy(y_pred_rf2, test_y); F1_Score(y_pred_rf2, test_y) # accuracy : 0.5441, f1-score : 0.6287
Accuracy(y_pred_svm2, test_y); F1_Score(y_pred_svm2, test_y) # accuracy : 0.54, f1-score : 0.6753
