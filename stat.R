setwd('C:/Users/cptas')
install.packages('dplyr')
install.packages('readxl')
install.packages('ggplot2')
install.packages('agricolae')
library(dplyr)
library(ggplot2)
library(readxl)
library(reshape2)
library(agricolae)

data <- read_excel('월별소비자동향조사.xlsx')
head(data)


#자영업자, 봉급생활자 비교
data1 <- data %>%
  filter(분류코드별 %in% c('자영업자', '봉급생활자')) %>% 
  filter(지수코드별 %in% c('임금수준전망CSI', '현재생활형편CSI',
                      '현재경기판단CSI', '생활형편전망CSI', '향후경기전망CSI')) %>% 
  select(-항목, -단위) %>% 
  melt(id.vars=c('지수코드별', '분류코드별'))

data1$지수코드별 = as.factor(data1$지수코드별)
data1$분류코드별 = as.factor(data1$분류코드별)

data1_b <- data1[c(1:240), ]
data1_a <- data1[c(241:340), ]
data1_ao <- data1_a %>%
  filter(분류코드별 == '자영업자')
data1_as <- data1_a %>%
  filter(분류코드별 == '봉급생활자')
data1_bo <- data1_b %>%
  filter(분류코드별 == '자영업자')
data1_bs <- data1_b %>%
  filter(분류코드별 == '봉급생활자')

#자영업자와 봉급생활자 임금수준전망 코로나 이전
a <- data1_bo %>% filter(지수코드별 == '임금수준전망CSI')
b <- data1_bs %>% filter(지수코드별 == '임금수준전망CSI')

shapiro.test(b$value)
var.test(a$value, b$value)
t.test(a$value, b$value)

#자영업자와 봉급생활자 임금수준전망 코로나 이후
x <- data1_ao %>% filter(지수코드별 == '임금수준전망CSI')
x$value
y <- data1_as %>% filter(지수코드별 == '임금수준전망CSI')
y$value
wilcox.test(x$value, y$value)

#자영업자 코로나 이전 이후 임금수준전망
a <- data1_bo %>% filter(지수코드별 == '임금수준전망CSI')
x <- data1_ao %>% filter(지수코드별 == '임금수준전망CSI')
shapiro.test(a$value)
shapiro.test(x$value)
var.test(a$value, x$value)
t.test(a$value, x$value, var.equal = F)
wilcox.test(a$value, x$value)

#자영업자 코로나 이후 임금수준전망
x1 <- x[c(2:5), ]
x2 <- x[c(8:10), ]
wilcox.test(x1$value, x2$value)

#봉급생활자 코로나 이전 이후 임금수준전망
b <- data1_bs %>% filter(지수코드별 == '임금수준전망CSI')
y <- data1_as %>% filter(지수코드별 == '임금수준전망CSI')
shapiro.test(b$value)
shapiro.test(y$value)
wilcox.test(b$value, y$value)

#봉급생활자 코로나 이후 임금수준전망
y1 <- y[c(2:5), ]
y2 <- y[c(8:10), ]
wilcox.test(y1$value, y2$value)

#자영업자와 봉급생활자 현재생활형편 코로나 이전
a <- data1_bo %>% filter(지수코드별 == '현재생활형편CSI')
b <- data1_bs %>% filter(지수코드별 == '현재생활형편CSI')

shapiro.test(a$value)
shapiro.test(b$value)
var.test(a$value, b$value)
t.test(a$value, b$value)

#자영업자와 봉급생활자 현재생활형편 코로나 이후
x <- data1_ao %>% filter(지수코드별 == '현재생활형편CSI')
x$value
y <- data1_as %>% filter(지수코드별 == '현재생활형편CSI')
y$value
shapiro.test(x$value)
shapiro.test(y$value)
var.test(x$value, y$value)
t.test(x$value, y$value, var.equal = F)

#자영업자 코로나 이전 이후 현재생활형편
a <- data1_bo %>% filter(지수코드별 == '현재생활형편CSI')
x <- data1_ao %>% filter(지수코드별 == '현재생활형편CSI')
shapiro.test(a$value)
shapiro.test(x$value)
var.test(a$value, x$value)
t.test(a$value, x$value, var.equal = F)
wilcox.test(a$value, x$value)

#자영업자 코로나 이후 현재생활형편
x1 <- x[c(2:5), ]
x2 <- x[c(8:10), ]
wilcox.test(x1$value, x2$value)

#봉급생활자 코로나 이전 이후 현재생활형편
b <- data1_bs %>% filter(지수코드별 == '현재생활형편CSI')
y <- data1_as %>% filter(지수코드별 == '현재생활형편CSI')
shapiro.test(b$value)
shapiro.test(y$value)
var.test(b$value, y$value)
t.test(a$value, x$value, var.equal = F)
wilcox.test(b$value, y$value)

#봉급생활자 코로나 이후 현재생활형편
y1 <- y[c(2:5), ]
y2 <- y[c(8:10), ]
wilcox.test(y1$value, y2$value)

#자영업자와 봉급생활자 향후경기전망 코로나 이전
a <- data1_bo %>% filter(지수코드별 == '향후경기전망CSI')
b <- data1_bs %>% filter(지수코드별 == '향후경기전망CSI')

wilcox.test(a$value, b$value)

#자영업자와 봉급생활자 향후경기전망 코로나 이후
x <- data1_ao %>% filter(지수코드별 == '향후경기전망CSI')
x$value
y <- data1_as %>% filter(지수코드별 == '향후경기전망CSI')
y$value
shapiro.test(x$value)
shapiro.test(y$value)
var.test(x$value, y$value)
t.test(x$value, y$value, var.equal = T)

#자영업자 코로나 이전 이후 향후경기전망
a <- data1_bo %>% filter(지수코드별 == '향후경기전망CSI')
x <- data1_ao %>% filter(지수코드별 == '향후경기전망CSI')
shapiro.test(a$value)
shapiro.test(x$value)
wilcox.test(a$value, x$value)

#자영업자 코로나 이후 향후경기전망
x1 <- x[c(2:5), ]
x2 <- x[c(8:10), ]
wilcox.test(x1$value, x2$value)

#봉급생활자 코로나 이전 이후 향후경기전망
b <- data1_bs %>% filter(지수코드별 == '향후경기전망CSI')
y <- data1_as %>% filter(지수코드별 == '향후경기전망CSI')
shapiro.test(b$value)
shapiro.test(y$value)
wilcox.test(b$value, y$value)

#봉급생활자 코로나 이후 향후경기전망
y1 <- y[c(2:5), ]
y2 <- y[c(8:10), ]
wilcox.test(y1$value, y2$value)

#자영업자와 봉급생활자 현재경기판단 코로나 이전
a <- data1_bo %>% filter(지수코드별 == '현재경기판단CSI')
b <- data1_bs %>% filter(지수코드별 == '현재경기판단CSI')
shapiro.test(a$value)
shapiro.test(b$value)
wilcox.test(a$value, b$value)

#자영업자와 봉급생활자 현재경기판단 코로나 이후
x <- data1_ao %>% filter(지수코드별 == '현재경기판단CSI')
y <- data1_as %>% filter(지수코드별 == '현재경기판단CSI')
shapiro.test(x$value)
shapiro.test(y$value)
var.test(x$value, y$value)
t.test(x$value, y$value, var.equal = T)
wilcox.test(x$value, y$value)


#자영업자 코로나 이전 이후 현재경기판단
a <- data1_bo %>% filter(지수코드별 == '현재경기판단CSI')
x <- data1_ao %>% filter(지수코드별 == '현재경기판단CSI')
shapiro.test(a$value)
shapiro.test(x$value)
wilcox.test(a$value, x$value)

#자영업자 코로나 이후 현재경기판단
x1 <- x[c(2:5), ]
x2 <- x[c(8:10), ]
wilcox.test(x1$value, x2$value)

#봉급생활자 코로나 이전 이후 현재경기판단
b <- data1_bs %>% filter(지수코드별 == '현재경기판단CSI')
y <- data1_as %>% filter(지수코드별 == '현재경기판단CSI')
shapiro.test(b$value)
shapiro.test(y$value)
wilcox.test(b$value, y$value)

#봉급생활자 코로나 이후 현재경기판단
y1 <- y[c(2:5), ]
y2 <- y[c(8:10), ]
wilcox.test(y1$value, y2$value)

#자영업자와 봉급생활자 생활형편전망 코로나 이전
a <- data1_bo %>% filter(지수코드별 == '생활형편전망CSI')
b <- data1_bs %>% filter(지수코드별 == '생활형편전망CSI')
shapiro.test(a$value)
shapiro.test(b$value)
wilcox.test(a$value, b$value)

#자영업자와 봉급생활자 생활형편전망 코로나 이후
x <- data1_ao %>% filter(지수코드별 == '생활형편전망CSI')
y <- data1_as %>% filter(지수코드별 == '생활형편전망CSI')
shapiro.test(x$value)
shapiro.test(y$value)
var.test(x$value, y$value)
t.test(x$value, y$value, var.equal = T)
wilcox.test(x$value, y$value)


#자영업자 코로나 이전 이후 생활형편전망
a <- data1_bo %>% filter(지수코드별 == '생활형편전망CSI')
x <- data1_ao %>% filter(지수코드별 == '생활형편전망CSI')
shapiro.test(a$value)
shapiro.test(x$value)
wilcox.test(a$value, x$value)

#자영업자 코로나 이후 생활형편전망
x1 <- x[c(2:5), ]
x2 <- x[c(8:10), ]
wilcox.test(x1$value, x2$value)

#봉급생활자 코로나 이전 이후 생활형편전망
b <- data1_bs %>% filter(지수코드별 == '생활형편전망CSI')
y <- data1_as %>% filter(지수코드별 == '생활형편전망CSI')
shapiro.test(b$value)
shapiro.test(y$value)
wilcox.test(b$value, y$value)

#봉급생활자 코로나 이후 생활형편전망
y1 <- y[c(2:5), ]
y2 <- y[c(8:10), ]
wilcox.test(y1$value, y2$value)


#소득별 비교
data2 <- data %>%
  filter(분류코드별 %in% c('100만원미만', '100-200만원', '200-300만원', '300-400만원', '400-500만원', '500만원이상')) %>% 
  filter(지수코드별 %in% c('가계수입전망CSI', '현재가계저축CSI', '가계저축전망CSI', '주택가격전망CSI', '소비지출전망CSI', '의료·보건비 지출전망CSI', '교양·오락·문화생활비 지출전망CSI', '의류비 지출전망CSI', '외식비 지출전망CSI', '여행비 지출전망CSI', '교육비 지출전망CSI')) %>% 
  select(-항목, -단위) %>% 
  melt(id.vars=c('지수코드별', '분류코드별'))

data2$지수코드별 = as.factor(data2$지수코드별)
data2$분류코드별 = as.factor(data2$분류코드별)
dim(data2)
data2_b <- data2[c(1:1584), ]
data2_a <- data2[c(1585:2244), ]

#소득별 가계수입전망 코로나 이후
model1 = aov(value ~ 분류코드별 + variable, data=data2_a[data2_a$지수코드별 == '가계수입전망CSI', ])
summary(model1)
a=HSD.test(model1, '분류코드별', group=TRUE)
a

#소득별 가계수입전망 코로나 이전
model2 = aov(value ~ 분류코드별 + variable, data=data2_b[data2_b$지수코드별 == '가계수입전망CSI', ])
summary(model2)
b=HSD.test(model2, '분류코드별', group=TRUE)
b


#소득별 주택가격전망 코로나 이후
model1 = aov(value ~ 분류코드별 + variable, data=data2_a[data2_a$지수코드별 == '주택가격전망CSI', ])
summary(model1)
a=HSD.test(model1, '분류코드별', group=TRUE)
a

#소득별 주택가격전망 코로나 이전
model2 = aov(value ~ 분류코드별 + variable, data=data2_b[data2_b$지수코드별 == '주택가격전망CSI', ])
summary(model2)
b=HSD.test(model2, '분류코드별', group=TRUE)
b

#소득별 소비지출전망 코로나 이후
model1 = aov(value ~ 분류코드별 + variable, data=data2_a[data2_a$지수코드별 == '소비지출전망CSI', ])
summary(model1)
a=HSD.test(model1, '분류코드별', group=TRUE)
a

#소득별 소비지출전망 코로나 이전
model2 = aov(value ~ 분류코드별 + variable, data=data2_b[data2_b$지수코드별 == '소비지출전망CSI', ])
summary(model2)
b=HSD.test(model2, '분류코드별', group=TRUE)
b


#소득별 가계저축전망 코로나 이후
model1 = aov(value ~ 분류코드별 + variable, data=data2_a[data2_a$지수코드별 == '가계저축전망CSI', ])
summary(model1)
a=HSD.test(model1, '분류코드별', group=TRUE)
a

#소득별 가계저축전망 코로나 이전
model2 = aov(value ~ 분류코드별 + variable, data=data2_b[data2_b$지수코드별 == '가계저축전망CSI', ])
summary(model2)
b=HSD.test(model2, '분류코드별', group=TRUE)
b

#소득별 의료·보건비 지출전망 코로나 이후
model1 = aov(value ~ 분류코드별 + variable, data=data2_a[data2_a$지수코드별 == '의료·보건비 지출전망CSI', ])
summary(model1)
a=HSD.test(model1, '분류코드별', group=TRUE)
a

#소득별 의료·보건비 지출전망 코로나 이전
model2 = aov(value ~ 분류코드별 + variable, data=data2_b[data2_b$지수코드별 == '의료·보건비 지출전망CSI', ])
summary(model2)
b=HSD.test(model2, '분류코드별', group=TRUE)
b

#소득별 교양·오락·문화생활비 지출전망 코로나 이후
model1 = aov(value ~ 분류코드별 + variable, data=data2_a[data2_a$지수코드별 == '교양·오락·문화생활비 지출전망CSI', ])
summary(model1)
a=HSD.test(model1, '분류코드별', group=TRUE)
a

#소득별 교양·오락·문화생활비 지출전망 코로나 이전
model2 = aov(value ~ 분류코드별 + variable, data=data2_b[data2_b$지수코드별 == '교양·오락·문화생활비 지출전망CSI', ])
summary(model2)
b=HSD.test(model2, '분류코드별', group=TRUE)
b

#소득별 교육비 지출전망 코로나 이후
model1 = aov(value ~ 분류코드별 + variable, data=data2_a[data2_a$지수코드별 == '교육비 지출전망CSI', ])
summary(model1)
a=HSD.test(model1, '분류코드별', group=TRUE)
a

#소득별 교육비 지출전망 코로나 이전
model2 = aov(value ~ 분류코드별 + variable, data=data2_b[data2_b$지수코드별 == '교육비 지출전망CSI', ])
summary(model2)
b=HSD.test(model2, '분류코드별', group=TRUE)
b

#소득별 여행비 지출전망 코로나 이후
model1 = aov(value ~ 분류코드별 + variable, data=data2_a[data2_a$지수코드별 == '여행비 지출전망CSI', ])
summary(model1)
a=HSD.test(model1, '분류코드별', group=TRUE)
a

#소득별 여행비 지출전망 코로나 이전
model2 = aov(value ~ 분류코드별 + variable, data=data2_b[data2_b$지수코드별 == '여행비 지출전망CSI', ])
summary(model2)
b=HSD.test(model2, '분류코드별', group=TRUE)
b


#소득별 의류비 지출전망 코로나 이후
model1 = aov(value ~ 분류코드별 + variable, data=data2_a[data2_a$지수코드별 == '의류비 지출전망CSI', ])
summary(model1)
a=HSD.test(model1, '분류코드별', group=TRUE)
a

#소득별 의류비 지출전망 코로나 이전
model2 = aov(value ~ 분류코드별 + variable, data=data2_b[data2_b$지수코드별 == '의류비 지출전망CSI', ])
summary(model2)
b=HSD.test(model2, '분류코드별', group=TRUE)
b

#소득별 외식비 지출전망 코로나 이후
model1 = aov(value ~ 분류코드별 + variable, data=data2_a[data2_a$지수코드별 == '외식비 지출전망CSI', ])
summary(model1)
a=HSD.test(model1, '분류코드별', group=TRUE)
a

#소득별 외식비 지출전망 코로나 이전
model2 = aov(value ~ 분류코드별 + variable, data=data2_b[data2_b$지수코드별 == '외식비 지출전망CSI', ])
summary(model2)
b=HSD.test(model2, '분류코드별', group=TRUE)
b

#연령별 비교
data3 <- data %>%
  filter(분류코드별 %in% c('40세미만', '40-50세', '50-60세', '60-70세', '70세이상')) %>% 
  filter(지수코드별 %in% c('현재생활형편CSI', '현재경기판단CSI', '생활형편전망CSI', '향후경기전망CSI', '주택가격전망CSI', '소비지출전망CSI', '의료·보건비 지출전망CSI', '교양·오락·문화생활비 지출전망CSI', '의류비 지출전망CSI', '외식비 지출전망CSI', '여행비 지출전망CSI', '교육비 지출전망CSI')) %>% 
  select(-항목, -단위) %>% 
  melt(id.vars=c('지수코드별', '분류코드별'))

data3$지수코드별 = as.factor(data3$지수코드별)
data3$분류코드별 = as.factor(data3$분류코드별)
dim(data3)
data3_b <- data3[c(1:1440), ]
data3_a <- data3[c(1441:2040), ]

#연령별 현재생활형편 코로나 이후
model1 = aov(value ~ 분류코드별 + variable, data=data3_a[data3_a$지수코드별 == '현재생활형편CSI', ])
summary(model1)
a=HSD.test(model1, '분류코드별', group=TRUE)
a

#연령별 현재생활형편 코로나 이전
model2 = aov(value ~ 분류코드별 + variable, data=data3_b[data3_b$지수코드별 == '현재생활형편CSI', ])
summary(model2)
b=HSD.test(model2, '분류코드별', group=TRUE)
b


#연령별 현재생활형편 코로나 이후
model1 = aov(value ~ 분류코드별 + variable, data=data3_a[data3_a$지수코드별 == '현재생활형편CSI', ])
summary(model1)
a=HSD.test(model1, '분류코드별', group=TRUE)
a

#연령별 현재생활형편 코로나 이전
model2 = aov(value ~ 분류코드별 + variable, data=data3_b[data3_b$지수코드별 == '현재생활형편CSI', ])
summary(model2)
b=HSD.test(model2, '분류코드별', group=TRUE)
b
#연령별 현재경기판단 코로나 이후
model1 = aov(value ~ 분류코드별 + variable, data=data3_a[data3_a$지수코드별 == '현재경기판단CSI', ])
summary(model1)
a=HSD.test(model1, '분류코드별', group=TRUE)
a

#연령별 현재경기판단 코로나 이전
model2 = aov(value ~ 분류코드별 + variable, data=data3_b[data3_b$지수코드별 == '현재경기판단CSI', ])
summary(model2)
b=HSD.test(model2, '분류코드별', group=TRUE)
b

#연령별 생활형편전망 코로나 이후
model1 = aov(value ~ 분류코드별 + variable, data=data3_a[data3_a$지수코드별 == '생활형편전망CSI', ])
summary(model1)
a=HSD.test(model1, '분류코드별', group=TRUE)
a

#연령별 생활형편전망 코로나 이전
model2 = aov(value ~ 분류코드별 + variable, data=data3_b[data3_b$지수코드별 == '생활형편전망CSI', ])
summary(model2)
b=HSD.test(model2, '분류코드별', group=TRUE)
b

#연령별 주택가격전망 코로나 이후
model1 = aov(value ~ 분류코드별 + variable, data=data3_a[data3_a$지수코드별 == '주택가격전망CSI', ])
summary(model1)
a=HSD.test(model1, '분류코드별', group=TRUE)
a

#연령별 주택가격전망 코로나 이전
model2 = aov(value ~ 분류코드별 + variable, data=data3_b[data3_b$지수코드별 == '주택가격전망CSI', ])
summary(model2)
b=HSD.test(model2, '분류코드별', group=TRUE)
b

#연령별 소비지출전망 코로나 이후
model1 = aov(value ~ 분류코드별 + variable, data=data3_a[data3_a$지수코드별 == '소비지출전망CSI', ])
summary(model1)
a=HSD.test(model1, '분류코드별', group=TRUE)
a

#연령별 소비지출전망 코로나 이전
model2 = aov(value ~ 분류코드별 + variable, data=data3_b[data3_b$지수코드별 == '소비지출전망CSI', ])
summary(model2)
b=HSD.test(model2, '분류코드별', group=TRUE)
b

#연령별 의료·보건비 지출전망 코로나 이후
model1 = aov(value ~ 분류코드별 + variable, data=data3_a[data3_a$지수코드별 == '의료·보건비 지출전망CSI', ])
summary(model1)
a=HSD.test(model1, '분류코드별', group=TRUE)
a

#연령별 의료·보건비 지출전망 코로나 이전
model2 = aov(value ~ 분류코드별 + variable, data=data3_b[data3_b$지수코드별 == '의료·보건비 지출전망CSI', ])
summary(model2)
b=HSD.test(model2, '분류코드별', group=TRUE)
b

#연령별 교양·오락·문화생활비 지출전망 코로나 이후
model1 = aov(value ~ 분류코드별 + variable, data=data3_a[data3_a$지수코드별 == '교양·오락·문화생활비 지출전망CSI', ])
summary(model1)
a=HSD.test(model1, '분류코드별', group=TRUE)
a

#연령별 교양·오락·문화생활비 지출전망 코로나 이전
model2 = aov(value ~ 분류코드별 + variable, data=data3_b[data3_b$지수코드별 == '교양·오락·문화생활비 지출전망CSI', ])
summary(model2)
b=HSD.test(model2, '분류코드별', group=TRUE)
b

#연령별 의류비 지출전망 코로나 이후
model1 = aov(value ~ 분류코드별 + variable, data=data3_a[data3_a$지수코드별 == '의류비 지출전망CSI', ])
summary(model1)
a=HSD.test(model1, '분류코드별', group=TRUE)
a

#연령별 의류비 지출전망 코로나 이전
model2 = aov(value ~ 분류코드별 + variable, data=data3_b[data3_b$지수코드별 == '의류비 지출전망CSI', ])
summary(model2)
b=HSD.test(model2, '분류코드별', group=TRUE)
b

#연령별 외식비 지출전망 코로나 이후
model1 = aov(value ~ 분류코드별 + variable, data=data3_a[data3_a$지수코드별 == '외식비 지출전망CSI', ])
summary(model1)
a=HSD.test(model1, '분류코드별', group=TRUE)
a

#연령별 외식비 지출전망 코로나 이전
model2 = aov(value ~ 분류코드별 + variable, data=data3_b[data3_b$지수코드별 == '외식비 지출전망CSI', ])
summary(model2)
b=HSD.test(model2, '분류코드별', group=TRUE)
b

#연령별 여행비 지출전망 코로나 이후
model1 = aov(value ~ 분류코드별 + variable, data=data3_a[data3_a$지수코드별 == '여행비 지출전망CSI', ])
summary(model1)
a=HSD.test(model1, '분류코드별', group=TRUE)
a

#연령별 여행비 지출전망 코로나 이전
model2 = aov(value ~ 분류코드별 + variable, data=data3_b[data3_b$지수코드별 == '여행비 지출전망CSI', ])
summary(model2)
b=HSD.test(model2, '분류코드별', group=TRUE)
b

#연령별 교육비 지출전망 코로나 이후
model1 = aov(value ~ 분류코드별 + variable, data=data3_a[data3_a$지수코드별 == '교육비 지출전망CSI', ])
summary(model1)
a=HSD.test(model1, '분류코드별', group=TRUE)
a

#연령별 교육비 지출전망 코로나 이전
model2 = aov(value ~ 분류코드별 + variable, data=data3_b[data3_b$지수코드별 == '교육비 지출전망CSI', ])
summary(model2)
b=HSD.test(model2, '분류코드별', group=TRUE)
b

data3_af <- data3_a %>%
  filter(분류코드별 == '40세미만')

data3_bf <- data3_b %>%
  filter(분류코드별 == '40세미만')

#40대 미만 코로나 이전 이후 현재생활형편
a <- data3_af %>% filter(지수코드별 == '현재생활형편CSI')
x <- data3_bf %>% filter(지수코드별 == '현재생활형편CSI')
shapiro.test(a$value)
shapiro.test(x$value)
var.test(a$value, x$value)
t.test(a$value, x$value, var.equal = T)
wilcox.test(a$value, x$value, alternative = 'less')
?wilcox.test

#40대 미만 코로나 이후 현재생활형펀
a1 <- a[c(2:5), ]
a2 <- a[c(8:10), ]
wilcox.test(a1$value, a2$value, 'less')

#40대 미만 코로나 이전 이후 현재생활형편
a <- data3_af %>% filter(지수코드별 == '현재경기판단CSI')
x <- data3_bf %>% filter(지수코드별 == '현재경기판단CSI')
shapiro.test(a$value)
shapiro.test(x$value)
wilcox.test(a$value, x$value, alternative = 'less')

#40대 미만 코로나 이후 현재생활형펀
a1 <- a[c(2:5), ]
a2 <- a[c(8:10), ]
wilcox.test(a1$value, a2$value, 'less')

#40대 미만 코로나 이전 이후 생활형편전망
a <- data3_af %>% filter(지수코드별 == '생활형편전망CSI')
x <- data3_bf %>% filter(지수코드별 == '생활형편전망CSI')
shapiro.test(a$value)
shapiro.test(x$value)
var.test(a$value, x$value)
t.test(a$value, x$value, var.equal = T)
wilcox.test(a$value, x$value, alternative = 'less')

#40대 미만 코로나 이후 생활형편전망
a1 <- a[c(2:5), ]
a2 <- a[c(8:10), ]
wilcox.test(a1$value, a2$value, 'less')

#40대 미만 코로나 이전 이후 향후경기전망
a <- data3_af %>% filter(지수코드별 == '향후경기전망CSI')
x <- data3_bf %>% filter(지수코드별 == '향후경기전망CSI')
shapiro.test(a$value)
shapiro.test(x$value)
wilcox.test(a$value, x$value, alternative = 'less')

#40대 미만 코로나 이후 생활형편전망
a1 <- a[c(2:5), ]
a2 <- a[c(8:10), ]
wilcox.test(a1$value, a2$value, 'less')

#40대 미만 코로나 이전 이후 주택가격전망
a <- data3_af %>% filter(지수코드별 == '주택가격전망CSI')
x <- data3_bf %>% filter(지수코드별 == '주택가격전망CSI')
shapiro.test(a$value)
shapiro.test(x$value)
var.test(a$value, x$value)
t.test(a$value, x$value)
wilcox.test(a$value, x$value)

#40대 미만 코로나 이후 주택가격전망
a1 <- a[c(2:5), ]
a2 <- a[c(8:10), ]
wilcox.test(a1$value, a2$value, 'less')

#40대 미만 코로나 이전 이후 소비지출전망
a <- data3_af %>% filter(지수코드별 == '소비지출전망CSI')
x <- data3_bf %>% filter(지수코드별 == '소비지출전망CSI')
shapiro.test(a$value)
shapiro.test(x$value)
wilcox.test(a$value, x$value)

#40대 미만 코로나 이후 소비지출전망
a1 <- a[c(2:5), ]
a2 <- a[c(8:10), ]
wilcox.test(a1$value, a2$value, 'less')


#40대 미만 코로나 이전 이후 의료·보건비 지출전망
a <- data3_af %>% filter(지수코드별 == '의료·보건비 지출전망CSI')
x <- data3_bf %>% filter(지수코드별 == '의료·보건비 지출전망CSI')
shapiro.test(a$value)
shapiro.test(x$value)
var.test(a$value, x$value)
t.test(a$value, x$value, var.equal = F)
wilcox.test(a$value, x$value)

#40대 미만 코로나 이후 의료·보건비 지출전망
a1 <- a[c(2:5), ]
a2 <- a[c(8:10), ]
wilcox.test(a1$value, a2$value, 'less')

#40대 미만 코로나 이전 이후 의류비 지출전망
a <- data3_af %>% filter(지수코드별 == '의류비 지출전망CSI')
x <- data3_bf %>% filter(지수코드별 == '의류비 지출전망CSI')
shapiro.test(a$value)
shapiro.test(x$value)
wilcox.test(a$value, x$value)

#40대 미만 코로나 이후 의류비지출전망
a1 <- a[c(2:5), ]
a2 <- a[c(8:10), ]
wilcox.test(a1$value, a2$value, 'less')

#40대 미만 코로나 이전 이후 외식비 지출전망
a <- data3_af %>% filter(지수코드별 == '외식비 지출전망CSI')
x <- data3_bf %>% filter(지수코드별 == '외식비 지출전망CSI')
shapiro.test(a$value)
shapiro.test(x$value)
wilcox.test(a$value, x$value)

#40대 미만 코로나 이후 외식비 지출전망
a1 <- a[c(2:5), ]
a2 <- a[c(8:10), ]
wilcox.test(a1$value, a2$value, 'less')

#40대 미만 코로나 이전 이후 여행비 지출전망
a <- data3_af %>% filter(지수코드별 == '여행비 지출전망CSI')
x <- data3_bf %>% filter(지수코드별 == '여행비 지출전망CSI')
shapiro.test(a$value)
shapiro.test(x$value)
var.test(a$value, x$value)
t.test(a$value, x$value, var.equal = F)
wilcox.test(a$value, x$value)

#40대 미만 코로나 이후 외식비 지출전망
a1 <- a[c(2:5), ]
a2 <- a[c(8:10), ]
wilcox.test(a1$value, a2$value, 'less')

#40대 미만 코로나 이전 이후 교육비 지출전망
a <- data3_af %>% filter(지수코드별 == '교육비 지출전망CSI')
x <- data3_bf %>% filter(지수코드별 == '교육비 지출전망CSI')
shapiro.test(a$value)
shapiro.test(x$value)
wilcox.test(a$value, x$value)

#40대 미만 코로나 이후 외식비 지출전망
a1 <- a[c(2:5), ]
a2 <- a[c(8:10), ]
wilcox.test(a1$value, a2$value, 'less')