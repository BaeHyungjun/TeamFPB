library(tidyverse)
library(data.table)
library(readxl)
library(gridExtra)
library(reshape2)
library(agricolae)
#install.packages("agricolae")
#install.packages("gridExtra")
#install.packages("reshape2")

## 그래프 그리기 근데 아직 하나밖에 안함 그래프도 만들예정
customer <- read_excel("월별소비자동향조사.xlsx")
head(customer)

customer<-customer %>% 
  select(-c("항목","단위"))

customer_age <- customer %>% filter(grepl('세+',분류코드별))
customer_age

customer_base <- customer_age[1:2]
bf_age <- customer_age[3:27]
af_age <- customer_age[28:36]

customer_base
bf_age <- customer_base %>% bind_cols(bf_age)
af_age <- customer_base %>% bind_cols(af_age)

bf_age <- bf_age %>% cbind(data.frame(평균 = apply(bf_age[, 3:27], 1, mean)))
af_age <- af_age %>% cbind(data.frame(평균 = apply(af_age[, 3:11], 1, mean)))

head(bf_age)
head(af_age)

bf_age <- bf_age %>% filter(!grepl(' ', 지수코드별))
af_age <- af_age %>% filter(!grepl(' ', 지수코드별)) 

bf_age

unique(bf_age[1])

bf_hal <- bf_age %>% filter(지수코드별 %in% c("현재생활형편CSI","소비지출전망CSI"))
af_hal <- af_age %>% filter(지수코드별 %in% c("현재생활형편CSI","소비지출전망CSI"))


bf_hal <- bf_hal %>% mutate(group = "1")
af_hal <- af_hal %>% mutate(group = "2")
bf_hal <- bf_hal %>% select(c("지수코드별","분류코드별","평균","group"))
af_hal <- af_hal %>% select(c("지수코드별","분류코드별","평균","group"))

bf_hal
af_hal
df <- bf_hal %>% bind_rows(af_hal)
df

routine <- df %>% filter(지수코드별 == "현재생활형편CSI")
consume <- df %>% filter(지수코드별 == "소비지출전망CSI")

routine
consume

routinep = ggplot(routine, aes(x = 분류코드별, y = 평균, fill = group)) + 
  geom_bar(stat = "identity",position = "dodge") +
  labs(x = "연령", y = "CSI평균") +
  ggtitle("현재생활형편CSI") + 
  coord_cartesian(ylim = c(70, 100))
  

consumep = ggplot(consume, aes(x = 분류코드별, y = 평균, fill = group)) + 
  geom_bar(stat = "identity",position = "dodge") +
  labs(x = "연령", y = "CSI평균") +
  ggtitle("소비지출전망CSI") + 
  coord_cartesian(ylim = c(70, 120))

grid.arrange(routinep,consumep, nrow = 2, ncol = 1)

# 여기서부터는 연령별 CSI 종류 분산분석
#_-------------------------------------------------------------------------------


data1 <- customer %>%
  filter(분류코드별 %in% c('자영업자', '봉급생활자')) %>% 
  filter(지수코드별 %in% c('임금수준전망CSI', '현재생활형편CSI',
                      '현재경기판단CSI', '생활형편전망CSI', '향후경기전망CSI')) %>% 
  select(-항목, -단위) %>% 
  melt(id.vars=c('지수코드별', '분류코드별'))

data1$지수코드별 = as.factor(data1$지수코드별)
data1$분류코드별 = as.factor(data1$분류코드별)

data1_b <- data1[c(1:240), ]
data1_a <- data1[c(241:340), ]

category = c('가계수입전망CSI', '현재가계저축CSI',
             '가계저축전망CSI', '주택가격전망CSI',
             '소비지출전망CSI', '의료·보건비 지출전망CSI',
             '교양·오락·문화생활비 지출전망CSI', '의류비 지출전망CSI',
             '외식비 지출전망CSI', '여행비 지출전망CSI', '교육비 지출전망CSI')

data2 <- customer %>%
  filter(grepl('세+',분류코드별)) %>% 
  filter(지수코드별 %in% category) %>% 
  select(-항목, -단위) %>% 
  melt(id.vars=c('지수코드별', '분류코드별')) %>% 
  mutate(지수코드별 = as.factor(지수코드별), 분류코드별 = as.factor(분류코드별))

data2_b <- data2[c(1:1375), ]
data2_a <- data2[c(1376:1870), ]

anova_result = function(df, category) {
  
  aov_list = list()
  summary_list = list()
  posthoc_list = list()
  
  for (i in 1:length(category)){
    aov_model = aov(value ~ 분류코드별 + variable,
                    data=df[df$지수코드별 == category[i], ])
    aov_list[[i]] = aov_model

    summary_list[[i]] = summary(aov_model)
    
    posthoc = HSD.test(aov_model, '분류코드별', group=TRUE)
    posthoc_list[[i]] = posthoc
  }
  
  return(list('aov' = aov_list, 'summary' = summary_list, 'posthoc' = posthoc_list))
  
}

anova_result_before = anova_result(df=data2_b, category=category)
anova_result_after = anova_result(df=data2_a, category=category)

for (i in 1:length(category)){
  cat('\n', category[i], '에 대한 분산분석 결과입니다.\n\n')
  print(anova_result_before$summary[[i]])
  print(anova_result_before$posthoc[[i]])
}

for (i in 1:length(category)){
  cat('\n', category[i], '에 대한 분산분석 결과입니다.\n\n')
  print(anova_result_after$summary[[i]])
  print(anova_result_after$posthoc[[i]])
}