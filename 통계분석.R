library(tidyverse)
library(data.table)
library(readxl)
library(gridExtra)
library(reshape2)
library(agricolae)
#install.packages("agricolae")
#install.packages("gridExtra")
#install.packages("reshape2")

# 소비자 심리지수 비교
customer <- read_excel("월별소비자동향조사.xlsx")
CCSI <- customer %>% 
  filter(customer$지수코드별 == "소비자심리지수") %>% 
  select(-c("항목","단위","분류코드별")) %>% 
  melt(id.vars = "지수코드별")

CCSI_bf <- CCSI[1:25,2:3]
CCSI_af <- CCSI[26:34,2:3]

var.test(CCSI_bf$value,CCSI_af$value)
t.test(CCSI_bf$value,CCSI_af$value, var.equal = FALSE, alternative = "greater")

# bargraph 그린 곳.
customer <- read_excel("월별소비자동향조사.xlsx")
head(customer)

customer<-customer %>% 
  select(-c("항목","단위"))

# 연령별 데이터 전처리
customer_age <- customer %>% filter(grepl('세+',분류코드별))
customer_age

customer_age$지수코드별 = as.factor(customer_age$지수코드별)
customer_age$분류코드별 = as.factor(customer_age$분류코드별)

customer_age$분류코드별 <- relevel(customer_age$분류코드별,"40세미만")

customer_base <- customer_age[1:2]
bf_age <- customer_age[3:27]
af_age <- customer_age[28:36]

bf_age <- customer_base %>% bind_cols(bf_age)
af_age <- customer_base %>% bind_cols(af_age)

bf_age <- bf_age %>% cbind(data.frame(평균 = apply(bf_age[, 3:27], 1, mean)))
af_age <- af_age %>% cbind(data.frame(평균 = apply(af_age[, 3:11], 1, mean)))

head(bf_age)
head(af_age)

category <- c("가계수입전망CSI","현재가계저축CSI","가계저축전망CSI",
              "주택가격전망CSI","소비지출전망CSI",
              "의료·보건비 지출전망CSI","교양·오락·문화생활비 지출전망CSI",
              "의류비 지출전망CSI","외식비 지출전망CSI","여행비 지출전망CSI", 
              "교육비 지출전망CSI")

# 소득별 데이터 전처리
customer_wage <- customer %>% filter(grepl('만원+',분류코드별))
customer_wage

customer_wage$지수코드별 = as.factor(customer_wage$지수코드별)
customer_wage$분류코드별 = as.factor(customer_wage$분류코드별)
customer_wage$분류코드별 <- relevel(customer_wage$분류코드별,"100만원미만")

customer_base <- customer_wage[1:2]
bf_wage <- customer_wage[3:27]
af_wage <- customer_wage[28:36]

bf_wage <- customer_base %>% bind_cols(bf_wage)
af_wage <- customer_base %>% bind_cols(af_wage)

bf_wage <- bf_wage %>% cbind(data.frame(평균 = apply(bf_wage[, 3:27], 1, mean)))
af_wage <- af_wage %>% cbind(data.frame(평균 = apply(af_wage[, 3:11], 1, mean)))

head(bf_wage)
head(af_wage)

draw_bargraph = function(before, after, category) {
  before <- before %>% 
    filter(지수코드별 %in% category)
  after <- after %>%
    filter(지수코드별 %in% category)
  
  bf_hal <- before %>% mutate(group = "이전") %>%
    select(c("지수코드별","분류코드별","평균","group"))
  af_hal <- after %>% mutate(group = "이후") %>% 
    select(c("지수코드별","분류코드별","평균","group"))
  
  df <- bf_hal %>% bind_rows(af_hal)
  graph = ggplot(df , aes(x = 분류코드별, y = 평균, fill = group)) + 
    geom_bar(stat = "identity",position = "dodge") +
    labs(x = "Category", y = "CSI평균") +
    theme(axis.text.x = element_text(angle = 45, hjust=1)) +
    ggtitle(category) + 
    coord_cartesian(ylim = c(min(df$평균) - 10, max(df$평균) + 10))
  
  return(graph)
}

category_age = c('가계수입전망CSI', '의료·보건비 지출전망CSI',
                 '가계저축전망CSI', '주택가격전망CSI',
                 '여행비 지출전망CSI', '교육비 지출전망CSI')

category_wage = c('가계수입전망CSI',
                  '가계저축전망CSI', '주택가격전망CSI',
                  '여행비 지출전망CSI', '의료·보건비 지출전망CSI','교양·오락·문화생활비 지출전망CSI')

age_graph_list <- list()
wage_graph_list <- list()

for (i in 1:length(category_age)){
  age_graph_list[[i]] = draw_bargraph(bf_age, af_age, category_age[i])
}

for (i in 1:length(category_wage)){
  wage_graph_list[[i]] = draw_bargraph(bf_wage, af_wage, category_wage[i])
}


do.call("grid.arrange", c(wage_graph_list, ncol=3, nrow=2))
do.call("grid.arrange", c(age_graph_list, ncol=3, nrow=2))

# 여기서부터는 연령별 CSI 종류 분산분석
#_-------------------------------------------------------------------------------

customer <- read_excel("월별소비자동향조사.xlsx")
head(customer)

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

category_age = c('가계수입전망CSI', '의료·보건비 지출전망CSI',
                 '가계저축전망CSI', '주택가격전망CSI',
                 '여행비 지출전망CSI', '교육비 지출전망CSI')

category_wage = c('가계수입전망CSI',
                  '가계저축전망CSI', '주택가격전망CSI',
                  '여행비 지출전망CSI', '의료·보건비 지출전망CSI','교양·오락·문화생활비 지출전망CSI')

data_wage <- customer %>%
  filter(grepl('만원+',분류코드별)) %>% 
  filter(지수코드별 %in% category) %>% 
  select(-항목, -단위) %>% 
  melt(id.vars=c('지수코드별', '분류코드별')) %>% 
  mutate(지수코드별 = as.factor(지수코드별), 분류코드별 = as.factor(분류코드별))

data_wage_b <- data_wage[c(1:1650), ]
data_wage_a <- data_wage[c(1651:2244), ]

data_wage_b

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

anova_result_before = anova_result(df=data2_b, category=category_age)
anova_result_after = anova_result(df=data2_a, category=category_age)

anova_result_before_wage = anova_result(df = data_wage_b, category = category_wage)
anova_result_after_wage = anova_result(df = data_wage_a, category = category_wage)

for (i in 1:length(category_age)){
  cat('\n', category_age[i], '에 대한 분산분석 결과입니다.\n\n')
  print(anova_result_before$summary[[i]])
  print(anova_result_before$posthoc[[i]])
  print(anova_result_after$summary[[i]])
  print(anova_result_after$posthoc[[i]])
}

for (i in 1:length(category_wage)){
  cat('\n', category_wage[i], '에 대한 분산분석 결과입니다.\n\n')
  print(anova_result_before_wage$summary[[i]])
  print(anova_result_before_wage$posthoc[[i]])
  print(anova_result_after_wage$summary[[i]])
  print(anova_result_after_wage$posthoc[[i]])
}
