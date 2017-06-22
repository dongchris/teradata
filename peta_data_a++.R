setwd("C:/Users/bmast/Desktop")

library(dplyr)
library(ggplot2)
library(magrittr)
library(tidyr)
library(readr)
library(lubridate)
library(readxl)
library(stringr)
library(RSNNS)

econ<-tbl_df(read_csv("economic.csv"))

econ$Week<-as.Date(econ$Week)
econ$Year<-year(econ$Week)

names(econ)<-c("week","house_price","tax_rate","unemployment","year")

econ

econ_table<-econ %>% group_by(year) %>% summarise(sum_house_price=sum(house_price),
  sum_tax_rate=sum(tax_rate),sum_unemployment=sum(unemployment))

econ_coef<-prop.table(as.matrix(econ_table[,2:4]),1) %>% as.data.frame() %>% 
  tbl_df() %>% mutate(year=2011:2015) 
econ_coef<-econ_coef[,1:3]

mean(econ_coef[[1]]) #house price 7.8%
mean(econ_coef[[2]]) #tax rate 18.8%
mean(econ_coef[[3]]) #unemployment 73.5%

#########
safety<-tbl_df(read_csv("safety.csv"))
safety$Week<-as.Date(safety$Week)
safety$year<-year(safety$Week)

names(safety)<-c("week","crime","traffic_collision","disaster","year")

safety

safety_table<-safety %>% group_by(year) %>% summarise(sum_crime=sum(crime),
  sum_traffic_col=sum(traffic_collision),sum_disaster=sum(disaster))

safety_coef<-prop.table(as.matrix(safety_table[,2:4]),1) %>% as.data.frame() %>%  tbl_df() %>% mutate(year=2011:2015)

safety_coef<-safety_coef[,1:3]

mean(safety_coef[[1]]) #crime 44.8%
mean(safety_coef[[2]]) #traffic collision 44.3%
mean(safety_coef[[3]]) #disaster 10.9%

###

#########
domestic<-tbl_df(read_csv("domestic.csv"))
domestic$Week<-as.Date(domestic$Week)
domestic$year<-year(domestic$Week)

names(domestic)<-c("week","divorce","education","depression","year")

domestic

domestic_table<-domestic %>% group_by(year) %>% summarise(sum_divorce=sum(divorce),
  sum_education=sum(education),sum_depression=sum(depression))

domestic_coef<-prop.table(as.matrix(domestic_table[,2:4]),1) %>% as.data.frame() %>%  tbl_df() %>% mutate(year=2011:2015)

domestic_coef<-domestic_coef[,1:3]

mean(domestic_coef[[1]]) #divorce 17.3%
mean(domestic_coef[[2]]) #education 64.4%
mean(domestic_coef[[3]]) #depression 18.3%

#############

depression<-read_excel("depression.xls")
names(depression)<-c("state","depression_rate")
depression<-depression[-c(9,52,53),]
depression$depression_rate<-as.numeric(str_sub(depression$depression_rate,start=1,end=3))
depression$depression_rate

final_depression<-as.vector(scale(depression$depression_rate))*mean(domestic_coef[[3]]))

#########
divorce<-read_csv("divorce.csv")
divorce<-divorce[-9,]
divorce$avg_rate<-(divorce$Men+divorce$Women)/2
final_divorce<-as.vector(scale(divorce$avg_rate))*mean(mean(domestic_coef[[1]])))

#####
eq<-read_excel("eq.xlsx")
eq<-eq[-51,1:2]
names(eq)<-c("state","number")
final_eq<-as.vector(scale(eq$number))*mean(safety_coef[[3]])

######
income<-read_excel("income.xls")
income<-income[-51,]
final_income<-as.vector(scale(income$median_income))*mean(econ_coef[[2]])

#####
crime<-read_excel("crime.xls")
crime<-crime[-1,1:2]
names(crime)<-c("state","rate")
final_crime<-as.vector(scale(crime$rate))*mean(safety_coef[[1]])

####
employment<-read_excel("employment.xlsx")
final_employment<-as.vector(scale(employment$Rate))*mean(econ_coef[[3]])

###
car_crash<-read_excel("car_crash.xlsx")
names(car_crash)<-c("state","death")
final_car_crash<-as.vector(scale(car_crash$death))*mean(safety_coef[[2]])

###
GED<-read_excel("GED.xlsx",col_names = F)
names(GED)<-c("state","grad_rate")
final_GED<-as.vector(scale(GED$grad_rate))*mean(domestic_coef[[2]])

###

peta_data<-data.frame(final_car_crash,final_crime,final_depression,
  final_divorce,final_employment,final_eq,final_income,final_GED)

names(peta_data)<-c("Traffic Collision","Crime","Depression","Divorce",
  "Unemployment","Earthquakes","Income","GED")
peta_data[,1:6]<-peta_data[,1:6]*-1
peta_data<-tbl_df(peta_data)
peta_data$state<-unique(car_crash$state)

peta_index<-c()
for(i in 1:50){
  peta_index[i]<-sum(peta[i,])
}

peta_data$sum<-peta_index



write.table(peta_data,"peta.csv",row.names = T,col.names = T)
peta_data


#twitter data
twitter<-read_csv("twitter.csv")
twitter_table<-as.matrix(twitter[,2:8])
twitter_table<-prop.table(twitter_table,2)



##########
final_car_crash11.14<-as.vector(scale(car_crash$death))*twitter_table[1,1]
final_crime11.14<-as.vector(scale(crime$rate))*twitter_table[2,1]
final_depression11.14<-as.vector(scale(depression$depression_rate))*twitter_table[3,1]
final_divorce11.14<-as.vector(scale(divorce$avg_rate))*twitter_table[4,1]
final_eq11.14<-as.vector(scale(eq$number))*twitter_table[5,1]
final_tax11.14<-as.vector(scale(income$median_income))*twitter_table[6,1]
final_unemployment11.14<-as.vector(scale(employment$Rate))*twitter_table[7,1]
final_ged11.14<-as.vector(scale(GED$grad_rate))*twitter_table[8,1]

d11.14<-data.frame(final_car_crash11.14,final_crime11.14,final_depression11.14,final_divorce11.14,
  final_eq11.14,final_tax11.14,final_unemployment11.14,final_ged11.14)

peta_index11.14<-c()
for(i in 1:50){
  peta_index11.14[i]<-sum(d11.14[i,])
}

d11.14$sum<-peta_index11.14
d11.14$state<-unique(car_crash$state)
write.table(d11.14,"d11.14.csv",row.names = T,col.names = T)
##########

final_car_crash11.15<-as.vector(scale(car_crash$death))*twitter_table[1,2]
final_crime11.15<-as.vector(scale(crime$rate))*twitter_table[2,2]
final_depression11.15<-as.vector(scale(depression$depression_rate))*twitter_table[3,2]
final_divorce11.15<-as.vector(scale(divorce$avg_rate))*twitter_table[4,2]
final_eq11.15<-as.vector(scale(eq$number))*twitter_table[5,2]
final_tax11.15<-as.vector(scale(income$median_income))*twitter_table[6,2]
final_unemployment11.15<-as.vector(scale(employment$Rate))*twitter_table[7,2]
final_ged11.15<-as.vector(scale(GED$grad_rate))*twitter_table[8,2]

d11.15<-data.frame(final_car_crash11.15,final_crime11.15,final_depression11.15,final_divorce11.15,
  final_eq11.15,final_tax11.15,final_unemployment11.14,final_ged11.15)

peta_index11.15<-c()
for(i in 1:50){
  peta_index11.15[i]<-sum(d11.15[i,])
}

d11.15$sum<-peta_index11.14
d11.15$state<-unique(car_crash$state)
write.table(d11.15,"d11.15.csv",row.names = T,col.names = T)

###

final_car_crash11.16<-as.vector(scale(car_crash$death))*twitter_table[1,3]
final_crime11.16<-as.vector(scale(crime$rate))*twitter_table[2,3]
final_depression11.16<-as.vector(scale(depression$depression_rate))*twitter_table[3,3]
final_divorce11.16<-as.vector(scale(divorce$avg_rate))*twitter_table[4,3]
final_eq11.16<-as.vector(scale(eq$number))*twitter_table[5,3]
final_tax11.16<-as.vector(scale(income$median_income))*twitter_table[6,3]
final_unemployment11.16<-as.vector(scale(employment$Rate))*twitter_table[7,3]
final_ged11.16<-as.vector(scale(GED$grad_rate))*twitter_table[8,3]

d11.16<-data.frame(final_car_crash11.16,final_crime11.16,final_depression11.16,final_divorce11.16,
  final_eq11.16,final_tax11.16,final_unemployment11.16,final_ged11.16)

peta_index11.16<-c()
for(i in 1:50){
  peta_index11.16[i]<-sum(d11.16[i,])
}

d11.16$sum<-peta_index11.16
d11.16$state<-unique(car_crash$state)
write.table(d11.16,"d11.16.csv",row.names = T,col.names = T)
###

final_car_crash11.16<-as.vector(scale(car_crash$death))*twitter_table[1,4]
final_crime11.16<-as.vector(scale(crime$rate))*twitter_table[2,4]
final_depression11.16<-as.vector(scale(depression$depression_rate))*twitter_table[3,4]
final_divorce11.16<-as.vector(scale(divorce$avg_rate))*twitter_table[4,4]
final_eq11.16<-as.vector(scale(eq$number))*twitter_table[5,4]
final_tax11.16<-as.vector(scale(income$median_income))*twitter_table[6,4]
final_unemployment11.16<-as.vector(scale(employment$Rate))*twitter_table[7,4]
final_ged11.16<-as.vector(scale(GED$grad_rate))*twitter_table[8,4]

d11.16<-data.frame(final_car_crash11.16,final_crime11.16,final_depression11.16,final_divorce11.16,
  final_eq11.16,final_tax11.16,final_unemployment11.16,final_ged11.16)

peta_index11.16<-c()
for(i in 1:50){
  peta_index11.16[i]<-sum(d11.16[i,])
}

d11.16$sum<-peta_index11.16
d11.16$state<-unique(car_crash$state)
write.table(d11.16,"d11.17.csv",row.names = T,col.names = T)



final_car_crash11.16<-as.vector(scale(car_crash$death))*twitter_table[1,5]
final_crime11.16<-as.vector(scale(crime$rate))*twitter_table[2,5]
final_depression11.16<-as.vector(scale(depression$depression_rate))*twitter_table[3,5]
final_divorce11.16<-as.vector(scale(divorce$avg_rate))*twitter_table[4,5]
final_eq11.16<-as.vector(scale(eq$number))*twitter_table[5,5]
final_tax11.16<-as.vector(scale(income$median_income))*twitter_table[6,5]
final_unemployment11.16<-as.vector(scale(employment$Rate))*twitter_table[7,5]
final_ged11.16<-as.vector(scale(GED$grad_rate))*twitter_table[8,5]

d11.16<-data.frame(final_car_crash11.16,final_crime11.16,final_depression11.16,final_divorce11.16,
  final_eq11.16,final_tax11.16,final_unemployment11.16,final_ged11.16)

peta_index11.16<-c()
for(i in 1:50){
  peta_index11.16[i]<-sum(d11.16[i,])
}

d11.16$sum<-peta_index11.16
d11.16$state<-unique(car_crash$state)
write.table(d11.16,"d11.18.csv",row.names = T,col.names = T)



final_car_crash11.16<-as.vector(scale(car_crash$death))*twitter_table[1,6]
final_crime11.16<-as.vector(scale(crime$rate))*twitter_table[2,6]
final_depression11.16<-as.vector(scale(depression$depression_rate))*twitter_table[3,6]
final_divorce11.16<-as.vector(scale(divorce$avg_rate))*twitter_table[4,6]
final_eq11.16<-as.vector(scale(eq$number))*twitter_table[5,6]
final_tax11.16<-as.vector(scale(income$median_income))*twitter_table[6,6]
final_unemployment11.16<-as.vector(scale(employment$Rate))*twitter_table[7,6]
final_ged11.16<-as.vector(scale(GED$grad_rate))*twitter_table[8,6]

d11.16<-data.frame(final_car_crash11.16,final_crime11.16,final_depression11.16,final_divorce11.16,
  final_eq11.16,final_tax11.16,final_unemployment11.16,final_ged11.16)

peta_index11.16<-c()
for(i in 1:50){
  peta_index11.16[i]<-sum(d11.16[i,])
}

d11.16$sum<-peta_index11.16
d11.16$state<-unique(car_crash$state)
write.table(d11.16,"d11.19.csv",row.names = T,col.names = T)



final_car_crash11.16<-as.vector(scale(car_crash$death))*twitter_table[1,7]
final_crime11.16<-as.vector(scale(crime$rate))*twitter_table[2,7]
final_depression11.16<-as.vector(scale(depression$depression_rate))*twitter_table[3,7]
final_divorce11.16<-as.vector(scale(divorce$avg_rate))*twitter_table[4,7]
final_eq11.16<-as.vector(scale(eq$number))*twitter_table[5,7]
final_tax11.16<-as.vector(scale(income$median_income))*twitter_table[6,7]
final_unemployment11.16<-as.vector(scale(employment$Rate))*twitter_table[7,7]
final_ged11.16<-as.vector(scale(GED$grad_rate))*twitter_table[8,7]

d11.16<-data.frame(final_car_crash11.16,final_crime11.16,final_depression11.16,final_divorce11.16,
  final_eq11.16,final_tax11.16,final_unemployment11.16,final_ged11.16)

peta_index11.16<-c()
for(i in 1:50){
  peta_index11.16[i]<-sum(d11.16[i,])
}

d11.16$sum<-peta_index11.16
d11.16$state<-unique(car_crash$state)
write.table(d11.16,"d11.20.csv",row.names = T,col.names = T)

head(final_car_crash)
head(final_car_crash11.14)
head(peta$sum)
head(d11.16$sum)
