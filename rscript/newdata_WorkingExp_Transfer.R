library(data.table)
library(dplyr)

tmp    <- read.csv("D:\\Share\\jimbo\\newdata2016\\ESO105092109-1.csv", stringsAsFactors=F)
tmp2   <- read.csv("D:\\Share\\jimbo\\newdata2016\\ESO105092109-2.csv", stringsAsFactors=F)
tmp3   <- read.csv("D:\\Share\\jimbo\\newdata2016\\ESO105092109-3.csv", stringsAsFactors=F)
people <- do.call(rbind,list(tmp,tmp2,tmp3))
rm(tmp, tmp2, tmp3)
dim(people)
class(people)
names(people)
str(people)

##save.image("People_2016_loaded")
##load("People_2016_loaded")
setwd("DB4Raw")

##Mutate new col...
##Accumulated experience...
setDT(people)
people$工作結束時間 %>% unique %>% sort
people$工作結束時間[people$工作結束時間=="在職中"] <- format(Sys.Date(), "%Y/%m/%d")
people$工作結束時間 <- as.Date(people$工作結束時間)
people$工作開始時間 <- as.Date(people$工作開始時間)

#people <- people %>% mutate(工作時段 = (as.numeric(工作結束時間 - 工作開始時間)/365))
people[,工作時段 := (as.numeric(工作結束時間 - 工作開始時間)/365), ]
people[,職務小類總年資 := sum(工作時段), by=.(履歷編號, 職務小類名稱)]
#people <- people %>% group_by(履歷編號, 職務小類名稱) %>% mutate(職務小類總年資 = sum(工作時段))

write.csv(people, "newdata_2016_edit.csv", row.names = F)

##Filter
people_tmp <- people[工作待遇>20000 & 工作時段>0, ]
people_tmp[,職務小類總年資 := sum(工作時段), by=.(履歷編號, 職務小類名稱)]
people_tmp <- people_tmp[which((工作結束時間 %like% "2017") | (工作結束時間 %like% "2016") | (工作結束時間 %like% "2015")),]
#people_tmp <- people_tmp[which(grepl("2017",people_tmp$工作結束時間) | grepl("2015",people_tmp$工作結束時間) | grepl("2016",people_tmp$工作結束時間) | grepl("在職中",people_tmp$工作結束時間)),]
dim(people_tmp)
write.csv(people_tmp, "篩選後2015-2016_newdata.csv",row.names=F)
