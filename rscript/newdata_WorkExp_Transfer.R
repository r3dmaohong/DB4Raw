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
people$�u�@�����ɶ� %>% unique %>% sort
people$�u�@�����ɶ�[people$�u�@�����ɶ�=="�b¾��"] <- format(Sys.Date(), "%Y/%m/%d")
people$�u�@�����ɶ� <- as.Date(people$�u�@�����ɶ�)
people$�u�@�}�l�ɶ� <- as.Date(people$�u�@�}�l�ɶ�)

#people <- people %>% mutate(�u�@�ɬq = (as.numeric(�u�@�����ɶ� - �u�@�}�l�ɶ�)/365))
people[,�u�@�ɬq := (as.numeric(�u�@�����ɶ� - �u�@�}�l�ɶ�)/365), ]
people[,¾�Ȥp���`�~�� := sum(�u�@�ɬq), by=.(�i���s��, ¾�Ȥp���W��)]
#people <- people %>% group_by(�i���s��, ¾�Ȥp���W��) %>% mutate(¾�Ȥp���`�~�� = sum(�u�@�ɬq))

write.csv(people, "newdata_2016_edit.csv", row.names = F)

##Filter
people_tmp <- people[�u�@�ݹJ>20000 & �u�@�ɬq>0, ]
people_tmp[,¾�Ȥp���`�~�� := sum(�u�@�ɬq), by=.(�i���s��, ¾�Ȥp���W��)]
people_tmp <- people_tmp[which((�u�@�����ɶ� %like% "2017") | (�u�@�����ɶ� %like% "2016") | (�u�@�����ɶ� %like% "2015")),]
#people_tmp <- people_tmp[which(grepl("2017",people_tmp$�u�@�����ɶ�) | grepl("2015",people_tmp$�u�@�����ɶ�) | grepl("2016",people_tmp$�u�@�����ɶ�) | grepl("�b¾��",people_tmp$�u�@�����ɶ�)),]
dim(people_tmp)
write.csv(people_tmp, "�z���2015-2016_newdata.csv",row.names=F)