## Stability of employment
## (Seniority of one job)
## Background variable : Department

setwd("DB4Raw")

library(stringr)

dat <- read.csv(file.choose(), stringsAsFactors = F)

setDT(dat)
names(dat)
dat <- unique(dat)
#¾�ȸg��

dep_trans <- read.csv("����VS18�Ǹs.csv", stringsAsFactors=F)


#dep_trans$�Ǹs <- str_split_fixed(dep_trans$�Ǹs, "_", 2)[,2]
#dep_trans$�Ǫ� <- str_split_fixed(dep_trans$�Ǫ�, "_", 2)[,2]
#dep_trans$���� <- str_split_fixed(dep_trans$����, "_", 2)[,2]
names(dep_trans)[4] <- "����"
names(dep_trans)[8] <- "�Ǹs"

dat$Dep <- ""
for(i in 1:nrow(dep_trans)){
  dat[dat$��t���O==dep_trans$����[i], Dep:=dep_trans$�Ǹs[i]]
  #dat$Dep[which(dat$��t���O==dep_trans$����[i])] <- dep_trans$�Ǹs[i]
  cat("\r", format(round(i/nrow(dep_trans)*100, 3), nsmall=3), "%")
}

dat$Dep %>% table
dat$��t���O[dat$Dep==""] %>% table

head(dat$�u�@�}�l�ɶ�)
head(dat$�u�@�����ɶ�)
head(dat$¾�ȸg��)


dat$month <- str_split_fixed(dat$¾�ȸg��, "�~", 2)[,2]
dat$month %>% table
dat$year <- str_split_fixed(dat$¾�ȸg��, "�~", 2)[,1]
unique(dat$year) %>% as.numeric %>% .[order(.)]

dat$period_days <- as.Date(dat$�u�@�����ɶ�) - as.Date(dat$�u�@�}�l�ɶ�)
dat$period_year <- dat$period_days/365
dat$period_year <- dat$period_year %>% as.numeric()

tmp <- dat[, .(mean(period_year[!period_year %in% boxplot.stats(period_year)$out]), .N), by = Dep][Dep!=""][order(-V1)] %>% setnames(., "V1", "year(s)")
write.csv(tmp, "output\\�Ǹs�N�~í�w��_2016.csv", row.names = F)