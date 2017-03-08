## Stability of employment
## (Seniority of one job)
## Background variable : Department

setwd("DB4Raw")

library(stringr)

dat <- read.csv(file.choose(), stringsAsFactors = F)

setDT(dat)
names(dat)
dat <- unique(dat)
#職務經驗

dep_trans <- read.csv("學類VS18學群.csv", stringsAsFactors=F)


#dep_trans$學群 <- str_split_fixed(dep_trans$學群, "_", 2)[,2]
#dep_trans$學門 <- str_split_fixed(dep_trans$學門, "_", 2)[,2]
#dep_trans$學類 <- str_split_fixed(dep_trans$學類, "_", 2)[,2]
names(dep_trans)[4] <- "學類"
names(dep_trans)[8] <- "學群"

dat$Dep <- ""
for(i in 1:nrow(dep_trans)){
  dat[dat$科系類別==dep_trans$學類[i], Dep:=dep_trans$學群[i]]
  #dat$Dep[which(dat$科系類別==dep_trans$學類[i])] <- dep_trans$學群[i]
  cat("\r", format(round(i/nrow(dep_trans)*100, 3), nsmall=3), "%")
}

dat$Dep %>% table
dat$科系類別[dat$Dep==""] %>% table

head(dat$工作開始時間)
head(dat$工作結束時間)
head(dat$職務經驗)


dat$month <- str_split_fixed(dat$職務經驗, "年", 2)[,2]
dat$month %>% table
dat$year <- str_split_fixed(dat$職務經驗, "年", 2)[,1]
unique(dat$year) %>% as.numeric %>% .[order(.)]

dat$period_days <- as.Date(dat$工作結束時間) - as.Date(dat$工作開始時間)
dat$period_year <- dat$period_days/365
dat$period_year <- dat$period_year %>% as.numeric()

tmp <- dat[, .(mean(period_year[!period_year %in% boxplot.stats(period_year)$out]), .N), by = Dep][Dep!=""][order(-V1)] %>% setnames(., "V1", "year(s)")
write.csv(tmp, "output\\學群就業穩定度_2016.csv", row.names = F)
