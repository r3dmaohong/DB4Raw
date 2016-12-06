rm(list = ls()) #Remove all objects in the environment
gc() ##Free up the memory

if(!exists("original_path"))
  original_path <- getwd()
setwd(file.path("DB4Raw"))
options(scipen=999)

##Libraries
library(pbapply)
library(data.table)
library(dplyr)
library(plyr)

# Demand
##Get all files' names.
files <- list.files(file.path("per.month", "求才"), full.names = TRUE)
files <- files[grepl("csv", files)]
##Import files
files.lists <- pblapply(files,function(file.name){
  file.list <- fread(file.name)
  ##Create new col:date by it's file name...
  date <- unlist(strsplit(file.name, "/"))[length(unlist(strsplit(file.name, "/")))] %>% gsub("[A-z.]", "", .) ##%>% substr(., 1, 7)
  file.list <- cbind(file.list, date)
  names(file.list) <- names(file.list) %>% iconv(., "UTF-8")
  return(file.list)
})

##Combine all the lists into data frame.
total.data <- do.call(rbind.fill,files.lists) # do.call(rbind, files.lists)
gc()
rm(files.lists,files)
dim(total.data)
#str(total.data)
names(total.data)
setDT(total.data)

#dat <- total.data
##Import with encoding will face error...
##File's content itself is garbled...
#names(total.data) <- names(total.data) %>% iconv(., "UTF-8")
cols <- 1:length(names(total.data))

#total.data[, (cols) := lapply(.SD, function(x) iconv(x, "UTF-8"))]
total.data[, (cols) := lapply(.SD, function(x) ifelse(is.na(x %>% iconv(., "UTF-8")), x, x %>% iconv(., "UTF-8")))]

if(length(names(total.data)) != length(unique(names(total.data))))
  total.data <- total.data[, unique(names(total.data)), with=F]
dim(total.data)
names(total.data)

total.data <- total.data[ 實習職缺=="N" ]
total.data$職缺屬性 %>% table
total.data <- total.data[ 職缺屬性=="全職" | 職缺屬性=="中高階" ]

## Working area transfer
total.data$area.work <- ""
North <- c("台北", "新北", "基隆", "桃園", "新竹")
Mid   <- c("苗栗", "台中", "彰化", "南投", "雲林")
South <- c("嘉義", "台南", "高雄", "屏東")
East  <- c("宜蘭", "花蓮", "台東", "澎湖", "金門", "連江")
Out   <- c(North, Mid, South, East)
total.data[ substr(total.data[,工作地點],1,2) %in% North, area.work:="北部地區"]
total.data[ substr(total.data[,工作地點],1,2) %in% Mid, area.work:="中部地區"]
total.data[ substr(total.data[,工作地點],1,2) %in% South, area.work:="南部地區"]
total.data[ substr(total.data[,工作地點],1,2) %in% East, area.work:="東部與離島地區"]
total.data[ !(substr(total.data[,工作地點],1,2) %in% Out) , area.work:="非台灣地區"]

unique(total.data$area.work)
##Check areas which are outside Taiwan
total.data[ area.work=="非台灣地區" , 工作地點] %>% substr(., 1, 3) %>% table

total.data$date %>% unique
total.data$date <- total.data$date %>% substr(., 1, 5)
total.data$date <- as.integer(total.data$date)
total.data$date <- total.data$date - 1

#不動產經紀人 => 不動產經紀人/營業員
total.data$職務小類名稱[total.data$職務小類名稱=="不動產經紀人"] <- "不動產經紀人/營業員"

##Function for Trend analysis...
source("rscript/function/JobTrendFunc.R", print.eval  = TRUE)

#####################################
#Top25 Job demand, grouped by area...
#####################################
JobTrend(total.data, "職務小類名稱", "area.work", "AreaJobDemand")

###################################################
###################################################
##############    I am a divider..   ##############
###################################################
###################################################

##Resume: Supply
##Import all files.
files <- list.files(file.path("per.month", "求職"), full.names = TRUE)
files <- files[grepl(".csv", files, fixed=TRUE)]
##Import files
files.lists <- pblapply(files,function(file.name){
  file.list <- fread(file.name)
  date <- unlist(strsplit(file.name, "/"))[length(unlist(strsplit(file.name, "/")))] %>% gsub("[A-z.]", "", .) ##%>% substr(., 1, 7)
  file.list$date <- date
  #file.list <- cbind(file.list, date)
  return(file.list)
})

##Combine all the lists into data frame.
totalResumeData <- do.call(rbind,files.lists)
rm(files.lists,files)
dim(totalResumeData)
str(totalResumeData)
names(totalResumeData)

totalResumeData$希望工作性質 %>% table
totalResumeData <- totalResumeData[希望工作性質=="全職" | 希望工作性質=="中高階",]

###Expect working area transferation
totalResumeData$area.work <- ""
totalResumeData[ substr(totalResumeData[,希望上班地區名稱],1,2) %in% North, area.work:="北部地區"]
totalResumeData[ substr(totalResumeData[,希望上班地區名稱],1,2) %in% Mid, area.work:="中部地區"]
totalResumeData[ substr(totalResumeData[,希望上班地區名稱],1,2) %in% South, area.work:="南部地區"]
totalResumeData[ substr(totalResumeData[,希望上班地區名稱],1,2) %in% East, area.work:="東部與離島地區"]
totalResumeData[ !(substr(totalResumeData[,希望上班地區名稱],1,2) %in% Out) , area.work:="非台灣地區"]

###Living area transferation
totalResumeData$area.live <- ""
totalResumeData[ substr(totalResumeData[,居住地區],1,2) %in% North, area.live:="北部地區"]
totalResumeData[ substr(totalResumeData[,居住地區],1,2) %in% Mid, area.live:="中部地區"]
totalResumeData[ substr(totalResumeData[,居住地區],1,2) %in% South, area.live:="南部地區"]
totalResumeData[ substr(totalResumeData[,居住地區],1,2) %in% East, area.live:="東部與離島地區"]
totalResumeData[ !(substr(totalResumeData[,居住地區],1,2) %in% Out) , area.live:="非台灣地區"]

unique(totalResumeData$area.work)
## Check areas which are outside Taiwan
totalResumeData[ area.work=="非台灣地區" , 希望上班地區名稱] %>% substr(., 1, 3) %>% table

##Country
totalResumeData$country.work <- totalResumeData$希望上班地區名稱 %>% substr(., 1, 3)
totalResumeData[ !(substr(totalResumeData[, country.work],1,2) %in% Out) , country.work:="非台灣地區"]

totalResumeData$country.live <- totalResumeData$居住地區 %>% substr(., 1, 3)
totalResumeData[ !(substr(totalResumeData[, country.live],1,2) %in% Out) , country.live:="非台灣地區"]

totalResumeData$date %>% unique
totalResumeData$date <- totalResumeData$date %>% substr(., 1, 5)
totalResumeData$date <- as.integer(totalResumeData$date)
totalResumeData$date <- totalResumeData$date - 1

##Grouped by Department
dpt.match <- read.csv("1111學群學門學類-20160617-1.csv", stringsAsFactors=F)
for(i in 1:ncol(dpt.match)){
  dpt.match[,i] <- gsub("[0-9+_]", "", dpt.match[,i])
}
totalResumeData$dpt <- ""
for(i in 1:nrow(dpt.match)){
  totalResumeData$dpt[which(totalResumeData$最高學歷_科系小類名稱==dpt.match[i,3])] <- dpt.match[i,2]
}

########################################
###Top25 Job wanted, grouped by area...
########################################
JobTrend(totalResumeData, "希望職務小類名稱", "area.work", "AreaJobWanted")

########################################
###Top25 Job wanted, grouped by dpt...
########################################
JobTrend(totalResumeData, "希望職務小類名稱", "dpt", "DepartmentJobWanted")

########################################
###Top10 working area, grouped by dpt...
########################################
JobTrend(totalResumeData, "country.work", "dpt", "DepartmentExpectArea", 10)

########################################
###Top10 industry, grouped by dpt...
########################################
JobTrend(totalResumeData, "希望產業大類名稱", "dpt", "DepartmentExpectIndustry", 10)

########################################
###Top10 working area, grouped by living area...
########################################
JobTrend(totalResumeData, "country.work", "area.live", "LivePExpectWorkP", 10)
