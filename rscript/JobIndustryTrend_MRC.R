#MapReduce like concept : Job Industry Trend
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
files <- sort(files)

##Function for Trend analysis...
source("rscript/function/JobTrendFunc_MRC.R", print.eval  = TRUE)

## Total history of data...

## Generate historical changes
datHistory <- lapply(files, function(file.name){
  dat <- fread(file.name)
  date <- unlist(strsplit(file.name, "/"))[length(unlist(strsplit(file.name, "/")))] %>% gsub("[A-z.]", "", .) ##%>% substr(., 1, 7)
  dat <- cbind(dat, date)
  
  if(any(is.na(names(dat) %>% iconv(., "UTF-8")))){
    names(dat) <- names(dat)
  }else{
    names(dat) <- names(dat) %>% iconv(., "UTF-8")
    cols <- 1:length(names(dat))
    dat[, (cols) := lapply(.SD, function(x) ifelse(is.na(x %>% iconv(., "UTF-8")), x, x %>% iconv(., "UTF-8")))]
  }
  
  gc()
  
  ## view
  dim(dat)
  names(dat)
  setDT(dat)
  dat <- dat[職缺關閉時間=="",]
  #dat <- dat
  ##Import with encoding will face error...
  ##File's content itself is garbled...
  #names(dat) <- names(dat) %>% iconv(., "UTF-8")
  #cols <- 1:length(names(dat))
  
  #dat[, (cols) := lapply(.SD, function(x) iconv(x, "UTF-8"))]
  #dat$職缺狀態 %>% table
  #dat[, (cols) := lapply(.SD, function(x) ifelse(is.na(x %>% iconv(., "UTF-8")), x, x %>% iconv(., "UTF-8")))]
  #dat$職缺狀態 %>% table
  
  if(length(names(dat)) != length(unique(names(dat))))
    dat <- dat[, unique(names(dat)), with=F]
  dim(dat)
  names(dat)
  
  dat <- dat[ 實習職缺=="N" ]
  dat$職缺屬性 %>% table
  dat <- dat[ 職缺屬性=="全職" | 職缺屬性=="中高階" ]
  
  ## Working area transfer
  dat$area.work <- ""
  North <- c("台北", "新北", "基隆", "桃園", "新竹")
  Mid   <- c("苗栗", "台中", "彰化", "南投", "雲林")
  South <- c("嘉義", "台南", "高雄", "屏東")
  East  <- c("宜蘭", "花蓮", "台東", "澎湖", "金門", "連江")
  Out   <- c(North, Mid, South, East)
  dat[ substr(dat[,工作地點],1,2) %in% North, area.work:="北部地區"]
  dat[ substr(dat[,工作地點],1,2) %in% Mid, area.work:="中部地區"]
  dat[ substr(dat[,工作地點],1,2) %in% South, area.work:="南部地區"]
  dat[ substr(dat[,工作地點],1,2) %in% East, area.work:="東部與離島地區"]
  dat[ !(substr(dat[,工作地點],1,2) %in% Out) , area.work:="非台灣地區"]
  
  unique(dat$area.work)
  ##Check areas which are outside Taiwan
  dat[ area.work=="非台灣地區" , 工作地點] %>% substr(., 1, 3) %>% table
  
  ## This proccess method is weird, but use it first... for temporarily...
  dat$date %>% unique
  dat$date <- dat$date %>% substr(., 1, 5)
  dat$date <- as.integer(dat$date)
  dat$date <- dat$date - 1
  ## 10600 => 10512
  dat$date[grepl("00$", dat$date)] <- dat$date[grepl("00$", dat$date)] - 100 + 12
  gc()
  
  #不動產經紀人 => 不動產經紀人/營業員
  dat$職務小類名稱[dat$職務小類名稱=="不動產經紀人"] <- "不動產經紀人/營業員"
  
  tmp <- JobTrend_unitHistory(dat, "職務小類名稱", "area.work")
  #####################################
  #Top25 Job demand, grouped by area...
  #####################################
  #if(exists("totalTMP")){
  #  totalTMP <- rbind(totalTMP,tmp)
  #}else{
  #  totalTMP <- tmp
  #}
  gc()
  return(tmp)
})
datHistory <- do.call(rbind, datHistory)
datHistory <- datHistory[,.(N=sum(N)), by = .(date, area.work, 職務小類名稱)]
datHistory <- FillMissingJobRow(datHistory, "職務小類名稱", "area.work")
datHistory[, percentage:=N/sum(N), by=c("date", "area.work")]
datHistory$date %>% unique
datHistory$date <- as.numeric(datHistory$date)
datHistory$percentage[is.nan(datHistory$percentage)] <- 0
## Get standard hitorical data...
## Export
filename <- "AreaJobDemand"
JobTrend_standard(datHistory, "職務小類名稱", "area.work", filename)


write.csv(datHistoryStandard, paste0("output\\per.month\\", format(Sys.time(), "%Y%m%d_"), filename, "_History.csv"), row.names = FALSE)
## 

if(F){
  for(i_file in 1:length(files)){
    ##import file
    file.name <- files[i_file]
    dat <- fread(file.name)
    date <- unlist(strsplit(file.name, "/"))[length(unlist(strsplit(file.name, "/")))] %>% gsub("[A-z.]", "", .) ##%>% substr(., 1, 7)
    dat <- cbind(dat, date)
    
    if(any(is.na(names(dat) %>% iconv(., "UTF-8")))){
      names(dat) <- names(dat)
    }else{
      names(dat) <- names(dat) %>% iconv(., "UTF-8")
      cols <- 1:length(names(dat))
      dat[, (cols) := lapply(.SD, function(x) ifelse(is.na(x %>% iconv(., "UTF-8")), x, x %>% iconv(., "UTF-8")))]
    }
    
    gc()
    
    ## view
    dim(dat)
    names(dat)
    setDT(dat)
    
    #dat <- dat
    ##Import with encoding will face error...
    ##File's content itself is garbled...
    #names(dat) <- names(dat) %>% iconv(., "UTF-8")
    #cols <- 1:length(names(dat))
    
    #dat[, (cols) := lapply(.SD, function(x) iconv(x, "UTF-8"))]
    #dat$職缺狀態 %>% table
    #dat[, (cols) := lapply(.SD, function(x) ifelse(is.na(x %>% iconv(., "UTF-8")), x, x %>% iconv(., "UTF-8")))]
    #dat$職缺狀態 %>% table
    
    if(length(names(dat)) != length(unique(names(dat))))
      dat <- dat[, unique(names(dat)), with=F]
    dim(dat)
    names(dat)
    
    dat <- dat[ 實習職缺=="N" ]
    dat$職缺屬性 %>% table
    dat <- dat[ 職缺屬性=="全職" | 職缺屬性=="中高階" ]
    
    ## Working area transfer
    dat$area.work <- ""
    North <- c("台北", "新北", "基隆", "桃園", "新竹")
    Mid   <- c("苗栗", "台中", "彰化", "南投", "雲林")
    South <- c("嘉義", "台南", "高雄", "屏東")
    East  <- c("宜蘭", "花蓮", "台東", "澎湖", "金門", "連江")
    Out   <- c(North, Mid, South, East)
    dat[ substr(dat[,工作地點],1,2) %in% North, area.work:="北部地區"]
    dat[ substr(dat[,工作地點],1,2) %in% Mid, area.work:="中部地區"]
    dat[ substr(dat[,工作地點],1,2) %in% South, area.work:="南部地區"]
    dat[ substr(dat[,工作地點],1,2) %in% East, area.work:="東部與離島地區"]
    dat[ !(substr(dat[,工作地點],1,2) %in% Out) , area.work:="非台灣地區"]
    
    unique(dat$area.work)
    ##Check areas which are outside Taiwan
    dat[ area.work=="非台灣地區" , 工作地點] %>% substr(., 1, 3) %>% table
    
    ## This proccess method is weird, but use it first... for temporarily...
    dat$date %>% unique
    dat$date <- dat$date %>% substr(., 1, 5)
    dat$date <- as.integer(dat$date)
    dat$date <- dat$date - 1
    ## 10600 => 10512
    dat$date[grepl("00$", dat$date)] <- dat$date[grepl("00$", dat$date)] - 100 + 12
    gc()
    
    #不動產經紀人 => 不動產經紀人/營業員
    dat$職務小類名稱[dat$職務小類名稱=="不動產經紀人"] <- "不動產經紀人/營業員"
    
    tmp <- JobTrend_unitHistory(dat, "職務小類名稱", "area.work", "AreaJobDemand", i, length(files))
    #####################################
    #Top25 Job demand, grouped by area...
    #####################################
    #if(exists("totalTMP")){
    #  totalTMP <- rbind(totalTMP,tmp)
    #}else{
    #  totalTMP <- tmp
    #}
    gc()
  }
}



gc()
#####################################
#Top25 Job demand, grouped by area...
#####################################
JobTrend(dat, "職務小類名稱", "area.work", "AreaJobDemand")

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
## 10600 => 10512
totalResumeData$date[grepl("00$", totalResumeData$date)] <- totalResumeData$date[grepl("00$", totalResumeData$date)] - 100 + 12
gc()

##Grouped by Department
dpt.match <- read.csv("1111學群學門學類-20160617-1.csv", stringsAsFactors=F)

for(i in 1:ncol(dpt.match)){
  dpt.match[,i] <- gsub("[0-9+_]", "", dpt.match[,i])
}
totalResumeData$dpt <- ""
for(i in 1:nrow(dpt.match)){
  totalResumeData$dpt[which(totalResumeData$最高學歷_科系小類名稱==dpt.match[i,3])] <- dpt.match[i,2]
}

##Encoding problem
#EncodingCheck(dpt.match)
EncodingCheck(totalResumeData)

setDT(totalResumeData)
totalResumeData[, names(totalResumeData) := lapply(.SD, function(x) {if (is.character(x)) Encoding(x) <- "unknown"; x})]
for(i in 1:ncol(totalResumeData)){
  if(sum(is.na(totalResumeData[[i]] %>% iconv("UTF-8")))<(nrow(totalResumeData)/2)){
    #print(i)
    totalResumeData[[i]] <- totalResumeData[[i]] %>% iconv("UTF-8")
  }
}
#[1] 1  4  7  9 11  13 15  18  21  26

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
