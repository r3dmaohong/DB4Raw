#' @title Job Industry Trend Analysis
#' @description Analyze historical changes of data, and get the latest trend.
#' @note Solving Ram Problem: Read and analyze one file and another, 
#'       then combine the result. 

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

#' Define area
North <- c("�x�_", "�s�_", "��", "���", "�s��")
Mid   <- c("�]��", "�x��", "����", "�n��", "���L")
South <- c("�Ÿq", "�x�n", "����", "�̪F")
East  <- c("�y��", "�Ὤ", "�x�F", "���", "����", "�s��")
Out   <- c(North, Mid, South, East)

#' Demand
#' Get all files' names.
files <- list.files(file.path("per.month", "�D�~"), full.names = TRUE)
files <- files[grepl("csv", files)]
files <- sort(files)

#' Function for Trend analysis...
source("rscript/function/JobTrendFunc_MRC.R", print.eval  = TRUE)

#' Read and analyze all the file one by one
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
  setDT(dat)
  
  #' Those which are not closed.
  dat <- dat[¾�������ɶ�=="",]
  #dat <- dat
  ##Import with encoding will face error...
  ##File's content itself is garbled...
  #names(dat) <- names(dat) %>% iconv(., "UTF-8")
  #cols <- 1:length(names(dat))
  
  #dat[, (cols) := lapply(.SD, function(x) iconv(x, "UTF-8"))]
  #dat$¾�ʪ��A %>% table
  #dat[, (cols) := lapply(.SD, function(x) ifelse(is.na(x %>% iconv(., "UTF-8")), x, x %>% iconv(., "UTF-8")))]
  #dat$¾�ʪ��A %>% table
  
  if(length(names(dat)) != length(unique(names(dat))))
    dat <- dat[, unique(names(dat)), with=F]
  
  dat <- dat[ ���¾��=="N" ]
  dat$¾���ݩ� %>% table
  dat <- dat[ ¾���ݩ�=="��¾" | ¾���ݩ�=="������" ]
  
  #' Transfer working area
  dat$area.work <- ""
  dat[ substr(dat[,�u�@�a�I],1,2) %in% North, area.work:="�_���a��"]
  dat[ substr(dat[,�u�@�a�I],1,2) %in% Mid, area.work:="�����a��"]
  dat[ substr(dat[,�u�@�a�I],1,2) %in% South, area.work:="�n���a��"]
  dat[ substr(dat[,�u�@�a�I],1,2) %in% East, area.work:="�F���P���q�a��"]
  dat[ !(substr(dat[,�u�@�a�I],1,2) %in% Out) , area.work:="�D�x�W�a��"]
  
  #unique(dat$area.work)
  
  #' Check areas which are outside Taiwan
  dat[ area.work=="�D�x�W�a��" , �u�@�a�I] %>% substr(., 1, 3) %>% table
  
  #' Date processing
  #' This proccess method is weird, but use it first... for temporarily...
  dat$date %>% unique
  dat$date <- dat$date %>% substr(., 1, 5)
  dat$date <- as.integer(dat$date)
  dat$date <- dat$date - 1
  ## 10600 => 10512
  dat$date[grepl("00$", dat$date)] <- dat$date[grepl("00$", dat$date)] - 100 + 12
  gc()
  
  #' Old name to New name
  dat$¾�Ȥp���W��[dat$¾�Ȥp���W��=="���ʲ��g���H"] <- "���ʲ��g���H/��~��"
  
  #
  tmp <- JobTrend_unitHistory(dat, "¾�Ȥp���W��", "area.work")
  gc()
  return(tmp)
})
datHistory <- do.call(rbind, datHistory)
datHistory <- datHistory[,.(N=sum(N)), by = .(date, area.work, ¾�Ȥp���W��)]
datHistory <- FillMissingJobRow(datHistory, "¾�Ȥp���W��", "area.work")
datHistory[, percentage:=N/sum(N), by=c("date", "area.work")]
datHistory$date %>% unique
datHistory$date <- as.numeric(datHistory$date)
datHistory$percentage[is.nan(datHistory$percentage)] <- 0
## Get standard hitorical data...
## Export
filename <- "AreaJobDemand"
JobTrend_standard(datHistory, "¾�Ȥp���W��", "area.work", filename)

## 
###################################################
###################################################
##############    I am a divider..   ##############
###################################################
###################################################

#' Resume: Supply
#' Import all files.
files <- list.files(file.path("per.month", "�D¾"), full.names = TRUE)
files <- files[grepl(".csv", files, fixed=TRUE)]

files.lists <- pblapply(files,function(file.name){
  file.list <- fread(file.name)
  date <- unlist(strsplit(file.name, "/"))[length(unlist(strsplit(file.name, "/")))] %>% gsub("[A-z.]", "", .) ##%>% substr(., 1, 7)
  file.list$date <- date
  #file.list <- cbind(file.list, date)
  return(file.list)
})

#' Combine all the lists into data frame.
totalResumeData <- do.call(rbind,files.lists)
rm(files.lists,files)
dim(totalResumeData)
str(totalResumeData)
names(totalResumeData)

totalResumeData$�Ʊ�u�@�ʽ� %>% table
totalResumeData <- totalResumeData[�Ʊ�u�@�ʽ�=="��¾" | �Ʊ�u�@�ʽ�=="������",]

#' Expect working area transferation
totalResumeData$area.work <- ""
totalResumeData[ substr(totalResumeData[,�Ʊ�W�Z�a�ϦW��],1,2) %in% North, area.work:="�_���a��"]
totalResumeData[ substr(totalResumeData[,�Ʊ�W�Z�a�ϦW��],1,2) %in% Mid, area.work:="�����a��"]
totalResumeData[ substr(totalResumeData[,�Ʊ�W�Z�a�ϦW��],1,2) %in% South, area.work:="�n���a��"]
totalResumeData[ substr(totalResumeData[,�Ʊ�W�Z�a�ϦW��],1,2) %in% East, area.work:="�F���P���q�a��"]
totalResumeData[ !(substr(totalResumeData[,�Ʊ�W�Z�a�ϦW��],1,2) %in% Out) , area.work:="�D�x�W�a��"]

#' Living area transferation
totalResumeData$area.live <- ""
totalResumeData[ substr(totalResumeData[,�~���a��],1,2) %in% North, area.live:="�_���a��"]
totalResumeData[ substr(totalResumeData[,�~���a��],1,2) %in% Mid, area.live:="�����a��"]
totalResumeData[ substr(totalResumeData[,�~���a��],1,2) %in% South, area.live:="�n���a��"]
totalResumeData[ substr(totalResumeData[,�~���a��],1,2) %in% East, area.live:="�F���P���q�a��"]
totalResumeData[ !(substr(totalResumeData[,�~���a��],1,2) %in% Out) , area.live:="�D�x�W�a��"]

unique(totalResumeData$area.work)
#' Check areas which are outside Taiwan
totalResumeData[ area.work=="�D�x�W�a��" , �Ʊ�W�Z�a�ϦW��] %>% substr(., 1, 3) %>% table

#' Country
totalResumeData$country.work <- totalResumeData$�Ʊ�W�Z�a�ϦW�� %>% substr(., 1, 3)
totalResumeData[ !(substr(totalResumeData[, country.work],1,2) %in% Out) , country.work:="�D�x�W�a��"]

totalResumeData$country.live <- totalResumeData$�~���a�� %>% substr(., 1, 3)
totalResumeData[ !(substr(totalResumeData[, country.live],1,2) %in% Out) , country.live:="�D�x�W�a��"]

totalResumeData$date %>% unique
totalResumeData$date <- totalResumeData$date %>% substr(., 1, 5)
totalResumeData$date <- as.integer(totalResumeData$date)
totalResumeData$date <- totalResumeData$date - 1
#' 10600 => 10512
totalResumeData$date[grepl("00$", totalResumeData$date)] <- totalResumeData$date[grepl("00$", totalResumeData$date)] - 100 + 12
gc()

#' Grouped by Department
dpt.match <- read.csv("1111�Ǹs�Ǫ�����-20160617-1.csv", stringsAsFactors=F)

for(i in 1:ncol(dpt.match)){
  dpt.match[,i] <- gsub("[0-9+_]", "", dpt.match[,i])
}
totalResumeData$dpt <- ""
for(i in 1:nrow(dpt.match)){
  totalResumeData$dpt[which(totalResumeData$�̰��Ǿ�_��t�p���W��==dpt.match[i,3])] <- dpt.match[i,2]
}

#'Encoding problem
EncodingCheck(totalResumeData)

setDT(totalResumeData)
totalResumeData[, names(totalResumeData) := lapply(.SD, function(x) {if (is.character(x)) Encoding(x) <- "unknown"; x})]
for(i in 1:ncol(totalResumeData)){
  if(sum(is.na(totalResumeData[[i]] %>% iconv("UTF-8")))<(nrow(totalResumeData)/2)){
    #print(i)
    totalResumeData[[i]] <- totalResumeData[[i]] %>% iconv("UTF-8")
  }
}

########################################
###Top25 Job wanted, grouped by area...
########################################
JobTrend(totalResumeData, "�Ʊ�¾�Ȥp���W��", "area.work", "AreaJobWanted")

########################################
###Top25 Job wanted, grouped by dpt...
########################################
JobTrend(totalResumeData, "�Ʊ�¾�Ȥp���W��", "dpt", "DepartmentJobWanted")

########################################
###Top10 working area, grouped by dpt...
########################################
JobTrend(totalResumeData, "country.work", "dpt", "DepartmentExpectArea", 10)

########################################
###Top10 industry, grouped by dpt...
########################################
JobTrend(totalResumeData, "�Ʊ沣�~�j���W��", "dpt", "DepartmentExpectIndustry", 10)

########################################
###Top10 working area, grouped by living area...
########################################
JobTrend(totalResumeData, "country.work", "area.live", "LivePExpectWorkP", 10)