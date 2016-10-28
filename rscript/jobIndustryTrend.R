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
  
  return(file.list)
})

##Combine all the lists into data frame.
total.data <- do.call(rbind, files.lists) # do.call(rbind.fill,files.lists)
rm(files.lists,files)
dim(total.data)
str(total.data)
names(total.data)
setDT(total.data)

##Import with encoding will face error...
##File's content itself is garbled...
names(total.data) <- names(total.data) %>% iconv(., "UTF-8")
cols <- 1:length(names(total.data))
total.data[, (cols) := lapply(.SD, function(x) iconv(x, "UTF-8"))]
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

###########
##Old Ver.
###########
if(F){
  ## Top 25 Demanded Jobs
  if(T){
    #top25_DemandJob <- total.data %>% filter(., 職務小類名稱!="工讀生") %>% group_by(date, area.work, 職務小類名稱) %>% 
    #  summarize(.,Freq=n()) %>% arrange(.,date, area.work, -Freq) 
    top25_DemandJob <- total.data[ 職務小類名稱!="工讀生", .N, by = .(date, area.work, 職務小類名稱)]
    top25_DemandJob <- top25_DemandJob[order(date, area.work, -N)]
    top25_DemandJob[, percentage:=N/sum(N), by=.(date, area.work)]
    #top25_DemandJob <- top25_DemandJob %>% group_by(., date, area.work) %>% mutate(., percentage=Freq/sum(Freq))
    unique(top25_DemandJob$area.work)
    
    ##Backup 
    totalDemandJob <- top25_DemandJob
    
    ##change freq to a new standard
    standard.top25_DemandJob <- top25_DemandJob[date==top25_DemandJob$date[1]]
    #standard.top25_DemandJob <- top25_DemandJob %>% filter(., date==top25_DemandJob$date[1])
    top25_DemandJob <- top25_DemandJob[, head(.SD, 25), by=.(date, area.work)]
    #top25_DemandJob <- top25_DemandJob %>% group_by(date, area.work) %>% top_n(n = 25)
    top25_DemandJob$Freq <- sapply(1:nrow(top25_DemandJob), function(x){
      #top25_DemandJob$N[x] - standard.top25_DemandJob$N[which(standard.top25_DemandJob$職務小類名稱==top25_DemandJob$職務小類名稱[x] & standard.top25_DemandJob$area.work==top25_DemandJob$area.work[x])]
      if(standard.top25_DemandJob$N[which(standard.top25_DemandJob$職務小類名稱==top25_DemandJob$職務小類名稱[x] & standard.top25_DemandJob$area.work==top25_DemandJob$area.work[x])] %>% toString != ""){
        return(top25_DemandJob$N[x] - standard.top25_DemandJob$N[which(standard.top25_DemandJob$職務小類名稱==top25_DemandJob$職務小類名稱[x] & standard.top25_DemandJob$area.work==top25_DemandJob$area.work[x])])
      }
      ##Add missing standard
      standard.top25_DemandJob <<- rbind(standard.top25_DemandJob, data.table(date=standard.top25_DemandJob$date[1], area.work=top25_DemandJob$area.work[x], 職務小類名稱=top25_DemandJob$職務小類名稱[x], N=0, percentage=0))
      return(top25_DemandJob$N[x])
    })
    
    ##Set ranking
    top25_DemandJob$rank <-  0
    countdown <- 0
    for(i in 1:nrow(top25_DemandJob)){
      if(i==1){
        top25_DemandJob$rank[i] <- 1
      }else{
        if(top25_DemandJob$area.work[i]==top25_DemandJob$area.work[i-1]){
          if(top25_DemandJob$percentage [i]==top25_DemandJob$percentage [i-1]){
            top25_DemandJob$rank[i] <- top25_DemandJob$rank[i-1]
            countdown <- countdown + 1
          }else{
            top25_DemandJob$rank[i] <- top25_DemandJob$rank[i-1] + 1 + countdown
            countdown <- 0
          }      
        }else{
          top25_DemandJob$rank[i] <- 1
          countdown <- 0
        }
      } 
    }
    
    top25_DemandJob$percentage <- paste0(format(round(top25_DemandJob$percentage*100,2), nsmall=2), "%")
    #Generate index
    top25_DemandJob[,index:=paste(area.work, 職務小類名稱, sep="_")]
    
    ##Historical changes...
    totalDemandJob$Freq <- sapply(1:nrow(totalDemandJob), function(x){
      if(standard.top25_DemandJob$N[which(standard.top25_DemandJob$職務小類名稱==totalDemandJob$職務小類名稱[x] & standard.top25_DemandJob$area.work==totalDemandJob$area.work[x])] %>% toString != ""){
        return(totalDemandJob$N[x] - standard.top25_DemandJob$N[which(standard.top25_DemandJob$職務小類名稱==totalDemandJob$職務小類名稱[x] & standard.top25_DemandJob$area.work==totalDemandJob$area.work[x])])
      }
      
      if(standard.top25_DemandJob$N[which(standard.top25_DemandJob$職務小類名稱==totalDemandJob$職務小類名稱[x] & standard.top25_DemandJob$area.work==totalDemandJob$area.work[x])] %>% toString != ""){
        return(totalDemandJob$N[x] - standard.top25_DemandJob$N[which(standard.top25_DemandJob$職務小類名稱==totalDemandJob$職務小類名稱[x] & standard.top25_DemandJob$area.work==totalDemandJob$area.work[x])])
      }
      ##Add missing standard
      standard.top25_DemandJob <<- rbind(standard.top25_DemandJob, data.table(date=standard.top25_DemandJob$date[1], area.work=totalDemandJob$area.work[x], 職務小類名稱=totalDemandJob$職務小類名稱[x], N=0, percentage=0))
      return(top25_DemandJob$N[x])
    })
    totalDemandJob[,index:=paste(area.work, 職務小類名稱, sep="_")]
    top25_DemandJob <- top25_DemandJob[date==max(date)]
    ##Rank, Area, Job, Percentage, Freq
    ##Keep the latest data
    OutputDemandJob <- top25_DemandJob[, .(rank, area.work, 職務小類名稱, percentage, Freq)]
    ##Output of %in% is wrong
    totalDemandJob <- totalDemandJob[index %in% top25_DemandJob$index, ]
    
    ##Check
    tmp <- totalDemandJob[top25_DemandJob$index %in% index, index] %>% table %>% data.frame
    stopifnot(tmp$Freq %>% unique %>% length ==1)
    #tmp$.[tmp$Freq==min(tmp$Freq %>% unique)] %>% unique
    ##不動產經紀人 => 不動產經紀人/營業員
    totalDemandJob <- totalDemandJob[, .(date, area.work, 職務小類名稱, Freq)]
    #apply(totalDemandJob, 2, class)
    
    write.csv(OutputDemandJob, paste0("output\\per.month\\", format(Sys.time(), "%Y%m%d"), "_OutputDemandJob.csv"), row.names=F)
    write.csv(totalDemandJob, paste0("output\\per.month\\", format(Sys.time(), "%Y%m%d"), "_totalDemandJob.csv"), row.names=F)
  }
  
}


###################################################
###################################################
##############    I am a divider..   ##############
###################################################
###################################################

##Resume: Supply
##Import all files.
if(F){
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
  
  ##Working area transferation
  totalResumeData$area.work <- ""
  totalResumeData[ substr(totalResumeData[,希望上班地區名稱],1,2) %in% North, area.work:="北部地區"]
  totalResumeData[ substr(totalResumeData[,希望上班地區名稱],1,2) %in% Mid, area.work:="中部地區"]
  totalResumeData[ substr(totalResumeData[,希望上班地區名稱],1,2) %in% South, area.work:="南部地區"]
  totalResumeData[ substr(totalResumeData[,希望上班地區名稱],1,2) %in% East, area.work:="東部與離島地區"]
  totalResumeData[ !(substr(totalResumeData[,希望上班地區名稱],1,2) %in% Out) , area.work:="非台灣地區"]
  
  unique(totalResumeData$area.work)
  ## Check areas which are outside Taiwan
  totalResumeData[ area.work=="非台灣地區" , 希望上班地區名稱] %>% substr(., 1, 3) %>% table
  
  totalResumeData$date %>% unique
  totalResumeData$date <- totalResumeData$date %>% substr(., 1, 5)
  totalResumeData$date <- as.integer(totalResumeData$date)
  totalResumeData$date <- totalResumeData$date - 1
  
}
if(F){
  ###########################
  ##Top 10 most beloved jobs
  ###########################
  ## Top 10 most beloved jobs
  ## Top 25 Demanded Jobs
  top10beloved.job <- totalResumeData[希望職務小類名稱!="工讀生", .N, by=.(date, area.work, 希望職務小類名稱)]
  top10beloved.job <- top10beloved.job[order(date, area.work, -N),]
  top10beloved.job[, percentage:=N/sum(N), by=.(date, area.work)]
  unique(top10beloved.job$area.work)
  
  ##Backup
  totalBelovedJob <- top10beloved.job
  
  ##Change freq to a new standard
  standard.top10beloved.job <- top10beloved.job[date==top10beloved.job$date[1]]
  top10beloved.job <- top10beloved.job[, head(.SD, 10), by=.(date, area.work)]
  top10beloved.job$Freq <- sapply(1:nrow(top10beloved.job), function(x){
    if(standard.top10beloved.job$N[which(standard.top10beloved.job$希望職務小類名稱==top10beloved.job$希望職務小類名稱[x] & standard.top10beloved.job$area.work==top10beloved.job$area.work[x])] %>% toString != ""){
      return(top10beloved.job$N[x] - standard.top10beloved.job$N[which(standard.top10beloved.job$希望職務小類名稱==top10beloved.job$希望職務小類名稱[x] & standard.top10beloved.job$area.work==top10beloved.job$area.work[x])])
    }
    ##Add missing standard
    standard.top10beloved.job <<- rbind(standard.top10beloved.job, data.table(date=standard.top10beloved.job$date[1], area.work=top10beloved.job$area.work[x], 希望職務小類名稱=top10beloved.job$希望職務小類名稱[x], N=0, percentage=0))
    return(top10beloved.job$N[x])
  })
  
  ##Set ranking
  top10beloved.job$rank <-  NA
  countdown <- 0
  for(i in 1:nrow(top10beloved.job)){
    if(i==1){
      top10beloved.job$rank[i] <- 1
    }else{
      if(top10beloved.job$area.work[i]==top10beloved.job$area.work[i-1]){
        if(top10beloved.job$percentage[i]==top10beloved.job$percentage[i-1]){
          top10beloved.job$rank[i] <- top10beloved.job$rank[i-1]
          countdown <- countdown + 1
        }else{
          top10beloved.job$rank[i] <- top10beloved.job$rank[i-1] + 1 + countdown
          countdown <- 0
        }      
      }else{
        top10beloved.job$rank[i] <- 1
        countdown <- 0
      }
    } 
  }
  
  write.csv(top10beloved.job, paste0("output\\per.month\\",gsub("[:punct:]","_", Sys.time()),"_top10beloved.job.csv"), row.names=F)
  
}
if(F){###################
      ##department vs job
      ###################
      dpt.match <- read.csv("1111學群學門學類-20160617-1.csv", stringsAsFactors=F)
      for(i in 1:ncol(dpt.match)){
        dpt.match[,i] <- gsub("[0-9+_]", "", dpt.match[,i])
      }
      totalResumeData$dpt <- ""
      for(i in 1:nrow(dpt.match)){
        totalResumeData$dpt[which(totalResumeData$最高學歷_科系小類名稱==dpt.match[i,3])] <- dpt.match[i,2]
      }
      
      dpt.top10beloved.job <- totalResumeData %>% filter(., 希望職務小類名稱!="工讀生") %>% group_by(date, dpt, 希望職務小類名稱) %>% 
        summarize(.,Freq=n()) %>% arrange(.,date, -Freq) 
      dpt.top10beloved.job <- dpt.top10beloved.job %>% filter(., dpt!="")
      dpt.top10beloved.job <- dpt.top10beloved.job %>% group_by(., date, dpt) %>% mutate(., percentage=Freq/sum(Freq))
      unique(dpt.top10beloved.job$dpt)
      
      ##change freq to a new standard
      standard.dpt.top10beloved.job <- dpt.top10beloved.job %>% filter(., date==dpt.top10beloved.job$date[1])
      dpt.top10beloved.job <- dpt.top10beloved.job %>% group_by(date,dpt) %>% top_n(n = 10)
      dpt.top10beloved.job$Freq <- sapply(1:nrow(dpt.top10beloved.job), function(x){
        if(standard.dpt.top10beloved.job$Freq[which(standard.dpt.top10beloved.job$希望職務小類名稱==dpt.top10beloved.job$希望職務小類名稱[x] & standard.dpt.top10beloved.job$dpt==dpt.top10beloved.job$dpt[x])] %>% toString !="")
          return(dpt.top10beloved.job$Freq[x] - standard.dpt.top10beloved.job$Freq[which(standard.dpt.top10beloved.job$希望職務小類名稱==dpt.top10beloved.job$希望職務小類名稱[x] & standard.dpt.top10beloved.job$dpt==dpt.top10beloved.job$dpt[x])])
        return(dpt.top10beloved.job$Freq[x])
        
      })
      
      ##Set ranking
      dpt.top10beloved.job$rank <-  NA
      countdown <- 0
      for(i in 1:nrow(dpt.top10beloved.job)){
        if(i==1){
          dpt.top10beloved.job$rank[i] <- 1
        }else{
          if(dpt.top10beloved.job$dpt[i]==dpt.top10beloved.job$dpt[i-1]){
            if(dpt.top10beloved.job$percentage[i]==dpt.top10beloved.job$percentage[i-1]){
              dpt.top10beloved.job$rank[i] <- dpt.top10beloved.job$rank[i-1]
              countdown <- countdown + 1
            }else{
              dpt.top10beloved.job$rank[i] <- dpt.top10beloved.job$rank[i-1] + 1 + countdown
              countdown <- 0
            }      
          }else{
            dpt.top10beloved.job$rank[i] <- 1
            countdown <- 0
          }
        } 
      }
      
      write.csv(dpt.top10beloved.job, paste0("output\\per.month\\",gsub("[:punct:]","_", Sys.time()),"_dpt.top10beloved.job.csv"), row.names=F)
}




##
##地區是否要再做處理
top10beloved.area <- totalResumeData %>% filter(., 希望職務小類名稱!="工讀生") %>% mutate(., 希望上班地區名稱=substr(希望上班地區名稱, 1, 3)) %>% group_by(date, 希望上班地區名稱) %>% 
  summarize(.,Freq=n()) %>% arrange(.,date, -Freq) 
top10beloved.area <- top10beloved.area %>% group_by(., date) %>% mutate(., percentage=Freq/sum(Freq))


##change freq to a new standard
standard.top10beloved.area <- top10beloved.area %>% filter(., date==top10beloved.area$date[1])
top10beloved.area <- top10beloved.area %>% group_by(date) %>% top_n(n = 10)
top10beloved.area$Freq <- sapply(1:nrow(top10beloved.area), function(x){
  top10beloved.area$Freq[x] - standard.top10beloved.area$Freq[which(standard.top10beloved.area$希望上班地區名稱==top10beloved.area$希望上班地區名稱[x])]
})

##Set ranking
top10beloved.area$rank <-  NA
countdown <- 0
for(i in 1:nrow(top10beloved.area)){
  if(i==1){
    top10beloved.area$rank[i] <- 1
  }else{
    if(top10beloved.area$date[i]==top10beloved.area$date[i-1]){
      if(top10beloved.area$percentage[i]==top10beloved.area$percentage[i-1]){
        top10beloved.area$rank[i] <- top10beloved.area$rank[i-1]
        countdown <- countdown + 1
      }else{
        top10beloved.area$rank[i] <- top10beloved.area$rank[i-1] + 1 + countdown
        countdown <- 0
      }      
    }else{
      top10beloved.area$rank[i] <- 1
      countdown <- 0
    }
  } 
}

write.csv(top10beloved.area, paste0("output\\per.month\\",gsub("[:punct:]","_", Sys.time()),"_top10beloved.area.csv"), row.names=F)


##dpt.top10beloved.area
##地區是否要再做處理
dpt.top10beloved.area <- totalResumeData %>% filter(., 希望職務小類名稱!="工讀生") %>% mutate(., 希望上班地區名稱=substr(希望上班地區名稱, 1, 3)) %>% group_by(date, dpt, 希望上班地區名稱) %>% 
  summarize(.,Freq=n()) %>% arrange(.,date, -Freq) 
dpt.top10beloved.area <- dpt.top10beloved.area %>% filter(., dpt!="")
dpt.top10beloved.area <- dpt.top10beloved.area %>% group_by(., date, dpt) %>% mutate(., percentage=Freq/sum(Freq))
unique(dpt.top10beloved.area$dpt)

##change freq to a new standard
standard.dpt.top10beloved.area <- dpt.top10beloved.area %>% filter(., date==dpt.top10beloved.area$date[1])
dpt.top10beloved.area <- dpt.top10beloved.area %>% group_by(date, dpt) %>% top_n(n = 10)
dpt.top10beloved.area$Freq <- sapply(1:nrow(dpt.top10beloved.area), function(x){
  if(standard.dpt.top10beloved.area$Freq[which(standard.dpt.top10beloved.area$希望上班地區名稱==dpt.top10beloved.area$希望上班地區名稱[x] & standard.dpt.top10beloved.area$dpt==dpt.top10beloved.area$dpt[x])] %>% toString !="")
    return(dpt.top10beloved.area$Freq[x] - standard.dpt.top10beloved.area$Freq[which(standard.dpt.top10beloved.area$希望上班地區名稱==dpt.top10beloved.area$希望上班地區名稱[x] & standard.dpt.top10beloved.area$dpt==dpt.top10beloved.area$dpt[x])])
  return(dpt.top10beloved.area$Freq[x])
})

##Set ranking
dpt.top10beloved.area$rank <-  NA
countdown <- 0
for(i in 1:nrow(dpt.top10beloved.area)){
  if(i==1){
    dpt.top10beloved.area$rank[i] <- 1
  }else{
    if(dpt.top10beloved.area$dpt[i]==dpt.top10beloved.area$dpt[i-1]){
      if(dpt.top10beloved.area$percentage[i]==dpt.top10beloved.area$percentage[i-1]){
        dpt.top10beloved.area$rank[i] <- dpt.top10beloved.area$rank[i-1]
        countdown <- countdown + 1
      }else{
        dpt.top10beloved.area$rank[i] <- dpt.top10beloved.area$rank[i-1] + 1 + countdown
        countdown <- 0
      }      
    }else{
      dpt.top10beloved.area$rank[i] <- 1
      countdown <- 0
    }
  } 
}

write.csv(dpt.top10beloved.area, paste0("output\\per.month\\",gsub("[:punct:]","_", Sys.time()),"_dpt.top10beloved.area.csv"), row.names=F)


#################
#################
##quan mo
#################
#################

# Demand
##Get all files' names.
files <- list.files(file.path('per.month','求才'),full.names = TRUE)
files <- files[grepl("csv", files)]
##Import files
file.list <- fread(files[1])
tmp_names <- names(file.list)
files.lists <- pblapply(files,function(file.name){
  file.list <- fread(file.name)
  #date <- unlist(strsplit(file.name, "/"))[length(unlist(strsplit(file.name, "/")))] %>% gsub("[A-z.]", "", .) ##%>% substr(., 1, 7)
  #file.list <- cbind(file.list, date)
  names(file.list) <- tmp_names
  return(file.list)
})

for(i in 1:length(files.lists)){
  print(ncol(files.lists[[i]]))
}

##Combine all the lists into data frame.
totalResumeData <- do.call(rbind.fill,files.lists)
setDF(totalResumeData)
rm(files.lists,files)
dim(totalResumeData)
str(totalResumeData)
names(totalResumeData)
totalResumeData <- totalResumeData[,unique(names(totalResumeData))]
##
##資料時間 地理區域  新_1111行業中類名稱
## Top 10 most beloved jobs
demand_job_data <- totalResumeData %>% select(資料時間, ID1111職務小類名稱, 地理區域, 新_1111行業中類名稱)

## Working area transfer
demand_job_data$area.work <- NA
"北部地區"       -> demand_job_data[which(substr(demand_job_data[,"地理區域"],1,3) %in% c("台北市","新北市","基隆市","桃園市","桃園縣","新竹市","新竹縣")),"area.work"]
"中部地區"       -> demand_job_data[which(substr(demand_job_data[,"地理區域"],1,3) %in% c("苗栗縣","台中市","彰化縣","南投縣","雲林縣")),"area.work"]
"南部地區"       -> demand_job_data[which(substr(demand_job_data[,"地理區域"],1,3) %in% c("嘉義縣","嘉義市","台南市","高雄市","屏東縣")),"area.work"]
"東部與離島地區" -> demand_job_data[which(substr(demand_job_data[,"地理區域"],1,3) %in% c("宜蘭縣","花蓮縣","台東縣","澎湖縣","金門縣","連江縣")),"area.work"]
"非台灣地區"     -> demand_job_data[which(substr(demand_job_data[,"地理區域"],1,3) %in% c("台北市","新北市","基隆市","桃園市","桃園縣","新竹市","新竹縣",
                                                                           "苗栗縣","台中市","彰化縣","南投縣","雲林縣", 
                                                                           "嘉義縣","嘉義市","台南市","高雄市","屏東縣", 
                                                                           "宜蘭縣","花蓮縣","台東縣","澎湖縣","金門縣","連江縣")==FALSE),"area.work"]
## Check areas which are outside Taiwan
demand_job_data[which(substr(demand_job_data[,"地理區域"],1,3) %in% c("台北市","新北市","基隆市","桃園市","桃園縣","新竹市","新竹縣",
                                                            "苗栗縣","台中市","彰化縣","南投縣","雲林縣", 
                                                            "嘉義縣","嘉義市","台南市","高雄市","屏東縣", 
                                                            "宜蘭縣","花蓮縣","台東縣","澎湖縣","金門縣","連江縣")==FALSE),"地理區域"] %>% table

## Top 25 Demanded Jobs
demand_job_data <- demand_job_data[,c(1, 2, 4, 5)]
names(demand_job_data) <- c("date", "job", "mid_area", "area.work")

area_top10beloved.job <- demand_job_data %>% select(date, area.work, job) %>% 
  filter(., job!="工讀生") %>% group_by(date, area.work, job) %>%
  dplyr::summarize(.,Freq=n()) %>% arrange(.,date, area.work, -Freq) 
area_top10beloved.job <- area_top10beloved.job %>% group_by(., date, area.work) %>% mutate(., percentage=Freq/sum(Freq))
unique(area_top10beloved.job$area.work)

##change freq to a new standard
standard.area_top10beloved.job <- area_top10beloved.job %>% filter(., date==area_top10beloved.job$date[1])
area_top10beloved.job$Freq     <- sapply(1:nrow(area_top10beloved.job), function(x){
  standard.Freq <- standard.area_top10beloved.job$Freq[which(standard.area_top10beloved.job$job==area_top10beloved.job$job[x] & standard.area_top10beloved.job$area.work==area_top10beloved.job$area.work[x])]
  if(toString(standard.Freq)==""){
    return(0)
  }else{
    return(area_top10beloved.job$Freq[x] - standard.Freq)
  }
})
total_area_top10beloved.job    <- area_top10beloved.job
area_top10beloved.job          <- area_top10beloved.job %>% group_by(date,area.work) %>% top_n(n = 25)


##Set ranking
area_top10beloved.job$rank <-  NA
countdown <- 0
for(i in 1:nrow(area_top10beloved.job)){
  if(i==1){
    area_top10beloved.job$rank[i] <- 1
  }else{
    if(area_top10beloved.job$area.work[i]==area_top10beloved.job$area.work[i-1]){
      if(area_top10beloved.job$percentage[i]==area_top10beloved.job$percentage[i-1]){
        area_top10beloved.job$rank[i] <- area_top10beloved.job$rank[i-1]
        countdown <- countdown + 1
      }else{
        area_top10beloved.job$rank[i] <- area_top10beloved.job$rank[i-1] + 1 + countdown
        countdown <- 0
      }      
    }else{
      area_top10beloved.job$rank[i] <- 1
      countdown <- 0
    }
  } 
}

write.csv(area_top10beloved.job, paste0("output\\per.month\\",gsub("[:punct:]","_", Sys.time()),"_area_top10beloved.job.csv"), row.names=F)



#Line chart format
source('.\\rscript\\function\\linechart_format.R', print.eval  = TRUE)

sd.area  <- "北部地區"
sd.job   <- "行政人員"##"專櫃／門市人員"
x.axis   <- "date"
y.axis   <- "Freq"
filename <- "output\\per.month\\測試看看"
df       <- area_top10beloved.job %>% filter(area.work==sd.area, job==sd.job)# %>% select(date, Freq)
line_graf(graf, x.axis, y.axis, filename)
















##live.area vs work.area
##
newdata <- fread("newdata.csv")
names(newdata)
setDF(newdata)
newdata$area.work <- NA
"北部地區"       -> newdata[which(substr(newdata[,"工作地點"],1,3) %in% c("台北市","新北市","基隆市","桃園市","桃園縣","新竹市","新竹縣")),"area.work"]
"中部地區"       -> newdata[which(substr(newdata[,"工作地點"],1,3) %in% c("苗栗縣","台中市","彰化縣","南投縣","雲林縣")),"area.work"]
"南部地區"       -> newdata[which(substr(newdata[,"工作地點"],1,3) %in% c("嘉義縣","嘉義市","台南市","高雄市","屏東縣")),"area.work"]
"東部與離島地區" -> newdata[which(substr(newdata[,"工作地點"],1,3) %in% c("宜蘭縣","花蓮縣","台東縣","澎湖縣","金門縣","連江縣")),"area.work"]
"非台灣地區"     -> newdata[which(substr(newdata[,"工作地點"],1,3) %in% c("台北市","新北市","基隆市","桃園市","桃園縣","新竹市","新竹縣",
                                                                                 "苗栗縣","台中市","彰化縣","南投縣","雲林縣", 
                                                                                 "嘉義縣","嘉義市","台南市","高雄市","屏東縣", 
                                                                                 "宜蘭縣","花蓮縣","台東縣","澎湖縣","金門縣","連江縣")==FALSE),"area.work"]
newdata$area.live <- NA
"北部地區"       -> newdata[which(substr(newdata[,"居住地點"],1,3) %in% c("台北市","新北市","基隆市","桃園市","桃園縣","新竹市","新竹縣")),"area.live"]
"中部地區"       -> newdata[which(substr(newdata[,"居住地點"],1,3) %in% c("苗栗縣","台中市","彰化縣","南投縣","雲林縣")),"area.live"]
"南部地區"       -> newdata[which(substr(newdata[,"居住地點"],1,3) %in% c("嘉義縣","嘉義市","台南市","高雄市","屏東縣")),"area.live"]
"東部與離島地區" -> newdata[which(substr(newdata[,"居住地點"],1,3) %in% c("宜蘭縣","花蓮縣","台東縣","澎湖縣","金門縣","連江縣")),"area.live"]
"非台灣地區"     -> newdata[which(substr(newdata[,"居住地點"],1,3) %in% c("台北市","新北市","基隆市","桃園市","桃園縣","新竹市","新竹縣",
                                                                                 "苗栗縣","台中市","彰化縣","南投縣","雲林縣", 
                                                                                 "嘉義縣","嘉義市","台南市","高雄市","屏東縣", 
                                                                                 "宜蘭縣","花蓮縣","台東縣","澎湖縣","金門縣","連江縣")==FALSE),"area.live"]
newdata[,"居住地點"] <- substr(newdata[,"居住地點"],1,3)
newdata[,"工作地點"] <- substr(newdata[,"工作地點"],1,3)
##
live2work <- newdata %>% filter(., 職務小類名稱!="工讀生") %>% 
  select(居住地點, 工作地點) %>% group_by(live = 居住地點, work = 工作地點) %>%
  dplyr::summarize(.,Freq=n()) %>% arrange(.,live, -Freq)
live2work <- live2work %>% filter(live!="" & work!="")

##先到這: 註記: 住哪轉哪
live2work <- live2work %>% group_by(., date, area.work) %>% mutate(., percentage=Freq/sum(Freq))
unique(live2work$area.work)

##change freq to a new standard
standard.live2work <- live2work %>% filter(., date==live2work$date[1])
live2work$Freq     <- sapply(1:nrow(live2work), function(x){
  standard.Freq <- standard.live2work$Freq[which(standard.live2work$job==live2work$job[x] & standard.live2work$area.work==live2work$area.work[x])]
  if(toString(standard.Freq)==""){
    return(0)
  }else{
    return(live2work$Freq[x] - standard.Freq)
  }
})
total_live2work    <- live2work
live2work          <- live2work %>% group_by(date,area.work) %>% top_n(n = 10)


##Set ranking
live2work$rank <-  NA
countdown <- 0
for(i in 1:nrow(live2work)){
  if(i==1){
    live2work$rank[i] <- 1
  }else{
    if(live2work$area.work[i]==live2work$area.work[i-1]){
      if(live2work$percentage[i]==live2work$percentage[i-1]){
        live2work$rank[i] <- live2work$rank[i-1]
        countdown <- countdown + 1
      }else{
        live2work$rank[i] <- live2work$rank[i-1] + 1 + countdown
        countdown <- 0
      }      
    }else{
      live2work$rank[i] <- 1
      countdown <- 0
    }
  } 
}

write.csv(live2work, paste0("output\\per.month\\",gsub("[:punct:]","_", Sys.time()),"_live2work.csv"), row.names=F)
