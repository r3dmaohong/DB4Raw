rm(list = ls()) #Remove all objects in the environment
gc() ##Free up the memory

original_path <- getwd()
setwd(file.path('raw data'))
options(stringsAsFactors=F)

##Libraries
options(java.parameters = "-Xmx4g")
library(XLConnect)
library(pbapply)
library(data.table)
library(dplyr)

##Import all files.
files <- list.files(file.path('per.month','求才'),full.names = TRUE)

if(F){
  ##Something went wrong with lapply
  files.lists <- pblapply(files,function(file.name){
    wb <- loadWorkbook(file.name)
    ##Read all sheets in a xlsx file as a list.
    file.list <- readWorksheet(wb, sheet = getSheets(wb)) 
    ##Turn the list into a data frame.
    file.list <- do.call(rbind,file.list) 
    gc()
    rm(wb)
    ##If the file only have 1 sheet, it should use method below to traslate to data frame.
    if(class(file.list)!="data.frame")
      file.list <- data.frame(t(file.list),stringsAsFactors=F)
    return(file.list)
  })
}

## Remember to xls2csv
files <- files[grepl("csv", files)]
files.lists <- pblapply(files,function(file.name){
  file.list <- fread(file.name)
  gc()
  return(file.list)
})


##Combine all the lists into data frame.
total.data <- do.call(rbind,files.lists)
rm(files.lists,files)
names(total.data)
str(total.data)

data.intern <- total.data %>% filter(., 實習職缺=="Y")

## Supply
data.intern         <- data.intern %>% mutate(., job.location=substr(工作地點, 1, 3))
job.freq            <- table(data.intern$職務小類名稱) %>% data.frame %>% arrange(-Freq)
location.freq       <- table(data.intern$工作地點) %>% data.frame %>% arrange(-Freq)
location.sub.freq   <- table(data.intern$job.location) %>% data.frame %>% arrange(-Freq)
job.vs.location     <- data.intern %>% group_by(職務小類名稱,工作地點) %>% summarize(.,Freq=n()) 
job.vs.location     <- job.vs.location %>% arrange(職務小類名稱, -Freq)
dpt.restrictions    <- data.intern$科系限制 %>% strsplit(",") %>% unlist %>% table %>% data.frame() %>% arrange(., -Freq)
dpt.restrictions[,1]<- as.character(dpt.restrictions[,1])
dpt.restrictions    <- rbind(dpt.restrictions,c("不限制科系", data.intern$科系限制[which(is.na(data.intern$科系限制))] %>% length))
dpt.restrictions[,2]<- as.integer(dpt.restrictions[,2])
dpt.restrictions    <- dpt.restrictions %>% arrange(-Freq)

## Match table
match.table <- read.csv(file.choose())
for(i in 1:ncol(match.table)){
  match.table[,i] <- match.table[,i] %>% gsub("[0-9+_]", "", .)
}
tmp <- dpt.restrictions %>% filter(grepl("學門", .))
for(i in 1:nrow(tmp)){
  tmp.i            <- match.table[match.table[,2]==tmp[i,1],3] %>% data.frame %>% cbind(tmp[i,2])
  names(tmp.i)     <- names(dpt.restrictions)
  dpt.restrictions <- rbind(dpt.restrictions, tmp.i)
}
dpt.restrictions        <- dpt.restrictions %>% filter(!grepl("學門", .))
names(dpt.restrictions) <- c("category", "Freq")
dpt.restrictions        <- dpt.restrictions %>% group_by(category) %>% mutate(Freq=sum(Freq)) %>% unique

##Percentage
job.freq          <- job.freq %>% mutate(., percentage=Freq/sum(Freq))
location.freq     <- location.freq %>% mutate(., percentage=Freq/sum(Freq))
location.sub.freq <- location.sub.freq %>% mutate(., percentage=Freq/sum(Freq))
job.vs.location   <- job.vs.location %>% mutate(., percentage=Freq/sum(Freq))
dpt.restrictions  <- dpt.restrictions %>% group_by(.) %>% mutate(., percentage=Freq/sum(Freq)) %>% arrange(-Freq)

dir.create("output\\intern.analysis")
write.csv(job.freq, "output\\intern.analysis\\job.freq.csv", row.names=F)
write.csv(location.freq, "output\\intern.analysis\\location.freq.csv", row.names=F)
write.csv(location.sub.freq, "output\\intern.analysis\\location.sub.freq.csv", row.names=F)
write.csv(job.vs.location, "output\\intern.analysis\\job.vs.location.csv", row.names=F)
write.csv(dpt.restrictions, "output\\intern.analysis\\dpt.restrictions.csv", row.names=F)

## Additional Analysis
if(F){
  ##Industry distribution
  c_df <- data.intern$公司名稱 %>% unique %>% data.frame(., stringsAsFactors=F)
  c_df$industry <- ""
  
  ## Industry
  library(rvest)
  
  # returns string w/o leading or trailing whitespace
  trim <- function (x) gsub("^\\s+|\\s+$", "", x)
  
  for(i in 27:nrow(c_df)){
    x <- trim(substr(c_df[i,1],1,3))
    url <- paste0("http://www.1111.com.tw/job-bank/job-index.asp?ss=s&ks=",x,"&si=2&ps=40&trans=1")
    total_css <- read_html(url)
    title_css <- total_css %>% html_nodes(".showCoTradeCss") %>% html_text() %>% iconv(., "utf8")
    c_df[i,2] <- title_css[2]
    Sys.sleep(runif(1,1,3))
    cat("\r",i/nrow(c_df)*100)
  }
  
  tmp_match <- read.csv(file.choose(),stringsAsFactors=F)
  names(tmp_match)
  
  
  c_df$bigIN <- ""
  x=0
  for(i in 1:nrow(c_df)){
    if(is.na(c_df[i,2])){
      x = x + 1
    }else{
      c_df[i,3] <- tmp_match[which(tmp_match$X1111產業小類名稱==c_df[i,2]),2]
      
    }
  }
  
  
  tmp <- c_df[,3] %>% table %>% data.frame(., stringsAsFactors=F)
  tmp <- tmp[order(-tmp[,2]),]
  tmp <- tmp[tmp[,1]!="",]
  tmp$percentage <- tmp$Freq/sum(tmp$Freq)
  tmp$percentage <- tmp$percentage *100
  tmp$percentage <- paste0(round(tmp$percentage,2), "%")
}
