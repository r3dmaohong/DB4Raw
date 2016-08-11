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
