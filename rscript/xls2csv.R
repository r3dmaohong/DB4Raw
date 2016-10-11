## Transfer xls to csv...
original_path <- getwd()
setwd(file.path('raw data'))
options(stringsAsFactors=F)

options(java.parameters = "-Xmx4g")
library(XLConnect)
library(dplyr)
library(stringr)

##Get all files' names.
folders <- c("求職", "求才")
folders <- "求才"
for(folder in folders){
  files <- list.files(file.path('per.month', folder),full.names = TRUE)
  xls.files <- files[grepl("xls",files)]
  csv.files <- files[grepl("csv",files)]
  
  ##Import files
  if(folder=="求職"){
    for(i in 1:length(xls.files)){
      file.name <- xls.files[i]
      if(gsub("xls", "csv", file.name) %in% csv.files){
        print(paste0(gsub("xls", "csv", file.name), " already exist."))
      }else{
        wb <- loadWorkbook(file.name)
        ##Read all sheets in a xlsx file as a list.
        file.list <- readWorksheet(wb, sheet = 3) 
        ##Turn the list into a data frame.
        file.list <- do.call(rbind,file.list) 
        
        ##If the file only have 1 sheet, it should use method below to traslate to data frame.
        if(class(file.list)!="data.frame")
          file.list <- data.frame(t(file.list),stringsAsFactors=F)
        
        date <- unlist(strsplit(file.name, "/"))[length(unlist(strsplit(file.name, "/")))] %>% gsub("[A-z.]", "", .) ##%>% substr(., 1, 7)
        file.list <- cbind(file.list, date)
        write.csv(file.list,gsub("xls", "csv", file.name), row.names=F)
        print(paste0(gsub("xls", "csv", file.name), " has exported."))
        rm(wb, file.list)
        gc()
        xlcFreeMemory()
      }
    }
  }else if(folder=="0求才"){
    for(i in 1:length(xls.files)){
      file.name <- xls.files[i]
      if(gsub("xls", "csv", file.name) %in% csv.files){
        print(paste0(gsub("xls", "csv", file.name), " already exist."))
      }else{
        wb <- loadWorkbook(file.name)
        ##Read all sheets in a xlsx file as a list.
        file.list <- readWorksheet(wb, sheet = getSheets(wb)) 
        ##Turn the list into a data frame.
        file.list <- do.call(rbind,file.list) 
        
        ##If the file only have 1 sheet, it should use method below to traslate to data frame.
        if(class(file.list)!="data.frame")
          file.list <- data.frame(t(file.list),stringsAsFactors=F)
        
        date <- unlist(strsplit(file.name, "/"))[length(unlist(strsplit(file.name, "/")))] %>% gsub("[A-z.]", "", .) ##%>% substr(., 1, 7)
        file.list <- cbind(file.list, date)
        write.csv(file.list,gsub("xls", "csv", file.name), row.names=F)
        print(paste0(gsub("xls", "csv", file.name), " has exported."))
        rm(wb, file.list)
        gc()
        xlcFreeMemory()
      }
    }
  }else{
    for(i in 1:length(xls.files)){
      file.name <- xls.files[i]
      if(gsub("xls", "csv", file.name) %in% csv.files){
        print(paste0(gsub("xls", "csv", file.name), " already exist."))
      }else{
        wb <- loadWorkbook(file.name)
        ##Read all sheets in a xlsx file as a list.
        file.list <- readWorksheet(wb, sheet = getSheets(wb)) 
        ##Turn the list into a data frame.
        file.list <- do.call(rbind,file.list) 
        
        ##If the file only have 1 sheet, it should use method below to traslate to data frame.
        if(class(file.list)!="data.frame")
          file.list <- data.frame(t(file.list),stringsAsFactors=F)
        
        write.csv(file.list,gsub("xls", "csv", file.name), row.names=F)
        print(paste0(gsub("xls", "csv", file.name), " has exported."))
        rm(wb, file.list)
        gc()
        xlcFreeMemory()
      }
    }
  }

}

##read big xlsx which has only one sheet
library("openxlsx")

folder <- "求才"

files <- list.files(file.path('per.month', folder),full.names = TRUE)
xls.files <- files[grepl("xlsx",files)]
csv.files <- files[grepl("csv",files)]

for(i in 1:length(xls.files)){
  file.name <- xls.files[i]
  if(gsub("xlsx", "csv", file.name) %in% csv.files){
    print(paste0(gsub("xlsx", "csv", file.name), " already exist."))
  }else{
    file.list <- read.xlsx(file.name, sheet = 1, startRow = 1, colNames = TRUE) 
    write.csv(file.list, gsub("xlsx", "csv", file.name), row.names=F)
    print(paste0(gsub("xlsx", "csv", file.name), " has exported."))
    gc()
    print(i)
  }
}
#mydf <- read.xlsx("BigExcelFile.xlsx", sheet = 1, startRow = 2, colNames = TRUE)

