files.lists <- pblapply(files,function(file.name){
  wb <- loadWorkbook(file.name)
  ##Read all sheets in a xlsx file as a list.
  file.list <- readWorksheet(wb, sheet = 3) 
  ##Turn the list into a data frame.
  file.list <- do.call(rbind,file.list) 
  gc()
  rm(wb)
  ##If the file only have 1 sheet, it should use method below to traslate to data frame.
  if(class(file.list)!="data.frame")
    file.list <- data.frame(t(file.list),stringsAsFactors=F)
  
  date <- unlist(strsplit(file.name, "/"))[length(unlist(strsplit(file.name, "/")))] %>% gsub("[A-z.]", "", .) ##%>% substr(., 1, 7)
  file.list <- cbind(file.list, date)
  
  return(file.list)
})