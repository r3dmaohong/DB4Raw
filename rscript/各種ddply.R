##
rm(list = ls()) #去除工作空間中所有物件
gc() #記憶體釋放
path<-"D:\\abc\\wjhong\\projects\\raw data"
setwd(path)
dir.create(file.path('output'), showWarnings = FALSE)
dir.create(file.path('rscript'), showWarnings = FALSE)
#setwd(paste0(path,'\\output'))

time_now = gsub('[- :]','_',Sys.time())
setwd(paste0(path,'\\output'))
dir.create(file.path(time_now), showWarnings = FALSE)
setwd(paste0(path,'\\output\\',time_now))
dir.create(file.path('png'), showWarnings = FALSE)

options(java.parameters = "-Xmx3g")
library(XLConnect)
library(plyr)
library(ggplot2)

##newdata
if(F){
  import_data = read.csv(file.choose(),stringsAsFactors=F)
  tmp = ddply(tmp, c('科系類別', '職務小類名稱'), nrow)
  tmp = tmp[which(tmp[,1]!=''),]
  tmp = tmp[which(tmp[,2]!=''),]
  tmp = tmp[which(tmp[,2]!='工讀生'),]
  tmp = tmp[order(tmp[,1],-tmp[,3]),]
  write.csv(tmp,'output\\整體科系與職務交叉.csv',row.names=F)
}

##ddply xls 每月
if(T){
  ddpltByCol <- function(col1,col2) {
    ddply(import_data, c(col1,col2), "nrow")
  }
  roundUpNice <- function(x, nice=c(1,2,4,5,6,8,10)) {
    if(length(x) != 1) stop("'x' must be of length 1")
    10^floor(log10(x)) * nice[[which(x <= 10^floor(log10(x)) * nice)[[1]]]]
  }
  
  print('請選擇資料')
  print('xls')
  filename <- file.choose()
  name <- basename(filename)
  print(paste0('資料讀取中: ',name))
  wb <- loadWorkbook(filename) 
  lst = readWorksheet(wb, sheet = getSheets(wb))
  
  print(paste0('包含多個活頁: ',toString(names(lst))))
  for(i in 1:length(names(lst))){
    cat(paste0('活頁 ',names(lst)[i],'\n'))
    print(colnames(lst[[i]]))
    cat('\n')
  }
  print('輸入要匯入的活頁index或整併全部')
  n <- readline(prompt="輸入要匯入的活頁index或整併全部(1,2,3 or all):")
  if(n=='all'){
    import_data <- do.call("rbind", lst)
  }else if(as.numeric(n)>0 & as.numeric(n)<4){
    import_data = lst[[as.numeric(n)]]
  }else{
    print('輸入錯誤')
  }
  
  rm(wb,lst)
  print('請選擇想要交叉的欄位index')
  print('目前只交受兩個以下參數 進行nrow')
  cat(paste0(which(colnames(import_data)==colnames(import_data)),'. ',colnames(import_data)))
  
  if(toString(which(colnames(import_data)=='工作地點'))!=''){
    import_data$area.work=''
    import_data[which(substr(import_data[,"工作地點"],1,3) %in% c("台北市","新北市","基隆市","桃園縣","新竹市","新竹縣")),"area.work"] <-"北部地區"
    import_data[which(substr(import_data[,"工作地點"],1,3) %in% c("苗栗縣","台中市","彰化縣","南投縣","雲林縣")),"area.work"] <-"中部地區"
    import_data[which(substr(import_data[,"工作地點"],1,3) %in% c("嘉義縣","嘉義市","台南市","高雄市","屏東縣")),"area.work"] <-"南部地區"
    import_data[which(substr(import_data[,"工作地點"],1,3) %in% c("宜蘭縣","花蓮縣","台東縣","澎湖縣","金門縣","連江縣")),"area.work"] <-"東部與離島地區"
    import_data[which(substr(import_data[,"工作地點"],1,3) %in% c("台北市","新北市","基隆市","桃園縣","新竹市","新竹縣",
                                                              "苗栗縣","台中市","彰化縣","南投縣","雲林縣", 
                                                              "嘉義縣","嘉義市","台南市","高雄市","屏東縣", 
                                                              "宜蘭縣","花蓮縣","台東縣","澎湖縣","金門縣","連江縣")==FALSE),"area.work"] <-"非台灣地區"
    import_data$工作地點 = import_data$area.work
  }
  if(toString(which(colnames(import_data)=='希望上班地區名稱'))!=''){
    import_data$area.work=''
    import_data[which(substr(import_data[,"希望上班地區名稱"],1,3) %in% c("台北市","新北市","基隆市","桃園縣","新竹市","新竹縣")),"area.work"] <-"北部地區"
    import_data[which(substr(import_data[,"希望上班地區名稱"],1,3) %in% c("苗栗縣","台中市","彰化縣","南投縣","雲林縣")),"area.work"] <-"中部地區"
    import_data[which(substr(import_data[,"希望上班地區名稱"],1,3) %in% c("嘉義縣","嘉義市","台南市","高雄市","屏東縣")),"area.work"] <-"南部地區"
    import_data[which(substr(import_data[,"希望上班地區名稱"],1,3) %in% c("宜蘭縣","花蓮縣","台東縣","澎湖縣","金門縣","連江縣")),"area.work"] <-"東部與離島地區"
    import_data[which(substr(import_data[,"希望上班地區名稱"],1,3) %in% c("台北市","新北市","基隆市","桃園縣","新竹市","新竹縣",
                                                              "苗栗縣","台中市","彰化縣","南投縣","雲林縣", 
                                                              "嘉義縣","嘉義市","台南市","高雄市","屏東縣", 
                                                              "宜蘭縣","花蓮縣","台東縣","澎湖縣","金門縣","連江縣")==FALSE),"area.work"] <-"非台灣地區"
    import_data$希望上班地區名稱 = import_data$area.work
  }
  sm_to_big <- read.csv('D:\\abc\\wjhong\\projects\\raw data\\1111學群學門學類-20160617-1.csv',stringsAsFactors=F)
  for(i in 1:ncol(sm_to_big)){
    for(j in 1:nrow(sm_to_big)){
      sm_to_big[j,i] = substr(sm_to_big[j,i],unlist(gregexpr('_',sm_to_big[j,i]))+1,nchar(sm_to_big[j,i]))
    }
  }
  if(toString(which(colnames(import_data)=='最高學歷_科系小類名稱'))!=''){
    for(i in 1:nrow(sm_to_big)){
      import_data$最高學歷_科系小類名稱[which(import_data$最高學歷_科系小類名稱==sm_to_big[i,3])] = sm_to_big[i,2]
    }
  }
  
  
  n <- readline(prompt="輸入欄位index(多個用空格分開): ")
  n = unlist(strsplit(n,' '))
  n = as.numeric(n)
  if(length(n)==1){
    tmp = as.data.frame(table(import_data[,n]))
    tmp = tmp[order(tmp[,1],-tmp[,2]),]
    write.csv(tmp,paste0(gsub('[- :]','_',Sys.time()),colnames(import_data)[n],'.csv'),row.names=F)
  }else if(length(n)==2){
    tmp = ddpltByCol(colnames(import_data)[n[1]],colnames(import_data)[n[2]])
    tmp = tmp[order(tmp[,1],-tmp[,3]),]
    colnames(tmp)[3] = '次數'
    tmp$百分比 = 0
    for(j in 1:nrow(tmp)){
      tmp$百分比[j] = tmp$次數[j]/sum(tmp$次數[which(tmp[,1]==tmp[j,1])])
    }
    write.csv(tmp,paste0(time_now,colnames(import_data)[n[1]],'_and_',colnames(import_data)[n[2]],'.csv'),row.names=F)
    
    all_tmp = tmp
    ##製圖
    ##調整班轉後bar chart順序
    tmp1u = unique(tmp[,1])
    tmp1u = tmp1u[which(!is.na(tmp1u))]
    for(uni1 in 1:length(tmp1u)){
      png_name = paste0('png//',time_now,'_',uni1,'.png')
      
      tmp = all_tmp[which(all_tmp[,1]==tmp1u[uni1]),]
      if(nrow(tmp)>=10){
        tmp = tmp[1:10,] 
      }
      height = 600
      width = 600
      if(nrow(tmp)<10){
        height=600-(10-nrow(tmp))*50
      }
      
      rownames(tmp)=1:nrow(tmp)
      tmp = tmp[order(-as.numeric(rownames(tmp))),]
      tmp[,2]=factor(tmp[,2],levels=tmp[,2])
      if(F){
        if(length(unique(tmp$次數))<=3){
          ##還是用(roundUpNice(max(tmp$次數))+1/10*roundUpNice(max(tmp$次數))) - max(tmp$次數) 的百分比差異 1/1.1
          myPlot <- ggplot(data=tmp, aes(x=tmp[,2], y=tmp[,3])) +
            geom_bar(stat="identity",position = "dodge") + 
            geom_text(aes(label = paste0(format(round(100*百分比, 2), nsmall=2),' %')
            ), hjust = .0001, size=5)+
            scale_y_continuous(limits = c(0,roundUpNice(max(tmp$次數))+1/10*roundUpNice(max(tmp$次數)))) +
            labs(y = "次數", x = "類別")+
            theme(text = element_text(size=15))+
            coord_flip()
        }else{
          myPlot <- ggplot(data=tmp, aes(x=tmp[,2], y=tmp[,3])) +
            geom_bar(stat="identity",position = "dodge") + 
            geom_text(aes(label = paste0(format(round(100*百分比, 2), nsmall=2),' %')
            ), hjust = .0001, size=5)+
            scale_y_continuous(limits = c(0,max(tmp$次數)*1.1)) +
            labs(y = "次數", x = "類別")+
            theme(text = element_text(size=15))+
            coord_flip()
        }
        
      }
      myPlot <- ggplot(data=tmp, aes(x=tmp[,2], y=tmp[,3])) +
        geom_bar(stat="identity",position = "dodge") + 
        geom_text(aes(label = paste0(format(round(100*百分比, 2), nsmall=2),' %')
        ), hjust = .0001, size=5)+
        scale_y_continuous(limits = c(0,max(tmp$次數)*1.1)) +
        labs(y = "次數", x = "類別")+
        theme(text = element_text(size=15))+
        coord_flip()
      
      
      
      png(png_name,width=width,height=height)
      print(myPlot)
      dev.off()
    }

    
    
    }else{
    print('輸入錯誤')
    print('目前只接受兩個以下參數')
  }
  

}

##純table最新xls
if(F){
  print('請選擇資料')
  print('xls')
  filename <- file.choose()
  name <- basename(filename)
  print(paste0('資料讀取中: ',name))
  wb <- loadWorkbook(filename) 
  lst = readWorksheet(wb, sheet = getSheets(wb))
  import_data <- do.call("rbind", lst)
  rm(wb,lst)
  
  tmp = as.data.frame(table(import_data$職務小類名稱))
  tmp = tmp[order(-tmp[,2]),]
  write.csv(tmp,'output\\職務小類.csv',row.names=F)
}


if(F){
  #match_DB = read.csv('資料庫合併-11902.csv',stringsAsFactor=F)
  ##read all sheet in xlsx
  print('請選取第一個檔案')
  print('xls')
  print('接下來會讀取童資料夾的所有檔案')
  filename <- file.choose()
  file_names <- list.files(dirname(filename))
  
  for(i in 1:length(file_names)){
    ##寫到一半
    ##寫到一半
    ##寫到一半
    ##寫到一半
    name <- basename(paste0(dirname(filename),file_names[i]))
    print(paste0('資料讀取與合併中: ',name))
    wb <- loadWorkbook(filename) 
    lst = readWorksheet(wb, sheet = getSheets(wb))
    match_DB <- do.call("rbind", lst)
    rm(wb,lst)
  }
}




