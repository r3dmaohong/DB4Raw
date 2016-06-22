rm(list = ls()) #去除工作空間中所有物件
gc() #記憶體釋放
path<-"D:\\abc\\wjhong\\projects\\raw data"
setwd(path)
start.time<-Sys.time()

people = read.csv('newdata.csv',stringsAsFactors=F)

##為何不用for會是NA?
#for(i in 1:nrow(people)){
#  people$職務經驗轉換[i] = as.numeric(substr(people$職務經驗[i],1,unlist(gregexpr(pattern ='年',people$職務經驗[i]))-1))*12+as.numeric(substr(people$職務經驗[i],unlist(gregexpr(pattern ='年',people$職務經驗[i]))+1,nchar(toString(people$職務經驗[i]))-1))
#  print(people$職務經驗轉換[i])
#  print(paste0('職務經驗轉換',i/nrow(people)*100,'%'))
#}
all_job_list = unique(people$職務小類名稱)
output_df = data.frame('職務名稱'=character(),'轉職後薪資成長'=character(),'轉職前平均年資'=character(),'轉職薪資樣本數'=character(),'轉職後晉升機率'=character(),'轉職後晉升前年資'=character(),'轉職晉升樣本數'=character(),stringsAsFactors=F)

for(i_dpm in 1:length(all_job_list)){
    
  job_list = all_job_list[i_dpm]
  
  for(i in 1:length(job_list)){
    job_name = job_list[i]
    
    people_sep = people[which(people$職務小類名稱== job_name& people$職務經驗!='0年0月'& people$職務經驗!='0年1月'& people$職務經驗!='0年2月'& people$職務經驗!='0年3月'& people$職務經驗!='0年4月'& people$職務經驗!='0年5月'& people$職務經驗!='0年6月'),]
    people_sep_no_manage = people_sep[which(people_sep$管理人數=='0人'),]
    #numofalltemp=0
    alltemp = {}
    allmonth = {}
    for(i in 1:nrow(people_sep)){
      temp = people[which(people$工作待遇>=18000 & people$履歷編號==people_sep$履歷編號[i] &people$第幾工作經歷==(people_sep[i,'第幾工作經歷']+1)),'工作待遇'][1]/people[which(people$工作待遇!=0 & people$履歷編號==people_sep$履歷編號[i] &people$第幾工作經歷==(people_sep[i,'第幾工作經歷'])),'工作待遇'][1]
      if(toString(temp)!="NA" & temp >=1){
        alltemp = c(alltemp,temp)
        #numofalltemp = numofalltemp + 1 
        allmonth = c(allmonth ,as.numeric(substr(people_sep$職務經驗[i],1,unlist(gregexpr(pattern ='年',people_sep$職務經驗[i]))-1))*12+as.numeric(substr(people_sep$職務經驗[i],unlist(gregexpr(pattern ='年',people_sep$職務經驗[i]))+1,nchar(toString(people_sep$職務經驗[i]))-1)))
        
        
      }
      
      
      
      cat(paste0('\r',i,' ',job_name , ' 處理薪資晉升狀態 ' , i/nrow(people_sep)*100,'%'))
      #print(temp)
      #print(alltemp)
    }
    薪資成長 = paste0(round(mean(alltemp[!alltemp %in% boxplot.stats(alltemp)$out])*100,2),'%')
    平均年資 = paste0(round(mean(allmonth[!allmonth %in% boxplot.stats(allmonth)$out]),0),'個月')
    
    people_next = people_sep[1,]
    people_next = people_next[-1,]
    
    n_of_up = 0
    allmonthup = 0
    numofallup={}
    cat('\n')
    for(i in 1:nrow(people_sep_no_manage)){
      temp = people[which(people$履歷編號==people_sep_no_manage[i,'履歷編號'] & people$第幾工作經歷==(people_sep_no_manage[i,'第幾工作經歷']+1 & people$管理人數!='未定' & people$管理人數!='0人')),][1]
      #people_next = rbind(people_next,temp)
      if(!is.na(as.numeric(temp[1,1]))[1]){
        n_of_up = n_of_up + 1
        allmonthup = c(allmonthup , as.numeric(substr(people_sep_no_manage$職務經驗[i],1,unlist(gregexpr(pattern ='年',people_sep_no_manage$職務經驗[i]))-1))*12+as.numeric(substr(people_sep_no_manage$職務經驗[i],unlist(gregexpr(pattern ='年',people_sep_no_manage$職務經驗[i]))+1,nchar(toString(people_sep_no_manage$職務經驗[i]))-1)))
        numofallup = numofallup +1
      }
      cat(paste0('\r',i,' ',job_name ,' 晉升比例與年資計算 ',i/nrow(people_sep_no_manage)*100,'%'))
    }
    晉升機率 = paste0(round(n_of_up/nrow(people_sep_no_manage)*100,2),'%')
    晉升年資 = paste0(round(mean(allmonthup[!allmonthup %in% boxplot.stats(allmonthup)$out]),0),'個月')
    ##晉升速度為多少月晉升
    
    temp = data.frame('職務名稱'=character(),'轉職後薪資成長'=character(),'轉職前平均年資'=character(),'轉職薪資樣本數'=character(),'轉職後晉升機率'=character(),'轉職後晉升前年資'=character(),'轉職晉升樣本數'=character(),stringsAsFactors=F)
    
    temp[1,1] = job_name
    temp[1,2] = 薪資成長
    temp[1,3] = 平均年資
    temp[1,4] = max(length(allmonth),length(alltemp))
    temp[1,5] = 晉升機率
    temp[1,6] = 晉升年資
    temp[1,7] = nrow(people_sep_no_manage)
    output_df = rbind(output_df,temp)
    
    
  }
  print(paste0(i_dpm,'. ',job_list, '分析完成 - ',format(round((i_dpm/length(all_job_list))*100,2),nsmall=2),' %'))
  write.csv(output_df,paste0('output\\全部薪資成長與晉升狀況.csv'),row.names=F)
}



if(F){
  ##化學／化工工程師
  department_job_match_list <- read.csv('中原職務優勢比較.csv',stringsAsFactors=F)
  
  #department_job_match_list <- read.csv(file.choose(),stringsAsFactors=F)
  
  for(i_dpm in 1:length(unique(department_job_match_list[,2]))){
    output_df = data.frame('職務名稱'=character(),'轉職後薪資成長'=character(),'轉職前平均年資'=character(),'轉職薪資樣本數'=character(),'轉職後晉升機率'=character(),'轉職後晉升前年資'=character(),'轉職晉升樣本數'=character(),stringsAsFactors=F)
    
    學系 = unique(department_job_match_list[,2])[i_dpm]
    job_list = department_job_match_list$X1111職務小類名稱[which(department_job_match_list[,2]==unique(department_job_match_list[,2])[i_dpm])]
    
    for(i in 1:length(job_list)){
      job_name = job_list[i]
      
      people_sep = people[which(people$職務小類名稱== job_name& people$職務經驗!='0年0月'& people$職務經驗!='0年1月'& people$職務經驗!='0年2月'& people$職務經驗!='0年3月'& people$職務經驗!='0年4月'& people$職務經驗!='0年5月'& people$職務經驗!='0年6月'),]
      people_sep_no_manage = people_sep[which(people_sep$管理人數=='0人'),]
      #numofalltemp=0
      alltemp = {}
      allmonth = {}
      for(i in 1:nrow(people_sep)){
        temp = people[which(people$工作待遇>=18000 & people$履歷編號==people_sep$履歷編號[i] &people$第幾工作經歷==(people_sep[i,'第幾工作經歷']+1)),'工作待遇'][1]/people[which(people$工作待遇!=0 & people$履歷編號==people_sep$履歷編號[i] &people$第幾工作經歷==(people_sep[i,'第幾工作經歷'])),'工作待遇'][1]
        if(toString(temp)!="NA" & temp >=1){
          alltemp = c(alltemp,temp)
          #numofalltemp = numofalltemp + 1 
          allmonth = c(allmonth ,as.numeric(substr(people_sep$職務經驗[i],1,unlist(gregexpr(pattern ='年',people_sep$職務經驗[i]))-1))*12+as.numeric(substr(people_sep$職務經驗[i],unlist(gregexpr(pattern ='年',people_sep$職務經驗[i]))+1,nchar(toString(people_sep$職務經驗[i]))-1)))
          
          
        }
        
        
        
        print(paste0(i,' ',job_name , ' 處理薪資晉升狀態 ' , i/nrow(people_sep)*100,'%'))
        #print(temp)
        #print(alltemp)
      }
      薪資成長 = paste0(round(mean(alltemp[!alltemp %in% boxplot.stats(alltemp)$out])*100,2),'%')
      平均年資 = paste0(round(mean(allmonth[!allmonth %in% boxplot.stats(allmonth)$out]),0),'個月')
      
      people_next = people_sep[1,]
      people_next = people_next[-1,]
      
      n_of_up = 0
      allmonthup = 0
      numofallup={}
      for(i in 1:nrow(people_sep_no_manage)){
        temp = people[which(people$履歷編號==people_sep_no_manage[i,'履歷編號'] & people$第幾工作經歷==(people_sep_no_manage[i,'第幾工作經歷']+1 & people$管理人數!='未定' & people$管理人數!='0人')),][1]
        #people_next = rbind(people_next,temp)
        if(!is.na(as.numeric(temp[1,1]))[1]){
          n_of_up = n_of_up + 1
          allmonthup = c(allmonthup , as.numeric(substr(people_sep_no_manage$職務經驗[i],1,unlist(gregexpr(pattern ='年',people_sep_no_manage$職務經驗[i]))-1))*12+as.numeric(substr(people_sep_no_manage$職務經驗[i],unlist(gregexpr(pattern ='年',people_sep_no_manage$職務經驗[i]))+1,nchar(toString(people_sep_no_manage$職務經驗[i]))-1)))
          numofallup = numofallup +1
        }
        print(paste0(i,' ',job_name ,' 晉升比例與年資計算 ',i/nrow(people_sep_no_manage)*100,'%'))
      }
      晉升機率 = paste0(round(n_of_up/nrow(people_sep_no_manage)*100,2),'%')
      晉升年資 = paste0(round(mean(allmonthup[!allmonthup %in% boxplot.stats(allmonthup)$out]),0),'個月')
      ##晉升速度為多少月晉升
      
      temp = data.frame('職務名稱'=character(),'轉職後薪資成長'=character(),'轉職前平均年資'=character(),'轉職薪資樣本數'=character(),'轉職後晉升機率'=character(),'轉職後晉升前年資'=character(),'轉職晉升樣本數'=character(),stringsAsFactors=F)
      
      temp[1,1] = job_name
      temp[1,2] = 薪資成長
      temp[1,3] = 平均年資
      temp[1,4] = max(length(allmonth),length(alltemp))
      temp[1,5] = 晉升機率
      temp[1,6] = 晉升年資
      temp[1,7] = nrow(people_sep_no_manage)
      output_df = rbind(output_df,temp)
      
      
    }
    write.csv(output_df,paste0('output\\',學系,'薪資成長與晉升狀況1.csv'),row.names=F)
    
  }
  
  
  #系統時間
  end.time <- Sys.time()
  #記錄一段程序結束執行時間
  run.time <- end.time - start.time
  run.time
}




##盒鬚圖離異值
if(FALSE){
  "   interquartile range (IQR) = Q3 ??? Q1
  
  盒子兩端延伸出去的虛線是Q1 ??? 1.5 x IQR 或 Q3 + 1.5 x IQR
  
  至於outlier是指在 boxplot 中各別顯示的點，
  
  落在Q1 ??? 1.5 x IQR 下方 或 高於 Q3 + 1.5 x IQR 上方
  "
}




