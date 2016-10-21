#單一目標欄位
#最終篩選欄位
#原始DF : total.data

#OutputData
#OrigiData
#targerCol

#OrigiData=total.data
#targerCol="職務小類名稱"
#GroupByCol = "area.work"
#----GroupByCol = c("date", "area.work")
#filename="AreaJobDemand"
#fixed: date


JobTrend <- function(OrigiData, targetCol, GroupByCol, filename){
  parseTargetCol  <- parse(text=targetCol)
  #parseGroupByCol <- parse(text=paste0("by = .(", paste0(GroupByCol, collapse=", "), ")"))
  
  OutputData <- OrigiData[ eval(parseTargetCol)!="工讀生", .N, by= c("date", GroupByCol, targetCol)]
  OutputData <- OutputData[eval(parse(text=paste0("order(", paste(paste("date", GroupByCol, sep=", "), "-N", sep=", "), ")")))]
  OutputData[, percentage:=N/sum(N), by=c("date", GroupByCol)]
  cat(GroupByCol, " : ", unique(OutputData[,eval(parse(text=GroupByCol))]))
  
  ##Backup 
  totalOutputData <- OutputData
  
  ##change freq to a new standard
  ##fixed: date
  standard.OutputData <- OutputData[date==OutputData$date[1]]
  OutputData <- OutputData[, head(.SD, 25), by=c("date", GroupByCol)]
  OutputData$Freq <- sapply(1:nrow(OutputData), function(x){
    #OutputData$N[x] - standard.OutputData$N[which(standard.OutputData[,eval(parse(text=targetCol))]==OutputData[,eval(parse(text=targetCol))][x] & standard.OutputData[,eval(parse(text=GroupByCol))]==OutputData[,eval(parse(text=GroupByCol))][x])]
    if(standard.OutputData$N[which(standard.OutputData[,eval(parse(text=targetCol))]==OutputData[,eval(parse(text=targetCol))][x] & standard.OutputData[,eval(parse(text=GroupByCol))]==OutputData[,eval(parse(text=GroupByCol))][x])] %>% toString != ""){
      return(OutputData$N[x] - standard.OutputData$N[which(standard.OutputData[,eval(parse(text=targetCol))]==OutputData[,eval(parse(text=targetCol))][x] & standard.OutputData[,eval(parse(text=GroupByCol))]==OutputData[,eval(parse(text=GroupByCol))][x])])
    }
    ##Add missing standard
    standard.OutputData <<- rbind(standard.OutputData, eval(parse(text=paste0("data.table(date=standard.OutputData$date[1], ", GroupByCol, "=totalOutputData[, ", GroupByCol, "][x], ", targetCol, "=totalOutputData[, ", targetCol, "][x], N=0, percentage=0)"))))
    return(OutputData$N[x])
  })
  
  ##Set ranking
  OutputData$rank <-  0
  countdown <- 0
  for(i in 1:nrow(OutputData)){
    if(i==1){
      OutputData$rank[i] <- 1
    }else{
      if(OutputData[,eval(parse(text=GroupByCol))][i]==OutputData[,eval(parse(text=GroupByCol))][i-1]){
        if(OutputData$percentage [i]==OutputData$percentage [i-1]){
          OutputData$rank[i] <- OutputData$rank[i-1]
          countdown <- countdown + 1
        }else{
          OutputData$rank[i] <- OutputData$rank[i-1] + 1 + countdown
          countdown <- 0
        }      
      }else{
        OutputData$rank[i] <- 1
        countdown <- 0
      }
    } 
  }
  
  OutputData$percentage <- paste0(format(round(OutputData$percentage*100,2), nsmall=2), "%")
  #Generate index
  OutputData[,index:=paste(eval(parse(text=GroupByCol)), eval(parse(text=targetCol)), sep="_")]
  
  ##Historical changes...
  totalOutputData$Freq <- sapply(1:nrow(totalOutputData), function(x){
    if(standard.OutputData$N[which(standard.OutputData[,eval(parse(text=targetCol))]==totalOutputData[,eval(parse(text=targetCol))][x] & standard.OutputData[,eval(parse(text=GroupByCol))]==totalOutputData[,eval(parse(text=GroupByCol))][x])] %>% toString != ""){
      return(totalOutputData$N[x] - standard.OutputData$N[which(standard.OutputData[,eval(parse(text=targetCol))]==totalOutputData[,eval(parse(text=targetCol))][x] & standard.OutputData[,eval(parse(text=GroupByCol))]==totalOutputData[,eval(parse(text=GroupByCol))][x])])
    }
    ##Add missing standard
    standard.OutputData <<- rbind(standard.OutputData, eval(parse(text=paste0("data.table(date=standard.OutputData$date[1], ", GroupByCol, "=totalOutputData[, ", GroupByCol, "][x], ", targetCol, "=totalOutputData[, ", targetCol, "][x], N=0, percentage=0)"))))
    return(totalOutputData$N[x])
  })
  totalOutputData[,index:=paste(eval(parse(text=GroupByCol)), eval(parse(text=targetCol)), sep="_")]
  OutputData <- OutputData[date==max(date)]
  ##Rank, Area, Job, Percentage, Freq
  ##Keep the latest data
  OutputDemandJob <- OutputData[, eval(parse(text=paste0(".(rank, ", GroupByCol, ", ",  targetCol, ", percentage)")))]
  totalOutputData <- totalOutputData[index %in% OutputData$index, ]
  
  ##Check
  tmp <- totalOutputData[OutputData$index %in% index, index] %>% table %>% data.frame
  stopifnot(tmp$Freq %>% unique %>% length ==1)
  #tmp$.[tmp$Freq==min(tmp$Freq %>% unique)] %>% unique
  ##不動產經紀人 => 不動產經紀人/營業員
  totalOutputData <- totalOutputData[, eval(parse(text=paste0(".(date, ", GroupByCol, ", ",  targetCol, ", Freq)")))]
  #apply(totalOutputData, 2, class)
  
  write.csv(OutputDemandJob, paste0("output\\per.month\\", format(Sys.time(), "%Y%m%d_"), filename, ".csv"), row.names=F)
  write.csv(totalOutputData, paste0("output\\per.month\\", format(Sys.time(), "%Y%m%d_"), filename, "_History.csv"), row.names=F)
  cat("Completed.")
}