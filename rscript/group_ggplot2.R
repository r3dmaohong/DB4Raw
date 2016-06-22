

y_by_y = data.frame('類別'=character(),'今年'=numeric(),'去年'=numeric(),stringsAsFactors=F)

tmp = c('不拘','1年工作經驗','2年工作經驗','3年工作經驗','4年工作經驗','5年工作經驗','6年工作經驗')
y_by_y[1:length(tmp),1] = tmp
x <- runif(length(tmp))
x = x/sum(x)
x = format(round(x*100,2),nsmall=2)
y_by_y[1:length(tmp),2] = c(x)

x <- runif(length(tmp))
x = x/sum(x)
x = format(round(x*100,2),nsmall=2)
y_by_y[1:length(tmp),3] = c(x)

tmp = y_by_y

# melt the data frame for plotting
data.m <- melt(tmp, id.vars='類別')

# plot everything
colnames(data.m) = c('類別','年份','百分比')
data.m[,3] = as.numeric(data.m[,3])
ggplot(data.m, aes(類別, 百分比)) +   
  geom_bar(aes(fill = 年份), position = "dodge", stat="identity")+
  scale_y_continuous(limits = c(0,max(data.m$百分比))) +
  theme(text = element_text(size=15))+
  labs(x = "類別", y = "百分比(%)")+
  coord_flip()


myPlot <- ggplot(data=tmp, aes(x=tmp[,2], y=tmp[,3])) +
  geom_bar(stat="identity",position = "dodge") + 
  geom_text(aes(label = paste0(format(round(100*百分比, 2), nsmall=2),' %')
  ), hjust = .0001, size=5)+
  scale_y_continuous(limits = c(0,max(tmp$次數)*1.1)) +
  labs(y = "次數", x = "類別")+
  theme(text = element_text(size=15))+
  coord_flip()