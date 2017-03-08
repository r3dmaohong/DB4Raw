dat <- read.csv(file.choose(), stringsAsFactors=F)

names(dat)
names(dat) <- c("dep", 1, 3, 5, 7, 9, "up")

fbarrels <- data.frame("dep"= dat[,1], "year(s)_Interpolation"=0, stringsAsFactors = F)

#Interpolation
for(i in 1:nrow(fbarrels)){
  tmp <- t(dat)[, i] %>% as.data.frame(., stringsAsFactors=F) %>% cbind(., t(dat)[, i] %>% as.data.frame(., stringsAsFactors=F) %>% rownames())
  tmp <- tmp[2:6,]
  names(tmp) <- c("salary", "year")
  
  tmp$year <- as.numeric(as.character(tmp$year))
  
  for(missing in c(2, 4, 6, 8)){
    tmp <- rbind(tmp, c(approx(tmp$year, tmp$salary, xout = missing)$y, missing))
  }
  tmp             <- tmp[order(tmp$year),]
  tmp$salary_year <- tmp$salary %>% as.numeric() *13
  ## Expenditure of one people per month in 2015 : $ 20,421 
  tmp$salary_year <- tmp$salary_year - (20421 * 12)
  tmp$cum_salary  <- tmp$salary_year %>% cumsum
  fbarrels[i, 2]  <- approx(tmp$cum_salary, tmp$year, xout = 1000000)$y
}


write.csv(fbarrels, "output\\學類第一桶金_2016.csv", row.names = F)



