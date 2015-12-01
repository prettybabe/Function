Momentum <- function(Data, Days){
  Data <- as.vector(Data)
  Momentum <- vector()
  for(i in c(1:length(Data))){
    if(i < Days) Momentum[i] <- NA else Momentum[i] <- exp(sum(log1p(Data[(i-Days+1):i]))) - 1
  }
  return(Momentum)
}

Volatility <- function(Data, Days){
  Data <- as.vector(Data)
  Volatility <- vector()
  for(i in c(1:length(Data))){
    if(i < Days) Volatility[i] <- NA else Volatility[i] <- sd(Data[(i-Days+1):i])
  }
  return(Volatility)
}

Average <- function(Data, Days){
  Data <- as.vector(Data)
  Average <- vector()
  for(i in c(1:length(Data))){
    if(i < Days) Average[i] <- NA else Average[i] <- mean(Data[(i-Days+1):i])
  }
  return(Average)
}

Sum <- function(Data, Days){
  Data <- as.vector(Data)
  Sum <- vector()
  for(i in c(1:length(Data))){
    if(i < Days) Sum[i] <- NA else Sum[i] <- sum(Data[(i-Days+1):i])
  }
  return(Sum)
}

Fill <- function(Data, Days){
  Data <- as.vector(Data)
  Fill <- Data
  for(i in c(1:length(Data))){
    if(is.na(Fill[i])) Fill[i] <- mean(Data[(i-Days):(i-1)])
  }
  return(Fill)
}

AdjustDate <- function(TradingDate, InfoPublicDate){
  InfoPublicDate <- as.vector(InfoPublicDate)
  NewDate <- vector()
  for(i in c(1:length(InfoPublicDate))){
    temp <- TradingDate[TradingDate<=InfoPublicDate[i]]
    NewDate[i] <- temp[length(temp)]
  }
  return(NewDate)
}

AdjustUSA <- function(China, USA){
  NewDate <- vector()
  for(i in c(1:length(China))){
    temp <- USA[USA[, 1] <= China[i], ]
    NewDate[i] <- temp[nrow(temp), 2]
  }
  return(NewDate)
}

Score <- function(Data, ScoreNumber, IsMinusMean = 1){
  Data <- as.vector(Data)
  Score <- vector()
  for(i in c(1:length(Data))){
    if(i >= ScoreNumber){
      if(sum(is.na(Data[(i-ScoreNumber+1):i])) == 0){
        if(IsMinusMean == 1){
          Score[i] <- (Data[i] - mean(Data[(i-ScoreNumber+1):i]))/sd(Data[(i-ScoreNumber+1):i])
        }else{
          Score[i] <- Data[i]/sd(Data[(i-ScoreNumber+1):i])
        }
      }
    }else  Score[i] <- NA
  }
  return(Score)
}

Count <- function(Data, Index, SumNumber = 8){
  count <- ifelse(Data*Index > 0, 1, 0)
  sumcount <- vector()
  for(i in c(SumNumber:length(count))){
      sumcount[i] <- sum(count[(i-SumNumber+1):i])
  }
  return(sumcount)
}

PlotCumlateReturn_rchart <- function(Data){
  names(Data) <- c("Date", "variable", "value")
  a <- Data %>%
    group_by(variable) %>%
    arrange(Date) %>%
    mutate(CumulateReturn = expm1(cumsum(log1p(value))),
           Date = as.numeric(as.POSIXct(Date))*1000) %>%
    ungroup()
  
  p <- hPlot(x = "Date", y = "CumulateReturn", data = a , type = "line", group = "variable")
  p$xAxis(type = 'datetime', labels = list(format = '{value:%Y-%M-%d}' ))
  return(p)
}
PlotCumlateReturn_ggplot <-function(Data){
  names(Data) <- c("Date", "variable", "value")
  a <- Data %>%
    group_by(variable) %>%
    arrange(Date) %>%
    mutate(CumulateReturn = expm1(cumsum(log1p(value)))) %>%
    ungroup()
  
  p <- ggplot(a, aes(x = Date, y = CumulateReturn, color = variable)) + 
    geom_line() + xlab(NULL) + ylab(NULL)
  return(p)
}

YearlyReturn <- function(Data, kNumber){
  Return = exp(mean(log1p(Data))*kNumber) - 1
  return(Return)
}
YearlySD <- function(Data, kNumber){
  SD = sd(Data)*sqrt(kNumber)
  return(SD)
}
YearlyIR <- function(Data, kNumber){
  Return = exp(mean(log1p(Data))*kNumber) - 1
  SD = sd(Data)*sqrt(kNumber)
  IR = Return/SD
  return(IR)
}

TotalPerformance <- function(Data, kNumber){
   output <- data.frame(Return = expm1(mean(log1p(Data))*kNumber),
              SD = sd(Data)*sqrt(kNumber)) %>%
     mutate(IR = Return/SD, MaxDrowdown = MaxDropDownRatio(Data))
  return(output)
}

YearPerformance <- function(Data, kNumber){
  temp <- lapply(Data, TotalPerformance, kNumber)
  output <- as.data.frame(lapply(temp, unlist))
  return(output)
}

Probably <- function(Data){
  temp <- ifelse(Data > 0, 1, 0) 
  count <- data.frame(AllNumber = length(temp), RightNumber = sum(temp)) %>%
    mutate(WinPro = RightNumber/AllNumber)
  return(count)
}

Performance <- function(Data, kNumber){
  temp <- melt(Data, id = "Date") %>% mutate(Year = format(Date, "%Y")) %>%
    group_by(Year, variable) %>%
    summarise(Return = exp(mean(log1p(value))*kNumber) - 1,
              SD = sd(value)*sqrt(kNumber),
              IR = Return/SD, 
              MaxDrowdown = MaxDropDownRatio(value)) %>%
    ungroup()
  
  p1 <- hPlot(x = "Year", y = "Return", group = "variable", data = temp, type = "column")
  p2 <- hPlot(x = "Year", y = "SD", group = "variable", data = temp, type = "column")
  p3 <- hPlot(x = "Year", y = "IR", group = "variable", data = temp, type = "column")
  p4 <- hPlot(x = "Year", y = "MaxDrowdown", group = "variable", data = temp, type = "column")

  print(p1)
  print(p2)
  print(p3)
  print(p4)
}

MaxDropDownRatio <-  function(returns){
  maxdropdown <- 0
  cum.return <- cumsum(log1p(returns))
  kNumber <- length(cum.return)
  if(maxdropdown > cum.return[1]) maxdropdown <- cum.return[1]
  
  for(i in c(2:kNumber)){
    temp <- cum.return[i] - max(cum.return[1:i])
    if(maxdropdown >  temp) maxdropdown <- temp
  }
  maxdropdown <- exp(maxdropdown) - 1
  return(maxdropdown)
}

Show <- function(forecast, index, date, knumber = 52){
  names(forecast) <- c("Date", "Score")
  names(index) <- c("Date", "Return")
  names(date) <- c("Date", "ForecastDate")
  
  returns <- forecast %>%
    filter(!is.na(Score)) %>%
    left_join(date, by = "Date") %>%
    left_join(index, by = c("ForecastDate" = "Date")) %>%
    arrange(Date) %>%
    mutate(LongOnly = ifelse(Score > 0, Return, 0),
           LongShort = ifelse(Score > 0, Return, -Return)) %>%
    filter(!is.na(Return))
  
  
  p <- PlotCumlateReturn(returns %>% select(Date = ForecastDate, Index = Return, LongOnly, LongShort))
  print(p)
  a <- YearPerformance(returns %>% select(Index = Return, LongOnly, LongShort), 52)
  cat("Summary\n\n")
  print(a)
  cat("\n\n")
  
  temp <- returns %>% 
    mutate(Forecast = Score * Return) %>%
    select(Forecast)
  
  cat("WinProbit\n\n")
  print(Probably(temp))
  cat("\n\n")
  
  
  Year <- returns %>%
    select(Date = ForecastDate, Return, LongOnly, LongShort) %>%
    mutate(Year = format(Date, "%Y")) %>%
    select(-Date)
  
  Year <- melt(Year, id = "Year")
  YearReturn <- Year %>% 
    group_by(Year, variable) %>%
    summarise(Return = YearlyReturn(value, 52)) %>%
    ungroup()
  YearReturn <- dcast(YearReturn, Year ~ variable) %>%
    rename(Index = Return)

  YearIR <- Year %>% 
    group_by(Year, variable) %>%
    summarise(IR = YearlyIR(value, 52)) %>%
    ungroup()
  YearIR <- dcast(YearIR, Year ~ variable) %>%
    rename(Index = Return)
  
  cat("Return\n\n")
  print(YearReturn)
  cat("\n\n")
  cat("IR\n\n")
  print(YearIR)
  cat("\n\n")
 
  Performance(returns %>%  select(Date = ForecastDate, Return, LongOnly, LongShort), knumber)
}

PerformanceCompare <- function(forecast, index, date, knumber = 52){
  names(index) <- c("Date", "Return")
  names(date) <- c("Date", "ForecastDate")
  
  LongOnly <- function(forecast, Return){
    temp <- data.frame(Forecast = forecast, Return = Return)
    returns <- temp %>%
      mutate(LongOnly = ifelse(Forecast > 0, Return, 0))
    return(returns$LongOnly)
  }
  
  LongShort <- function(forecast, Return){
    temp <- data.frame(Forecast = forecast, Return = Return)
    returns <- temp %>%
      mutate(LongShort = ifelse(Forecast > 0, Return, -Return))
    return(returns$LongShort)
  }
  
  Win <- function(forecast, index){
    temp <- data.frame(Forecast = forecast, Index = index) %>%
      filter(Forecast != 0) %>%
      mutate(IsRight = ifelse(Forecast * Index > 0, 1, 0))
    temp <- temp$IsRight
    winpro <- sum(temp)/length(temp)
    return(winpro)
  }
  

  temp <- forecast %>%
    left_join(date, by = "Date") %>%
    left_join(index, by = c("ForecastDate" = "Date")) 
  temp <- na.omit(temp)
  
  
  temp_forecast <- temp %>% select(-Date, -ForecastDate, -Return) 
  temp_index <- temp$Return
  longonly <- data.frame(Date = temp$ForecastDate,
                         as.data.frame(lapply(temp_forecast, LongOnly, temp_index)))
  
  longshort <- data.frame(Date = temp$ForecastDate,
                          as.data.frame(lapply(temp_forecast, LongShort, temp_index)))
  
  winpro <- as.data.frame(lapply(temp_forecast, Win, temp_index))
  
  year_return_longonly <- as.data.frame(lapply(longonly %>% select(-Date), YearlyReturn, knumber))
  year_sd_longonly <- as.data.frame(lapply(longonly %>% select(-Date), YearlySD, knumber))
  year_ir_longonly <- as.data.frame(lapply(longonly %>% select(-Date), YearlyIR, knumber))
  
  a <- rbind(year_return_longonly, year_sd_longonly, year_ir_longonly)
  row.names(a) <- c("Return", "SD", "IR")
  
  
  year_return_longshort <- as.data.frame(lapply(longshort %>% select(-Date), YearlyReturn, knumber))
  year_sd_longshort <- as.data.frame(lapply(longshort %>% select(-Date), YearlySD, knumber))
  year_ir_longshort <- as.data.frame(lapply(longshort %>% select(-Date), YearlyIR, knumber))
  b <- rbind(year_return_longshort, year_sd_longshort, year_ir_longshort)
  row.names(b) <- c("Return", "SD", "IR")
  
  p1 <- PlotCumlateReturn(longonly)
  p1$title(text = "LongOnly")
  print(p1)
  p2 <- PlotCumlateReturn(longshort)
  p2$title(text = "LongShort")
  print(p2)
  
  cat("WinPro\n")
  print(winpro)
  
  cat("\n")
  cat("LongOnly\n")
  print(a)
  
  cat("\n")
  cat("LongShort\n")
  print(b)
}

DailyReturnToMonthReturn <- function(Data){
  # Data format  Date DailyReturn
  names(Data) <- c("Date", "Return") 
  MonthReturn <- Data %>%
    mutate(Month = format(Date, "%Y%m")) %>%
    group_by(Month) %>%
    arrange(Date) %>%
    mutate(Date = last(Date)) %>%
    group_by(Date) %>%
    summarise(Return = expm1(sum(log1p(Return))))
  return(MonthReturn)
}

Rsquare <- function(IndexCode, Date){
  options(java.parameters="-Xmx4g")
  library(dplyr)
  library(lubridate)
  library(RSQLServer)
  
  channel <- src_sqlserver(server="SQL",database="XY",user="libo.jin",password="123456")
  
  data <- list()
  data$IndexComponent <- tbl(channel, "LC_IndexComponent") %>%
    select(IndexInnerCode, SecuInnerCode, InDate, OutDate) %>%
    filter(IndexInnerCode == IndexCode) %>%
    collect %>%
    mutate(InDate = as.Date(InDate), OutDate = as.Date(OutDate)) %>%
    filter(InDate <= Date, OutDate > Date | is.na(OutDate)) %>%
    select(SecuInnerCode)
  
  StartDay <- tbl(channel, "QT_TradingDayNew") %>%
    filter(TradingDate <= Date, IfTradingDay == 1, SecuMarket == 83) %>%
    select(TradingDate) %>%
    arrange(desc(TradingDate)) %>%
    collect %>%
    slice(20) %>%
    mutate(TradingDate = as.Date(TradingDate))
  StartDay <- StartDay$TradingDate
  
  data$IndexReturn <- tbl(channel,"QT_IndexQuote") %>%
    filter(InnerCode == IndexCode, TradingDay <= Date & TradingDay >= StartDay) %>%
    select(TradingDay, ClosePrice, PrevClosePrice) %>% 
    collect %>%
    mutate(TradingDay = as.Date(TradingDay), Return = ClosePrice/PrevClosePrice - 1) %>%
    select(TradingDay, Return)
  
  data$StocksReturn <- tbl(channel,"QT_DailyQuote") %>%
    filter(TradingDay >= StartDay & TradingDay <= Date, InnerCode %in% data$IndexComponent$SecuInnerCode) %>%
    select(TradingDay, InnerCode, PrevClosePrice, ClosePrice) %>%
    collect %>%
    group_by(InnerCode) %>% 
    arrange(TradingDay) %>%
    mutate(TradingDay = as.Date(TradingDay), Return = ClosePrice/PrevClosePrice - 1) 
  
  Temp <- function(x, y){a <- summary(lm(x ~ y))$r.squared}
  rsquare <- data$StocksReturn %>%
    left_join(data$IndexReturn, by = "TradingDay") %>%
    group_by(InnerCode) %>%
    filter(!n()<20, !any(Return.x == 0), !is.na(Return.x)) %>%
    summarise(Rsquare = Temp(Return.x, Return.y)) %>%
    summarise(Rsquare = mean(Rsquare))  
  
  return(rsquare$Rsquare)
} 

EP <- function(IndexCode, Date){
  options(java.parameters="-Xmx4g")
  library(dplyr)
  library(lubridate)
  library(RSQLServer)
  
  channel <- src_sqlserver(server="SQL",database="XY",user="libo.jin",password="123456")
  data <- list()
  data$IndexComponent <- tbl(channel, "LC_IndexComponent") %>%
    select(IndexInnerCode, SecuInnerCode, InDate, OutDate) %>%
    filter(IndexInnerCode == IndexCode) %>%
    collect %>%
    mutate(InDate = as.Date(InDate), OutDate = as.Date(OutDate)) %>%
    filter(InDate <= Date, OutDate > Date | is.na(OutDate)) %>%
    select(SecuInnerCode)
  
  data$SecuMain <- tbl(channel, "SecuMain") %>%
    filter(InnerCode %in% data$IndexComponent$SecuInnerCode) %>%
    select(InnerCode, CompanyCode, SecuCode, SecuAbbr) %>%
    collect 
  
  
  data$StocksPrice <- tbl(channel,"ReturnDaily") %>%
    filter(DataDate == Date, InnerCode %in% data$IndexComponent$SecuInnerCode) %>%
    select(InnerCode, TotalShares, ClosePrice, NonRestrictedShares ) %>%
    collect 
  
   temp <- format(Date,"%Y%m%d")
  data$StocksEarning <- tbl(channel, "TTM_LC_IncomeStatementAll") %>% 
    filter(DataDate == temp, SecuCode %in% data$SecuMain$SecuCode) %>%
    select(SecuCode, NPParentCompanyOwners) %>%
    collect 

    
  ep <- data$StocksPrice %>%
    left_join(data$SecuMain %>% select(InnerCode, SecuCode), by = "InnerCode") %>%
    left_join(data$StocksEarning, by = "SecuCode") %>%
    mutate(EPS = NPParentCompanyOwners/ClosePrice/TotalShares, FloatMarketCap = NonRestrictedShares * ClosePrice) %>%
    summarise(EP_FloatMarketCapWeighted =  weighted.mean(EPS, FloatMarketCap),
              EP_sumE_sum_marketcap = sum(NPParentCompanyOwners)/sum(TotalShares*ClosePrice))
    
  return(ep)
}

BP <- function(IndexCode, Date){
  options(java.parameters="-Xmx4g")
  library(dplyr)
  library(lubridate)
  library(RSQLServer)
  
  channel <- src_sqlserver(server="SQL",database="XY",user="libo.jin",password="123456")
  data <- list()
  data$IndexComponent <- tbl(channel, "LC_IndexComponent") %>%
    select(IndexInnerCode, SecuInnerCode, InDate, OutDate) %>%
    filter(IndexInnerCode == IndexCode) %>%
    collect %>%
    mutate(InDate = as.Date(InDate), OutDate = as.Date(OutDate)) %>%
    filter(InDate <= Date, OutDate > Date | is.na(OutDate)) %>%
    select(SecuInnerCode)
  
  data$SecuMain <- tbl(channel, "SecuMain") %>%
    filter(InnerCode %in% data$IndexComponent$SecuInnerCode) %>%
    select(InnerCode, CompanyCode, SecuCode, SecuAbbr) %>%
    collect 
  
  
  data$StocksPrice <- tbl(channel,"ReturnDaily") %>%
    filter(DataDate == Date, InnerCode %in% data$IndexComponent$SecuInnerCode) %>%
    select(InnerCode, TotalShares, ClosePrice) %>%
    collect 
  
  data$StocksBookValue <- tbl(channel, "LC_BalanceSheetAll") %>% 
    filter(InfoPublDate <= Date, CompanyCode %in% data$SecuMain$CompanyCode) %>%
    group_by(CompanyCode) %>%
    arrange(desc(InfoPublDate)) %>%
    slice(1) %>%
    select(CompanyCode, TotalShareholderEquity) %>%
    collect %>%
    ungroup()
  
  
  bp <- data$StocksPrice %>%
    left_join(data$SecuMain %>% select(InnerCode, CompanyCode), by = "InnerCode") %>%
    left_join(data$StocksBookValue, by = "CompanyCode") %>%
    summarise(BP <- sum(TotalShareholderEquity)/sum(TotalShares*ClosePrice))
  
  return(ep$BP)
}

Score <- function(Data, half_life){
  Data <- as.vector(Data)
  
  Score <- vector()
  
  ExponentialSmooth <- function(x, lambda){
    ans <- x
    oneml <- 1 - lambda
    for(i in 2:length(x)) {
      ans[i] <- lambda * ans[i-1] + oneml * x[i]
    }
      
    return(ans)
  }
  
  for(i in c(1:length(Data))){
    if(i > half_life){
      lambda <- 1 - log(2)/(i - half_life)
      temp <- ExponentialSmooth(Data[1:i], lambda)
      Score[i] <- (Data[i] - temp[i])/sd(Data[1:i])
    }else Score[i] <- NA   
  }
  return(Score)
}

Order <- function(data){
  temp <- data.frame(RawDate = data, Rank = percent_rank(desc(data)))
  order <- temp %>%
    mutate(Order = ifelse(Rank <= 1/5, "First",
                          ifelse(Rank > 1/5 & Rank <= 2/5, "Second",
                                 ifelse(Rank > 2/5 & Rank <= 3/5, "Third",
                                        ifelse(Rank > 3/5 & Rank <= 4/5, "Fourth", "Fifth"))))) %>%
    mutate(Order = factor(Order,order = TRUE, levels = c("First", "Second", "Third", "Fourth", "Fifth")))
  return(order$Order)
}

IndustryShow <- function(industry_value, half_life, valuename){
  
  names(industry_value) <- c("TradingDay", "IndustryName","IndustryValue",
                             "IndustryReturn", "FloatMarketCap")
  
  portfolio <-a <- industry_value %>%
    group_by(IndustryName) %>%
    arrange(TradingDay) %>%
    mutate(IndustryValueScore =  Score(IndustryValue, half_life)) %>%
    filter(!is.na(IndustryValueScore)) %>%
    group_by(TradingDay) %>%
    mutate(Order = Order(IndustryValueScore)) %>% 
    group_by(TradingDay, Order) %>%
    summarise(Return = weighted.mean(IndustryReturn, FloatMarketCap)) %>%
    group_by(Order) %>%
    arrange(TradingDay) %>%
    mutate(CumulateReturn = expm1(cumsum(log1p(Return)))) %>%
    ungroup() 
  
  
  print(ggplot(portfolio, aes(x = TradingDay, y =  CumulateReturn, color = Order)) + geom_line() +
          ggtitle(paste(valuename, half_life)) +
          xlab(NULL) + ylab(NULL))
}

IndustryReturn <- function(data, nIndexCode, startdate, enddate, frequency){
  
  if(frequency == "Monthly"){
    trading_date <- data$TradingDay %>%
      filter(IfMonthEnd == 1)
  }else{
    trading_date <- data$TradingDay %>%
      filter(IfWeekEnd == 1)
  }
  
  trading_date <- trading_date %>% 
    filter(TradingDate >= startdate & TradingDate <= enddate) %>%
    select(TradingDate) %>%
    mutate(Start = lag(TradingDate)) %>%
    rename(End =  TradingDate) %>%
    select(Start, End) %>%
    na.omit()
  
  industry_return <- data.frame()
  for(i in c(1:nrow(trading_date))){
    start <- trading_date[[i, 1]]
    end <- trading_date[[i, 2]]
    temp <- data$ReturnDaily %>%
      filter(TradingDay == start) %>% # 筛选日期
      semi_join(data$IndexComponent %>% 
                  filter(IndexInnerCode == nIndexCode, start >= InDate & start < OutDate),
                by = c("InnerCode" = "SecuInnerCode"))
    
    industry_return_temp <- temp %>%
      filter(IfSuspended == 0) %>%
      inner_join(data$ReturnDaily %>% 
                   filter(TradingDay > start & TradingDay <= end) %>%
                   select(InnerCode, DailyReturn), by = "InnerCode") %>% 
      group_by(InnerCode, FloatMarketCap, IndustryName) %>% 
      summarise(StockReturn = expm1(sum(log1p(DailyReturn.y)))) %>%
      group_by(IndustryName) %>% 
      summarise(IndustryReturn = weighted.mean(StockReturn, FloatMarketCap),
                FloatMarketCap = sum(FloatMarketCap)) %>%
      ungroup() %>% 
      mutate(TradingDay = start)
    
    industry_return <- rbind(industry_return, industry_return_temp)
  }
  
  return(industry_return)
}

IndexReturn <- function(data, startdate, enddate, frequency){
  if(frequency == "Monthly"){
    trading_date <- data$TradingDay %>%
      filter(IfMonthEnd == 1)
  }else{
    trading_date <- data$TradingDay %>%
      filter(IfWeekEnd == 1)
  }
  
  trading_date <- trading_date %>% 
    filter(TradingDate >= startdate & TradingDate <= enddate) %>%
    select(TradingDate) %>%
    mutate(Start = lag(TradingDate)) %>%
    rename(End =  TradingDate) %>%
    select(Start, End) %>%
    na.omit()
  
  
  index_return <- data.frame()
  for(i in c(1:nrow(trading_date))){
    start <- trading_date[[i, 1]]
    end <- trading_date[[i, 2]]
    index_return_temp <- data$SecuMainIndex %>% 
      filter(TradingDay > start & TradingDay <= end) %>% 
      group_by(InnerCode) %>% 
      summarise(IndexReturn = expm1(sum(log(ClosePrice/PrevClosePrice)))) %>% 
      mutate(TradingDay = end)
    
    index_return <- rbind(index_return, index_return_temp)
  }
  
  return(index_return)
}

TradingDay <- function(data, startdate, enddate, frequency){
  
  if(frequency == "Monthly"){
    trading_date <- data$TradingDay %>%
      filter(IfMonthEnd == 1)
  }else{
    trading_date <- data$TradingDay %>%
      filter(IfWeekEnd == 1)
  }
  
  trading_date <- trading_date %>% 
    filter(TradingDate >= startdate & TradingDate <= enddate) %>%
    select(TradingDate) %>%
    mutate(Start = lag(TradingDate)) %>%
    rename(End =  TradingDate) %>%
    select(Start, End) %>%
    na.omit()
  
  return(trading_date)
}