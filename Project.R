library(tidyquant)
library(dplyr)
library(stargazer)

#Change WD 
setwd('/Users/oliverreidmiller/Desktop/Empirical Finance/Project')

getPrices <- function(tickers,start,end){
  coefficients_df <- data.frame(
    ticker = character(),
    Model = character(),
    Intercept = numeric(),
    Mkt.RF = numeric(),
    SMB = numeric(),
    HML = numeric(),
    RMW = numeric(),
    CMA = numeric(),
    Mom = numeric(),
    stringsAsFactors = FALSE 
  )
  
  for (i in tickers){
    df <- assetpricing(i,start,end)
    
    coefficients_df <- rbind(coefficients_df,df)
  }
  return(coefficients_df)
}

getTickerData <- function(ticker,beg_date, end_date){
#  ticker= 'AMZN'
# beg_date = '2020-01-01'
#  end_date = '2022-01-01'
  
  
  
  
  tickerDf <- tq_get(ticker, from = beg_date, to = end_date)
  tickerMonthlyDf <- tq_transmute(tickerDf, 
                                  select = adjusted,
                                  mutate_fun = periodReturn,
                                  period = "monthly",
                                  col_rename = "Log_Returns",
                                  type = "log")
  
  tickerMonthlyDf$date <- format(tickerMonthlyDf$date, "%Y-%m")
  
  
  
  ff_df1 <- read.csv('F-F_Research_Data_5_Factors_2x3.csv')
  
  year <- substr(ff_df1$X, 1, 4)
  month <- substr(ff_df1$X, 5, 6)
  ff_df1$date <- paste(year, month,'01', sep = "-")
  ff_df1$date <- as.Date(ff_df1$date)
  ff_df1$date <- format(ff_df1$date, "%Y-%m")
  
  
  
  ff_df2 <- read.csv('F-F_Momentum_Factor.CSV')
  
  year <- substr(ff_df2$Date, 1, 4)
  month <- substr(ff_df2$Date, 5, 6)
  ff_df2$date <- paste(year, month,'01', sep = "-")
  ff_df2$date <- as.Date(ff_df2$date)
  ff_df2$date<- format(ff_df2$date, "%Y-%m")
  
  ff_df <- merge(ff_df1, ff_df2, by = 'date')
  
  merged_df <- merge (tickerMonthlyDf, ff_df, by='date')
  merged_df$Ticker <- ticker
  
  merged_df$Mkt.RF<- as.numeric(merged_df$Mkt.RF)  
  merged_df$SMB<- as.numeric(merged_df$SMB)
  merged_df$HML<- as.numeric(merged_df$HML)
  merged_df$RMW<- as.numeric(merged_df$RMW)
  merged_df$CMA<- as.numeric(merged_df$CMA)
  merged_df$RF<- as.numeric(merged_df$RF)
  merged_df$Mom<- as.numeric(merged_df$Mom)
  
  
  return(merged_df)
}

getIndustryTickersData <- function(tickers, beg_date, end_date) {
  industry_df <- data.frame()
  for (ticker in tickers) {
    print(ticker)
    ticker_df <- getTickerData(ticker, beg_date, end_date)
    industry_df <- rbind(industry_df, ticker_df)
  }
  return(industry_df)
}

addElectionYears <- function(df){
  
  df$year <-as.numeric(substr(df$date, start = 1, stop = 4)) 
  
  
  # Create a dummy variable for election years (every 4th year)
  df$election_year <- ifelse(df$year %% 4 == 0, 1, 0)
  
    df$Republican <- ifelse(df$year %in% c(2004:2008, 2016:2020), 1, 0)
    return(df)
  }

start = '2004-01-01'
end = '2024-01-01'


# Large Caps --------------------------------------------------------------
techTickers = c("MSFT", "AAPL", "NVDA", "TSM", "AVGO")
healthcareTickers <- c("LLY", "NVO", "UNH", "JNJ", "MRK")
financeTickers <- c("BRK-B", "JPM", "V", "BAC", "MA")
retailTickers <- c("AMZN", "HD", "MCD", "BABA", "PDD")
automotiveTickers <- c("TM", "HMC", "GM", "F", "VWAGY", "BMWYY")
energyTickers <- c('XOM','CVX','SHEL','TTE','COP') 

tech <- getIndustryTickersData(techTickers,start,end)
health <- getIndustryTickersData(healthcareTickers,start,end)
finance <- getIndustryTickersData(financeTickers,start,end)
retail <-getIndustryTickersData(retailTickers,start,end)
automotive <- getIndustryTickersData(automotiveTickers,start,end)
energy <- getIndustryTickersData(energyTickers,start,end)


tech2<- addElectionYears(tech)
health2<- addElectionYears(health)
finance2<- addElectionYears(finance)
retail2<- addElectionYears(retail)
automotive2 <- addElectionYears(automotive)
energy2 <- addElectionYears(energy)

#run Regressions = regressions without party
runRegressions <- function(model_data, industry_name) {
  capm <- lm(Log_Returns ~ Mkt.RF + election_year , data = model_data)
  ff3 <- lm(Log_Returns ~ Mkt.RF + SMB + HML + election_year, data = model_data)
  ff4 <- lm(Log_Returns ~ Mkt.RF + SMB + HML + Mom + election_year, data = model_data)
  ff5 <- lm(Log_Returns ~ Mkt.RF + SMB + HML + RMW + CMA + election_year, data = model_data)
  
  # Write regression results to file
  output_file <- paste(industry_name, ".htm", sep = "")
  
  stargazer(capm, ff3, ff4, ff5, title = industry_name, out = output_file,
            column.labels = c("CAPM", "FF3", "FF4", "FF5"),
            covariate.labels = c("Mkt RF", "SMB", "HML", "MOM", "RMW", "CMA", "Election Year"),
            align = TRUE)
}


#runRegressions2 = regressions with party and interation
runRegressions2 <- function(model_data, industry_name) {
  capm <- lm(Log_Returns ~ Mkt.RF + election_year + Republican + election_year:Republican, data = model_data)
  ff3 <- lm(Log_Returns ~ Mkt.RF + SMB + HML + election_year + Republican + election_year:Republican, data = model_data)
  ff4 <- lm(Log_Returns ~ Mkt.RF + SMB + HML + Mom + election_year + Republican + election_year:Republican, data = model_data)
  ff5 <- lm(Log_Returns ~ Mkt.RF + SMB + HML + RMW + CMA + election_year + Republican + election_year:Republican, data = model_data)
  
  # Write regression results to file
  output_file <- paste(industry_name,'2', ".htm", sep = "")
  
  stargazer(capm, ff3, ff4, ff5, title = industry_name, out = output_file,
            column.labels = c("CAPM", "FF3", "FF4", "FF5"),
            covariate.labels = c("Mkt RF", "SMB", "HML", "MOM", "RMW", "CMA", "Election Year", "Republican",'Election Year * Republican'),
            align = TRUE)
}

runRegressions(tech2, "Tech")
runRegressions(health2, "Healthcare")
runRegressions(finance2, "Finance")
runRegressions(retail2, "Retail")
runRegressions(automotive2, "Automotive")
runRegressions(energy2, 'Energy')


runRegressions2(tech2, "Tech")
runRegressions2(health2, "Healthcare")
runRegressions2(finance2, "Finance")
runRegressions2(retail2, "Retail")
runRegressions2(automotive2, "Automotive")
runRegressions2 (energy2,'Energy')


# Small Cap ---------------------------------------------------------------
smallCap_tech_tickers <- c("MXL", "QRVO", "MCHP", "ON", "RMBS", "SLAB", "UI", "NVDA", "LOGI")
smallCap_healthcare_tickers <- c("ABMD", "ACAD", "ALKS", "EXAS", "ICLR", "LNTH", "NBIX", "UTHR", "VRTX")
smallCap_finance_tickers <- c("BOH", "CBOE", "CINF", "DFS", "FHN", "HOMB", "MKTX", "PFG", "STI", "WAL")
smallCap_retail_tickers <- c("BKE", "BOOT", "FIVE", "LOCO", "RH", "SKX", "TSCO", "ZUMZ")
smallCap_automotive_tickers <- c("ALV", "BWA", "DAN", "GNTX", "LKQ", "MPAA", "VC", "WNC")
smallCap_energy_tickers <- c( "CPE", "AR", "SM", "CHK", "MRO", "MTDR","GTE","VNOM")


smallCap_tech <- getIndustryTickersData(smallCap_tech_tickers,start,end)
smallCap_health <- getIndustryTickersData(smallCap_healthcare_tickers,start,end)
smallCap_finance <- getIndustryTickersData(smallCap_finance_tickers,start,end)
smallCap_retail <-getIndustryTickersData(smallCap_retail_tickers,start,end)
smallCap_automotive <- getIndustryTickersData(smallCap_automotive_tickers,start,end)
smallCap_energy <- getIndustryTickersData(smallCap_energy_tickers,start,end)


smallCap_tech2<- addElectionYears(smallCap_tech)
smallCap_health2<- addElectionYears(smallCap_health)
smallCap_finance2<- addElectionYears(smallCap_finance)
smallCap_retail2<- addElectionYears(smallCap_retail)
smallCap_automotive2 <- addElectionYears(smallCap_retail)
smallCap_energy2 <- addElectionYears(smallCap_energy)


runRegressions(smallCap_tech2, "Small Cap Tech")
runRegressions(smallCap_health2, "Small Cap Healthcare")
runRegressions(smallCap_finance2, "Small Cap Finance")
runRegressions(smallCap_retail2, "Small Cap Retail")
runRegressions(smallCap_automotive2, "Small Cap Automotive")
runRegressions(smallCap_energy2, "Small Cap Energy")


runRegressions2(smallCap_tech2, "Small Cap Tech")
runRegressions2(smallCap_health2, "Small Cap Healthcare")
runRegressions2(smallCap_finance2, "Small Cap Finance")
runRegressions2(smallCap_retail2, "Small Cap Retail")
runRegressions2(smallCap_automotive2, "Small Cap Automotive")
runRegressions2(smallCap_energy2, "Small Cap Energy")








