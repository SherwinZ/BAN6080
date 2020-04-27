rm(list=ls())
###########################

library(tidyverse)

setwd("C:/WFU/Courses/Spring/Financial_Risk_Analysis/Project")

# source("https://raw.githubusercontent.com/ScientistJake/StockScraper.R/master/StockScraper.R")

NASDAQ <- stockhistoricals(stocklist="NASDAQ", start_date = "2016-09-11", end_date = "2017-09-11", verbose = FALSE)

stockhistoricals<-function(stocklist="GOOG", start_date="1970-01-01", end_date=as.Date(Sys.time()), verbose=TRUE, freq = "1mo"){
  
  ## Usage stockhistoricals(stocklist, start_date, end_date, verbose=TRUE)
  ##    stocklist : A vector of stock tickers. Default = GOOG. Can also pass "NYSE", "NASDAQ", or "AMEX" to get those lists.
  ##    start_date : start date in format Year-month-day. Default = "1970-01-01"
  ##    end_date : end date in format Year-month-day. Default = system date
  ##    verbose : Logical. if F, suppresses messages
  
  require(XML)
  require(RCurl)
  require(httr)
  require(readr)
  
  options(warn=-1)
  if(stocklist=="NASDAQ" | stocklist=="NYSE" | stocklist=="AMEX"){
    stocklist <- get_stocklists(stocklist)
    stocklist <- stocklist$Symbol
  }
  options(warn=0)
  
  #86400 is the conversion from days to minutes (how yahoo is counting time)
  #t=1 is 1970-01-01. So any is 86400 minutes per day from 1970-01-01
  start <- as.numeric(as.Date(start_date)-as.Date("1970-01-01"))* 86400
  end <- as.numeric(as.Date(end_date)-as.Date("1970-01-01"))* 86400
  
  #grab a cookie. Try 5 times because sometimes yahoo puts fucking escape characters in the crumb
  tries = 1
  status = 1
  while (tries <= 5 && status !=200){
    url <- paste0("https://finance.yahoo.com/quote/GOOG/history")
    h <- handle(url)
    res <- GET(handle = h)
    response <- content(res, "text")
    cookie <- unlist(strsplit(unlist(res$headers[5]),';'))[1]
    
    #this gets a crumb pair to use. I hate regex
    crumbled = stringr::str_extract(response, '\"CrumbStore\\\":\\{\\\"crumb\\\":\\\"[[:graph:]]+?\\\"\\}')
    crumb <- unlist(strsplit(crumbled,split='"'))[6]
    
    #test them
    testurl <- paste0("https://query1.finance.yahoo.com/v7/finance/download/GOOG?period1=1451606400&period2=1483228800&interval=1d&events=history&crumb=",crumb)
    scraped <- GET(testurl, config(cookie= cookie))
    
    status <- status_code(scraped)
    tries = tries + 1
  }
  
  if (status != 200){
    message("ERROR: Couldn't access Yahoo after 5 tries")
    if (status == 401){
      message("ERROR: The cookie/crumb scrape didn't work...")
    }
    stop("Sorry about that...")
  }
  
  if (verbose == TRUE){
    message("Grabbing Stock data... This takes a while for long stocklists")
  }
  
  stocksdf<- lapply(stocklist,function(x){
    if (verbose == TRUE){
      message(paste0("Downloading ",x))
    }
    capture <- paste0("https://query1.finance.yahoo.com/v7/finance/download/",x,"?period1=",start,"&period2=",end,"&interval=",freq,"&frequency=",freq, "&events=history")
    scraped <- read_csv(capture)
    #the content() call is loud so I suppress messages for now
    suppressMessages(scraped)
  })
  names(stocksdf) <- stocklist
  return(stocksdf)
}

SP500_symbol <- read_csv("list.csv")
SP500_symbol <- SP500_symbol$Symbol
SP500_symbol <- str_replace(SP500_symbol,"\\.","-")

sp_90 <- head(SP500_symbol,90)

sp_500_aval <- SP500_symbol[!SP500_symbol %in% c("OTIS", "CARR")]

# NASDAQ_m <- stockhistoricals("NASDAQ", "2009-11-30", "2020-01-01")

#SP90_p <- stockhistoricals(sp_90, "2009-11-30", "2020-01-01")

#prices <- select(SP90_p[[2]], Date)
#dates <- select(SP90_p[[2]], Date)

for (i in seq_along(sp_90)) {
  print(sp_90[i])
  temp <- dates %>%
    left_join(SP90_p[[i]], by = "Date")
  
  prices[[sp_90[i]]] <- temp$`Adj Close`
}

#sp_180 <- SP500_symbol[91:180]
#sp_270 <- SP500_symbol[181:270]
#sp_360 <- SP500_symbol[271:360]
#sp_450 <- SP500_symbol[361:450]
#sp_500 <- SP500_symbol[451:500]

#SP180_p <- stockhistoricals(sp_180, "2009-11-30", "2020-01-01")
#SP270_p <- stockhistoricals(sp_270, "2009-11-30", "2020-01-01")
#SP360_p <- stockhistoricals(sp_360, "2009-11-30", "2020-01-01")
#SP450_p <- stockhistoricals(sp_250, "2009-11-30", "2020-01-01")
# SP500_p <- stockhistoricals(sp_500, "2009-11-30", "2020-01-01")

###### Scrape SP500 ################
sp_500_aval <- SP500_symbol[!SP500_symbol %in% c("OTIS", "CARR")]

#### CORE CODE
# SP500_p <- stockhistoricals(sp_500_aval, "2009-11-30", "2020-01-01")

prices <- select(SP500_p[[1]], Date)
dates <- select(SP500_p[[1]], Date)

for (i in seq_along(sp_500_aval)) {
  print(sp_500_aval[i])
  temp <- dates %>%
    left_join(SP500_p[[i]], by = "Date")
  
  prices[[sp_500_aval[i]]] <- temp$`Adj Close`
}





################# Append self-select stocks prices ####################
price_appender <- function(stks, scraped, prices, dates){
  for (i in seq_along(stks)) {
    print(stks[i])
    temp <- dates %>%
      left_join(scraped[[i]], by = "Date")
    
    prices[[stks[i]]] <- temp$`Adj Close`
  }
  return(prices)
}


# extra <- c("%5EIRX")
# extra_p <- stockhistoricals(extra, "2009-11-30", "2020-01-01")
# prices <- price_appender(extra, extra_p, prices=prices, dates=dates)

##################### FINAL TIDYING #############

prices_cleaned <- prices%>%
  select_if(~ !any(is.na(.))) %>%
  select(-TT)

prices_cleaned$Date <- as.Date(prices_cleaned$Date)

write_csv(prices_cleaned, "prices.csv")

prices_cleaned<-read_csv("prices.csv")

### GET RETURNS ###################

library(tidyquant)
prices_gathered <-prices_cleaned %>%
  rename(date = Date)%>%
  gather(-date, key = "symbol", value = "adj_close") %>%
  mutate(adj_close = as.numeric(adj_close))

returns <- prices_gathered %>%
  group_by(symbol) %>%
  tq_transmute(select     = adj_close, 
               mutate_fun = periodReturn,
               period = "daily",
               col_rename = "monthly.returns") %>%
  ungroup() %>%
  spread(symbol, monthly.returns)

############ Add rf #########
rf_name <- c("%5EIRX")
rf <- stockhistoricals(rf_name, "2009-11-30", "2020-01-01")


returns$risk_free <- rf[[1]]$`Adj Close`

returns %>%
  filter(date!=as.Date("2009-12-01"))%>%
  write_csv("returns.csv")

# prices <- price_appender(sp_180, SP180_p, prices, dates)
# prices <- price_appender(sp_rest, SP180_p, prices, dates)