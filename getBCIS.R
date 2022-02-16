# Reference file
require("httr");require("rvest");require("xml2");require("quantmod")

# ticker to get IS
ticker = "AAPL"

# this function will get you the latest quarterly IS (5 quarters)
getLatestIS = function(ticker)
{
  url = paste0("https://www.barchart.com/stocks/quotes/",ticker,
               "/income-statement/quarterly")
  pg = read_html(url)
  
  df = pg %>% html_nodes("table") %>% html_table() %>% as.data.frame()
  VAL = pg %>% html_nodes(xpath=paste0("/html/body/main/div/div[2]/div[2]/",
                                       "div/div[2]/div/div/div/div[3]/div[1]",
                                       "/span/span")) %>% html_text()
  
  if(VAL == "thousands"){VAL = "000"}
  # change 1st value of df - for colname change
  df[1,1] <- "Description"
  colnames(df) <- df[1,]
  # subset df to remove colnames
  df = df[2:nrow(df),]
  
  # get rid of special characters
  df[,2] <- gsub("\\$","",gsub("\\,","",df[,2]))
  df[,3] <- gsub("\\$","",gsub("\\,","",df[,3]))
  df[,4] <- gsub("\\$","",gsub("\\,","",df[,4]))
  df[,5] <- gsub("\\$","",gsub("\\,","",df[,5]))
  df[,6] <- gsub("\\$","",gsub("\\,","",df[,6]))
  
  # select rows only
  ROWS = c("Sales","Cost of Goods","Gross Profit","Operating Expenses",                   
           "Operating Income","Interest Expense","Other Income","Pre-tax Income",
           "Income Tax","Net Income","EPS Basic Total Ops",
           "EPS Diluted Total Ops","Ebitda")
  
  df = lapply(as.list(1:length(ROWS)), function(ii){
    tmp = subset(df, df$Description == ROWS[ii])
    tmp[1,2:ncol(tmp)] = paste0(tmp[1,2:ncol(tmp)],VAL) 
    tmp
  })
  # remove any empty cases
  df = df[lapply(df, length)>0]
  # rbind list
  df = do.call(rbind,df)
  # remove NA row
  INT = which(is.na(df[,1]))
  if(length(INT)!=0){df= df[-INT,]}
  
  # make sure it is data.frame
  df <- as.data.frame(df)
  # convert to numeric
  df[,2:ncol(df)]<- round(sapply(df[,2:ncol(df)], as.numeric),2)
  
  # make sure it is data.frame
  df = as.data.frame(df)
  
  # return data.frame
  df
}
# test function
pg1 = getLatestIS(ticker=ticker)

# this function will get you 9 pages of quarterly IS (45 quarters)
getRestIS = function(ticker)
{
  urls = paste0("https://www.barchart.com/stocks/quotes/",ticker,
                "/income-statement/quarterly?reportPage=",paste(2:10))
  
  pg = lapply(as.list(1:length(urls)), function(ii){
    # 3 second script sleep
    Sys.sleep(3)
    pg = try(read_html(urls[ii]),silent = TRUE)
    if(!inherits(pg,'try-error'))
      pg
  })
  # remove empty lists - removes pages without content
  pg = pg[lapply(pg, length)>0]
  # extract tables
  df = lapply(as.list(1:length(pg)), function(ii){
    
    df = pg[ii][[1]] %>% html_nodes("table") %>% html_table() %>% as.data.frame()
    VAL = pg[ii][[1]] %>% html_nodes(xpath=paste0("/html/body/main/div/div[2]/div[2]/",
                                                  "div/div[2]/div/div/div/div[3]/div[1]",
                                                  "/span/span")) %>% html_text()
    
    if(VAL == "thousands"){VAL = "000"}
    # change 1st value of df - for colname change
    df[1,1] <- "Description"
    colnames(df) <- df[1,]
    # subset df to remove colnames
    df = df[2:nrow(df),]
    
    # get rid of special characters
    df[,2] <- gsub("\\$","",gsub("\\,","",df[,2]))
    df[,3] <- gsub("\\$","",gsub("\\,","",df[,3]))
    df[,4] <- gsub("\\$","",gsub("\\,","",df[,4]))
    df[,5] <- gsub("\\$","",gsub("\\,","",df[,5]))
    df[,6] <- gsub("\\$","",gsub("\\,","",df[,6]))
    
    
    # select rows only
    ROWS = c("Sales","Cost of Goods","Gross Profit","Operating Expenses",                   
             "Operating Income","Interest Expense","Other Income","Pre-tax Income",
             "Income Tax","Net Income","EPS Basic Total Ops",
             "EPS Diluted Total Ops","Ebitda")
    # subset select rows
    df = lapply(as.list(1:length(ROWS)), function(ii){
      tmp = subset(df, df$Description == ROWS[ii])
      tmp[1,2:ncol(tmp)] = paste0(tmp[1,2:ncol(tmp)],VAL) 
      tmp
    })
    # remove any empty cases
    df = df[lapply(df, length)>0]
    # rbind list
    df = do.call(rbind,df)
    # remove NA row
    INT = which(is.na(df[,1]))
    if(length(INT)!=0){df= df[-INT,]}
    
    # make sure it is data.frame
    df <- as.data.frame(df)
    # convert to numeric
    df[,2:ncol(df)]<- round(sapply(df[,2:ncol(df)], as.numeric),2) %>% suppressWarnings()
    # make sure it is data.frame
    df = as.data.frame(df)
    # return data.frame
    df[,2:ncol(df)]
  })
  # combine tables
  df = do.call(cbind,df)
  # return table
  df
}
# test function
PGS = getRestIS(ticker=ticker)
# combine tables
IS = cbind(pg1, PGS)


# add IS ratios
getISRatios = function(IS,ticker)
  {
  # ***************************************************************************************
  #                                     Gross Margin
  # ***************************************************************************************
  GM = round(as.numeric(subset(IS,IS$Description == "Gross Profit"))/
        as.numeric(subset(IS,IS$Description == "Sales")),digits = 4) %>% suppressWarnings()
  # convert to data.frame
  GM = as.data.frame(t(GM))
  # change row description + add colnames
  GM[1,1] = "Gross Margin"; colnames(GM) = names(IS)
  # ***************************************************************************************
  #                                     Profit Margin
  # ***************************************************************************************
  PM = round(as.numeric(subset(IS,IS$Description == "Net Income"))/
               as.numeric(subset(IS,IS$Description == "Sales")),digits = 4) %>% suppressWarnings()
  # convert to data.frame
  PM = as.data.frame(t(PM))
  # change row description + add colnames
  PM[1,1] = "Profit Margin"; colnames(PM) = names(IS)
  # ***************************************************************************************
  #                                     add STK PRC
  # ***************************************************************************************
  # get data convert to quarterly and extract closing prices
  tmp = Cl(to.quarterly(getSymbols(ticker, auto.assign = FALSE, from="2000-01-01")))
  # convert IS - names to Quarterly timestamps
  Qdates = as.yearqtr(names(IS)[2:length(IS)], format="%m-%Y")
  # order is decreasing
  Qdates = Qdates[order(Qdates,decreasing = TRUE)]
  # convert stk prices to as.data.frame
  stk = as.data.frame(t(tmp[Qdates]), row.names = NULL)
  # run through each column to extract stk price at the end of each Quarter
  toR <- as.data.frame(NA)
  for(ii in 2:ncol(IS))
  {
    # converts to yearqtr for each column
    QQ = as.yearqtr(names(IS)[ii], format="%m-%Y")
    # subset desired Quarter price
    prc = try(stk[,paste(QQ)],silent = TRUE)
    if(inherits(prc,'try-error'))
    {
      # add NA if no stk Price is available
      toR = cbind(toR,NA)
    }else{
      # otherwise add stock price
      toR = cbind(toR,prc)  
    }
  }
  # add description
  toR[1,1] = "Stock Price"
  # format colnames
  colnames(toR) = names(IS)
  # ***************************************************************************************
  #                                     add P/E Ratio (wont consider negative earnings)
  # ***************************************************************************************
  AnnualEPS = suppressWarnings(as.numeric(subset(IS,IS$Description == "EPS Basic Total Ops"))*4)
  PE = round(as.numeric(toR)/AnnualEPS,
             digits = 4) %>% suppressWarnings()
  # convert to data.frame
  PE = as.data.frame(t(PE))
  # change row description + add colnames
  PE[1,1] = "PE Ratio"; colnames(PE) = names(IS)
  # ***************************************************************************************
  #                                     Rowbind -> Output
  # ***************************************************************************************
  ALL = rbind(GM,PM,toR,PE)
  
  ALL
}
# test function
RATIOS = getISRatios(IS=IS,ticker=ticker)
# rowbind with IS
IS = rbind(IS,RATIOS)

# write table as csv
write.table(IS,paste0("~/Desktop/",ticker,"_IS.csv"),sep = ",")


# transpose table
ISt = as.data.frame(t(IS))
# write table as csv
write.table(ISt,paste0("~/Desktop/",ticker,"_IST.csv"),sep = ",")

