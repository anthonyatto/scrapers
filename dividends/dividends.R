###########################
### DIVIDEND.COM SCRAPE ###
###########################

### REMEMBER TO SET WORKING DIRECTORY ###

#####################
### LOAD PACKAGES ###
#####################
require(XML)
require(stringr)
require(data.table)
require(lubridate)
require(quantmod)
require(dplyr)
dividend_lookup <- readRDS("lookup_table.RData")

####################
### LOOKUP TABLE ###
####################

build_lookup_table <- function(){
  
  ### BASE URL ###
  base_url <- "http://www.dividend.com/dividend-stocks/"
  
  ### SECTOR ###
  sector <- readHTMLTable(base_url)$stocksbysector[,1]
  sector_url <- paste0(base_url, tolower(sector) %>% str_replace_all(" ", "-"))
  
  ### INDUSTRY ###
  industry <- lapply(sector_url, function(x){
    readHTMLTable(x)$subcategories[,1]
  })
  names(industry) <- sector
  industry_url <- lapply(industry, function(x){
    tolower(x) %>% str_replace_all(" ", "-") %>% 
      str_replace_all("---", "-") %>% 
      str_replace_all("--", "-") %>% 
      str_replace_all(",","") %>% 
      str_replace_all("&", "and")
  })
  industry_url <- paste0(rep(sector_url, lapply(industry, length)), "/", unlist(industry_url))
  
  ### COMPANY ###
  company <- lapply(industry_url, function(x){
    readHTMLTable(x)$stocktable[,1:2]
  })
  names(company) <- unlist(industry)
  
  company_symbol <- lapply(company, function(x){
    (x[[1]])
  })
  
  company_name <- lapply(company, function(x){
    (x[[2]])
  })
  
  company_url <- lapply(company, function(x){
    tolower(x[[2]]) %>% 
      str_replace_all(" ", "-") %>% 
      str_replace_all("\\.", "") %>% 
      str_replace_all("'", "") %>% 
      str_replace_all("&", "and") %>% 
      str_replace_all(",", "") %>% 
      str_replace_all("\\(", "") %>% 
      str_replace_all("\\)", "") %>% 
      str_replace_all("%", "") %>% 
      str_replace_all("\nadr", "")
  })
  
  company_url <- paste0(rep(industry_url, lapply(company, length)), "/", 
                        tolower(unlist(company_symbol)), "-", unlist(company_url))
  
  ### DIVIDEND LOOKUP ### 
  dividend_lookup <- data.frame(Symbol = unlist(company_symbol), 
                                Company = unlist(company_name), 
                                URL = company_url)
  rownames(dividend_lookup) <- NULL
  dividend_lookup
}
# no need to run code if 'lookup_table.RData' has been loaded
# dividend_lookup <- build_lookup_table()

# only need to save if there is new dividend data available
# saveRDS(dividend_lookup, "lookup_table.RData")

#######################
### LOOKUP FUNCTION ###
#######################

div_dat <- function(SYM){
  output <- readHTMLTable(paste(dividend_lookup[dividend_lookup$Symbol == SYM ,3]))[4]
  output <- as.data.table(output[[1]])
  colnames(output) <- c("PayAmount", "Declared", "ExDiv", "Record", "PayDate", "Qualified", 
                        "PayType", "Frequency")
  output$PayAmount <- as.numeric(str_sub(output$PayAmount, 2, 7))
  output$Declared <- ymd(output$Declared)
  output$ExDiv <- ymd(output$ExDiv)
  output$Record <- ymd(output$Record)
  output$PayDate <- ymd(output$PayDate)
  output
}

# Examples
div_dat("XOM")
div_dat("CVX")

# Analysis set up
div_dat_update <- function(SYM){
  output <- div_dat(SYM)
  output <- output[Declared >= "2007-01-01", -(Qualified:PayType), with = FALSE]
  output[, Dec_Div_Dif:=as.numeric(ExDiv - Declared)][, Dec_Day:=weekdays(Declared, TRUE)]
  # not sure how to load getSymbols data into the function's 'environment'
  getSymbols(SYM)
  output[, Declared_Price:=rev(Cl(SYM[output$Declared]))]
  output
}

exxon <- div_dat("XOM")
getSymbols("XOM")
exxon <- exxon[Declared >= "2007-01-01", -(Qualified:PayType), with = FALSE]
exxon[, Dec_Div_Dif:=as.numeric(ExDiv - Declared)][, Dec_Day:=weekdays(Declared, TRUE)]
exxon[, Declared_Price:=rev(Cl(XOM[exxon$Declared]))]
exxon[, Declared_Price_L1:=rev(Lag(Cl(XOM))[exxon$Declared])]
exxon[, ExDiv_Price:=rev(Cl(XOM[exxon$ExDiv]))]
exxon[, ExDiv_Price_L1:=rev(Lag(Cl(XOM))[exxon$ExDiv])]



