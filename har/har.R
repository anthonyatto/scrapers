######################################
#### SCRAPE COMP INFO OFF HAR.COM ####
######################################

#### PACKAGES
require(httr)
require(rvest)
require(stringr)
require(magrittr)
require(googlesheets)
require(dplyr)

#### FUNCTION FOR SCRAPE

har_api <- function(url) {
  # html stuff
  resp <- GET(url) %>% content("parsed")
  
  # grep stuff for value cleaning
  price <- "[0-9]{3}[,][0-9]{3}"
  date <- "[0-9]{2}[/][0-9]{2}[/][0-9]{4}"
  price.sf <- "[0-9]{3}[.][0-9]{2}"
  sq.ft <- "[0-9]{4}"
  keys <- "[#]*[©]*[:]"
  year <- "[0-9]{4}"
  
  # to_list function to use on each table
  to_list <- function(nodes) {
    # store keys and values
    key <- resp %>% html_nodes(nodes[1]) %>% html_text() %>% str_replace(keys,"")
    value <- resp %>% html_nodes(nodes[2]) %>% html_text()
    value <- value %>% str_replace_all(",","")

    # create data.frame
    df <- as.data.frame(t(value), stringsAsFactors = FALSE)
    colnames(df) <- key
    as.list(df)
  }
  
  # node stuff
  # 1. store [table].nodes as 'key, value' pairs
  # 2. save as variable
  # 3. clean up
  ### general description
  general.nodes <- c(".row-fluid+ .row-fluid .dc_label",
                     ".row-fluid+ .row-fluid .dc_value")
  General <- to_list(general.nodes)
  
  General$`Listing Status` <- General$`Listing Status` %>% str_trim()
  General$Lotsize <- General$Lotsize %>% str_extract(year)
  General$`Building Sqft` <- General$`Building Sqft` %>% str_extract(sq.ft)
  General$Lotsize <- General$Lotsize %>% str_extract(sq.ft)
  General$`Sold Price Range` %>% str_replace_all("[$]","")
  
  ### room/lot dimensions
  ### interior features
  ### exterior features
  ### cost/sqft based on tax value
  ### [current year] harris county appraisal district value & tax history
  apraisal.nodes <- c("#TaxDiv .pt10:nth-child(1) td:nth-child(1)",
                       "#TaxDiv .pt10:nth-child(1) td+ td")
  Apraisal <- to_list(apraisal.nodes)
  
  Apraisal$`Market Land Value` <- Apraisal$`Market Land Value` %>% 
    str_replace("[$]","")
  Apraisal$`Market Improvement Value` <- Apraisal$`Market Improvement Value` %>% 
    str_replace("[$]","")
  Apraisal$`Total Market Value` <- Apraisal$`Total Market Value` %>% 
    str_replace("[$]","")
  
  ### [current year] tax rates
  ### [current year] subdivision facts
  neighborhood.nodes <- c("#neighborhood td:nth-child(1)",
                          "#neighborhood td+ td")
  Neighborhood <- to_list(neighborhood.nodes)
  # grep stuff
  
  # OUTPUT LIST
  list('Price Low' = General$`Sold Price Range` %>% 
         str_extract_all("[0-9]{6}") %>% extract2(1) %>% extract(1) %>% as.numeric(),
       'Price High' = General$`Sold Price Range` %>% 
         str_extract_all("[0-9]{6}") %>% extract2(1) %>% extract(2) %>% as.numeric(),
       'House Type' = General$`Property Type`,
       'Lot Sqft' = ifelse(length(General$Lotsize %>% as.numeric())==0, 
                           "Unknown", General$Lotsize %>% as.numeric()),
       Sqft = General$`Building Sqft` %>% as.numeric(),
       'Land Apraisal' = ifelse(length(Apraisal$`Market Land Value` %>% 
                                         as.numeric())==0, "Go to HCAD", 
                                Apraisal$`Market Land Value` %>% as.numeric()),
       'Improvement Apraisal' = ifelse(length(Apraisal$`Market Improvement Value` %>% 
                                                as.numeric())==0, "Go to HCAD", 
                                       Apraisal$`Market Improvement Value` %>% 
                                         as.numeric()),
       'Total Apraisal' = ifelse(length(Apraisal$`Total Market Value` %>% 
                                          as.numeric())==0, "Go to HCAD", 
                                 Apraisal$`Total Market Value` %>% as.numeric()),
       Beds = General$Bedrooms,
       Baths = General$Baths)
}

welch <- gs_title("606 Welch Analysis")
comps <- welch %>% gs_read("Comps", range = "A1:B10", col_names = TRUE)
comps <- comps %>% mutate('Sale Min' = NA,
           'Sale Max' = NA,
           'House Type' = NA,
           'Lot Sqft' = NA,
           'Building Sqft' = NA,
           'Land Apraisal' = NA,
           'Improvement Apraisal' = NA,
           'Total Apraisal' = NA,
           Beds = NA,
           Baths = NA)

for (i in 1:nrow(comps)) {
  temp <- as.data.frame(har_api(as.character(comps[i,2])), stringsAsFactors = FALSE)
  comps[i, 3:12] <- temp
  print(i)
}

gs_edit_cells(welch, ws = "Comps", input = comps[,3:12], anchor = "C1")

hcad_api <- function(path) {
  url <- modify_url("http://hcad.org", path = path)
  resp <- GET(url) %>% content("parsed")
  
  # node stuff
  # stnum & stname nodes
  resp %>% html_nodes("td:nth-child(2) > input")
  resp2 %>% html_nodes("tr:nth-child(3) td:nth-child(3) input")
  
  # form stuff that doesn't work 
  form <- resp %>% html_nodes("form") %>% extract2(1)
}

"records/Real.asp?search=addr"

