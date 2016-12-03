###########################
### DIVIDEND.COM SCRAPE ###
###########################

### REMEMBER TO SET WORKING DIRECTORY ###

#####################.l
### LOAD PACKAGES ###
#####################
require(XML)
require(stringr)
require(plyr)
require(dplyr)
require(rvest)
require(map)

####################
### LOOKUP TABLE ###
####################

# build_lookup_table <- function(x){
  
  ### CITIES ###
  cities_url <- "http://www.golfcalifornia.com/courses/"
  cities_page <- read_html(cities_url)
  cities_nodes <- html_nodes(cities_page, "a")
  
  cities_names <- cities_nodes %>% html_text() %>% str_subset("Golf courses in") %>% 
    str_replace("Golf courses in ", "") %>% str_replace(", CA", "")
  
  cities_links <- cities_nodes %>% html_attr("href")
  start <- which(str_detect(cities_links, cities_names[1] %>% tolower()))
  end <- start + length(cities_names) - 1
  cities_links <- cities_links[start:end]
  
  out <- data.frame(City = cities_names,
                    `City Link` = cities_links, 
                    check.names = FALSE)
  
  # broken links.  takes ~5 minutes to run.
  # broken_city <- sapply(out$`City Link`, function(url){
  #   tryCatch(read_html(url), error = function(e){"Broken Link"})
  # })
  
  # filter out broken links
  out <- out %>% dplyr::filter(City != "Claremont" & 
                          City != "Point Mugu Nawc" & 
                          City != "Quartz Hill" & 
                          City != "Wasco")
  
  ### COURSES ###
  out2 <- lapply(out$`City Link`, function(x) {
  
    courses_page <- read_html(x)
    courses_nodes <- html_nodes(courses_page, "h2 a")
    
    courses_names <- courses_nodes %>% html_text()
    courses_links <- courses_nodes %>% html_attr("href")
    
    output <- data.frame(Course = courses_names, 
                         `Course Link` = courses_links,
                         check.names = FALSE)
  })
  
  names(out2) <- out$City
  out2 <- ldply(out2)
  
  out3 <- right_join(out, out2, by = c("City" = ".id"))
  
  # broken links.  takes ~10 minutes to run.
  # broken_course <- sapply(out3$`Course Link`, function(url){
  #   tryCatch(read_html(url), error = function(e){"Broken Link"})
  # })
  
  
  ### COURSE INFO ###
  out4 <- lapply(out3$`Course Link`, function(url){
    
    course_page <- read_html(url)
    course_address <- html_nodes(course_page, "h1+ div") %>% 
      html_text() %>% str_split("\n\n") %>% unlist()
    course_specs <- html_nodes(course_page, "li") %>% html_text() %>% str_subset(":")
    
    # address
    address <- course_address[2] %>% str_replace_all("\n", "") %>% str_replace_all("\r", " ")
    
    contact <- course_address[3] %>% str_split("\n") %>% unlist()
    phone <- contact %>% str_subset("Phone")
    # fax <- contact %>% str_subset("Fax")
    website <- contact %>% str_subset("Website")
    
    # specs
    weekdays <- course_specs %>% str_subset("Week days:") %>% str_replace("Week days: ", "")
    weekends <- course_specs %>% str_subset("Weekends:") %>% str_replace("Weekends: ", "")
    holes <- course_specs %>% str_subset("Holes:") %>% str_replace("Holes: ", "")
    
    output <- data.frame(`Course Link` = url,
                       Address = address,
                       Phone = phone,
                       # Fax = fax,
                       Website = ifelse(length(website) == 0, "No Website", website),
                       Weekdays = weekdays,
                       Weekends = weekends,
                       Holes = holes,
                       check.names = FALSE)
    
  })
  
  
  map('county', 'california', fill = TRUE, col = palette())
counties <- map('county', 'california', names = TRUE, plot = FALSE)
  

# close counties
c("del norte", "siskiyou", "modoc", "humboldt", "trinity", "shasta", "lassen", "mendocino", "tehama", "glenn", "butte")