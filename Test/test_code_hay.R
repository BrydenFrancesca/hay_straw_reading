setwd("L:/Prices/Dashboards/Hay and straw/Test")

if (!require("pacman")) install.packages("pacman")
library(pacman)
pacman::p_load(zoo, dplyr, readxl, stringr, reshape2, pdftools, writexl, tidyr, tibble, lubridate)

#Read in data
new_month_hay <- pdf_data("test_pdf.pdf")[[1]]

##Save trade comment
trade_row <- unique(new_month_hay[grep("Trade", new_month_hay$text), 4])
trade_comment <- new_month_hay %>% filter(y == as.numeric(trade_row))
trade_comment <- paste(trade_comment$text, collapse = " ")

##Find date on PDF
date_row <- new_month_hay[grep("W[[:punct:]]E", new_month_hay$text),4]
data_date <- new_month_hay %>% filter(y == as.numeric(date_row))
data_date$text <- as.Date(data_date$text, format = "%d/%m/%y") 
data_date <- data_date %>% 
  na.omit() %>% 
  select(text) %>% 
  dplyr::pull()

##Remove comment and rows below from data
trade_row <- unique(new_month_hay[grep("Trade", new_month_hay$text), 4])
new_month_hay <- new_month_hay %>% filter(y != as.numeric(trade_row))
  
##Find coordinates corresponding to rows and columns in data
new_month_hay <- new_month_hay %>% filter(y > as.numeric(new_month_hay[grep("Source", new_month_hay$text), 4]))
find_rows <- new_month_hay %>% filter(text == "hay" | text == "straw"| text == "West")
find_rows <- dplyr::pull(find_rows, x) +1
  find_cols <- new_month_hay %>% filter(text == "Pick-up" | text == "straw"| text == "baled")
  find_cols <- unique(dplyr::pull(find_cols, y))
  find_start <- min(new_month_hay$x)-1
  find_all <- unique(c(find_start, find_rows))
  
  ##Split into columns
  new_month_hay <-  new_month_hay %>%
    mutate(col = cut(x, breaks = c(find_all, Inf))) %>%
    arrange(col, y) %>%
    group_by(col, y) %>%
    mutate(text = paste(text, collapse = " ")) %>%
    ungroup() %>%
    select(y, text, col) %>%
    unique() %>%
    spread(col, text)
  
  #Create column names
  column_names <- new_month_hay %>% filter(y %in% find_cols) %>% select(-y)
  column_names <- apply(column_names, 2, paste0, collapse = " ")
  column_names <- gsub("NA", "", column_names)
  column_names <- str_trim(column_names)
  
  ##Rename data and remove in-text names
  "%ni%" <- Negate("%in%")
  new_month_hay <- new_month_hay %>% filter(y %ni% find_cols) %>% select(-y)
  
  ##Use new column names
  colnames(new_month_hay) <- column_names
  
  ##Clean data in table; remove punctuation and convert to numerics
  new_month_hay[] <- lapply(new_month_hay, gsub, pattern = "[*]", replacement = "")
  new_month_hay[,-1] <- lapply(new_month_hay[,-1], function(x) as.numeric(x))
  
  #Remove Scotland data, put into long format and create averages
  new_month_hay_summary <- new_month_hay %>% filter(!str_detect(Area, "Scotland")) %>% gather(-Area, key = Type, value = Price) %>%
    group_by(Type) %>% summarise(Weekly_price = mean(Price, na.rm = T)) %>% mutate(Date = data_date) %>% select(Date, Type, Weekly_price)
 
  file_path = paste0("L:/Prices/Dashboards/Hay and straw/Test/Test files/test_output", today(), ".xlsx")
  write_xlsx(list("weekly_prices" = new_month_hay_summary), path = file_path)