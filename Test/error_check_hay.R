setwd("L:/Prices/Dashboards/Hay and straw/Test")

if (!require("pacman")) install.packages("pacman")
library(pacman)
pacman::p_load(zoo, dplyr, readxl, stringr, reshape2, pdftools, writexl, tidyr, tibble, lubridate)

#Check backseries is there
backseries_available <- class(try(read_xlsx("../Data/hay_straw_prices.xlsx", sheet = 1))) %>% 
  grepl(pattern = "try-error") %>%
  sum() == 0

#Check data is available and read it in
pdf_available <- class(try(pdf_data("error_check.pdf"))) %>% 
  grepl(pattern = "try-error") %>%
  sum() == 0

new_month_hay <- pdf_data("error_check.pdf")[[1]]

##Check trade comment is available and save trade comment
trade_comment_available <- try(sum(grepl("Trade", new_month_hay$text)) != 0)
trade_row <- unique(new_month_hay[grep("Trade", new_month_hay$text), 4])
trade_comment <- new_month_hay %>% filter(y == as.numeric(trade_row))
trade_comment <- paste(trade_comment$text, collapse = " ")

##Check date is available on PDF
date_row <- new_month_hay[grep("W[[:punct:]]E", new_month_hay$text),4]
data_date <- new_month_hay %>% filter(y == as.numeric(date_row))
data_date$text <- as.Date(data_date$text, format = "%d/%m/%y") 
data_date <- data_date %>% 
  na.omit() %>% 
  select(text) %>% 
  dplyr::pull()

date_available <- try(is.Date(data_date))
date_range_correct <- try(data_date > "2019-01-01" & data_date < today())

##Remove comment and rows below from data
trade_row <- unique(new_month_hay[grep("Trade", new_month_hay$text), 4])
new_month_hay <- new_month_hay %>% filter(y != as.numeric(trade_row))

##Check key words are available
key_words_available <- try(sum(sum(grepl("Source", new_month_hay$text)) != 0,
                               sum(grepl("hay", new_month_hay$text)) != 0,
                               sum(grepl("straw", new_month_hay$text)) != 0,
                               sum(grepl("West", new_month_hay$text)) != 0,
                               sum(grepl("Pick-up", new_month_hay$text)) != 0,
                               sum(grepl("straw", new_month_hay$text)) != 0,
                               sum(grepl("baled", new_month_hay$text)) != 0) == 7)

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

##Check key words have split it correctly
number_of_columns <- try(ncol(new_month_hay) == 10)

#Create column names
column_names <- new_month_hay %>% filter(y %in% find_cols) %>% select(-y)
column_names <- apply(column_names, 2, paste0, collapse = " ")
column_names <- gsub("NA", "", column_names)
column_names <- str_trim(column_names)

##Check column names are correct
column_names_correct <- try(sum(column_names != c("Area", "Pick-up baled seed hay", "Pick-up baled meadow hay", "Big bale hay", "Pick-up baled barley straw", 
                                                  "Pick-up baled wheat straw", "Big sq baled barley straw", "Big sq baled wheat straw", "Big baled rape straw")) == 0)

##Rename data and remove in-text names
"%ni%" <- Negate("%in%")
new_month_hay <- new_month_hay %>% filter(y %ni% find_cols) %>% select(-y)

##Use new column names
colnames(new_month_hay) <- column_names

##Check only blank values will be removed
numeric_checker <- function(x)
{numeric_digits = grep("[[:digit:]]", x)
non_numeric_values = x %>% as.numeric() %>% is.na() %>% which()
value = sum(numeric_digits %in% non_numeric_values) == 0
}

blanks_removed_correctly <- lapply(new_month_hay[,-1], function(x) numeric_checker(x))
blanks_removed_correctly <- try(sum(unlist(blanks_removed_correctly)) == 8)

##Clean data in table; remove punctuation and convert to numerics
new_month_hay[] <- lapply(new_month_hay, gsub, pattern = "[*]", replacement = "")
new_month_hay[,-1] <- lapply(new_month_hay[,-1], function(x) as.numeric(x))

#Remove Scotland data, put into long format and create averages
new_month_hay_summary <- new_month_hay %>% filter(!str_detect(Area, "Scotland")) %>% gather(-Area, key = Type, value = Price) %>%
  group_by(Type) %>% summarise(Weekly_price = mean(Price, na.rm = T)) %>% mutate(Date = data_date) %>% select(Date, Type, Weekly_price)

summary_successful <- try(ncol(new_month_hay)-1 == nrow(new_month_hay_summary))
format_correct <- try(sum(c(class(new_month_hay_summary$Date), class(new_month_hay_summary$Type), class(new_month_hay_summary$Weekly_price)) == c("Date", "character", "numeric"))==3)

#Create table of checked values:
output_checker <- tibble(backseries_available, pdf_available, trade_comment_available, date_available, date_range_correct, 
                         key_words_available, number_of_columns, column_names_correct, blanks_removed_correctly,
                         summary_successful, format_correct)
output_checker[grep("Error", output_checker)] <- FALSE
output_checker[which(is.na(output_checker))] <- FALSE


file_path = paste0("L:/Prices/Dashboards/Hay and straw/Test/Test files/error_check_output", today(), ".xlsx")
write_xlsx(list("weekly_prices" = output_checker), path = file_path)