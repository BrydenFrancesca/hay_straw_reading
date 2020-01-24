setwd("L:/Prices/Dashboards/Hay and straw")
library(zoo)
library(dplyr)
library(readxl)
library(data.table)
library(stringr)
library(reshape2)
library(data.table)
library(pdftools)
library(shiny)
library(shinythemes)
library(writexl)
library(DT)
library(tidyr)
library(tibble)

# Define UI for data upload app ----
ui <- fluidPage(theme = shinytheme("united"),
                
                # App title ----
                headerPanel("Hay and straw reader"),
                
                # Sidebar layout with input and output definitions ----
                sidebarLayout(
                  
                  # Sidebar panel for inputs ----
                  sidebarPanel(
                    
                    # Input: Select a PDF file ----
                    conditionalPanel("input.excel_alternative_hay == 0", fileInput("file_hay", "Choose PDF of current week's data",
                                                                                   multiple = TRUE,
                                                                                   accept = c("pdf", ".pdf"))),
                    ##Output; explains how to use Excel file reader
                    conditionalPanel("input.excel_alternative_hay == 1",textOutput("excel_file_instructions")),
                    conditionalPanel("input.excel_alternative_hay == 1",hr()),
                    ##Input: select Excel file
                    conditionalPanel("input.excel_alternative_hay == 1", fileInput("file_hay_excel", "Upload alternative excel format",
                                                                                   multiple = FALSE,
                                                                                   accept = c("xlsx",".xlsx"))),
                    
                    checkboxInput("overwrite", label = "Overwrite record in backseries with new data", value = F),
                    checkboxInput("read", label = "Read backseries data only", value = F),
                    
                    ##This hides PDF input and brings up an excel format instead
                    checkboxInput("excel_alternative_hay", label = "PDF reader not working?", value = F),
                    # Horizontal line ----
                    hr(),
                    hr(),
                    textOutput("last_hay_date"),
                    hr(),
                    hr(),
                    ##download results
                    conditionalPanel("input.read == 0", downloadButton("dl_hay", "Download")),
                    conditionalPanel("input.read == 0",actionButton("submit_hay", "Submit")),
                    width = 3
                  ),#end of sidebar panel
                  
                  # Main panel for displaying outputs ----
                  mainPanel(
                    tabsetPanel(
                      tabPanel("Weekly prices", DTOutput("hay_contents")),
                      tabPanel("Monthly prices", DTOutput("hay_monthly"))
                    )
                  )#end of main panel
                ) #end of sidebar layout
) #end of fluid page

# Define server logic to read selected file ----
server <- function(input, output) {
  
  ##Output datatable conditionally. If all boxes unchecked, returns PDF file contents.
  ##If "PDF file not working" box checked, returns excel file contents.
  ##If view file box checked, just returns backseries
  output$hay_contents <- renderDT({
    if(input$read == F){
      return(DT::datatable(data_table_final(), 
                           options = list(sDom  = '<"top">lrt<"bottom">ip')))}#}
    else{
      return(DT::datatable(view_hay(), 
                           options = list(sDom  = '<"top">lrt<"bottom">ip')))
    }
  })
  
  output$hay_monthly <- renderDT({
    if(input$read == F){
      return(DT::datatable(monthly_mean(), 
                           options = list(sDom  = '<"top">lrt<"bottom">ip')))}#}
    else{
      return(DT::datatable(view_hay_monthly(), 
                           options = list(sDom  = '<"top">lrt<"bottom">ip')))
    }
  })
  
  ##Load in backseries####
  backseries_hay = reactive({
    data <- as_tibble(read_xlsx("Data/hay_straw_prices.xlsx", sheet = 1))
    data$Date <- as.Date(data$Date)
    data
  })
  
  ##Load in backseries of comments
  backseries_comments = reactive({
    comments <-  as_tibble(read_xlsx("Data/hay_straw_prices.xlsx", sheet = 4))
    comments$Date <- as.Date(comments$Date)
    comments
  })
  
  ##Text outputs####
  ##Text output of last upload date
  output$last_hay_date = renderText({
    paste0("Date of last dataset uploaded: w/e ", format(max(backseries_hay()$Date), "%d-%m-%Y"))
  })
  
  ##Text output explaining how the Excel uploader works
  output$excel_file_instructions = renderText({
    "If the PDF uploader no longer works, you can complete the Excel template with the hay and straw weekly data and upload it here"
  })
  
  ####PDF reading#####
  raw_prices = reactive({
    validate(
      need(input$file_hay != "", "Please select a PDF")
    )
    ##Load in data
    new_month_hay1 <- pdf_data(input$file_hay$datapath)
    new_month_hay1 <- as_tibble(new_month_hay1[[1]])
    new_month_hay1
  })
  
  ##Save trade comment
  pdf_comment = reactive({
    trade_row <- raw_prices()[grep("Trade", raw_prices()$text), 4]
    trade_comment <- raw_prices() %>% filter(y == as.numeric(trade_row))
    trade_comment <- paste(trade_comment$text, collapse = " ")
    trade_comment
  })
  
  ##Find date on PDF
  current_date = reactive({
    date_row <- raw_prices()[grep("W[[:punct:]]E", raw_prices()$text),4]
    data_date <- raw_prices() %>% filter(y == as.numeric(date_row))
    data_date$text <- as.Date(data_date$text, format = "%d/%m/%y") 
    data_date <- na.omit(data_date)
    data_date <- data_date %>% select(text) 
    data_date <- dplyr::pull(data_date)
    data_date})
  
  ##Remove comment
  pdf_hay_prices = reactive({
    trade_row <- raw_prices()[grep("Trade", raw_prices()$text), 4]
    new_month_hay1 <- raw_prices() %>% filter(y != as.numeric(trade_row))
    
    ##Clean data
    new_month_hay <- new_month_hay1 %>% filter(y > as.numeric(new_month_hay1[grep("Source", new_month_hay1$text), 4]))
    find_rows <- new_month_hay1 %>% filter(text == "hay" | text == "straw"| text == "West")  
    find_rows <- dplyr::pull(find_rows, x) +1
    find_cols <- new_month_hay1 %>% filter(text == "Pick-up" | text == "straw"| text == "baled")
    find_cols <- dplyr::pull(find_cols, y)
    find_start <- min(new_month_hay$x)-1
    
    ###Split into columns
    new_month_hay <-  new_month_hay %>% mutate(col = cut(x, breaks = c(find_start, find_rows, Inf) 
    )) %>% 
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
    colnames(new_month_hay) <- column_names
    
    ##Clean data in table
    new_month_hay[] <- lapply(new_month_hay, gsub, pattern = "[*]", replacement = "") 
    new_month_hay[,-1] <- lapply(new_month_hay[,-1], function(x) as.numeric(x))
    
    #Remove Scotland data, put into long format and create averages
    new_month_hay_summary <- new_month_hay %>% filter(!str_detect(Area, "Scotland")) %>% gather(-Area, key = Type, value = Price) %>% 
      group_by(Type) %>% summarise(Weekly_price = mean(Price, na.rm = T)) %>% mutate(Date = current_date()) %>% select(Date, Type, Weekly_price)
    new_month_hay_summary
  })
  
  ####Read Excel file####
  ##Read in basic excel file
  excel_hay_data <- reactive({
    validate(
      need(input$file_hay_excel != "", "Please select an excel file")
    )
    excel <- readxl::read_excel(input$file_hay_excel$datapath, col_names = T)
    excel[1:11]
  })
  
  ##Pull comments from sheet
  excel_comment <- reactive({
    excel_hay_data()[11] %>% na.omit()
  })
  
  ##Pull date from sheet
  excel_date <- reactive({
    date = excel_hay_data()[10] %>% na.omit() 
    date = dplyr::pull(date)
  })
  
  ##Calculate averages from prices on sheet
  excel_hay_averages <- reactive({
    data <- excel_hay_data()[1:9] %>% gather(Type, Price, -Region)
    data <- data[-grep("Scotland", data$Region),]
    data$Price <- as.numeric(gsub("[^[:digit:]., ]", "", data$Price))
    data <- data %>% 
      group_by(Type) %>% 
      summarise(Weekly_price = mean(Price, na.rm = T)) %>%
      ##Add date in
      mutate(Date = as.Date(excel_date()))
  })
  
  ####Data processing####  
  ##Switch between pdf and excel prices
  new_hay_prices = reactive({
    if(input$excel_alternative_hay == 1){
      excel_hay_averages()
    } else{
      pdf_hay_prices()
    }
  })
  
  #Switch between pdf and excel date 
  data_date = reactive({
    if(input$excel_alternative_hay == 1){
      excel_date()}
    else{current_date()}
  })
  
  ##Switch between pdf and excel trade comment
  trade_comment = reactive({
    if(input$excel_alternative_hay == 1){
      excel_comment()}
    else{pdf_comment()}
  })
  
  ##Bind data to backseries
  bound_series = reactive({
    if(input$overwrite == F){
      if(T %in% grepl(data_date(), backseries_hay()$Date)){
        backseries_hay()}
      else{
        bound_hay <- bind_rows(backseries_hay(), new_hay_prices())
        bound_hay$Weekly_price <- round(bound_hay$Weekly_price, 2)
        bound_hay <- bound_hay[order(bound_hay$Date),]
        bound_hay
      }
    }
    else if(input$overwrite == T){
      bound_hay <- backseries_hay() %>% dplyr::filter(Date != data_date()) 
      bound_hay <- bind_rows(bound_hay, new_hay_prices())
      bound_hay$Weekly_price <- round(bound_hay$Weekly_price, 2)
      bound_hay <- bound_hay[order(bound_hay$Date),]
      bound_hay
    }
  })
  
  ###Put into human readable format
  weekly_mean = reactive({
    means1 <- bound_series() %>% spread(key = Type, value = Weekly_price) 
    means1 <- means1[order(means1$Date),]
  })
  
  ###Select create monthly means
  monthly_mean = reactive({
    means2 <- bound_series() 
    means2$Date <- as.yearmon(means2$Date)
    means2 <- means2 %>% group_by(Date, Type) %>% summarize(Monthly_price = mean(Weekly_price)) %>% spread(key = Type, value = Monthly_price)
    means2 <- means2[order(means2$Date),]
    means2$Date <- format(means2$Date, "%b %Y")
    means2
  })
  
  ##Create series of comments, doesn't allow for overwriting
  bound_comments = reactive({
    comments <- bind_cols("Date" = as.Date(data_date()), "Comment" = trade_comment())
    if(input$overwrite == F){
      if(T %in% grepl(data_date(), backseries_comments()$Date)){
        backseries_comments()}
      else{
        comments <- bind_rows(comments, backseries_comments())
        comments <- comments[order(comments$Date),]
      }
    }
    else if(input$overwrite == T){
      backseries_comments <- backseries_comments() %>% dplyr::filter(Date != data_date()) 
      comments <- bind_rows(backseries_comments, comments)
      comments <- comments[order(comments$Date),]
    }
  })
  
  ##Tidy data for final view format
  data_table_final = reactive({
    data <- bound_series()
    data$Weekly_price <- round(data$Weekly_price, 1)
    data <- data %>% spread(Type, Weekly_price)
    data <- data[order(data$Date, decreasing = T),]
    data
  })
  
  ##View backseries only; formatting and tidying data
  view_hay = reactive({
    data1 <- backseries_hay() %>% spread(Type, Weekly_price)
    data1 <- data1[order(data1$Date, decreasing = T),] 
    data1
  })
  
  ##View monthly backseries only; formatting and tidying data
  view_hay_monthly = reactive({
    data <- as_tibble(read_xlsx("Data/hay_straw_prices.xlsx", sheet = 3))
  })
  
  output$dl_hay <- downloadHandler(
    filename = function() { "hay_straw_prices.xlsx"},
    content = function(file) {write_xlsx(list("Weekly prices" = weekly_mean(), "Monthly prices" = monthly_mean(), "Comments" = bound_comments()),format_headers = T, path = file)}
  )
  
  # When the Submit button is clicked, save the data to data folder for app to run
  observeEvent(input$submit_hay, {
    write_xlsx(list("Full series" = bound_series(), "Weekly prices" = weekly_mean(), "Monthly prices" = monthly_mean(), "Comments" = bound_comments()), path = "L:/Prices/Dashboards/Hay and straw/Data/hay_straw_prices.xlsx")
  })
  
  # When the Submit button is clicked, save the data to hay/straw folder for people to access
  observeEvent(input$submit_hay, {
    write_xlsx(list("Full series" = bound_series(), "Weekly prices" = weekly_mean(), "Monthly prices" = monthly_mean(), "Comments" = bound_comments()), path = "L:/Prices/AMR/Hay&Straw/hay_straw_prices.xlsx")
  })
  
  ##When the submit button is clicked, bring up box to say this
  observeEvent(input$submit_hay, {
    showModal(modalDialog(
      title = "Data saved to app!",
      "To download an xlsx version of this data instead please select download"
    ))
  })
  
}
# Run the app ----
shinyApp(ui, server)
