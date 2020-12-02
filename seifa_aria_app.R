library(shiny)
library(dplyr)
library(readr)
library(magrittr)
library(readxl)
library(writexl)
library(tools)
library(zeallot)
library(forcats)

ui <- fluidPage(
  
  # App title
  titlePanel("SEIFA and ARIA app"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    
    # Sidebar panel for inputs
    sidebarPanel(
      
      # Input: Select a file ----
      fileInput("file1", "Choose an Excel or CSV file",
                multiple = FALSE,
                accept = c(".xlsx",
                           ".csv")),
      
      downloadButton("downloadData", "Download data with new variables")
      
    ),
    mainPanel(
      
      tableOutput(""),
    )
  )
)



server <- function(input, output) {
  
  ext <- function(){
   return(file_ext(input$file1$datapath))
  }
    
  # Process ARIAs function
  process_arias <- function(){
    ra_2006 <- read_excel("PA2011_RA2006.xls", skip=5, sheet="Table 3")
    names(ra_2006) <- tolower(names(ra_2006))
    ra_2006 <- ra_2006 %>% group_by(postcode) %>% 
      filter(percentage == max(percentage)) %>% 
      select(c(postcode, ra_code, ra_name))
    
    ra_2011 <- read_excel("PA2012_RA2011.xls", skip=5, sheet="Table 3")
    names(ra_2011) <- tolower(names(ra_2011))
    ra_2011 <- ra_2011 %>% group_by(postcode) %>% 
      filter(percentage == max(percentage)) %>% 
      select(c(postcode, ra_code, ra_name))
    
    ra_2016 <- read_excel("PA2017_RA2016.xls", skip=5, sheet="Table 3")
    names(ra_2016) <- tolower(names(ra_2016))
    ra_2016 <- ra_2016 %>% group_by(postcode) %>% 
      filter(percentage == max(percentage)) %>% 
      select(c(postcode, ra_code, ra_name))
    
    out <- list(ra_2006, ra_2011, ra_2016)
    return(out)
  }
  
  # Process ARIAs
  c(ra_2006, ra_2011, ra_2016) %<-% process_arias()
  
  # Read SEIFA
  SEIFA_2006 <- read_csv("seifa_2006.csv") %>% mutate(postcode = as.character(postcode))
  SEIFA_2011 <- read_csv("seifa_2011.csv") %>% mutate(postcode = as.character(postcode))
  SEIFA_2016 <- read_csv("seifa_2016.csv") %>% mutate(postcode = as.character(postcode))
  
  # Main function
  read_xlsx_df <- function(){
    
    req(input$file1)
    
    data <- read_excel(input$file1$datapath, na = c("NA","NULL","N/A","n/a")) 
    
    # Process postcode variable name
    postcode_names <- c("Post Code", "Post.Code", "Post_Code", "Postcode", "Post code", "Post_code") 
    colnames(data)[colnames(data) %in% postcode_names] <- "postcode"
    data <- mutate(data, postcode = as.character(postcode))
    data <- mutate(data, postcode = ifelse(nchar(postcode) < 4, paste0("0", postcode), postcode))
    
    # Process year variable name
    year_names <- c("Year Of Death", "Year.Of.Death", "Year_Of_death", "Year", "Year of death", "Year_of_death",
                    "Year Of Birth", "Year of birth", "Year_of_birth", "Year_of_Birth") # add your variable name or change your column name
    colnames(data)[colnames(data) %in% year_names] <- "year"
    
    # Split data into census groups to join ARIA postcodes
    early_data <- filter(data, year < 2009) %>% 
      left_join(SEIFA_2006, on = "postcode") 
    early_data <- left_join(early_data, ra_2006, on = "postcode")
    
    mid_data <- filter(data, year > 2008 & year < 2014) %>% 
      left_join(SEIFA_2011, on = "postcode") 
    mid_data <- left_join(mid_data, ra_2011, on = "postcode")
    
    late_data <- filter(data, year > 2013) %>% 
      left_join(SEIFA_2016, on = "postcode") 
    late_data <- left_join(late_data, ra_2016, on = "postcode")
    
    # Generate ARIA scores from names
    ra_data <- do.call("rbind", list(early_data, mid_data, late_data)) %>% 
      mutate(ARIA = fct_recode(ra_name, "1" = "Major Cities of Australia", "2" = "Inner Regional Australia", 
                               "3" = "Outer Regional Australia", "4" = "Remote Australia", "5" = "Very Remote Australia"))
    
    return(ra_data)
    
  }
  
  read_csv_df <- function(){
    
    req(input$file1)
    
    data <- read_csv(input$file1$datapath, na = c("NA","NULL","N/A","n/a"))
    
    postcode_names <- c("Post Code", "Post.Code", "Post_Code", "Postcode", "Post code", "Post_code") 
    colnames(data)[colnames(data) %in% postcode_names] <- "postcode"
    data <- mutate(data, postcode = as.character(postcode))
    data <- mutate(data, postcode = ifelse(nchar(postcode) < 4, paste0("0", postcode), postcode))
    
    year_names <- c("Year Of Death", "Year.Of.Death", "Year_Of_death", "Year", "Year of death", "Year_of_death",
                    "Year Of Birth", "Year of birth", "Year_of_birth", "Year_of_Birth") # add your variable name or change your column name)
    colnames(data)[colnames(data) %in% year_names] <- "year"
    
    
    # Split data into census groups to join ARIA postcodes
    early_data <- filter(data, year < 2009) %>% 
      left_join(SEIFA_2006, on = "postcode") %>% 
      left_join(ra_2006, on = "postcode")
    
    mid_data <- filter(data, year > 2008 & year < 2014) %>% 
      left_join(SEIFA_2011, on = "postcode") %>% 
      left_join(ra_2011, on = "postcode")
    
    late_data <- filter(data, year > 2013) %>% 
      left_join(SEIFA_2016, on = "postcode") %>% 
      left_join(ra_2016, on = "postcode")
    
    # Generate ARIA scores from names
    ra_data <- do.call("rbind", list(early_data, mid_data, late_data)) %>% 
      mutate(ARIA = fct_recode(ra_name, "1" = "Major Cities of Australia", "2" = "Inner Regional Australia", 
                               "3" = "Outer Regional Australia", "4" = "Remote Australia", "5" = "Very Remote Australia"))
    
    return(ra_data)
    
  }
  
  # Downloadable Excel file of selected dataset ----
  output$downloadData <- downloadHandler(
    
    filename = function() {paste0("SEIFA_ARIA_", input$file1$name)},
    
    content = function(file) {
      if(ext() == "xlsx"){ 
        write_xlsx(read_xlsx_df(), path = file)}
      
      else if(ext() == "csv"){ 
        write_csv(read_csv_df(), path = file)}
        }
    
  )
}


shinyApp(ui = ui, server = server)