library(shinymaterial)
library(ggplot2)


npvCalcUI <- function(id) {
  ns <- NS(id)
  tagList(
    material_card(
      width=9,
      DT::dataTableOutput("npv_table")
    )
  )
}

npvCalcServer <- function(input, output, session, data) {
  
  
  defer <- function(df, def_period){
    # Delays project annual expenditures by specified deferral period in years.
    df <- df %>% mutate(Year = Year + def_period) 
    
    for (i in def_period:1){
      df <- df %>% add_row(Year=2020+i-1, 
                           Braidwood=0,
                           Byron=0,
                           Calvert=0,
                           Clinton=0,
                           Dresden=0,
                           Fitz=0,
                           Ginna=0,
                           LaSalle=0,
                           Limerick=0,
                           NMP=0,
                           `Peach Bottom`=0,
                           `Quad Cities`=0,
                           .before=1)
    }
    df
  }
  
  netpv <- function(df, cf0=0, rate=0.05, def_period=0){
    #calculate the NPV for each column
    blank_df <- df[FALSE,]
    vec <- c()
    
    for (i in 2:13){
      cf <-  pull(df[,i])
      
      times <- seq(1,dim(df)[[1]],1)
      
      npv <- NPV(cf0, cf, times, rate)
      vec <- append(vec, npv)
    }
    
    out_df <- blank_df %>% add_row(Year = def_period, 
                                   Braidwood = vec[1],
                                   Byron = vec[2],
                                   Calvert = vec[3],
                                   Clinton = vec[4],
                                   Dresden = vec[5],
                                   Fitz = vec[6],
                                   Ginna = vec[7],
                                   LaSalle = vec[8],
                                   Limerick = vec[9],
                                   NMP = vec[10],
                                   `Peach Bottom` = vec[11],
                                   `Quad Cities` = vec[12])
    out_df
  }
  
  round_df <- function(x, digits) {
    # round all numeric variables
    # x: data frame 
    # digits: number of digits to round
    numeric_columns <- sapply(x, mode) == 'numeric'
    x[numeric_columns] <-  round(x[numeric_columns], digits)
    x
  }
  
  ts_npv <-  data %>%
    select(-c(Total, Exclude, `Duplicate ID`, `Duplicate Issue`)) %>% 
    pivot_longer(cols = starts_with("20"), names_to = "Year", values_to = "Cost") %>% 
    select(-c(Type, `Issue ID`, `Issue Name`, Status, `Issue Outage`)) %>% 
    group_by(Plant, Year) %>% 
    summarize(Total = sum(Cost)) %>% 
    mutate(Year = as.numeric(Year)) %>% 
    filter(Year < 2040) %>% 
    pivot_wider(names_from = Plant, values_from = Total) %>%
    replace(is.na(.), 0)
  
    # Make reactive with slider input for interest rate
  
    def_period <- reactive(input$years)
    rate <- reactive(input$rate)/100
    
    def_df <- defer(ts_npv, def_period=def_period())
    npv <- netpv(def_df, rate=rate(), def_period=def_period())
    npv_df <- round_df(npv, 2)
  
  
    output$npv_table <- DT::renderDataTable({DT::datatable(npv_df)})
  
}