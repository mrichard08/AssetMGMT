library(shiny)
library(shinymaterial)
library(plotly)
library(ggplot2)
library(dplyr)
library(readxl)
library(scales)
library(here)
library(FinancialMath)
library(hrbrthemes)


################################################# 
# Functions
#################################################

defer <- function(df, def_period){
    # Delays project annual expenditures by specified deferral period in years.
    df <- df %>% mutate(Year = Year + def_period) 
    
    for (i in def_period:1){
        df <- df %>% add_row(Year=2020+i-1, 
                             Plant1=0,
                             Plant2=0,
                             Plant3=0,
                             Plant4=0,
                             Plant5=0,
                             Plant6=0,
                             Plant7=0,
                             Plant8=0,
                             Plant9=0,
                             Plant10=0,
                             Plant11=0,
                             Plant12=0,
                             .before=1)
    }
    df
}


netpv <- function(df, cf0=0, rate=0.05, def_period=0){
    #calculate the NPV for each column
    blank_df <- df[FALSE,]
    vec <- c()
    
    for (i in 2:dim(df)[2]){
        cf <-  pull(df[,i])
        
        times <- seq(1,dim(df)[[1]],1)
        
        npv <- NPV(cf0, cf, times, rate)
        vec <- append(vec, npv)
    }
    
    out_df <- blank_df %>% add_row(Year = def_period, 
                                   Plant1 = vec[1],
                                   Plant2 = vec[2],
                                   Plant3 = vec[3],
                                   Plant4 = vec[4],
                                   Plant5 = vec[5],
                                   Plant6 = vec[6],
                                   Plant7 = vec[7],
                                   Plant8 = vec[8],
                                   Plant9 = vec[9],
                                   Plant10 = vec[10],
                                   Plant11 = vec[11],
                                   Plant12 = vec[12])
                                  
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


plants <- c('Plant1', 'Plant2', 'Plant3', 'Plant4', 'Plant5', 'Plant6', 'Plant7', 'Plant8',
            'Plant9', 'Plant10', 'Plant11', 'Plant12')

################################################# 
# UI
#################################################

ui <- material_page(

    # Application title
    title = "Fleet Asset Management Benchmarking Tool",
    primary_theme_color = "gray",
    tags$br(),
    material_side_nav(
        fixed = FALSE,
        # Place side-nav tabs within side-nav
        material_side_nav_tabs(
            side_nav_tabs = c(
                "Spend Chart" = "nav_tab_1",
                "NPV Table" = "nav_tab_2",
                "Outlier Analysis" = "nav_tab_3",
                "Spend Table" = "nav_tab_4"
            ),
            icons = c("cast", "insert_chart", "insert_chart", "insert_chart")
        )
    ),
    material_side_nav_tab_content(
        side_nav_tab_id = "nav_tab_1",
        material_row(
            material_column(
                width = 2,
                    material_card(
                    material_dropdown(input_id = 'component1',
                                 label = 'Component:',
                                 choices = c('Motors', 'Transformers', 'Piping'),
                                 selected = 'Motors'),
                    tags$br(),
                    material_dropdown(input_id = 'plant_name1',
                                label = 'Plant 1 Name:',
                                choices = plants,
                                 selected = 'Braidwood'),
                    tags$br(),
                    material_dropdown(input_id = 'plant_name2',
                                      label = 'Plant 2 Name:',
                                      choices = plants,
                                      selected = 'Byron')
                    )
            ),
            material_column(
                width = 9,
                material_card(
                    plotOutput('barplot1')
                ),
                material_card(
                    plotOutput('barplot2')
                )
                
            )
        )
    ),
    material_side_nav_tab_content(
        side_nav_tab_id = "nav_tab_2",
        material_row(
            material_column(
                width = 2,
                material_card(
                    material_dropdown(input_id = 'component',
                                      label = 'Component',
                                      choices = c('Motors', 'Transformers', 'Piping'),
                                      selected = 'Motors'),
                    tags$br(),
                    material_slider(input_id = 'years',
                                    label = 'Years to Defer:',
                                    min_value = 0,
                                    max_value = 10,
                                    initial_value = 0),
                    tags$br(),
                    material_slider(input_id = 'interest_rate',
                                    label = 'Interest Rate:',
                                    min_value = 0,
                                    max_value = 20,
                                    initial_value = 5,
                                    step_size = 0.1)
                )
            ),
            material_column(
                width = 9,
                material_card(
                    tags$h4("Net Present Value:"),
                    tableOutput("npv_table")
                ),
                material_card(
                    tags$h4("Total Savings for Motors:")
                    
                )
            )
        )
    ),
    material_side_nav_tab_content(
        side_nav_tab_id = "nav_tab_3",
        plotOutput('barplot3')
    ),
    material_side_nav_tab_content(
        side_nav_tab_id = "nav_tab_4",
        tableOutput("npv_table")
    )
)


################################################# 
# Server
#################################################

server <- function(input, output) {
    setwd("~/Assetmgmt")
    data <- read_excel("amp_spreadsheet.xlsx", sheet = "Motor Data")

    plot1_data <- reactive({
        data %>%
        select (-c(Total, Exclude, `Duplicate ID`, `Duplicate Issue`)) %>%
        pivot_longer(cols = starts_with("20"), names_to = "Year", values_to = "Cost") %>%
        select(-c(Type, `Issue Name`)) %>%
        group_by(Plant, Year) %>%
        summarize(Total = mean(Cost)) %>%
        mutate(Year = as.numeric(Year)) %>%
        filter(Year < 2040) %>%
        replace(is.na(.), 0) %>%
        filter(Plant == input$plant_name1)
    })

    plot2_data <- reactive({
        data %>%
        select (-c(Total, Exclude, `Duplicate ID`, `Duplicate Issue`)) %>%
        pivot_longer(cols = starts_with("20"), names_to = "Year", values_to = "Cost") %>%
        select(-c(Type, `Issue Name`)) %>%
        group_by(Plant, Year) %>%
        summarize(Total = mean(Cost)) %>%
        mutate(Year = as.numeric(Year)) %>%
        filter(Year < 2040) %>%
        replace(is.na(.), 0) %>%
        filter(Plant == input$plant_name2)  
        })

    output$barplot1 <- renderPlot({plot1_data() %>%
            ggplot(aes(x = Year, y = Total, fill = Plant)) +
                geom_col()+
                scale_x_continuous(breaks = seq(2020, 2040, 1))+
                scale_y_continuous(labels = dollar) +
                scale_fill_manual(values = c(ft_cols$slate))+
                theme_light() +
                labs(title = "Yearly Motor Capital Project Cost", x = "Year", y = "Cost ($K)") +
                theme(axis.title.x = element_text(color="black", size = 12, face = "plain", hjust = 0.5),
                      axis.text.x = element_text(face="bold", size=10, angle=-45),
                      axis.text.y = element_text(face="bold", size=10))
                })

    output$barplot2 <- renderPlot({plot2_data() %>%
            ggplot(aes(x = Year, y = Total, fill = Plant)) +
                geom_col()+
                scale_x_continuous(breaks = seq(2020, 2040, 1))+
                scale_y_continuous(labels = dollar) +
                scale_fill_manual(values = c("#324f76"))+
                theme_light() +
                labs(title = "Yearly Motor Capital Project Cost", x = "Year", y = "Cost ($K)") +
                theme(axis.title.x = element_text(color="black", size = 12, face = "plain", hjust = 0.5),
                      axis.text.x = element_text(face="bold", size=10, angle=-45),
                      axis.text.y = element_text(face="bold", size=10))
                })
    
    ts_npv <-  data %>%
        select(-c(Total, Exclude, `Duplicate ID`, `Duplicate Issue`)) %>% 
        filter(Plant %in% plants) %>% 
        pivot_longer(cols = starts_with("20"), names_to = "Year", values_to = "Cost") %>% 
        select(-c(Type, `Issue Name`)) %>% 
        group_by(Plant, Year) %>% 
        summarize(Total = sum(Cost)) %>% 
        mutate(Year = as.numeric(Year)) %>% 
        filter(Year < 2040) %>% 
        pivot_wider(names_from = Plant, values_from = Total) %>%
        replace(is.na(.), 0)
    
    # Make reactive with slider input for interest rate
    
    npv_df <- reactive({def_df <- defer(ts_npv, def_period=input$years)
                        def_df_round <- round_df(netpv(def_df, cf0=0, rate=input$interest_rate/100, def_period=input$years),2)
                        def_df_round})

    
    output$npv_table <- renderTable({npv_df()})
    
    output$barplot3 <- ts_box <- data %>%
        select (-c(Total, Exclude, `Duplicate ID`, `Duplicate Issue`)) %>% 
        pivot_longer(cols = starts_with("20"), names_to = "Year", values_to = "Cost") %>% 
        select(-c(Type, `Issue Name`)) %>% 
        mutate(Year = as.numeric(Year)) %>% 
        filter(Year < 2040, Cost != 0) %>% 
        mutate( bin=cut_width(Year, width=1, boundary=0) ) %>% 
        ggplot(aes(x = Year, y = Cost)) +
        geom_boxplot(aes(x = bin, y = Cost)) +
        scale_y_continuous(labels = dollar) +
        scale_x_discrete(labels = seq(2020,2040,1))+
        scale_color_manual(values = c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99',
                                      '#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#ffff99','#b15928'))+
        theme_light()+
        labs(title = "Outlier Analysis of Motor Capital Projects", x = "Year", y = "Project Cost ($K)") +
        theme(axis.title.x = element_text(color="black", size = 12, face = "plain", hjust = 0.5),
              axis.text.x = element_text(face="bold", size=10, angle=-45),
              axis.text.y = element_text(face="bold", size=10))
    
#     def_df0 <- reactive({def_df_round0 <- round_df(netpv(ts_npv, cf0=0, rate=input$interest_rate/100, def_period=0),2)})
#     
#     def_sav <- tibble()
    
    output$spend_sum <- renderTable({data}) 
}
    
# Run the application 
shinyApp(ui = ui, server = server)

