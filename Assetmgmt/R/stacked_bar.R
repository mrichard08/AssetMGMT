library(shinymaterial)


stackedBarUI <- function(id) {
  ns <- NS(id)
  tagList(
  material_card(
    width=9,
    plotOutput('stackedBar')
    )
  )
}

stackedBarServer <- function(id){
  moduleServer(id, function(input, output, session, data){
    
  plant_name <- reactive(input$plant)
  
  annual_cost <-  data %>%
    select (-c(Total, Exclude, `Duplicate ID`, `Duplicate Issue`)) %>% 
    pivot_longer(cols = starts_with("20"), names_to = "Year", values_to = "Cost") %>% 
    select(-c(Type, `Issue ID`, `Issue Name`, Status, `Issue Outage`)) %>% 
    filter(Plant == plant_name) %>% 
    group_by(Plant, Year) %>% 
    summarize(Total = mean(Cost)) %>% 
    mutate(Year = as.numeric(Year)) %>% 
    filter(Year < 2040) %>% 
    replace(is.na(.), 0) %>% 
    ggplot(aes(x = Year, y = Total, fill = Plant)) +
      geom_col()+
      scale_x_continuous(breaks = seq(2020, 2040, 2))+
      scale_y_continuous(labels = dollar) +
      scale_fill_manual(values = c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99',
                                   '#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#ffff99','#b15928'))+
      theme_light() + 
      labs(title = "Yearly Motor Capital Project Cost", x = "Year", y = "Cost ($K)") +
      theme(axis.title.x = element_text(color="black", size = 12, face = "plain", hjust = 0.5),
            axis.text.x = element_text(face="bold", size=10, angle=-45),
            axis.text.y = element_text(face="bold", size=10))
  
    output$stackedBar <- renderPlotly({ggplotly(annual_cost)})
  })
}