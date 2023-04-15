library(shiny)
library(tidyverse)
library(dplyr)
library(plotly)
library(shinydashboard)
library(lubridate)
library(fresh)

mytheme <- create_theme(
  adminlte_color(
    light_blue = "#0078A8"
  ),
  adminlte_sidebar(
    width = "400px",
    dark_bg = "#47A8CF",
    dark_hover_bg = "#EFFCFF",
    dark_color = "#EFFCFF"
  ),
  adminlte_global(
    content_bg = "#EFFCFF",
    box_bg = "#EFFCFF", 
    info_box_bg = "#EFFCFF"
  )
)

ui <- dashboardPage(
  
  dashboardHeader(title = "Sodros duomenys"),
  dashboardSidebar(
    selectizeInput(inputId = "imones_kodas", label = "Imones vardas", choices = NULL, selected = NULL),
    menuItem("Pagrindine informacija", tabName = "Pagrindine informacija")
),
  
  dashboardBody( use_theme(mytheme),
    tabItems(
    tabItem(tabName = "Pagrindine informacija")
),
    fluidRow(
    infoBox("R + Shiny","LD2.")
),
    fluidRow(
    valueBox(561000, "Imones Kodas"),
),

    fluidRow(box("Atlyginimu dinamika", plotOutput("plot"), width = 500)),
    fluidRow(box("grafikas 2", plotlyOutput("plotly"), width = 500)))
)

server <- function(input, output, session) {
  
  data <- read_csv("https://github.com/aistt/KTU-duomenu-vizualizacija/raw/main/laboratorinis/data/lab_sodra.csv")
  data1 <- data %>% filter(ecoActCode==561000) 
  data12 <- mutate (data1,month1=parse_date_time(month, "ym"))

  updateSelectizeInput(session, "imones_kodas", choices = data1$name, server = TRUE)
  
  output$table <- renderTable(
    data1 %>%
      filter(name == input$imones_kodas) , digits = 0
  )

  output$plot <- renderPlot(
    data12 %>%
      filter(name == input$imones_kodas) %>%
      ggplot(aes(x = month1, y = avgWage)) +
      geom_line(size = 0.9, linetype = 4, alpha=0.4, colour="#483D8B" )+geom_point(color="#7B68EE")+
      theme_bw()+labs( y="Average salary", x="Month")+
      scale_x_datetime(date_labels="%b %y",date_breaks  ="1 month")  
  )
  
  output$plotly <- renderPlotly({
    p2 <- data %>%
      group_by(code, name) %>%
      summarise(avg_wage = mean(avgWage), avg_insured = median(numInsured), total_tax = sum(tax)) %>%
      na.omit() %>%
      arrange(desc(avg_wage)) %>%
      head(20) %>%
      ggplot(aes(x = avg_wage, y = total_tax, size = avg_insured, color = name)) +
      geom_point() +
      # geom_smooth(aes(x = avg_wage, y = total_tax), method="glm", se=T) +
      theme(legend.position = "none")
    plot(p2)
    ggplotly(p2)
  })
}

shinyApp(ui, server)
