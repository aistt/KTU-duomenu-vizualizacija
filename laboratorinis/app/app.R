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
    dark_bg = "#2F91B9",
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
  
  dashboardHeader(title = "Sodros Duomenys"),
  dashboardSidebar(
    selectizeInput(inputId = "imones_kodas", label = "Įmonės pavadinimas", choices = NULL, selected = NULL),
    menuItem("Pagrindinė informacija", tabName = "Pagrindinė informacija")
),
  
  dashboardBody( use_theme(mytheme),
    tabItems(
    tabItem(tabName = "Pagrindinė informacija")
),
    fluidRow(
    infoBox("R + Shiny","LD2.")
),
    fluidRow(
    valueBox(561000, "Įmonių Kodas", width = 500, color = "light-blue"),
),

    fluidRow(box("Žemiau esančiame grafike pateikiama, kaip kas mėnesį kinta vidutinis apskaičiuotas atlyginimas pasirinktoje įmonėje.", plotOutput("plot"), width = 500)),
    fluidRow(box("Žemiau esančiame grafike pateikiamas ryšys, tarp DU, bendro mokesčio ir apdraustųjų skaičiaus, pagal vidutinį atlyginimą.", 
                 plotlyOutput("plotly"), width = 500)))
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
      geom_line(size = 0.9, linetype = 4, alpha=0.4, colour="#001C8C" )+geom_point(color="#0029A9")+
      theme_bw()+labs( y="Vidutinis atlyginimas", x="Menuo")+
      ggtitle("Vidutinio darbo užmokesčio dinamika") +
      scale_x_datetime(date_labels="%b %y",date_breaks  ="1 month")  
  )
  
  output$plotly <- renderPlotly({
    p2 <- data12 %>%
      group_by(code, name) %>%
      summarise(avg_wage = mean(avgWage), avg_insured = median(numInsured), total_tax = sum(tax)) %>%
      na.omit() %>%
      arrange(desc(avg_wage)) %>%
      head(20) %>%
      ggplot(aes(x = avg_wage, y = total_tax, size = avg_insured, color = name)) +
      geom_point() +
      geom_smooth(aes(x = avg_wage, y = total_tax), method="glm", se=T) +
      labs(x = "Vidutinis atlyginimas", y = "Bendri mokeščiai") +
      ggtitle("Vidutinio darbo užmokesčio ir bendrų mokesčių priklausomybė") +
      theme(legend.position = "none")
    plot(p2)
    ggplotly(p2)
  })
}

shinyApp(ui, server)
