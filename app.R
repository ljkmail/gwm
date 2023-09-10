library(shiny)

source("R/230410_보관측망이상치검출_전체권역_module_v1.0.R")
source("R/230805_관측망이상치탐지_module_shiny_v1.6_openAPI.R")

ui <- fluidPage(
    titlePanel(
        h1("지하수관측망 이상치 검출 모니터링 서비스", align = "center"),
        windowTitle = "지하수관측망 모니터링 서비스"),
  tabsetPanel(
      tabPanel(title = "영섬유역 국가지하수관측소", ngwUI("ngw")),
      tabPanel(title = "보지하수관측소", boUI("bo")),
  )
)

server <- function(input, output, session) {
  boServer("bo")
  ngwServer("ngw")
}

shinyApp(ui, server)