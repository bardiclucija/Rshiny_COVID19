Sys.setlocale("LC_ALL", "Croatian")
options(encoding = "UTF-8")

library(shiny)

if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
source("data.R",local = TRUE, encoding = "UTF-8")$values
source("hrvatska.R",local = TRUE, encoding = "UTF-8")$values
source("svijet.R",local = TRUE, encoding = "UTF-8")$values
source("podaci.R",local = TRUE, encoding = "UTF-8")$values
source("model.R", local = TRUE, encoding = "UTF-8")$values
source("opcenito.r", local = TRUE, encoding = "UTF-8")$values




ui <- bootstrapPage(
  includeCSS("www/style.css"),
  theme = shinytheme("united"),
  navbarPage(title = "COVID-19", collapsible = TRUE, windowTitle = "COVID-19",
             tabPanel(title = "Općenito o virusu",
                      column(width = 6, align = "center", box_opcenito()),
                      column(width = 3, align = "center", box_simptomi()),
                      column(width = 3, align = "center", box_prevencija())),
             tabPanel("Covid-19 u Hrvatskoj",
                      column(width = 3, align = "center", box_zarazeni()),
                      column(width = 3, align = "center", box_oporavljeni()),
                      column(width = 3, align = "center", box_umrli()),
                      column(width = 3, align = "center", box_cijepljeni()),
                      fluidRow(id = "graf_1", column(6, graf1()),
                                              column(6,graf2() )),
                      fluidRow(id = "graf_3", graf3()),
                      fluidRow(id = "graf_3", graf4())),
             tabPanel("Covid-19 u svijetu",
                      column(width = 3, align = "center", box_zarazeni2()),
                      column(width = 3, align = "center", box_oporavljeni2()),
                      column(width = 3, align = "center", box_umrli2()),
                      column(width = 3, align = "center", box_cijepljeni2()),
                      fluidRow(id = "graf_1", column(6, graf11()),
                                              column(6, graf22())),
                      fluidRow(id = "graf_3", graf33()),
                      fluidRow(id = "graf_3", graf44())),
             navbarMenu("Interaktivna karta",
                        tabPanel(id ="karta", "Ukupno slučajeva",plotlyOutput("karta", height="800px")),
                        tabPanel(id ="karta", "Ukupno umrlih", plotlyOutput("karta2", height="800px"))),
             tabPanel(title = "Pregled podataka",
                      column(6, align = "center", graf5()),
                      column(6, align = "center", graf6()),
                      column(6, align = "center", graf7()),
                      column(6, align = "center", graf1D(100000))),
             tabPanel(title = "Model",
                      fluidRow(id = "model_box", box_model()),
                      fluidRow(id = "range_input", column(3, uiOutput("pocetak")),
                                                   column(3, uiOutput("kraj"))),
                      fluidRow(id = "graf_3", plotlyOutput("graf_predvidanje"))),
             tabPanel(title = "O aplikaciji",
                      fluidRow(column(6, align="center", box_aplikacija()),
                               column(6, align="center", box_aplikacija1()))
             )
  ),
  footer()
)

server <- function(input, output, session) {
  
  output$karta <- renderPlotly({
    karta()
  })
  output$karta2 <- renderPlotly({
    karta2()
  })
  
  output$pocetak <- renderUI({
    dateInput('pocetak',
              label = "Početak predikcije:",
              value = Sys.Date()-1,
              min = as.Date("2020-10-01"),
              max = Sys.Date()-1)
  })
  
  output$kraj <- renderUI({
    dateInput('kraj',
              label = "Kraj predikcije:",
              value = Sys.Date()+7,
              min = input$pocetak + 5,
    )
  })
  
  output$graf_predvidanje  <- renderPlotly({
    req(input$pocetak)
    req(input$kraj)
    model(input$pocetak[1], input$kraj[1])
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
