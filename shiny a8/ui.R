library(shiny)
library(ggplot2)
library(dplyr)
library(tibble)
library(shinythemes)
library(lubridate)
library(readr)
library(tidyr)
library(scales)
library(maps)
library(leaflet)
library(RColorBrewer)
library(plotly)
library(viridis)
library(rgdal)
library(tigris)
library(htmltools)
library(shinydashboard)


owid <-read.csv("data/owid-covid-data.csv",header=T,sep=",")
contact <-read.csv("data/covid-contact-tracing.csv",header=T,sep=",")
test_p <- read.csv("data/covid-19-testing-policy.csv")
vac_p <- read.csv("data/covid-vaccination-policy.csv")
income <- read.csv("data/income-support-covid.csv")
school <- read.csv("data/school-closures-covid.csv")
work <- read.csv("data/workplace-closures-covid.csv")
home <- read.csv("data/stay_home_requirements.csv")
countries <- readOGR("data/countries.geojson")
owid$date <- as.Date(owid$date)
contin <- c("Africa", "Asia" , "Europe" , "European Union", "North America" , "Oceania" , "South America", "Todos")

header <- dashboardHeader(titleWidth='20%', 
                          title = "Dashboard Covid"
)


sidebar <- dashboardSidebar(
  sidebarMenu(
    
    menuItem("Overview", tabName = "over"),
    
    menuItem("Vacunas", tabName = "vacu"),
    menuItem("Políticas", tabName = "polit")
    
  ) 
)
body <- dashboardBody( 
  tabItems(
    tabItem(tabName = "over",
            tabBox(width = 12,
                   title = "",
                   tabPanel("Casos por continentes",  plotlyOutput(width = "100%", height = "600px", outputId = "continente")),
                   tabPanel("Muertes por continentes",  plotlyOutput(width = "100%", height = "600px", outputId = "muertes")),
                   tabPanel("Gráfico de Burbuja Países",  plotlyOutput(width = "100%", height = "600px", outputId = "paises"))
            )),
    
    tabItem(tabName = "vacu", tabBox(width = 12, title = "",
                                     tabPanel("Total Vacunas",  plotlyOutput(width = "100%", height = "600px", outputId = "tvacunas")),
                                     tabPanel("Personas vacunadas",  plotlyOutput(width = "100%", height = "600px", outputId = "pvacunas")),
                                     tabPanel("Personas totalmente vacunadas",  plotlyOutput(width = "100%", height = "600px", outputId = "fvacunas"))
    )),
    tabItem(tabName = "polit", tabBox(width = 12, title = "", 
                                      tabPanel("Seguimiento Contacto",  leafletOutput(width = "100%", height = "600px", outputId = "mapa1")),
                                      tabPanel("Politica de pruebas",  leafletOutput(width = "100%", height = "600px", outputId = "mapa2")),
                                      tabPanel("Politica de vacunas",  leafletOutput(width = "100%", height = "600px", outputId = "mapa3")),
                                      tabPanel("Apoyo ingreso",  leafletOutput(width = "100%", height = "600px", outputId = "mapa4")),
                                      tabPanel("Cierre de escuelas",  leafletOutput(width = "100%", height = "600px", outputId = "mapa5")),
                                      tabPanel("Cierre de lugares de trabajo",  leafletOutput(width = "100%", height = "600px", outputId = "mapa6")),
                                      tabPanel("Quedarse en Casa",  leafletOutput(width = "100%", height = "600px", outputId = "mapa7"))))
    
  ))


ui <- dashboardPage(header, sidebar, body)