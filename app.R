library(readr)
library(rgdal)
library(leaflet)
library(tidyverse)
library(shiny)
library(shinydashboard)

dataset <- read_delim("Data/Reference_details.csv", 
                           ";", escape_double = FALSE, trim_ws = TRUE)
path=paste0(getwd(), "/Data")
# 
# dataset %>% 
#   filter(`Common name`=="") %>% 
#   select(pathtofile) %>% 
#   slice(1)->pathto1
# 
# dataset %>% 
#   filter(`Common name`=="Green peafowl")->data
# 
# data
# 
# # dataset %>% 
# #   filter(`Common name`=="Crested argus") %>% 
# #   select(pathtofile) %>% 
# #   slice(1)->pathto2
# 
# 
# shape1<-readOGR(path, as.character(pathto))
# # shape2<-readOGR(path, as.character(pathto2))
# 
# leaflet() %>% 
#   addTiles() %>% 
#   addMarkers(lng=data$lng, lat=data$lat,popup = data$Info) %>% 
#   #addPopups(lng=data$lng, lat=data$lat, data$Country) %>% 
#   addPolygons(data=shape1)

###################################################################################################################
###################################################################################################################
###################################################################################################################

#UI
title <- tags$a('Southeast Asian Galliformes', target="_blank")
ui <- dashboardPage(
  dashboardHeader(title = title, titleWidth=600),
    dashboardSidebar(
    sidebarMenu(id = "tabs",
                menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                menuItem(selectInput("species", "species", choices=c("Bar backed partridge"="Bar backed partridge",
                                                                     "Black partridge"="Black partridge",
                                                                     "Blyth's Tragopan [whole range]"="Blyth's Tragopan [whole range]",
                                                                     "Bornean peacock pheasant"="Bornean peacock pheasant",
                                                                     "Bronze tailed peacock pheasant"="Bronze tailed peacock pheasant",
                                                                     "Bulwer's pheasant"="Bulwer's pheasant",
                                                                     "Chestnut bellied partridge"="Chestnut bellied partridge",
                                                                     "Chinese francolin"="Chinese francolin",
                                                                     "Crested argus"="Crested argus",
                                                                     "Crested partridge"="Crested partridge",
                                                                     "Crimson headed partridge"="Crimson headed partridge",
                                                                     "Edward's pheasant"="Edward's pheasant",
                                                                     "Ferruginous partridge"="Ferruginous partridge",
                                                                     "Germain's peacock pheasant"="Germain's peacock pheasant",
                                                                     "Great argus"="Great argus",
                                                                     "Green junglefowl"="Green junglefowl",
                                                                     "Green peafowl"="Green peafowl",
                                                                     "Grey breasted (White faced) partridge"="Grey breasted (White faced) partridge",
                                                                     "Grey peacock pheasant"="Grey peacock pheasant",
                                                                     "Hainan partridge"="Hainan partridge",
                                                                     "Hainan peacock pheasant"="Hainan peacock pheasant",
                                                                     "Hill partridge [whole range]"="Hill partridge [whole range]",
                                                                     "Kalij pheasant [whole range]"="Kalij pheasant [whole range]",
                                                                     "Long billed partridge"="Long billed partridge",
                                                                     "Malay crested fireback"="Malay crested fireback",
                                                                     "Malay crestless fireback"="Malay crestless fireback",
                                                                     "Malayan peacock pheasant"="Malayan peacock pheasant",
                                                                     "Malaysian (or Campbell's) partridge"="Malaysian (or Campbell's) partridge",
                                                                     "Mountain bamboo partridge"="Mountain bamboo partridge",
                                                                     "Mountain peacock pheasant"="Mountain peacock pheasant",
                                                                     "Mrs Hume’s pheasant"="Mrs Hume’s pheasant",
                                                                     "Orange necked partridge"="Orange necked partridge",
                                                                     "Palawan peacock pheasant"="Palawan peacock pheasant",
                                                                     "Red billed partridge"="Red billed partridge",
                                                                     "Red breasted partridge"="Red breasted partridge",
                                                                     "Red junglefowl"="Red junglefowl",
                                                                     "Roll's partridge"="Roll's partridge",
                                                                     "Rufous throated partridge"="Rufous throated partridge",
                                                                     "Salvadori's pheasant"="Salvadori's pheasant",
                                                                     "Scaly breasted partridge"="Scaly breasted partridge",
                                                                     "Siamese fireback"="Siamese fireback",
                                                                     "Sichuan hill partridge"="Sichuan hill partridge",
                                                                     "Silver pheasant"="Silver pheasant",
                                                                     "Sumatran partridge"="Sumatran partridge",
                                                                     "White cheeked partridge"="White cheeked partridge",
                                                                     "White necklaced (Collared) partridge"="White necklaced (Collared) partridge")
                                     , selected = "Green peafowl" ))
                
    )
    
  ),
  
  
  
  dashboardBody(tags$head(
    tags$link(rel="stylesheet", type= "text/css", href="custom1.css")),
    tabsetPanel(id = "tabs",
                  tabPanel(
                    title = "Southeast Asian Galliformes",
                    value = "page1",
                    fluidRow(
                      box(selectInput("Information", "Information", choices=c("Site" ="Site",
                                                                              "Country" ="Country",
                                                                              "Study period" ="Study period",
                                                                              "Study effort"="Study effort",
                                                                              "Objective"="Objective",
                                                                              "Data sources"="Data sources",
                                                                              "Effort"="Effort"), selected = "Objective"))),
                    fluidRow(
                      box(leafletOutput("mymap")))
                  )
    )
  )
)

server = shinyServer(function(input,output){
 shape1<-reactive({
   pathto1=dataset %>% 
     filter(`Common name`==input$species) %>% 
     select(pathtofile) %>% 
     slice(1)
  shape1<-readOGR(path, as.character(pathto1))})
  dat1<-reactive({
    dataset %>% 
      filter(`Common name`==input$species) %>% 
      mutate("info"=as.character(input$Information))})
  output$mymap <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>% 
      addMarkers(lng=dat1()$lng, lat=dat1()$lat, popup=dat1()$info) %>% #Not working
      addPolygons(data=shape1())
      
    
  })
})


shinyApp(ui, server)
