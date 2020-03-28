#Actualizar version de R
#install.packages("installr")
#library(installr)
#updateR()
# Instalar Paquetes de shiny, dplyr, ggplot2 
#install.packages("tidyverse")
#install.packages("shiny") 
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("rsconnect")
#install.packages("rmarkdown")
#install.packages("shinydashboard")
#install.packages("DT")
# Se cargan las librerias para shiny, dplyr, ggplot2 
library(shiny)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(leaflet)
library(rmarkdown)
library(shinydashboard)
library(DT)
library(gridExtra)
library(htmltools)
rsconnect::setAccountInfo(name='shirleycr2018',
                          token='D2CEEC37EE2E9DC64B46857C888B445B',
                          secret='wnS90/ExFsEscWeWeLRAYofkOP6Mb+6KtbSqkuP2')

options(scipen=999)
# Se carga el DataSet
myVehiclesloc <- read.csv( "C:/Cursos Personal TEC/Data Science/Proyecto/myVehicles.csv",sep=";",na.strings="")
str(myVehiclesloc)
# Se eliminan los que son NA
myVehiclesloc <- drop_na(myVehiclesloc)
# Se convierte la logitud y latitud a numerico
myVehiclesloc <-
  myVehiclesloc %>% 
  rename( latitude = lat , longitude = long )

myVehiclesloc <- mutate_at(myVehiclesloc,vars(latitude,longitude), as.character)
str(myVehiclesloc)
myVehiclesloc <- mutate_at(myVehiclesloc,vars(latitude,longitude), as.numeric)
str(myVehiclesloc)


myVehiclesloc <- 
  myVehiclesloc %>%  filter(odometer != 'NA' & odometer != 0) 
str(myVehiclesloc)
# Se limitan los vehiculos por precio, nos interesan solo los que estan entre 100 y 20000 $$
myVehiclesloc <- 
  myVehiclesloc %>% 
  filter(as.numeric(price) > 100  & as.numeric(price) < 30000) 

# Reducimos el data set a partir del 2015, nos interesan los ultimos anios 
myVehiclesloc <- 
  myVehiclesloc %>% arrange(year)
myVehiclesloc$year = as.character(myVehiclesloc$year)
myVehiclesloc <- myVehiclesloc %>% filter(year >= 2015 & year <= 2020)
tail(myVehiclesloc)

myVehiclesloc$cylinders = as.numeric(myVehiclesloc$cylinders )
#myVehiclesloc$year = as.character(myVehiclesloc$year)
#myVehiclesloc <- myVehiclesloc %>% filter(year > 2010 & year != 'NA') 
 
#Se Carga el UI con el formato de la pagina 
myUi <- dashboardPage(
        dashboardHeader(title = "Venta de Vehiculos Usados"),
        dashboardSidebar(),
        dashboardBody(
          fluidRow(

            box(
              title = "Fabricante",
              width =3,
              selectInput("manufacturerSelect", label = "Filtrar por Fabricante", multiple = TRUE,
                          choices = list()),
            ),
            box(
              title = "Estado",
              width =3,
              selectInput("stateSelect", label = "Filtrar por Estado", multiple = TRUE,
                          choices = list()),
            ),
#            box(
#              title = "Modelo",
#              width =3,
#              selectInput("modeloSelect", label = "Filtrar por Modelo", multiple = TRUE,
#                          choices = list()),
#            ),
            box(
              title = "Condicion",
              width =3,
              selectInput("conditionSelect", label = "Filtrar por Condicion", multiple = TRUE,
                          choices = list()),
            )   
          ),
          
          fluidRow(
            box(
              title = "Anio",
              width =3,
              sliderInput("yearSlider", 
                          "Rango de Anios", 
                          min = 2014, 
                          max = 2020,
                          value = c(2015, 2020))
            ),
            box(
              title = "Precio", 
              width =3, 
              sliderInput("priceSlider", 
                          "Rango de Precios",
                          min = 100, 
                          max = 30000,
                          value = c(5000, 30000)
                          )
            ),

            box(
              title = "Transmision",
              width =3,
              radioButtons("transmisionRadio", label = "Filtrar por Transmision",
                           choices = list( "Cualquiera" = 1, 
                                           "Manual" = 2, 
                                           "Automatica" = 3,
                                           "Otra" = 4), 
                           selected = 1),
            ),
            
            box(
              title = "Combustible",
              width =3,
              selectInput("fuelSelect", label = "Filtrar por Combustible", multiple = TRUE,
                          choices = list()),
            )
            
          ), 
          fluidRow(
            tabBox(
              side = "left", width =12, height = "250px",
              selected = "Datos",
              tabPanel("Datos", 
                        dataTableOutput("myTable")
                       ),
              tabPanel("Graficos Conteos",   
                       plotOutput("conteoSize"),
                       plotOutput("conteoType"),
                       plotOutput("conteoCyl"),
                       plotOutput("conteoState"),
                       plotOutput("conteoFabricante"),
                       ), 
              tabPanel("Graficos Group by",
                       
                       plotOutput("groupbyFuel"),
                       plotOutput("groupbyScatter1") ,
                       plotOutput("groupbyState"),
                       plotOutput("groupbyManufacturer"),
                       plotOutput("groupbyTransmission"),
              )
              ,
              tabPanel("Graficos Precios", 
                       plotOutput("precioScatter2"),
                       plotOutput("precioScatter3"),
                       plotOutput("precioScatter4"),
                       plotOutput("precioScatter5"),
              )   
              ,
              tabPanel("Mapa", 
                       leafletOutput( "mapaVentas")
                       )
            )
      )
    )
)

myServer <- function(input, output,session){
   
# llenando los filtros de pantalla
  updateSelectizeInput(session,
                       'manufacturerSelect',
                       choices = c('Cualquiera', levels(unique(myVehiclesloc$manufacturer))),
                       selected = 'Cualquiera',
                       server = TRUE)
  
  updateSelectizeInput(session,
                       'stateSelect',
                       choices = c('Cualquiera', levels(unique(myVehiclesloc$state))),
                       selected = 'Cualquiera',
                       server = TRUE)
  
  updateSelectizeInput(session,
                       'conditionSelect',
                       choices = c('Cualquiera', levels(unique(myVehiclesloc$condition))),
                       selected = 'Cualquiera',
                       server = TRUE)
  
  
  updateSelectizeInput(session,
                       'fuelSelect',
                       choices = c('Cualquiera', 
                                   levels(unique(myVehiclesloc$fuel))),
                       selected = 'Cualquiera',
                       server = TRUE)
  
  loadData <- reactive({
    
    
    data <- myVehiclesloc
    
 # Aplicamos los slidersinput
    data <- data[data$year >= as.character(input$yearSlider[1]) & data$year <= as.character(input$yearSlider[2]), ]
    data <- data[data$price >= as.numeric(input$priceSlider[1]) & data$price <= as.numeric(input$priceSlider[2]), ]
 # Aplicamos el filter de Manufacturer   
    if (input$manufacturerSelect != "Cualquiera") {
      data <- data %>%
        filter(manufacturer == input$manufacturerSelect)
    }       
# Aplicamos el filter de state    
    if (input$stateSelect != "Cualquiera") {
      data <- data %>%
        filter(state == input$stateSelect)
    }   
# Aplicamos el filter de condition   
    if (input$conditionSelect != "Cualquiera") {
      data <- data %>%
        filter(condition == input$conditionSelect)
    }          
# El radioInput retorna un valor numérico:
# 1 = Cualquiera
# 2 = Manual
# 3 = Automatic 
# 4 = Others    
    if (input$transmisionRadio != 1) {
      if(input$transmisionRadio == 2){
        data <- data[data$transmission == "manual", ]
      }
      else if(input$transmisionRadio == 3){
        data <- data[data$transmission == "automatic", ]
      }else{
        data <- data[data$transmission == "other", ]
      }
    }    
# Aplicamos el filter de Gasolina     
    if (input$fuelSelect != "Cualquiera") {
      data <- data %>%
        filter(fuel == input$fuelSelect)
    }
 
    
    data
  })
  
  
  datosConteoS <- reactive({
    
   data <- loadData()
   data <-  data %>% 
            select(state,manufacturer, year,price)
#   str(data)
   data
  })  
  

  
  datosconteoFuel <- reactive({
    
    data <- loadData()
    data <-  data %>% 
      select(state,manufacturer,fuel, transmission, year,price)
    #   str(data)
    data
 data
  })
  
  datosconteoTransmission <- reactive({
    data   <- loadData()
    data2  <-  group_by(data,manufacturer,state,fuel, transmission, year) %>%
              summarise(qty_vehicles = n()) %>%
              arrange(desc(qty_vehicles))
    data2
  })
  
  datosconteoSize <- reactive({
    data   <- loadData()
    data2  <-  group_by(data,state,size, year) %>%
      summarise(qty_vehicles = n()) %>%
      arrange(desc(qty_vehicles))
    data2
  })
  
  datosconteoType <- reactive({
    data   <- loadData()
    data2  <-  group_by(data,state,type, year) %>%
      summarise(qty_vehicles = n()) %>%
      arrange(desc(qty_vehicles))
    data2
  })
  
  
  
  datosGroupByGraficos1 <- reactive({
    data  <- loadData()

    data  <- arrange( data ,manufacturer,condition)
    data2 <- group_by( data, manufacturer,condition) %>%
             summarise(qty_vehicles = n()) %>%
             arrange(desc(qty_vehicles))
    data2
  })  

  datosGroupByGraficos2 <- reactive({
    data  <- loadData()
      data  <- arrange( data,state, condition)
    data2 <- group_by( data, state, condition) %>%
      summarise(qty_vehicles = n()) %>%
      arrange(desc(qty_vehicles))
    data2
  })  
  
  datosGroupByGraficos3 <- reactive({
    data  <- loadData()
    data  <- arrange( data, year, condition)
    data2 <- group_by( data, year, condition) %>%
      summarise(qty_vehicles = n()) %>%
      arrange(desc(qty_vehicles))
    data2
  }) 
  
  datosGroupByGraficos31 <- reactive({
    data  <- loadData()
      data  <- arrange(data, year, fuel)
    data2 <- group_by( data, year, fuel) %>%
      summarise(qty_vehicles = n()) %>%
      arrange(desc(qty_vehicles))
    data2
  }) 
  
  datosGroupByGraficos32 <- reactive({
    data  <- loadData()
    data  <- arrange( data, year, cylinders)
    data2 <- group_by( data, year, cylinders) %>%
      summarise(qty_vehicles = n()) %>%
      arrange(desc(qty_vehicles))
    data2
  }) 
  
  datosGroupByGraficos4 <- reactive({
    data  <- loadData()
    data  <- arrange( data, condition, fuel)
    data2 <- group_by( data, condition, fuel) %>%
      summarise(qty_vehicles = n()) %>%
      arrange(desc(qty_vehicles))
    data2
  })   
  
  datosGroupByGraficos5 <- reactive({
    data  <- loadData()
    data  <- arrange( data, transmission, condition)
    data2 <- group_by( data, transmission, condition) %>%
      summarise(qty_vehicles = n()) %>%
      arrange(desc(qty_vehicles))
    data2
  }) 
  
  datosGroupByGraficos6 <- reactive({
    data  <- loadData()
    data  <- arrange( data, cylinders, condition)
    data2 <- group_by( data, cylinders, condition) %>%
      summarise(qty_vehicles = n()) %>%
      arrange(desc(qty_vehicles))
    data2
  })   
  output$myTable <- renderDataTable(datatable({
    loadData()
  }))
  
  

  
  output$mapaVentas <- renderLeaflet({
    
    data <- loadData()
    leaflet(data) %>% 
      addTiles() %>% 
#      addMarkers()
     addMarkers(~longitude, ~latitude, popup = ~htmlEscape(paste( paste("ID:",id), paste("Price:",price),sep=" ")))
  })  
#  output$plot1 <- renderPlot({
#    data <- histdata[seq_len(input$slider)]
#    hist(data)
#  })

# Visualizaciones de Conteos   
  

  
  output$conteoCyl <- renderPlot({
    data <- loadData()
     
    p1 <- data %>%
             ggplot(aes(x = cylinders))+
             geom_histogram(aes(color = transmission, fill = transmission),
                              position = "dodge", 
                              stat ="count", alpha=0.6)+
      labs(title = "Conteo de Vehiculos por Cilindraje y Transmision")
    p2 <- data %>%        
             ggplot(aes(x = cylinders))+
             geom_histogram(aes(color = fuel, fill = fuel),
                             position = "dodge", 
                             stat ="count", alpha=0.6)  +
      labs(title = "Conteo de Vehiculos por Cilindraje y Combustible") 
    
    grid.arrange( p1, p2, ncol=2)
  })
  

  output$conteoState <- renderPlot({
    data <- datosConteoS()
    ggplot(data, aes(x=manufacturer))+
      geom_bar()+
      # Change fill color 
      geom_bar( aes(fill = year), color ="steelblue") +
      labs(title = "Conteo de Vehiculos por Fabricante")
  })

  output$conteoFabricante <- renderPlot({
    data <- datosConteoS()
    ggplot(data, aes(x=state))+
      geom_bar()+
      # Change fill color 
      geom_bar( aes(fill = manufacturer), color ="steelblue") +
      labs(title = "Conteo de Vehiculos por Estado y Fabricante")
    
  })  

  output$conteoFuel <- renderPlot({
    data <- datosconteoFuel() 
    ggplot(data, aes(x=transmision))+
      geom_bar()+
      # Change fill color 
      geom_bar( aes(fill = fuel), color ="steelblue")+
      labs(title = "Cantidad Vehiculos por Tipo de Combustible y Transmision")  
  })
  
  output$conteoTransmission <- renderPlot({
    data <- datosconteoTransmission()
    
  })     

# Visualizaciones de Group by    
  
  output$conteoSize <- renderPlot({ 
    
    data <- datosconteoSize()
    data %>%
      ggplot(aes(fill=size,x=state,y=qty_vehicles)) + 
      geom_bar(position="dodge", stat="identity")  +
      labs(title = "Cantidad Vehiculos por Estado y Tamanio")  
    

    

  })
  
  output$conteoType <- renderPlot({ 
    
    data <- datosconteoType()
    data %>%
      ggplot(aes(fill=type,x=state,y=qty_vehicles)) + 
      geom_bar(position="dodge", stat="identity")+
      labs(title = "Cantidad Vehiculos por Estado y Tipo")  
    })
  
  output$groupbyState <-renderPlot({
    data <- datosGroupByGraficos2()
    data %>%
    ggplot(aes(fill=condition,x=state,y=qty_vehicles)) + 
      geom_bar(position="dodge", stat="identity")  +
      labs(title = "Cantidad Vehiculos por Estado y Condicion")  
    
  })
 
  output$groupbyManufacturer <- renderPlot({
    
    data <- datosGroupByGraficos1()
    data %>%
      ggplot(aes(fill=condition,x=manufacturer,y=qty_vehicles)) + 
      geom_bar(position="dodge", stat="identity")   +
      labs(title = "Cantidad Vehiculos por Fabricante y Condicion")  
    
  }) 
  
  output$groupbyScatter1 <- renderPlot({ 
    data <- datosGroupByGraficos3()
    data %>%
      ggplot(aes(fill=condition,x=year,y=qty_vehicles)) + 
      geom_bar(position="dodge", stat="identity")  +
      labs(title = "Cantidad Vehiculos por Anio y Condicion")  
  })
 
  output$groupbyFuel <- renderPlot({
    data <- datosGroupByGraficos4()
    p1 <- data %>%
        ggplot(aes(fill=condition,x=fuel,y=qty_vehicles)) + 
        geom_bar(position="dodge", stat="identity")  +
      labs(title = "Cantidad Vehiculos por Tipo Combustible y Condicion")  
    
    data <- datosGroupByGraficos5()
    p2 <-data %>%
      ggplot(aes(fill=condition,x=transmission,y=qty_vehicles)) + 
      geom_bar(position="dodge", stat="identity")+
      labs(title = "Cantidad Vehiculos por Tipo Transmision y Condicion")  
    
    data <- datosGroupByGraficos6()
    p3 <-data %>%
      ggplot(aes(fill=condition,x=cylinders,y=qty_vehicles)) + 
      geom_bar(position="dodge", stat="identity")+
      labs(title = "Cantidad Vehiculos por Cilindraje y Condicion") 
    grid.arrange( p1, p2,p3, ncol=3)
    
  })

  output$precioScatter2 <- renderPlot({
    data <- loadData() 
    myVehicleAgg <- aggregate( data$price, list( year = data$year , manufacturer = data$manufacturer), mean)
    names(myVehicleAgg)[3] <- "price"
    myVehicleAgg  %>%
    ggplot(aes(x = manufacturer, y = price))+ 
      geom_point(aes(color=year, size = 2))+
      labs(title = "Precio por Fabricante")
 #     geom_line(aes(color=year))
    
  })
  
  output$precioScatter3 <- renderPlot({ 
    data <- loadData() 
    myVehicleAgg <- aggregate( data$price, list( year = data$year , state= data$state), mean)
    names(myVehicleAgg)[3] <- "price"
    myVehicleAgg  %>%
      ggplot(aes(x = state, y = price))+ 
      geom_point(aes(color=year, size = 2)) +
      labs(title = "Precio por Estado")
    })
   
  output$precioScatter4 <- renderPlot({ 
    data <- loadData() 
    myVehicleAgg <- aggregate( data$price, list( year = data$year , size= data$size), mean)
    names(myVehicleAgg)[3] <- "price"
    myVehicleAgg  %>%
      ggplot(aes(x = size, y = price))+ 
      geom_point(aes(color=year, size = 2)) +
      labs(title = "Promedio de Precio por Tamanio")
  })
  
  output$precioScatter5 <- renderPlot({ 
    data <- loadData() 
    myVehicleAgg <- aggregate( data$price, list( year = data$year , type= data$type), mean)
    names(myVehicleAgg)[3] <- "price"
    myVehicleAgg  %>%
      ggplot(aes(x = type, y = price))+ 
      geom_point(aes(color=year, size = 2)) +
      labs(title = "Promedio de Precio por Tipo")
  })
#  output$groupbyTransmission <- renderPlot({ 
#    data <- datosGroupByGraficos5()
#    data %>%
#      ggplot(aes(fill=condition,x=transmission,y=qty_vehicles)) + 
#      geom_bar(position="dodge", stat="identity")  
#  })
  
}

shinyApp(myUi,myServer)

