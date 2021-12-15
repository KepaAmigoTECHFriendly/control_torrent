library(shiny)
library(shinyjs)  # Para eecutar comandos JS
library(DT)
library(htmltools)
library(httr)
library(timeDate)
library(anytime)
library(jsonlite)
library(stringr)
library(dplyr)
library(ggplot2)
library(rjson)
library(RCurl)
library(utils)
library(tidyr)
library(RColorBrewer)
library(janitor)
library(reshape2)
library(hrbrthemes)
library(ggthemes)
library(htmlwidgets)
library(shinyalert)
library(lubridate)
library(shinybusy)
library(shinyWidgets)
library(shinydashboard)





# ==============================================================================
# PETICIÓN TOKEN THB
# ==============================================================================

cuerpo <- toJSON(list(username="kepa@techfriendly.es",password='kepatech'))
post <- httr::POST(url = "http://88.99.184.239:30951/api/auth/login",
                   add_headers("Content-Type"="application/json","Accept"="application/json"),
                   body = cuerpo,
                   verify= FALSE,
                   encode = "json",verbose()
)

resultado_peticion_token <- httr::content(post)
auth_thb <- paste("Bearer",resultado_peticion_token$token)



ids <- c("57c08200-e9f7-11eb-9b5b-b36690b9a0e5", "52853e70-e9f7-11eb-9b5b-b36690b9a0e5", "4d305860-e9f7-11eb-9b5b-b36690b9a0e5", "48a57910-e9f7-11eb-9b5b-b36690b9a0e5", "43ce7590-e9f7-11eb-9b5b-b36690b9a0e5",
         "3e5bf560-e9f7-11eb-9b5b-b36690b9a0e5", "38f7f420-e9f7-11eb-9b5b-b36690b9a0e5", "1b5aca10-1acf-11ec-9dbd-2dda84d1e438", "334bed60-e9f7-11eb-9b5b-b36690b9a0e5", "2569f620-e9f6-11eb-a661-35f176fab723",
         "cd30a830-ea0b-11eb-97c1-35f176fab723", "d1f13d80-ea0b-11eb-97c1-35f176fab723", "d7f157b0-ea0b-11eb-97c1-35f176fab723", "16b901b0-1e90-11ec-a3bf-39d69cec15b9", "21e9d820-1e90-11ec-a3bf-39d69cec15b9",
         "26b2c7d0-ea0c-11eb-97c1-35f176fab723", "2b343140-ea0c-11eb-97c1-35f176fab723")
names(ids) <- c("RC1-PB","RC1-P1","RC1-P2","RC1-P3","RC1-P4","RC1-P5","RC1-P6-1","RC1-P6-2","RC1-P7","RC1-P8","RC7-PB","RC7-P1","RC7-P2","Metro-P3-Avenida","Metro-P3-Interior","Metro-P3-Plaza","Metro-P4")


#=====================================================
# INTERFAZ DE USUARIO
#=====================================================
ui <- fluidPage(style = "width: 100%; height: 100%;",
                
                #use_busy_spinner(spin = "fading-circle"),
                
                # Inicialización shinyjs
                useShinyjs(),
                useShinyalert(),
                withMathJax(),
                
                tags$style("@import url(https://use.fontawesome.com/releases/v5.7.2/css/all.css);"),  # Importación iconos Font awesome
                
                titlePanel(title=div(style = "display: inline;",
                                     a(href="https://www.torrent.es/torrentPublic/inicio.html",
                                       img(src="img/logo_torrent.png",style = 'width: 300px; high: 600px; display: inline;')
                                     )
                )),
                
                
                navbarPage(id ="menu", NULL,
                           
                           tabPanel("Horarios",
                                    sidebarLayout(
                                      sidebarPanel(
                                        # 0 - Selección edificio
                                        pickerInput(
                                          inputId = "select_edificio_planta",
                                          label = "Selección de edificio y planta", 
                                          choices = c("RC1-PB","RC1-P1","RC1-P2","RC1-P3","RC1-P4","RC1-P5","RC1-P6-1","RC1-P6-2","RC1-P7","RC1-P8","RC7-PB","RC7-P1","Metro-P3-Avenida","Metro-P3-Interior","Metro-P3-Plaza","Metro-P4")
                                        ),
                                        br(),
                                        tags$hr(),
                                        actionButton("boton_guardar_en_BBDD", "Guardar horarios"),
                                        width=3
                                      ),
                                      
                                      mainPanel(
                                        tags$h4(tags$b("Horarios semana Alumbrado")),
                                        dataTableOutput("calendario_alumbrado_df"),
                                        br(),
                                        tags$h4(tags$b("Horarios semana Climatizadoras")),
                                        dataTableOutput("calendario_climas_df"),
                                        width=9
                                      )
                                    )
                           )
                ) # Cierre navbarPage
) # Cierre UI



#==========================================================================
#==========================================================================
# LÓGICA DE SERVIDOR
#==========================================================================
#==========================================================================

server <- function(input, output, session) {
  

  # DF alumbrado
  creacion_df_alumbrado <- reactive({
    
    id_activo <- ids[grep(input$select_edificio_planta, names(ids))]
    claves <-  URLencode(c("Calendario_alumbrado"))
    url <- paste("http://88.99.184.239:30951/api/plugins/telemetry/ASSET/",id_activo,"/values/attributes?keys=",claves,sep = "")
    peticion <- GET(url, add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"=auth_thb))
    
    df <- jsonlite::fromJSON(rawToChar(peticion$content))
    
    if(class(df$value) == "character"){ 
      json_horario <- fromJSON(df$value)
      #df_json <- as.data.frame(fromJSON(df$value))
      #Creación DF
      columnas <- c("ON", "OFF")
      dias_semana <- names(json_horario)
      on <- c()
      off <- c()
      for(i in 1:length(dias_semana)){
        on <- c(on, json_horario[[i]][[1]][[1]])
        off <- c(off, json_horario[[i]][[1]][[2]])
      }
      
      df_horario <- data.frame(dias_semana, on, off, stringsAsFactors = FALSE)
      colnames(df_horario) <- c("Días", columnas)
    }else{ #JSON
      # SI ES JSON
      columnas <- c("ON", "OFF")
      dias_semana <- c("L","M","X","J","V","S","D")
      valores <- df$value
      on <- c()
      off <- c()
      for(i in 1:7){
        on <- c(on, unlist(valores[[i]])[1])
        off <- c(off, unlist(valores[[i]])[2])
      }
      df_horario <- data.frame(dias_semana, on, off, stringsAsFactors = FALSE)
      colnames(df_horario) <- c("Días", columnas)
    }
    
    return(df_horario)
  })
  
  # DF aclimas
  creacion_df_climas <- reactive({
    
    id_activo <- ids[grep(input$select_edificio_planta, names(ids))]
    claves <-  URLencode(c("Calendario_climatizadoras"))
    url <- paste("http://88.99.184.239:30951/api/plugins/telemetry/ASSET/",id_activo,"/values/attributes?keys=",claves,sep = "")
    peticion <- GET(url, add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"=auth_thb))
    
    df <- jsonlite::fromJSON(rawToChar(peticion$content))
    
    if(class(df$value) == "character"){ 
      json_horario <- fromJSON(df$value)
      #df_json <- as.data.frame(fromJSON(df$value))
      #Creación DF
      columnas <- c("ON", "OFF")
      dias_semana <- names(json_horario)
      on <- c()
      off <- c()
      for(i in 1:length(dias_semana)){
        on <- c(on, json_horario[[i]][[1]][[1]])
        off <- c(off, json_horario[[i]][[1]][[2]])
      }
      
      df_horario <- data.frame(dias_semana, on, off, stringsAsFactors = FALSE)
      colnames(df_horario) <- c("Días", columnas)
    }else{ #JSON
      # SI ES JSON
      columnas <- c("ON", "OFF")
      dias_semana <- c("L","M","X","J","V","S","D")
      valores <- df$value
      on <- c()
      off <- c()
      for(i in 1:7){
        on <- c(on, unlist(valores[[i]])[1])
        off <- c(off, unlist(valores[[i]])[2])
      }
      df_horario <- data.frame(dias_semana, on, off, stringsAsFactors = FALSE)
      colnames(df_horario) <- c("Días", columnas)
    }
    
    return(df_horario)
  })
  
  
  # Tabla alumbrado
  output$calendario_alumbrado_df <- renderDataTable({
    df <- creacion_df_alumbrado()
    
    datos$df_alumbrado = df
    
    datatable(df, editable = TRUE, rownames= FALSE, options = list(searchHighlight = TRUE,pageLength = 10, 
                                                  columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                                  scrollX=TRUE, 
                                                  scrollCollapse=TRUE))
  })
  
  # Tabla climas
  output$calendario_climas_df <- renderDataTable({
    df <- creacion_df_climas()
    
    datos$df_climas = df
    
    datatable(df, editable = TRUE, rownames= FALSE, options = list(searchHighlight = TRUE,pageLength = 10, 
                                                  columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                                  scrollX=TRUE, 
                                                  scrollCollapse=TRUE))
  })
  
  
  
  
  
  
  
  
  
  
  datos <- reactiveValues(df_alumbrado = NULL, df_climas = NULL)
  
  #Captura datos modificados
  
  # Alumbrado
  observeEvent(input$calendario_alumbrado_df_cell_edit, {
    print("SE HA EDITADO UN VALOR")
    print("Fila editada: ")
    print(input$calendario_alumbrado_df_cell_edit$row)
    print("Columna editada: ")
    print(input$calendario_alumbrado_df_cell_edit$col)
    print("Nuevo valor: ")
    print(input$calendario_alumbrado_df_cell_edit$value)
    
    datos$df_alumbrado[input$calendario_alumbrado_df_cell_edit$row, (input$calendario_alumbrado_df_cell_edit$col + 1)] <- input$calendario_alumbrado_df_cell_edit$value
    
  })
  
  # Climas
  observeEvent(input$calendario_climas_df_cell_edit, {
    print("SE HA EDITADO UN VALOR")
    print("Fila editada: ")
    print(input$calendario_climas_df_cell_edit$row)
    print("Columna editada: ")
    print(input$calendario_climas_df_cell_edit$col)
    print("Nuevo valor: ")
    print(input$calendario_climas_df_cell_edit$value)
    
    datos$df_climas[input$calendario_climas_df_cell_edit$row, (input$calendario_climas_df_cell_edit$col + 1)] <- input$calendario_climas_df_cell_edit$value
    
  })
  
  
  observeEvent(input$boton_guardar_en_BBDD, {
    shinyalert("¿Está seguro de que desea subir estos horarios?", "", showCancelButton=TRUE, showConfirmButton=TRUE, confirmButtonText = "Sí", cancelButtonText = "No", callbackR = confirmacion_subida)
  })
  
  
  
  
  confirmacion_subida <- function(value) {
    if(value){
      
      show_modal_spinner(
        spin = "double-bounce",
        color = "#D6E803",
        text = "Subiendo datos a la plataforma. Por favor, espere unos segundos",
        session = session
      )
      
      # Alumbrado
      df_alumbrado <-  datos$df_alumbrado
      print(df_alumbrado)
      
      json_envio <- '{'
      for(i in 1:nrow(df_alumbrado)){
        json_envio <- paste(json_envio,'"',df_alumbrado[i,1],'":[{"Horario_alumbrado_on":"',df_alumbrado[i,2],'","Horario_alumbrado_off":"',df_alumbrado[i,3],'"}],',sep = "")
        if(i == nrow(df_alumbrado)){
          json_envio <- paste(json_envio,'"',df_alumbrado[i,1],'":[{"Horario_alumbrado_on":"',df_alumbrado[i,2],'","Horario_alumbrado_off":"',df_alumbrado[i,3],'"}]}',sep = "")
        }
      }
      
      json_envio <- as.character(json_envio)
      
      json_envio <- paste('{"Calendario_alumbrado":',json_envio,'}',sep = "")
      print(json_envio)
      
      id_activo <- ids[grep(input$select_edificio_planta, names(ids))]
      url <- paste("http://88.99.184.239:30951/api/plugins/telemetry/ASSET/",id_activo,"/SERVER_SCOPE",sep = "")
      
      post <- httr::POST(url = url,
                         add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"=auth_thb),
                         body = json_envio,
                         verify= FALSE,
                         encode = "json",verbose()
      )
      
      
      
      
      
      
      # Climas
      df_climas <-  datos$df_climas
      print(df_climas)
      
      json_envio <- '{'
      for(i in 1:nrow(df_climas)){
        json_envio <- paste(json_envio,'"',df_climas[i,1],'":[{"Horario_climatizadoras_on":"',df_climas[i,2],'","Horario_climatizadoras_off":"',df_climas[i,3],'"}],',sep = "")
        if(i == nrow(df_climas)){
          json_envio <- paste(json_envio,'"',df_climas[i,1],'":[{"Horario_climatizadoras_on":"',df_climas[i,2],'","Horario_climatizadoras_off":"',df_climas[i,3],'"}]}',sep = "")
        }
      }
      
      json_envio <- paste('{"Calendario_climatizadoras":',json_envio,'}',sep = "")
      print(json_envio)
      
      id_activo <- ids[grep(input$select_edificio_planta, names(ids))]
      url <- paste("http://88.99.184.239:30951/api/plugins/telemetry/ASSET/",id_activo,"/SERVER_SCOPE",sep = "")
      
      post <- httr::POST(url = url,
                         add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"=auth_thb),
                         body = json_envio,
                         verify= FALSE,
                         encode = "json",verbose()
      )
      
      
      
      
      
      remove_modal_spinner(session = getDefaultReactiveDomain())
      
      shinyalert("Éxito", "Datos registrados correctamente", type = "success")
    }
  }
  
  
  
}


# Run the application
shinyApp(ui = ui, server = server)