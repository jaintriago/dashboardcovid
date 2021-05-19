
server <- function(input, output, session) {
  

  output$continente <- renderPlotly({
    owidhh1<- owid
    #owidhh1 <- owid %>%   filter(continent == input$w)
    owidhh1 <- owidhh1 %>% group_by(date, continent) %>% summarise(casos = sum(total_cases, na.rm = T))
    
    p <-   owidhh1 %>% ggplot( aes(x=date, y= casos, fill=continent)) +
      geom_col() +
      scale_fill_brewer(palette = "Set2") +
      scale_x_date(labels = function(x) format(x, "%Y.%m.%d"), date_breaks = "2 months")+
      theme_bw()+
      ggtitle("Evolución de los casos diarios por continente") +
      ylab("Total de casos") +
      xlab("Fechas") 
    ggplotly(p)
    
  })
  
  output$muertes <- renderPlotly({
    owidhh <- owid
    #owidhh <- owid %>%   filter(continent == input$w)
    owidhh <- owidhh %>% group_by(date, continent) %>% summarise(muertes = sum(total_deaths, na.rm = T))
    
    p0 <-   owidhh %>% ggplot( aes(x=date, y= muertes, fill=continent)) +
      geom_col() +
      scale_fill_brewer(palette = "Set2") +
      scale_x_date(labels = function(x) format(x, "%Y.%m.%d"), date_breaks = "2 months")+
      theme_bw()+
      ggtitle("Evolución de las muertes diarias por continente") +
      ylab("Total de muertes") +
      xlab("Fechas") 
    ggplotly(p0)
    
  })
  
  
  output$paises <- renderPlotly({
    owidg <- owid
    #owidg <- owid %>% filter(continent == input$w)
    owidg <- owidg %>% filter(!location %in% c("Africa", "Asia" , "Europe" , "European Union" , "International" ,
                                       "North America" , "Oceania" , "South America" , "World"))
    
    owid5 <- owidg %>% group_by(location) %>% summarise(casos = sum(new_cases_per_million, na.rm = T))
    owid6 <- owidg %>% group_by(location) %>% summarise(muertes = sum(new_deaths_per_million, na.rm = T))
    owid7 <- owidg %>% group_by(location) %>% summarise(test = sum(new_tests, na.rm = T))
    joinowid <- owid5 %>%  inner_join(owid6, by = "location")
    joinowid3 <- joinowid %>%  inner_join(owid7, by = "location")
    
    cm <-  joinowid3 %>% ggplot( aes(muertes, casos,  size = test, fill=location)) +
      geom_point(alpha=0.8, shape=21, color="black") +
      scale_fill_viridis(discrete=TRUE, guide=FALSE, option="A") +
      theme_bw() +
      ylab("Casos por millón") +
      xlab("Muertes por millón") +
      ggtitle("Casos y muertes") +
      theme(legend.position = "none")
    
    ggplotly(cm)
   
   
    
  })
  
  output$tvacunas <- renderPlotly({
    owig <- owid
    #owig <- owid %>% filter(continent == input$y)
    df <- owig %>% filter(!location %in% c("Africa", "Asia" , "Europe" , "European Union" , "International" ,
                                           "North America" , "Oceania" , "South America" , "World"))
    
    owidv <- df %>% group_by(location) %>% summarise(vacunas = sum(new_vaccinations, na.rm = T))
    
    pw <- owidv %>% arrange(desc(vacunas)) %>% head(30) %>% ggplot(aes(x = reorder(location, vacunas), y = vacunas, fill=vacunas )) +
      geom_col(show.legend = FALSE) + 
      coord_flip()  + scale_fill_viridis(discrete = F, option = "C", direction = -1) + theme_minimal() + 
      theme(plot.title = element_text(face = "bold", size = 13)) +
      theme(axis.text = element_text(size=10))+ 
      labs(x = "Países",
           y = "Total de vacunas",
           title = "Total de vacunas por país") + theme(legend.position = "none")
    
    ggplotly(pw)
  })
  
  output$pvacunas <- renderPlotly({
    owigf <- owid
    #owigf <- owid %>% filter(continent == input$y)
    dff <- owigf %>% filter(!location %in% c("Africa", "Asia" , "Europe" , "European Union" , "International" ,
                                           "North America" , "Oceania" , "South America" , "World"))
    
    owidvf <- dff %>% group_by(location) %>% summarise(peo_vac = max(people_vaccinated, na.rm = T))
    
    pwf <- owidvf %>% arrange(desc(peo_vac)) %>% head(30) %>% ggplot(aes(x = reorder(location, peo_vac), y = peo_vac, fill=peo_vac)) +
      geom_col(show.legend = FALSE) + 
      coord_flip()  + scale_fill_viridis(discrete = F, option = "C", direction = -1) + theme_minimal() + 
      theme(plot.title = element_text(face = "bold", size = 13)) +
      theme(axis.text = element_text(size=10))+ 
      labs(x = "Países",
           y = "Total de personas vacunadas",
           title = "Total de personas vacunadas  por país") + theme(legend.position = "none")
    
    ggplotly(pwf)
    
  })

  output$fvacunas <- renderPlotly({
    owigfh <- owid
    #owigfh <- owid %>% filter(continent == input$y)
    dffh <- owigfh %>% filter(!location %in% c("Africa", "Asia" , "Europe" , "European Union" , "International" ,
                                             "North America" , "Oceania" , "South America" , "World"))
    
    owidvfh <- dffh %>% group_by(location) %>% summarise(fully_vacunas = max(people_fully_vaccinated, na.rm = T))
    
    pwfh <- owidvfh %>% arrange(desc(fully_vacunas)) %>% head(30) %>% ggplot(aes(x = reorder(location, fully_vacunas), y = fully_vacunas, fill=fully_vacunas )) +
      geom_col(show.legend = FALSE) + 
      coord_flip()  + scale_fill_viridis(discrete = F, option = "C", direction = -1) + theme_minimal() + 
      theme(plot.title = element_text(face = "bold", size = 13)) +
      theme(axis.text = element_text(size=10))+ 
      labs(x = "Países",
           y = "Total de personas completamente vacunadas",
           title = "Total de personas completamente vacunadas  por país") + theme(legend.position = "none")
    
    ggplotly(pwfh)
  }) 
  
  output$mapa1 <- renderLeaflet({
    
    pal <- colorFactor(c("cyan2", "darksalmon", "lightgreen"), domain = c("Sin seguimiento", "Seguimiento limitado", "Seguimiento integral"))
    paises <- geo_join(countries, contact, 'ADMIN', 'Entity')

    tag.map.title <- tags$style(HTML("
                                     .leaflet-control.map-title { 
                                     transform: translate(-50%,20%);
                                     position: fixed !important;
                                     left: 50%;
                                     text-align: center;
                                     padding-left: 10px; 
                                     padding-right: 10px; 
                                     background: rgba(255,255,255,0.75);
                                     font-weight: bold;
                                     font-size: 28px;
                                     }
                                     "))
    
    title <- tags$div(
      tag.map.title, HTML("Mapa del rastreo de contactos Covid 19")
    )  
    
    
    map <- leaflet(paises) %>% addTiles()
    cont_tr <- map %>%
      addPolygons(stroke = F, smoothFactor = 0.5, fillOpacity = 0.5, opacity = 1, color = "black",
                  fillColor = ~pal(contact_tracing), popup = ~contact_tracing, 
                  label = ~contact_tracing
      ) %>%
      addLegend("bottomright", pal = pal, values = ~contact_tracing,
                title = "Seguimiento de contactos",  opacity = 1) %>% 
      addProviderTiles("CartoDB.Positron", options = providerTileOptions(opacity = 1)) %>% 
      addTiles() %>%
      addControl(title, position = "topright", className="map-title")
    
    cont_tr
  })
  
  
  output$mapa2 <- renderLeaflet({
    
    pal1 <- colorFactor(c("cyan2", "darksalmon", "lightgreen", "mediumorchid1"), domain = c("Sin políticas", "Con sintomas y ciertos grupos", "Cualquiera con sintomas", "Pruebas abiertas"))
    paises1 <- geo_join(countries, test_p, 'ADMIN', 'Entity')
    
    tag.map.title1 <- tags$style(HTML("
                                     .leaflet-control.map-title { 
                                     transform: translate(-50%,20%);
                                     position: fixed !important;
                                     left: 50%;
                                     text-align: center;
                                     padding-left: 10px; 
                                     padding-right: 10px; 
                                     background: rgba(255,255,255,0.75);
                                     font-weight: bold;
                                     font-size: 28px;
                                     }
                                     "))
    
    title1 <- tags$div(
      tag.map.title1, HTML("Mapa de politica de pruebas")
    )  
    
    
    map1 <- leaflet(paises1) %>% addTiles()
    cont_tr1 <- map1 %>%
      addPolygons(stroke = F, smoothFactor = 0.5, fillOpacity = 0.5, opacity = 1, color = "black",
                  fillColor = ~pal1(testing_policy), popup = ~testing_policy, 
                  label = ~testing_policy
      ) %>%
      addLegend("bottomright", pal = pal1, values = ~testing_policy,
                title = "Politica de pruebas",  opacity = 1) %>% 
      addProviderTiles("CartoDB.Positron", options = providerTileOptions(opacity = 1)) %>% 
      addTiles() %>%
      addControl(title1, position = "topright", className="map-title")
    
    cont_tr1
  })
  
  
  output$mapa3 <- renderLeaflet({
    
    pal2 <- colorFactor(c("cyan2", "darksalmon", "lightgreen", "mediumorchid1", "violetred1", "olivedrab1"), domain = c("Ninguna", "Un grupo vulnerable", "Dos grupos vulnerables", "Todos los grupos vulnerables", "Vulnerables más otros", "Todos"))
    paises2 <- geo_join(countries, vac_p, 'ADMIN', 'Entity')
    
    tag.map.title2 <- tags$style(HTML("
                                      .leaflet-control.map-title { 
                                      transform: translate(-50%,20%);
                                      position: fixed !important;
                                      left: 50%;
                                      text-align: center;
                                      padding-left: 10px; 
                                      padding-right: 10px; 
                                      background: rgba(255,255,255,0.75);
                                      font-weight: bold;
                                      font-size: 28px;
                                      }
                                      "))
    
    title2 <- tags$div(
      tag.map.title2, HTML("Mapa de politica de vacunas")
    )  
    
    
    map2 <- leaflet(paises2) %>% addTiles()
    cont_tr2 <- map2 %>%
      addPolygons(stroke = F, smoothFactor = 0.5, fillOpacity = 0.5, opacity = 1, color = "black",
                  fillColor = ~pal2(vaccination_policy), popup = ~vaccination_policy, 
                  label = ~vaccination_policy
      ) %>%
      addLegend("bottomright", pal = pal2, values = ~vaccination_policy,
                title = "Politica de vacunas",  opacity = 1) %>% 
      addProviderTiles("CartoDB.Positron", options = providerTileOptions(opacity = 1)) %>% 
      addTiles() %>%
      addControl(title2, position = "topright", className="map-title")
    
    cont_tr2
  })
 
  output$mapa4 <- renderLeaflet({
    
    pal3 <- colorFactor(c("cyan2", "darksalmon", "lightgreen"), domain = c("Sin apoyo", "Cubre<50% del salario perdido", "Cubre> 50% del salario perdido"))
    paises3 <- geo_join(countries, income, 'ADMIN', 'Entity')
    
    tag.map.title3 <- tags$style(HTML("
                                      .leaflet-control.map-title { 
                                      transform: translate(-50%,20%);
                                      position: fixed !important;
                                      left: 50%;
                                      text-align: center;
                                      padding-left: 10px; 
                                      padding-right: 10px; 
                                      background: rgba(255,255,255,0.75);
                                      font-weight: bold;
                                      font-size: 28px;
                                      }
                                      "))
    
    title3 <- tags$div(
      tag.map.title3, HTML("Mapa de apoyo de ingresos")
    )  
    
    map3 <- leaflet(paises3) %>% addTiles()
    cont_tr3 <- map3 %>%
      addPolygons(stroke = F, smoothFactor = 0.5, fillOpacity = 0.5, opacity = 1, color = "black",
                  fillColor = ~pal3(income_support), popup = ~income_support, 
                  label = ~income_support
      ) %>%
      addLegend("bottomright", pal = pal3, values = ~income_support,
                title = "Apoyo de ingresos",  opacity = 1) %>% 
      addProviderTiles("CartoDB.Positron", options = providerTileOptions(opacity = 1)) %>% 
      addTiles() %>%
      addControl(title3, position = "topright", className="map-title")
    
    cont_tr3
  }) 
  
  output$mapa5 <- renderLeaflet({
    
    pal4 <- colorFactor(c("cyan2", "darksalmon", "lightgreen", "mediumorchid1"), domain = c("Sin medidas", "Recomendado", "Requerido-algunos niveles", "Requerido-para todos"))
    paises4 <- geo_join(countries, school, 'ADMIN', 'Entity')
    
    tag.map.title4 <- tags$style(HTML("
                                      .leaflet-control.map-title { 
                                      transform: translate(-50%,20%);
                                      position: fixed !important;
                                      left: 50%;
                                      text-align: center;
                                      padding-left: 10px; 
                                      padding-right: 10px; 
                                      background: rgba(255,255,255,0.75);
                                      font-weight: bold;
                                      font-size: 28px;
                                      }
                                      "))
    
    title4 <- tags$div(
      tag.map.title4, HTML("Mapa de cierre de escuelas")
    )  
    
    map4 <- leaflet(paises4) %>% addTiles()
    cont_tr4 <- map4 %>%
      addPolygons(stroke = F, smoothFactor = 0.5, fillOpacity = 0.5, opacity = 1, color = "black",
                  fillColor = ~pal4(school_closures), popup = ~school_closures, 
                  label = ~school_closures
      ) %>%
      addLegend("bottomright", pal = pal4, values = ~school_closures,
                title = "Cierre de escuelas",  opacity = 1) %>% 
      addProviderTiles("CartoDB.Positron", options = providerTileOptions(opacity = 1)) %>% 
      addTiles() %>%
      addControl(title4, position = "topright", className="map-title")
    
    cont_tr4
  }) 
  
  
  output$mapa6 <- renderLeaflet({
    
    pal5 <- colorFactor(c("cyan2", "darksalmon", "lightgreen", "mediumorchid1"), domain = c("Sin medidas", "Recomendado", "Requerido-algunos", "Requerido-para todos menos trabajos claves"))
    paises5 <- geo_join(countries, work, 'ADMIN', 'Entity')
    
    tag.map.title5 <- tags$style(HTML("
                                      .leaflet-control.map-title { 
                                      transform: translate(-50%,20%);
                                      position: fixed !important;
                                      left: 50%;
                                      text-align: center;
                                      padding-left: 10px; 
                                      padding-right: 10px; 
                                      background: rgba(255,255,255,0.75);
                                      font-weight: bold;
                                      font-size: 28px;
                                      }
                                      "))
    
    title5 <- tags$div(
      tag.map.title5, HTML("Mapa de cierre de lugares de trabajo")
    )  
    
    map5 <- leaflet(paises5) %>% addTiles()
    cont_tr5 <- map5 %>%
      addPolygons(stroke = F, smoothFactor = 0.5, fillOpacity = 0.5, opacity = 1, color = "black",
                  fillColor = ~pal5(workplace_closures), popup = ~workplace_closures, 
                  label = ~workplace_closures
      ) %>%
      addLegend("bottomright", pal = pal5, values = ~workplace_closures,
                title = "Cierre de lugar de trabajo",  opacity = 1) %>% 
      addProviderTiles("CartoDB.Positron", options = providerTileOptions(opacity = 1)) %>% 
      addTiles() %>%
      addControl(title5, position = "topright", className="map-title")
    
    cont_tr5
  }) 
  
  output$mapa7 <- renderLeaflet({
    
    pal6 <- colorFactor(c("cyan2", "darksalmon", "lightgreen", "mediumorchid1"), domain = c("Sin medidas", "Recomendado", "Casos esenciales", "Todos menos pocas excepciones"))
    paises6 <- geo_join(countries, home, 'ADMIN', 'Entity')
    
    tag.map.title6 <- tags$style(HTML("
                                      .leaflet-control.map-title { 
                                      transform: translate(-50%,20%);
                                      position: fixed !important;
                                      left: 50%;
                                      text-align: center;
                                      padding-left: 10px; 
                                      padding-right: 10px; 
                                      background: rgba(255,255,255,0.75);
                                      font-weight: bold;
                                      font-size: 28px;
                                      }
                                      "))
    
    title6 <- tags$div(
      tag.map.title6, HTML("Mapa de requerimiento de quedarse en casa")
    )  
    
    map6 <- leaflet(paises6) %>% addTiles()
    cont_tr6 <- map6 %>%
      addPolygons(stroke = F, smoothFactor = 0.5, fillOpacity = 0.5, opacity = 1, color = "black",
                  fillColor = ~pal6(stay_home_requirements), popup = ~stay_home_requirements, 
                  label = ~stay_home_requirements
      ) %>%
      addLegend("bottomright", pal = pal6, values = ~stay_home_requirements,
                title = "Quedarse en casa",  opacity = 1) %>% 
      addProviderTiles("CartoDB.Positron", options = providerTileOptions(opacity = 1)) %>% 
      addTiles() %>%
      addControl(title6, position = "topright", className="map-title")
    
    cont_tr6
  }) 
  
}
