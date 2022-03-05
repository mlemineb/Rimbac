server <- function(input, output,session) {
  
  #creating the valueBoxOutput content
  
  
  
  
  
  
  
  output$value1 <- renderValueBox({
    valueBox(
      paste(formatC(Nb_candidats_Total, format="d", big.mark=' '))
      ,paste('Candidats, dont',paste0(Tx_candidats_NKTT,"% à Nouakchott seulement"))
      ,icon = icon("user",lib='glyphicon')
      ,color = "blue")
    
  })
  
  
  Taux_admis<-round(sum(summary_serie_seul$admi)/sum(summary_serie_seul$candidats),digits=3)*100
  
  
  output$value2 <- renderValueBox({
    valueBox(
      paste0(Taux_admis, "%")
      ,paste("d'admis et",paste0(Taux_Sessionnaire,"% de Sessionaire"))
      ,icon = icon("bullhorn",lib='glyphicon')
      ,color = "green")
    
  })
  
  output$value3 <- renderValueBox({
    valueBox(
      paste(formatC(Nb_etablissement, format="d", big.mark=' '))
      ,paste('Etablissements,dont',paste0(TX_etablissement_nktt,"% localisés à Nouakchott"))
      ,icon = icon("landmark")
      ,color = "purple")
    
  })
  

  
  
  #creating the plotOutput content
  
  
  output$dash_pays <- renderEcharts4r({
    
    
    df %>% 
      e_charts(pays) %>% 
      e_pictorial(Taux_admis, flag,label= list(show=TRUE,position='top',formatter='{@[1]}%')) %>% 
      e_title("Images", "Taux d'admis au BAC") %>%
      e_x_axis(axisLabel = list(interval = 0, rotate = 45)) %>%
      e_legend(show = FALSE)
    
    
  })
  
  
  output$dash_wilaya_tx <- renderPlotly({
    
    fig <- plot_ly(summary_wilaya_seul2, x = ~Willaya, y = ~taux_reussite*100, type = 'bar', name = "Taux d'admis")
    fig<- fig %>%    add_text(text=~paste(summary_wilaya_seul2$taux_reussite*100,"%"), hoverinfo='none', textposition = 'top', showlegend = FALSE, textfont=list(size=10, color="black"))
    fig <- fig %>% layout(xaxis = list(title = 'Willaya'),yaxis = list(title = "Taux d'admis"), barmode = 'stack')
    fig
    
    fig
  })
  
  
  
  output$dash_wilaya <- renderPlotly({
    
    fig <- plot_ly(summary_wilaya_seul, x = ~Willaya, y = ~candidats, type = 'bar', name = 'Nombre de candidats', marker = list(color = 'rgb(49,130,189)'))
    fig <- fig %>% add_trace(y = ~admi, name = "Nombre d'admis", marker = list(color = 'green'))
    fig <- fig %>% layout(xaxis = list(title = "", tickangle = -45),
                          yaxis = list(title = ""),
                          margin = list(b = 100),
                          barmode = 'group')
    
    fig
  })
  
  
  output$dash_series_nb <- renderEcharts4r({
    
    summary_serie_seul %>%
    e_charts(name) %>%
      e_pie(candidats, radius = c("50%", "70%"),label = list(
        formatter = htmlwidgets::JS(
          "function(params){
      var vals = params.name.split(',')
      return(vals[0])}"))
      ) %>%
      e_legend(formatter = htmlwidgets::JS(
        "function(name){
      var vals = name.split(',')
      return(vals[0])}")) %>% 
      e_tooltip(formatter = htmlwidgets::JS("
                                        function(params){
                                        var vals = params.name.split(',')
                                        
                                        return('<strong>' + vals[0] + 
                                        '</strong><br />total: ' + params.value + 
                                        '<br />percent: ' +  vals[1])   }  ")) %>%
      e_labels(show= TRUE,
               formatter="{c} \n {d}%",
               position="outside")
    
    
  })
  
  
  output$dash_series <- renderEcharts4r({
    
    
    summary_serie_seul2 %>%
      e_charts(Serie.x) %>%
      e_bar(taux_echec, stack = "grp") %>%
      e_bar(taux_sesionnaires, stack = "grp") %>%
      e_bar(taux_reussite, stack = "grp") %>%
      e_grid(containLabel = T) %>%
      e_tooltip(trigger = "axis", formatter = htmlwidgets::JS('
                                                          function (params) {
            let tooltip = `<p style="width: 100px;">${params[0].axisValue}</p>`;
            let total = 0
            params.forEach(({ seriesName, marker, value }) => {
              value = value || [0, 0];
              tooltip += `<p style="width: 100px;">${marker} ${seriesName}<span style="float: right;"><strong>${value[1]}%</strong></span></p>`;
              total += Number(value[1]);
            });
            
            tooltip += `<p style="width: 120px;">Total<span style="float: right;"><strong>${total}%</strong></span>`;
            
            return tooltip;
          }'))
    
    
  })
  
  
  
  output$dash_nktt_autre_nb <- renderPlotly({
    
    
    fig <- plot_ly(tab_nktt, labels = ~nktt, values = ~candidats, type = 'pie')
    fig <- fig %>% layout(
      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    fig
    
  })
  
  
  
  output$dash_nktt_autre_tx <- renderPlotly({
    colors <- c('#CC1480', '#FF9673')
    
    fig <- plot_ly(tab_nktt,  x = ~nktt, y = ~taux_admis*100, type = 'bar',color = ~nktt )
    fig<- fig %>%  add_text(text=~paste(tab_nktt$taux_admis*100,"%"), hoverinfo='none', textposition = 'top', showlegend = FALSE, textfont=list(size=10, color="black"))
    fig <- fig %>% layout(xaxis = list(title = 'Region'),yaxis = list(title = "Taux d'admis"), barmode = 'stack')
    fig
    
  })
  
  summary_mouqataa_seul<-summary_mouqataa_seul[order(summary_mouqataa_seul$taux_reussite,decreasing = T),]
  summary_mouqataa_seul<-summary_mouqataa_seul[1:10,]
  summary_mouqataa_seul$moughataa <- factor(summary_mouqataa_seul$moughataa, levels = summary_mouqataa_seul$moughataa[order(summary_mouqataa_seul$taux_reussite,decreasing = F)])
  
  output$dash_moukataa_tx <- renderPlotly({
    
    fig <- plot_ly(summary_mouqataa_seul, x = ~moughataa , y = ~taux_reussite, type = 'bar', name = "Taux d'admis")
    fig<- fig %>%  add_text(text=~paste(summary_mouqataa_seul$taux_reussite,"%"), hoverinfo='none', textposition = 'top', showlegend = FALSE, textfont=list(size=10, color="black"))
    fig <- fig %>% layout(xaxis = list(title = 'Mouqataa'),yaxis = list(title = "Taux d'admis"), barmode = 'stack')
    fig
    
    
  })
  
  summary_mouqataa_seul2<-summary_mouqataa_seul2[order(summary_mouqataa_seul2$taux_echec,decreasing = T),]
  summary_mouqataa_seul2<-summary_mouqataa_seul2[1:10,]
  summary_mouqataa_seul2$moughataa <- factor(summary_mouqataa_seul2$moughataa, levels = summary_mouqataa_seul2$moughataa[order(summary_mouqataa_seul2$taux_echec,decreasing = F)])
  
  output$dash_moukataa_tx_echec <- renderPlotly({
    
    fig <- plot_ly(summary_mouqataa_seul2, x = ~moughataa , y = ~taux_echec, type = 'bar', name = "Taux d'echec")
    fig<- fig %>%  add_text(text=~paste(summary_mouqataa_seul2$taux_echec,"%"), hoverinfo='none', textposition = 'top', showlegend = FALSE, textfont=list(size=10, color="black"))
    fig <- fig %>% layout(xaxis = list(title = 'Mouqataa'),yaxis = list(title = "Taux d'echec"), barmode = 'stack')
    fig
    
    
  })
  
  
  output$dash_moy <- renderPlotly({
    
    plot_ly(candidate_details, x = candidate_details$Serie.x, y = round(candidate_details$moyeneGeneral,1), type = "box",color = ~candidate_details$Serie.x) %>% 
      layout( xaxis = list(title='Séries'), 
              yaxis = list(title='Moyenne Generale'))
    
    
  })
  
  output$dash_moy_age <- renderPlotly({
    
    plot_ly(candidate_details, x = candidate_details$Serie.x, y = candidate_details$age, type = "box",color = ~candidate_details$Serie.x) %>% 
      layout( xaxis = list(title='Séries'), 
              yaxis = list(title='Age'))
    
    
  })
  
  
  
  ############################################################################################################################
  
  
  # Create a subset data frame
  sub_data <- reactive({
    tab_global
    
    if (input$i2_ed == "Toutes") {
      tab_global
    } else {
      tab_global[tab_global$moughataa == input$i2_ed,]
    }
  })
  
  
  nb_candidats_carte<-reactive( {
    n<-sum(sub_data()$candidats)
    n
  })
  
  
  
  
  ### Nombre de candidats
  
  output$count1 <-renderValueBox({
    valueBox(
      nb_candidats_carte(),
      'Nombre de candidats',
      icon = icon("user",lib='glyphicon'),
      color = "blue"
    )
  })
  
  ### Number of Deaths
  
  output$count2 <-renderValueBox({
    valueBox(
      sum(sub_data()$c_admis),
      "Nombre d'admis",
      icon("bullhorn",lib='glyphicon'),
      color = "green"
    )
  })
  
  ### Death Rate
  
  output$count3 <-renderValueBox({
    valueBox(
      paste0(sprintf("%.2f", round(sum(sub_data()$c_admis)/sum(sub_data()$candidats),digits=3)*100), "%"),
      "Taux d'admis",
      icon("exclamation-sign",lib='glyphicon'),
      color = 'red'
    )
  })
  
  ### Location
  output$count4 <-renderValueBox({
    valueBox(
      length(unique(sub_data()$etablissement)),
      "Nombre d'établissement",
      icon = icon("landmark"),
      color = "teal"
    )
  })
  
  
  
  
  centr <- rgeos::gCentroid(decoupage, byid = TRUE)
  
  centr <- sp::SpatialPointsDataFrame(centr, data= decoupage@data) 
  
  
  map_point_data <- centr@data %>%
    mutate(lat = sp::coordinates(centr)[,2],
           long = sp::coordinates(centr)[,1])
  
  
  
  popup_icons <- awesomeIconList(
    "1037-5372" = makeAwesomeIcon(icon = "stats", library = "glyphicon", markerColor = "green"),
    "218-1036" = makeAwesomeIcon(icon = "stats", library = "glyphicon", markerColor = "lightblue"),
    "20-217" = makeAwesomeIcon(icon = "stats", library = "glyphicon", markerColor = "orange"),
    "1-19" = makeAwesomeIcon(icon = "stats", library = "glyphicon", markerColor = "red"),
    "0" = makeAwesomeIcon(icon = "stats", library = "glyphicon", markerColor = "darkred")
  )
  
  map_point_data$icon_group = cut(
    map_point_data$candidats,
    breaks = c(-1, 0, 19, 217, 1036, 5372),
    labels = c("0", "1-19", "20-217", "218-1036", "1037-5372"))
  
  
  names(map_point_data)[c(23:24)]<-c('Latitude',"Longitude")
  
  
  bins_nb_chq <- c(0.0 ,3.7, 5.6, 8, 15, 28)
  mypal <- colorBin("RdYlGn", domain = decoupage$taux_reussite*100, bins = bins_nb_chq) 
  
  labels <- sprintf(
    "<strong>%s</strong><br/>%g candidats admis<br/> soit, %s</sup>",
    decoupage$ADM2_EN,decoupage$admi,  paste0(decoupage$taux_reussite*100,'% pour un total ', decoupage$candidats,' candidats')
  ) %>% lapply(htmltools::HTML)
  
  
  
  
  
  sub_data2 <- reactive({
    
    
    
    if (input$i2_ed == "Toutes" | input$i2_ed == "") {
      map_point_data
    } else {
      map_point_data[map_point_data$ADM2_EN == input$i2_ed,]
    }
  })
  
  
  
  output$leaflet_map <- renderLeaflet({
    leaflet(data=decoupage,options = leafletOptions(minZoom = 2, maxZoom = 14)) %>%
      setView(lng=	-15.95, lat= 18.1, zoom=6) %>%  
      
      addProviderTiles(providers$OpenStreetMap.Mapnik, group = "Open Street Map", options = providerTileOptions(noWrap = TRUE)) %>%
      leaflet.extras::addFullscreenControl(position = "topleft") %>%
      addControl(html = markerLegendHTML(popup_icons), position = "bottomright") %>%
      addPolygons(
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.4,
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.4,
          bringToFront = TRUE),
        fillColor = ~mypal(decoupage$taux_reussite*100),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")) %>%
      addLegend("bottomleft", pal = mypal, values = ~decoupage$taux_reussite*100, labels = "", title = "Pourcentage d'admis en 1ere session")
  })
  
  update_leaflet <- function(map_name, input) {
    
    # if (input$severity != 'all') {
    #   leaflet_data <- data[Date==input$date & icon_group==input$severity]
    # } else {
    #   leaflet_data <- data[Date==input$date]
    # }
    leaflet_data <- sub_data2()
    
    if (input$i2_ed == "Toutes") {
      leaflet_country <- list(
        Longitude = -15.95,
        Latitude = 18.1,
        zoomLevel = 6
      )
    } else if (input$i2_ed == "") {
      
      leaflet_country <- data.frame(
        Longitude = leaflet_data$Longitude,
        Latitude = leaflet_data$Latitude,
        zoomLevel = 6
      )
      
    } else {
      
      leaflet_country <- list(
        Longitude = leaflet_data$Longitude,
        Latitude = leaflet_data$Latitude,
        zoomLevel = 6
      )
      
    }
    
    leafletProxy(map_name, data = leaflet_data) %>%
      clearGroup(group = "moukataa") %>%
      addAwesomeMarkers(
        ~Longitude,
        ~Latitude,
        group = "moukataa",
        #label = ~lapply(label, htmltools::HTML),
        icon = ~popup_icons[icon_group]
      ) %>%
      setView(
        lng = mean(leaflet_country$Longitude),
        lat = mean(leaflet_country$Latitude),
        zoom = mean(leaflet_country$zoomLevel)
      )
  }
  
  observeEvent(
    {
      input$i2_ed
    },
    update_leaflet("leaflet_map", input)
  )
  
  
  
  # code to load the park card once the click event on a marker is intercepted 
  observeEvent(input$leaflet_map_marker_click, { 
    pin <- input$leaflet_map_marker_click
    print(Sys.time()) #uncomment to log coords
    print(pin) #uncomment to log coords
    selectedPoint <- reactive(sub_data2()[sub_data2()$Latitude == pin$lat & sub_data2()$Longitude == pin$lng,])
    leafletProxy("leaflet_map", data = selectedPoint()) %>% clearPopups() %>% 
      addPopups(~Longitude,
                ~Latitude,
                popup = ~park_card(selectedPoint()$ADM2_EN,selectedPoint()$ADM1_EN, selectedPoint()$candidats,
                                   paste0(selectedPoint()$taux_session*100,"%"), paste0(selectedPoint()$taux_triche*100,"%"),paste0(selectedPoint()$taux_reussite*100,"%"))
      )
  })
  
  observe({
    updateSelectizeInput(session, "select_wilaya", choices = m_willaya)
  })
  
  observe({
    updateSelectizeInput(session, "select_moukataa", choices = c("Toutes",as.character(sort(unique(tab_global$moughataa[tab_global$Willaya==input$select_wilaya])))))
  })
  
  observe({
    updateSelectizeInput(session, "select_etablissement", choices = c("Toutes",as.character(sort(unique(tab_global$etablissement[tab_global$moughataa==input$select_moukataa])))))
  })
  
  
  output$my_data_tab <- DT::renderDataTable(DT::datatable({
    data <- tab_global
    if (input$select_wilaya != "Toutes") {
      data <- data[data$Willaya == input$select_wilaya,]
    }
    if (input$select_moukataa != "Toutes") {
      data <- data[data$moughataa == input$select_moukataa,]
    }
    
    if (input$select_etablissement != "Toutes") {
      data <- data[data$etablissement == input$select_etablissement,]
    }
    names(data)<-c("Willaya","moughataa","etablissement","Serie",  "candidats","admis","session" ,"triches",  "ajournes", "abscents", "age moyen", "moyenne generale",
                   "taux ajourné" ,"taux admis" ,"taux sessionnaire" ,"taux triche" ,"taux abscents","moyenne matiere principale 1", "moyenne matiere principale 2",  
                   "moyenne matiere principale 3",   "moyenne matiere secondaire 1","moyenne matiere secondaire 2","moyenne matiere secondaire 3", "moyenne matiere secondaire 4")
    data
    data
  }, escape = FALSE, selection = 'single', extensions = c("Responsive"), 
  rownames = FALSE,
  options = list(
    language =list(url = "French_datable.json"),
    responsive = TRUE,
    autoWidth = T,
    #Bfrtip
    pageLength = 10 ,
    searching = TRUE,
    scrollX=T,
    scrollY=T
    #fixedColumns = list(leftColumns = 2)
  )))
  
  
}

