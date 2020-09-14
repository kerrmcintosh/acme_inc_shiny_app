library(shiny)

server <- function(input, output) {
  
  filtered_date_sources <- reactive({
    ai_source_regrouped %>% 
      filter(yearMonth >= input$date_tab3[1] & yearMonth <= input$date_tab3[2])
    
  })
  
  filtered_date_sourcesz <- reactive({
    ai_source_regrouped %>% 
      filter(yearMonth >= input$date_tab2[1] & yearMonth <= input$date_tab2[2])
    
  })
  
  #---------------------------------------------------------------------
  #TAB2 Traffic
  output$total_plot <- renderPlot({
    filtered_date_sourcesz() %>% 
      group_by(catchment, yearMonth) %>% 
      summarise(count = ifelse(input$usersesh == "sessions", sum(sessions), sum(users))) %>% 
      ggplot() +
      aes(x = yearMonth, y = count, colour = catchment) +
      geom_line(aes(group = catchment), size = 1.2) +
      labs(y = ifelse(input$usersesh == "sessions", "User Sessions", "User Traffic") ,
           title = paste("Acme Inc's Web Traffic Over Time")
      ) +
      theme_light() +
      theme(axis.text.x = element_text(size = 11), 
            axis.title.x = element_blank(),
            axis.title.y = element_text(size = 11),
            legend.title=element_blank(),
            legend.text=element_text(size=11),
            title = element_text(size = 14, face = "bold", colour = "#696969"),
            panel.border = element_rect(colour = "#696969", fill=NA, size=1))  +
      scale_x_date(date_breaks = "3 months", labels = scales::date_format("%m/%y")) +
      viridis::scale_fill_viridis()
  })
  
  output$date_traf<- renderText({
    paste("Overall Web Traffic for Chosen Date Range")
  })
  
  output$ed_users <- renderTable({
    filtered_date_sourcesz() %>%
      filter(catchment  == "Edinburgh") %>% 
      summarise(Sessions = format(as.integer(sum(sessions)), big.mark=","), Users = format(as.integer(sum(users)), big.mark=","))
  })
  output$gl_users <- renderTable({
    filtered_date_sourcesz() %>%
      filter(catchment  == "Glasgow") %>% 
      summarise(Sessions = format(as.integer(sum(sessions)), big.mark=","), Users = format(as.integer(sum(users)), big.mark=","))
  })
  
  output$iv_users <- renderTable({
    filtered_date_sourcesz() %>%
      filter(catchment  == "Inverness") %>% 
      summarise(Sessions = format(as.integer(sum(sessions)), big.mark=","), Users = format(as.integer(sum(users)), big.mark=","))
  })
  
  #---------------------------------------------------------------------
  #TAB3 Source Medium  - Kerr 
  
  
  
  output$source_bar_plot <- renderPlot({
    filtered_date_sources() %>%
      group_by(catchment, ai_source) %>% 
      summarise(count = sum(sessions)) %>%
      ggplot() +
      aes(x = ai_source, y = count, group = catchment, fill = catchment) +
      geom_col(position = "dodge") +
      labs(y = "User Sessions",
           title = paste("An Overview of the Source of Acme Inc's Website Traffic")) +
      theme_light() +
      theme(axis.text.x = element_text(size = 11), 
            axis.title.x = element_blank(),
            axis.title.y = element_text(size = 11),
            legend.title=element_blank(),
            legend.text=element_text(size=11),
            title = element_text(size = 14, face = "bold", colour = "#696969"),
            panel.border = element_rect(colour = "#696969", fill=NA, size=1)) +
      viridis::scale_color_viridis()
  })
  
  output$source_plot <- renderPlot({
    filtered_date_sources() %>%
      filter(ai_source == input$medium)  %>%
      group_by(catchment, yearMonth, ai_source) %>% 
      summarise(count =sum(sessions)) %>% 
      ggplot() +
      aes(x = yearMonth, y = count, colour = catchment) +
      geom_line(aes(group = catchment), size = 1.2) +
      labs(y = "User Sessions",
           title = paste(input$medium, "Website Traffic Generated Over Time")
      ) +
      theme_light() +
      theme(axis.text.x = element_text(size = 11), 
            axis.title.x = element_blank(),
            axis.title.y = element_text(size = 11),
            legend.title=element_blank(),
            legend.text=element_text(size=11),
            title = element_text(size = 14, face = "bold", colour = "#696969"),
            panel.border = element_rect(colour = "#696969", fill=NA, size=1)) +
      scale_x_date(date_breaks = "3 months", labels = scales::date_format("%m/%y")) +
      viridis::scale_fill_viridis()
      
      
  })
  
  output$grp_traf <- renderTable({
    filtered_date_sources() %>% 
      filter(ai_source == input$medium) %>% 
      group_by(catchment) %>% 
      summarise(total_traffic =format(as.integer(sum(sessions)), big.mark=","))
  }, colnames = FALSE)
  
  output$traf_med<- renderText({
    paste(input$medium, "traffic")
  })
  
  output$ed_traf_med<- renderText({
    paste("Top 5", input$medium, "mediums generating traffic for Edinburgh")
  })
  output$gl_traf_med<- renderText({
    paste("Top 5", input$medium, "mediums generating traffic for Glasgow")
  })
  output$iv_traf_med<- renderText({
    paste("Top 5", input$medium, "mediums generating traffic for Inverness")
  })
  

  output$gl_grp_traf <- renderTable({
    filtered_date_sources() %>% 
      filter( catchment == "Glasgow") %>% 
      filter(ai_source == input$medium) %>% 
      summarise(ormat(as.integer(sum(sessions)), big.mark=","))
  }, colnames = FALSE)
  
  output$iv_grp_traf <- renderTable({
    filtered_date_sources() %>% 
      filter( catchment == "Inverness") %>% 
      filter(ai_source == input$medium) %>% 
      summarise(format(as.integer(sum(sessions)), big.mark=","))
  }, colnames = FALSE)
  
  output$medium_detail_ed <- renderTable({head(
    filtered_date_sources() %>% 
      filter( catchment == "Edinburgh") %>% 
      filter(ai_source == input$medium) %>% 
      group_by(specific_source) %>% 
      summarise(count = as.integer(sum(sessions))) %>% 
      arrange(desc(count)) %>% 
      mutate(count = format(count, big.mark =",")), 5)
  }, colnames = FALSE, align = "lr")
  
  output$medium_detail_gl <- renderTable({head(
    filtered_date_sources() %>% 
      filter( catchment == "Glasgow") %>% 
      filter(ai_source == input$medium) %>% 
      group_by(specific_source) %>% 
      summarise(count = as.integer(sum(sessions))) %>% 
      arrange(desc(count)) %>% 
      mutate(count = format(count, big.mark =",")), 5)
  }, colnames = FALSE, align = "lr")
  
  output$medium_detail_iv <- renderTable({head(
    filtered_date_sources() %>% 
      filter( catchment == "Inverness") %>% 
      filter(ai_source == input$medium) %>% 
      group_by(specific_source) %>% 
      summarise(count = as.integer(sum(sessions))) %>% 
      arrange(desc(count)) %>% 
      mutate(count = format(count, big.mark =",")), 5)
  }, colnames = FALSE, align = "lr")

  output$medium_campaign_ed <- renderTable({
    filtered_date_sources() %>% 
      filter(catchment  == "Edinburgh") %>% 
      filter(acme_campaign != "(not set)") %>% 
      group_by(acme_campaign) %>% 
      summarise(count = as.integer(sum(sessions))) %>% 
      arrange(desc(count)) %>% 
      mutate(count = format(count, big.mark =",")) %>% 
      head(5)
  }, colnames = FALSE, align = "lr")
    
  
  output$medium_campaign_gl <- renderTable({head(
    filtered_date_sources() %>% 
      filter(catchment  == "Glasgow") %>% 
      filter(acme_campaign != "(not set)") %>% 
      group_by(acme_campaign) %>% 
      summarise(count = as.integer(sum(sessions))) %>% 
      arrange(desc(count)) %>% 
      mutate(count = format(count, big.mark =",")), 5)
  }, colnames = FALSE, align = "lr")

  
  output$medium_campaign_iv <- renderTable({head(
    filtered_date_sources() %>% 
      filter(catchment  == "Inverness") %>% 
      filter(acme_campaign != "(not set)") %>% 
      group_by(acme_campaign) %>% 
      summarise(count = as.integer(sum(sessions))) %>% 
      arrange(desc(count)), 5)
  }, colnames = FALSE, align = "lr")

  
   
  #  eventReactive(input$refresh, {
  leaflet_filter <- ai_ga_data_all %>%
    filter(catchment == "Glasgow"| 
             catchment == "Inverness"|
             catchment == "Edinburgh") 
  # filter(yearMonth == input$date_leaflet) 
  
  # })
  
  city <- leaflet_filter %>%
    group_by(city, longitude, latitude) %>%
    summarise(sum_city_users = sum(newUsers),
              sum_city_sessions = sum(sessions)) 
  
  poly_data <- leaflet_filter %>%
    group_by(catchment) %>%
    summarise(sum_catch_users = sum(newUsers),
              sum_catch_sessions = sum(sessions))
  
  scotland_join <-left_join(scotland_gcs, poly_data)
  
  labels <- sprintf(
    "<strong>%s</strong><br/>%g Users",
    scotland_join$catchment, 
    scotland_join$sum_catch_users
  ) %>% lapply(htmltools::HTML)
  
  labels2 <- sprintf(
    "<strong>%s</strong><br/>%g Users",
    city$city, 
    city$sum_city_users
  ) %>% lapply(htmltools::HTML)
  
  labels3 <- sprintf(
    "<strong>%s</strong><br/>%g Sessions",
    scotland_join$catchment, 
    scotland_join$sum_catch_sessions
  ) %>% lapply(htmltools::HTML)
  
  labels4 <- sprintf(
    "<strong>%s</strong><br/>%g Sessions",
    city$city, 
    city$sum_city_sessions
  ) %>% lapply(htmltools::HTML)
  
  
  bins <- c( 5000, 50000, 55000, Inf)
  pal <- colorBin(c("#00BFC4", "#7CAE00", "#F8766D"), domain = scotland_join$sum_catch_users, bins = bins)
  
  bins <- c( 5000, 50000, 100000, Inf)
  pal2 <- colorBin(c("#00BFC4", "#7CAE00", "#F8766D"), domain = scotland_join$sum_catch_sessions, bins = bins)
  
  output$scotland_leaflet <- renderLeaflet({
    
    leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
      addTiles() %>%
      setView(lng = -4.0, lat = 57.0, zoom = 7) %>%
      addProviderTiles("MapBox", options = tileOptions(
        updateWhenZooming = FALSE, 
        updateWhenIdle = TRUE,
        minZoom = 4, maxZoom = 6)) %>%
      addPolygons(
        data = scotland_join,
        fillColor = ~pal(sum_catch_users),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"),
        group = "Catchment Users") %>% 
      addPolygons(
        data = scotland_join,
        fillColor = ~pal2(sum_catch_sessions),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = labels3,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"),
        group = "Catchment Sessions") %>% 
      addCircles(
        data = city,
        lng = ~longitude, 
        lat = ~latitude,
        weight = 0,
        opacity = 0.5,
        fillOpacity = 0.5,
        radius = ~sqrt(sum_city_users) * 100,
        popup = labels2,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"),
        group = "City Users"
      ) %>%
      addCircles(
        data = city,
        lng = ~longitude, 
        lat = ~latitude,
        weight = 0,
        opacity = 0.5,
        fillOpacity = 0.5,
        radius = ~sqrt(sum_city_sessions) * 100,
        popup = labels4,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"),
        group = "City Sessions"
      ) %>%
      addLayersControl(
        baseGroups = c("Catchment Users", "Catchment Sessions", "City Users", "City Sessions"),
        options = layersControlOptions(collapsed = FALSE)
      )})
  
  
  #---------------------------------------------------------------------
  #TAB4 - 
  
  output$conv_plot <- renderPlot({
    ai_source_completions %>%
      filter(yearMonth >= input$date_tab4[1] & yearMonth <= input$date_tab4[2]) %>% 
      group_by(catchment, yearMonth) %>% 
      summarise(count = case_when(input$goal == "Info Requested" ~ sum(goal2Completions),
                                  input$goal == "Appointment Booked" ~ sum(goal9Completions),
                                  input$goal == "Confirmed Sale" ~ sum(goal11Completions),
                                  TRUE ~ sum(completions_all))) %>% 
 ggplot() +
      aes(x = yearMonth, y = count) +
      geom_line(aes(group = catchment, colour = catchment), size = 1.2) +
      labs(y = "User Sessions",
           title = paste("Website Goal Conversions:", input$goal)) +
      theme_light() +
      theme(axis.text.x = element_text(size = 11),
            axis.title.x = element_blank(),
            axis.title.y = element_text(size = 11),
            legend.title=element_blank(),
            legend.text=element_text(size=11),
            title = element_text(size = 14, face = "bold", colour = "#696969"),
            panel.border = element_rect(colour = "#696969", fill=NA, size=1))  +
      scale_x_date(date_breaks = "3 months", labels = scales::date_format("%m/%y")) +
      viridis::scale_fill_viridis()

  })
  
  output$table_conv <- renderTable({
    ai_source_completions %>%
      filter(yearMonth >= input$date_tab4[1] & yearMonth <= input$date_tab4[2])  %>% 
      group_by(catchment) %>% 
      summarise("Info Requested" = as.integer(sum(goal2Completions)), "Appointment Booked" = as.integer(sum(goal9Completions)), "Confirmed Sale" = as.integer(sum(goal11Completions)), "All Goals" = as.integer(sum(completions_all)))
  }, align = "c")
  
}

