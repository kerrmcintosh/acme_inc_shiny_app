

# UI section 

ui <- fluidPage(    tags$head(
  tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
),
                  theme = shinytheme("simplex"),
                  navbarPage(

  
  title = div(img(src="codeclanlogo.jpeg", id = "logo"), "An Analysis of Acme Inc's Website Traffic")),
  
  
  tabsetPanel(
    # Tab 1
    tabPanel("About", div(class = "separator"),

             
             fluidRow(
               column(6, h4("Brief"),
                      div(class = "separator"),
                      tags$p("We were asked to define catchment areas for each of Acme Inc's regional sales outlets
                             and assign web traffic in Scotland to the correct catchment. We were then to create
                             visualisations of Acme Inc website performance,comparing the three catchment areas Edinburgh, Glasgow and Inverness."),
                      div(class = "separator"),
                      h4("Planning"),
                      div(class = "separator"),div(class = "separator"),
                      tags$p("To define the catchment area we decide to consolidate Scotlands counties 
                      by distance from Acme Inc's 3 Scottish outlets. This split the country up well with the 
                      North of Scotland assign to Inverness, the West to Glasgow and the East to Edinburgh."),
                      div(class = "separator"),
                      tags$p("We defined website performance by multiple aspects. Firstly the amount of
                      users and sessions the website receives. Secondly the websites traffic and the its
                      source. Finally the completion of goals 2, 9 and 11 which are course application submissions."),
                      div(class = "separator"),
                      h4("Execution"),
                      div(class = "separator"),
                      tags$p("This dashboard displays various visualisation of the different aspects of the website
                             performance of the three defined catchment areas and have a brief description of 
                             what they show."),
                      div(class = "separator"),
                      tags$p("The map to the left shows the three defined catchment area with the total number of 
                             users and sessions it has received. It also shows the distribution of the users and 
                             sessions throughout Scotland. ")),
               
               column(6, leafletOutput("scotland_leaflet", height = 800, width = 700)
                      )
             )

             
             # End Tab 1
             
    ),
    
    # Tab2
    tabPanel("Overall Site Traffic", div(class = "separator"),
             fluidRow(column(9,
               
               tags$p(class = "indent", "This page summarises total website traffic split in to Acme Inc's
                    3 Scottish Regions: Edinburgh, Glasgow and Inverness.  The data was taken from 
                    Google Analytics.  Where the catchment is given as Scotland uncategorised, no locational 
                    was data was provided by Google Analytics other than that the user was in Scotland. 
                    Source Mediums (facebook, organic searches, blog posts, etc) have been grouped together 
                    in to categories."),
               
               tags$div(class="header_container",
                        tags$div(class ="div_in_topr",
                             sliderInput("date_tab2", 
                                         tags$b("Select time period to view"),
                                         min = min(ai_ga_data_all$yearMonth), max = max(ai_ga_data_all$yearMonth),
                                         value = c(min(ai_ga_data_all$yearMonth), max(ai_ga_data_all$yearMonth)),timeFormat="%Y-%m")),

               tags$div(class ="div_in_topr",
                             radioButtons("usersesh",
                                         tags$b("How would you like to view traffic?"),
                                         choices = c("users", "sessions"))
                      )),

             div(id = "separator"),
             fluidRow(
               tags$div(class="st_container",
                        
                        tags$div(class="center",
                                 tags$div(id = "separator"),
                                 tags$div(class="div_in",
                                          tags$b("Edinburgh"),
                                          tableOutput("ed_users")),
                                 
                                 tags$div(class="div_in",
                                          tags$b("Glasgow"),
                                          tableOutput("gl_users")),
                                 
                                 tags$div(class="div_in",
                                          tags$b("Inverness"),
                                          tableOutput("iv_users")),
                                 tags$br(class="clearBoth" )))),
             div(class = "separator"),
             plotOutput("total_plot")
             
             )
                      
                      
             )
             
             # End Tab 2
             
    ),
    
    # Tab 3
    tabPanel("Site Traffic by Catchment and Source", div(class = "separator"),
             fluidRow(column(11,
               tags$p(class = "indent", "This page analyses how traffic came to the Acme Inc's website for Acme Inc's 3 Scottish Regions: Edinburgh, Glasgow and Inverness.  
                    The data was taken from Google Analytics.  Where the catchment is given as Scotland uncategorised, no locational data was provided by 
                    Google Analytics other than that the user was in Scotland. Source Mediums (facebook, organic searches, blog posts, etc) have been 
                    grouped together in to categories.")),
               column(9,
               tags$div(class="header_container",
                        tags$div(class ="div_in_topr",
                                 sliderInput("date_tab3",
                                             tags$b("Select time period to view"),
                                             min = min(ai_ga_data_all$yearMonth), max = max(ai_ga_data_all$yearMonth),
                                             value = c(min(ai_ga_data_all$yearMonth), max(ai_ga_data_all$yearMonth)),
                                             timeFormat="%Y-%m")),
                        
                        tags$div(class ="div_in_topr",
                                 selectInput("medium",
                                             tags$b("Select the medium by which user came to the Acme Inc website"),
                                             choices = sort(unique(ai_source_regrouped$ai_source)),
                                             selected = "Social Media")
                        )))),
             
          


           
             fluidRow(
               column(9,
                      tags$div(class ="plot_cont",
             plotOutput("source_bar_plot")),
             tags$div(class ="plot_cont",
             plotOutput("source_plot")),
             fluidRow(
               tags$div(class ="container_tab3",
                        tags$div(class="center", 
                                 tags$div(class="div_in",
                                          tags$b("Top 5 Performing GA Campaigns", tags$br(), "for Edinburgh"),
                                          tableOutput("medium_campaign_ed")),
                                 tags$div(class="div_in_topl",
                                          tags$b("Top 5 Performing GA Campaigns", tags$br(), "for Glasgow"),
                                          tableOutput("medium_campaign_gl")),
                                 tags$div(class="div_in_topl",
                                          tags$b("Top 5 Performing GA Campaigns", tags$br(), "for Inverness"),
                                          tableOutput("medium_campaign_iv")))
                        
               ))),
             column(3,
                    tags$div(class = "side_container_blue",
                    tags$div(class = "side_center",
                    tags$b(textOutput("traf_med")),
                    tableOutput("grp_traf"))),
                    tags$div(class = "side_container_pink",
                             tags$div(class = "side_center",
                                      tags$b(textOutput("ed_traf_med")),
                                      tableOutput("medium_detail_ed"),
                                      tags$b(textOutput("gl_traf_med")),
                                      tableOutput("medium_detail_gl"),
                                      tags$b(textOutput("iv_traf_med")),
                                      tableOutput("medium_detail_iv"))
                             )
                             ))
 


             
    ),
    
    # Tab 4
    tabPanel("Goal Completions by Catchment", div(class = "separator"),
             fluidRow(column(11,
                             tags$p(class = "indent", "This page looks at the goal conversions defined in google analytics with 
                                    regard to website traffic from Scottish catchments")),
                      column(9,
                             tags$div(class="header_container",
                                      tags$div(class ="div_in_topr",
                                                           sliderInput("date_tab4",
                                                                       tags$b("Select time period to view"),
                                                                       min = min(ai_ga_data_all$yearMonth), max = max(ai_ga_data_all$yearMonth),
                                                                       value = c(min(ai_ga_data_all$yearMonth), max(ai_ga_data_all$yearMonth)),
                                                                       timeFormat="%Y-%m")),
                                      
                                      tags$div(class ="div_in_topr",
                                               selectInput("goal",
                                                           tags$b("Select the goal to view"),
                                                           choices = c("Info Requested", "Appointment Booked", "Confirmed Sale", "All Goals"))),
                                      
                             ))),
             fluidRow(column(9,
                             tags$div(class ="container_tab4",
                                      tags$div(class="center", 
                                               tags$div(class="div_in",
                                                        tags$b("GA Goal Conversions"),
                                                        tableOutput("table_conv")))),
                             tags$div(class ="plot_cont",         
             plotOutput("conv_plot"))
             
               )
         
                    
                      
                      
             )

             )
  ),
  
  div(class = "separator"),
  tags$footer(class = "footer_text", h5("Produced by CodeClan Group 2"),
              tags$div(class = "separator"))

)


