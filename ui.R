ui <- dashboardPage(
  
  
  title = 'RimBac',
  dashboardHeader(),
  dashboardSidebar(
    sidebarMenu(id = "sidebarmenu",
                HTML(paste0(
                  "<br>",
                  "<a href='https://www.messenger.com/t/104957751218813' target='_blank'><img style = 'display: block; margin-left: auto; margin-right: auto;' src='logo4.png' ></a>",
                  "<br>"
                )),
                
                menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                menuItem("Cartographie", tabName = "map", icon = icon("thumbtack")),
                conditionalPanel(
                  "input.sidebarmenu === 'map'",
                  # a. FILTERS
                  useShinyjs(),
                  div(id = "form",
                      tags$hr(),
                      selectInput("i2_ed", "Moughataa", choices = c('',m_mougahtaa),bookmarkButton(id = "bookmark1")),
                      
                  )),
                menuItem(tabName = "tab", "Tableau", icon = icon("table")),
                
                
                HTML(paste0(
                  "<br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br>",
                  "<table style='margin-left:auto; margin-right:auto;'>",
                  "<tr>",
                  "<td style='padding: 5px;'><a href='https://twitter.com/m_lemine_b' target='_blank'><i class='fab fa-twitter fa-lg'style='color:#DDDDDD;'></i></a></td>",
                  "<td style='padding: 5px;'><a href='https://www.linkedin.com/in/mohamed-lemine-beydia/' target='_blank'><i class='fab fa-linkedin fa-lg'style='color:#DDDDDD;'></i></a></td>",
                  "</tr>",
                  "</table>",
                  "<br>"),
                  HTML(paste0(
                    "<script>",
                    "var today = new Date();",
                    "var yyyy = today.getFullYear();",
                    "</script>",
                    "<p style = 'text-align: center;'><small>&copy; - <a href='mailto:m.beydia@gmail.com'style='color:#DDDDDD'; target='_blank'>Mohamed Beydia</a> - <script>document.write(yyyy);</script></small></p>")
                  ))
                
    )
  ),
  dashboardBody(
    ### changing theme
    shinyDashboardThemes(
      theme = "blue_gradient"
    ),
    
    useShinyjs(),
    introjsUI(),
    
    tabItems(
            tabItem(tabName = "dashboard",fluidRow(
        valueBoxOutput("value1")
        ,valueBoxOutput("value2")
        ,valueBoxOutput("value3")

      ),
      
      
      fluidRow(
        box(
          title = "Taux d'admission par pays (1iere + 2ieme session)"
          ,status = "primary"
          ,solidHeader = TRUE
          ,collapsible = TRUE
          ,echarts4rOutput("dash_pays", height = "350px") %>% withSpinner(color = "black")
        )
        ,
        
          box(
            title = "Taux d'admission Wilaya"
            ,status = "primary"
            ,solidHeader = TRUE
            ,collapsible = TRUE
            ,plotlyOutput("dash_wilaya_tx", height = "350px") %>% withSpinner(color = "black")
          )
          ,
      
        box(
          title = "Nombre de candidats & admis par Willaya"
          ,status = "primary"
          ,solidHeader = TRUE
          ,collapsible = TRUE
          ,plotlyOutput("dash_wilaya", height = "350px") %>% withSpinner(color = "black")
        ),
        box(
          title = "Nombre de candidats par Séries"
          ,status = "primary"
          ,solidHeader = TRUE
          ,collapsible = TRUE
          ,echarts4rOutput("dash_series_nb", height = "350px") %>% withSpinner(color = "black")
        )
        
        ,box(
          title = "Résultats par Séries"
          ,status = "primary"
          ,solidHeader = TRUE
          ,collapsible = TRUE
          ,echarts4rOutput("dash_series", height = "350px") %>% withSpinner(color = "black")
        ),
        
        box(
          title = "Nombre de candidats : Nktt vs autre Willaya"
          ,status = "primary"
          ,solidHeader = TRUE
          ,collapsible = TRUE
          ,plotlyOutput("dash_nktt_autre_nb", height = "350px") %>% withSpinner(color = "black")
        ),
        
        box(
          title = "Nombre d'admis : Nktt vs autre Willaya"
          ,status = "primary"
          ,solidHeader = TRUE
          ,collapsible = TRUE
          ,plotlyOutput("dash_nktt_autre_tx", height = "350px") %>% withSpinner(color = "black")
        ),
        
        box(
          title = "Taux d'admission par Mouqataa (top 10)"
          ,status = "primary"
          ,solidHeader = TRUE
          ,collapsible = TRUE
          ,plotlyOutput("dash_moukataa_tx", height = "350px") %>% withSpinner(color = "black")
        ),
        
        box(
          title = "Taux d'echec par Mouqataa (flop 10)"
          ,status = "primary"
          ,solidHeader = TRUE
          ,collapsible = TRUE
          ,plotlyOutput("dash_moukataa_tx_echec", height = "350px") %>% withSpinner(color = "black")
        ),
        box(
          title = "Moyenne Generale par série"
          ,status = "primary"
          ,solidHeader = TRUE
          ,collapsible = TRUE
          ,plotlyOutput("dash_moy", height = "350px") %>% withSpinner(color = "black")
        ),
        box(
          title = "Age des candidats par série"
          ,status = "primary"
          ,solidHeader = TRUE
          ,collapsible = TRUE
          ,plotlyOutput("dash_moy_age", height = "350px") %>% withSpinner(color = "black")
        )
      )
      
      ),
      tabItem(tabName = "map",
              fluidRow(style="height:50px;",
                       valueBoxOutput("count1",width = 3),
                       valueBoxOutput("count2",width = 3),
                       valueBoxOutput("count3",width = 3),
                       valueBoxOutput("count4",width = 3)
              ),
              br(),
              br(),
              fluidRow(column(10, offset = 2.5,leafletOutput('leaflet_map', width = 1600, height = 600)))
      ),
      
      
      tabItem(tabName = "tab",
              fluidPage(
                shinyjs::useShinyjs(),
                mainPanel(
                  fluidRow( column(4,selectInput("select_wilaya","Willaya:",m_willaya)),
                            column(4,selectInput("select_moukataa", "Mouqataa:",m_mougahtaa)),
                            column(4,selectInput("select_etablissement","Etablissement/lycée:",m_etablissement)),
                            
                            DT::dataTableOutput('my_data_tab',width = "1500px"))
                  
                )
              )
      )
      
      
      
    )
    
    
    
  )
)




