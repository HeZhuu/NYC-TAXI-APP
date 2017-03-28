library(leaflet)

#Choices for drop-downs
vars <- c(
  "Business Day" = 1,
  "Not Business Day" = 2

)
#

navbarPage("NYC TAXI", id="nav", 
           #title = 'taxi menu',
           
           tabPanel("Interactive Map",
                    div(class="outer",
                        
                        tags$head(
                          # Include our custom CSS
                          includeCSS("styles.css"),
                          includeScript("gomap.js")
                        ),
                        
                        leafletOutput("map", width="100%", height="100%"),
                        
                        # Shiny versions prior to 0.11 should use class="modal" instead.
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = F, top = 60, left = "auto", right = 0, bottom = "auto",
                                      width = 160, height = 180,
                                      
                                      radioButtons("CF", label = "Layers",
                                                   choices = list("Count Number" = "count", "Fare Per Distance" = "FPD","Cluster" = "cluster1" ,"Cash Paying Percentage" = "cash"), 
                                                   selected = "count")
                                      
                                      
                        ),
                        
                        
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = 60, left = 0, right = "auto", bottom = "auto",
                                      width = 330, height = "auto",
                                      
                                      h3("Panel"),
                                      

                                      selectInput("days", "Days", c("All Day", "Business Day", "Not Business Day"),selected = "All Day"),

                                      
                                      checkboxInput(inputId = "showhr",
                                                    label = strong("Show hours"),
                                                    value = FALSE),
                                      
                                        conditionalPanel(condition = "input.showhr == false"
                              
                                        ),
                                      
                                      
                                      conditionalPanel(condition = "input.showhr == true",
                                                       sliderInput(inputId = "hr_adjust",
                                                                   label = "Choose the time of the day:",
                                                                   min = 0, max = 23, value = NULL, step = 1)
                                      ),
                                      
                                      
                                      
                                      
                                      checkboxInput("top15count", "Top 5 Count", FALSE),
                                      checkboxInput("top15FPD", "Top 5 FPD", FALSE),
                                      
                                      
                                      checkboxInput(inputId = "showbr",
                                                    label = strong("Show Borough for Top 5 counts/FPD"),
                                                    value = FALSE),
                                      
                                      conditionalPanel(condition = "input.showbr == true",
                                                       selectInput("boroSelect", "Borough for Top 5 counts/FPD", 
                                                                   c("Manhattan", "Bronx", "Brooklyn", "Queens", "Staten Island", "All"), 
                                                                   selected = "All")
                                      ),
                                      
                                      
                                    
                                      
                                      radioButtons("subway", label = h4("Subway Station : "),
                                                   choices = list("Do not appear" = 1, "Show all stations" = 2, "Show unique station" = 3), 
                                                   selected = 1),
                                      
                                      plotOutput("districttimeplot", height = 280),
                                      helpText(   a("Analysis",
                                                    href="https://github.com/TZstatsADS/Spr2017-proj2-grp2/blob/master/doc/analysis.html")
                                      )
                        )
 
                        # absolutePanel(id="graphstuff",class = "panel panel-default", fixed=TRUE,
                        #               draggable = TRUE, top=55, left="auto", right= 5, bottom="auto",width=300,
                        #               height=100, style="opacity:0.65",
                        #               
                        #               
                        #               h4("hourly flow change", align = "center"),
                        #               plotOutput("districttimeplot",height = 200))
                        
                    )
           ),
           
           
           tabPanel("Dynamic Map",
                    div(class="outer",
                        
                        tags$head(
                          # Include our custom CSS
                          includeCSS("styles.css"),
                          includeScript("gomap.js")
                        ),
                        
                        leafletOutput("map2", width="100%", height="100%"),
                        
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = 60, left = 20, right = "auto", bottom = "auto",
                                      width = 330, height = "auto",
                                      
                                      h2("Dynamic map of NYC taxi hourly flow change"),
                                      
                                      
                                      selectInput("pd", "pick up or drop off", c("Pick up", "Drop off", "All"), selected = "Pick up"),
                                      
                                      textInput("choose date", "Choose date", "1/1/2015"),
                                      
                                      sliderInput("hours", "Hours of Day:", 
                                                  min = 0, max = 23, value = 0, step = 1,
                                                  animate=animationOptions(interval = 500)),
                                      helpText("Click play button to see dynamic flow data")
                        )

                        
                    )
           ),
           
           
           tabPanel("Raw Data",
                    hr(),
                    DT::dataTableOutput("rawtable")
           )
           
           
           
           
)
