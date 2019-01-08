#Difference Tests
#Waiter Experiment
#http://onlinelibrary.wiley.com/doi/10.1111/j.1559-1816.2002.tb00216.x/abstract

library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyDND)
library(truncnorm)
library(V8)

jsResetCode <- "shinyjs.reset0 = function() {history.go(0)}"

shinyUI(dashboardPage(skin = "black",
                      
                      dashboardHeader(title = "The Waiter Experiment", titleWidth = 300),
                      
                      dashboardSidebar(width = 250,
                                       sidebarMenu(id = "tabs",
                                                   menuItem("Prerequisites", tabName = "prereq", icon = icon("book")),
                                                   menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
                                                   menuItem("Simulation", tabName = "first", icon = icon("wpexplorer")),
                                                   menuItem("Determining P-Values", tabName = "second", icon = icon("gamepad"))
                                       )),
                      
                      dashboardBody(
                        #font for Header
                        #tags$style(HTML('.main-header .logo { font-family: "Georgia", Times, "Times New Roman", serif; font-weight: bold; font-size: 24px;}')),
                        #tags$style(type = "text/css", ".content-wrapper,.right-side {background-color: white;}"),
                        
                        tags$style(HTML('.main-header .logo { font-family: "Times New Roman", Times, serif; 
                                        font-weight: bold;
                                        font-size: 25px;
                                        color: white;}')),
                        #tags$style(type = "text/css", ".content-wrapper,.right-side {background-color: white;}"),
                        tags$head(tags$style(HTML('
                                                  .skin-black .main-header>.navbar {
                                                  background-color: #1E407C;
                                                  }
                                                  
                                                  .content-wrapper,.right-side {
                                                  background-color: white;
                                                  }
                                                  
                                                  .skin-black .main-header .logo {
                                                  background-color: #1E407C;
                                                  color: white;
                                                  }
                                                  .skin-black .main-header .logo:hover {
                                                  background-color: #9397BB;
                                                  }
                                                  .skin-black .main-header .navbar .sidebar-toggle:hover{
                                                  background-color: #9397BB;
                                                  }

                                                  .skin-black .main-header .navbar>.sidebar-toggle{
                                                  color: white;
                                                  }
                                                    
                                                  *, h1, h2, h3, h4, h5 {
                                                  font-family: "Times New Roman", Times, serif;
                                                  }

                                                  #shiny-tab-second h5 {
                                                  color: #1E407C;
                                                  font-size: 17px;
                                                  }

                                                  .box-body {
                                                  background-color: #1E407C;
                                                  }

                                                  .irs-bar, .irs-bar-edge, .irs-from, .irs-to, .irs-single {
                                                  border-top: 1px solid #1E407C;
                                                  border-bottom: 1px solid #1E407C;
                                                  background: #1E407C;
                                                  }
                                                  
                                                  # aside.main-sidebar {
                                                  # 	background-color: #CACACA !important;
                                                  #   color: white;
                                                  # }
                                                  '))),
                        
                        #Prerequisites Page
                        tabItems(
                          tabItem(tabName = "prereq",
                                  fluidRow(column(width = 10,
                                                  h3(strong("Background:")),br(),
                                                  h4(tags$li(strong("Two Sample T-Test"))),
                                                  h4("A two sample t-test is a commonly used hypothesis test to determine 
                                                     whether the average difference between two groups can be explained 
                                                     by random chance when the data arise from normally distributed populations. 
                                                     Two sample t-tests can help answer a question like whether the test results of 
                                                     patients who received a drug are significantly better than test results of those who received a placebo, 
                                                     or whether any observed difference can be explained by chance."),
                                                  br(), br(),
                                                  withMathJax(),
                                                  h4(tags$li(strong("Test Statistics & P-Values"))),
                                                  h4("The test statistic involves the difference between group averages standardized by an estimate of the 
                                                     standard deviation of the difference. The statistic is compared to a t distribution to determine the p-value 
                                                     (degrees of freedom = \\(n_{grp1}+n_{grp2} – 2\\)). 
                                                     The p-value represents the probability of obtaining a difference at least as extreme as 
                                                     the one in your sample data. Thus, a large p-value indicates the null hypothesis of no difference in means 
                                                     provides a reasonable explanation of the data. Small p-values (typically taken to be below 0.05) 
                                                     indicate the null hypothesis is a poor explanation of the data."),
                                                  
                                                  br(), br(), br(),
                                                  div(style = "text-align: center",
                                                      actionButton("next.page", HTML(paste(tags$b("Go to the overview"))), style = "color: #fff; background-color: #1E407C;", icon = icon("bolt")))

                                  ))
                          ),
                          
                          #Overview Page
                          tabItem(tabName = "overview",
                                  tags$a(href='http://stat.psu.edu/',tags$img(src='PS-HOR-RGB-2C.png', align = "left", width = 180)),
                                  br(),br(),br(),
                                  h3(strong("About:")),
                                  h4("This app demonstrates the reasoning of a two-sample t-test to determine
                                                  whether a waiter giving a table candy or not affects their tip percentages.
                                                  The null hypothesis is that the average tip percentages for tables receiving
                                                  candy and tables not receiving candy are equal. The alternative hypothesis is 
                                                  that the average tip percentages for tables receiving candy are greater than
                                                  the average tip percentages for tables not receiving candy."),
                                                  br(), 
                                                  
                                  h3(strong("Instructions:")),
                                                  h4(tags$li("Use the sliders to select a desired average tip percentage for tables receiving
                                                  candy and tables not receiving candy. Click Randomly Assign and move the sliders
                                                  to observe how the results change based on selected tip percent values. Is there
                                                  a difference in a waiter's average tip percentage depending on whether they give
                                                  a table candy or not? Each time the sliders are moved a new simulated sample is taken 
                                                     for the group of tables involved and the test results are printed at the bottom.")),
                                                  h4(tags$li("The Determining P-values section of the app allows the user to see how the p-value changes 
                                                      for different observed effect sizes and different sample sizes. 
                                                     Finally, a quiz tests your knowledge of how effect size and sample size affect the p-value.")),
                                  div(style = "text-align: center",
                                      actionButton("go", HTML(paste(tags$b("Explore"))), style = "color: #fff; background-color: #1E407C;", icon = icon("bolt"))),
                                  br(),
                                                  h3(strong("Acknowledgements:")),
                                                  h4(p("This application's the waiter experiment simulation was conceived and programmed by David Robinson and then redesigned with the matching game added by Angela Ting."))       
                          ),
                          
                          tabItem(tabName = "first",
                                  div(style="display: inline-block;vertical-align:top;",
                                      tags$a(href='https://shinyapps.science.psu.edu/',tags$img(src='homebut.PNG', width = 15))
                                  ),
                                  fluidRow(column(width = 4,
                                                  br(),
                                                  "A study published in the Journal of Applied Social Psychology claims that giving candy to customers can increase a waiter's tip by about 23%.",
                                                  br(),br(),
                                                  
                                                  "Click below to randomly assign which tables receive candy.",
                                                  br(), br(),
                                                  
                                                  actionButton("rand", HTML(paste(tags$b("Randomly Assign"))), style = "color: #fff; background-color: #1E407C;"),
                                                  br(), br(),
                                                  
                                                  sliderInput(inputId = "avgtipc", label = "Enter an average tip percentage for tables assigned to receive candy.", min = 0, max = 50, value = 0),
                                                  sliderInput(inputId = "avgtipnc", label = "Enter an average tip percentage for tables assigned to receive no candy.", min = 0, max = 50, value = 0),
                                                  
                                                  #Reset Button
                                                  useShinyjs(),
                                                  extendShinyjs(text = jsResetCode),
                                                  br(),
                                                  actionButton("reset_button", HTML(paste(tags$b("Reset"))), style = "color: #fff; background-color: #1E407C;"),
                                                  #Use jscode to disable all the buttons
                                                  tags$head(tags$script(HTML('Shiny.addCustomMessageHandler("jsCode", function(message) {console.log(message) eval(message.code);});')))
                                                  
                                  ),
                                  
                                  column(width = 2,
                                         htmlOutput("img1"),
                                         textInput(inputId = "tabtip1", label = NULL, value = "", width = '72px', placeholder = NULL),
                                         
                                         htmlOutput("img2"),
                                         textInput(inputId = "tabtip2", label = NULL, value = "", width = '72px', placeholder = NULL),
                                         
                                         htmlOutput("img3"),
                                         textInput(inputId = "tabtip3", label = NULL, value = "", width = '72px', placeholder = NULL),
                                         
                                         htmlOutput("img4"),
                                         textInput(inputId = "tabtip4", label = NULL, value = "", width = '72px', placeholder = NULL),
                                         
                                         htmlOutput("img5"),
                                         textInput(inputId = "tabtip5", label = NULL, value = "", width = '72px', placeholder = NULL),
                                         
                                         htmlOutput("img6"),
                                         textInput(inputId = "tabtip6", label = NULL, value = "", width = '72px', placeholder = NULL)
                                         
                                  ),
                                  
                                  column(width = 2,
                                         htmlOutput("img7"),
                                         textInput(inputId = "tabtip7", label = NULL, value = "", width = '72px', placeholder = NULL),
                                         
                                         htmlOutput("img8"),
                                         textInput(inputId = "tabtip8", label = NULL, value = "", width = '72px', placeholder = NULL),
                                         
                                         htmlOutput("img9"),
                                         textInput(inputId = "tabtip9", label = NULL, value = "", width = '72px', placeholder = NULL),
                                         
                                         htmlOutput("img10"),
                                         textInput(inputId = "tabtip10", label = NULL, value = "", width = '72px', placeholder = NULL),
                                         
                                         htmlOutput("img11"),
                                         textInput(inputId = "tabtip11", label = NULL, value = "", width = '72px', placeholder = NULL),
                                         
                                         htmlOutput("img12"),
                                         textInput(inputId = "tabtip12", label = NULL, value = "", width = '72px', placeholder = NULL)
                                         
                                  ),
                                  
                                  
                                  column(width = 2,
                                         htmlOutput("img13"),
                                         textInput(inputId = "tabtip13", label = NULL, value = "", width = '72px', placeholder = NULL),
                                         
                                         htmlOutput("img14"),
                                         textInput(inputId = "tabtip14", label = NULL, value = "", width = '72px', placeholder = NULL),
                                         
                                         htmlOutput("img15"),
                                         textInput(inputId = "tabtip15", label = NULL, value = "", width = '72px', placeholder = NULL),
                                         
                                         htmlOutput("img16"),
                                         textInput(inputId = "tabtip16", label = NULL, value = "", width = '72px', placeholder = NULL),
                                         
                                         htmlOutput("img17"),
                                         textInput(inputId = "tabtip17", label = NULL, value = "", width = '72px', placeholder = NULL),
                                         
                                         htmlOutput("img18"),
                                         textInput(inputId = "tabtip18", label = NULL, value = "", width = '72px', placeholder = NULL)
                                         
                                  ),
                                  
                                  column(width = 2,
                                         htmlOutput("img19"),
                                         textInput(inputId = "tabtip19", label = NULL, value = "", width = '72px', placeholder = NULL),
                                         
                                         htmlOutput("img20"),
                                         textInput(inputId = "tabtip20", label = NULL, value = "", width = '72px', placeholder = NULL),
                                         
                                         htmlOutput("img21"),
                                         textInput(inputId = "tabtip21", label = NULL, value = "", width = '72px', placeholder = NULL),
                                         
                                         htmlOutput("img22"),
                                         textInput(inputId = "tabtip22", label = NULL, value = "", width = '72px', placeholder = NULL),
                                         
                                         htmlOutput("img23"),
                                         textInput(inputId = "tabtip23", label = NULL, value = "", width = '72px', placeholder = NULL),
                                         
                                         htmlOutput("img24"),
                                         textInput(inputId = "tabtip24", label = NULL, value = "", width = '72px', placeholder = NULL),
                                         br(), br()
                                  )
                                  ),
                                  
                                  
                                  fluidRow(column(width = 8, offset = 1,
                                                  wellPanel(
                                                    h3(tags$b("Test Hypotheses:")),
                                                    br(),
                                                    
                                                    HTML(paste(tags$b("H", tags$sub(0)), ": Giving customers candy does not affect the waiter's tip percentage")),
                                                    br(),
                                                    "(Candy Tip = No Candy Tip)",
                                                    br(), br(),
                                                    
                                                    HTML(paste(tags$b("H", tags$sub(1)), ": Giving customers candy does affect the waiter's tip percentage")), 
                                                    br(),
                                                    "(Candy Tip > No Candy Tip)"
                                                  ),
                                                  
                                                  conditionalPanel(condition = "input.rand",
                                                                   wellPanel(
                                                                     h3(tags$b("Test Values:")),
                                                                     br(),
                                                                     
                                                                     htmlOutput("average.c"),
                                                                     br(),
                                                                     
                                                                     htmlOutput("average.nc"),
                                                                     br(),
                                                                     
                                                                     htmlOutput("effect.size"),
                                                                     br(),
                                                                     
                                                                     htmlOutput("test.stat"),
                                                                     br(),
                                                                     
                                                                     htmlOutput("p_value")
                                                                   )
                                                  )
                                                  
                                  ))
                          ),
                          
                          tabItem(tabName = "second",
                                  div(style="display: inline-block;vertical-align:top;",
                                      tags$a(href='https://shinyapps.science.psu.edu/',tags$img(src='homebut.PNG', width = 15))
                                  ),
                                  (tabsetPanel(id = "tabs2",
                                               tabPanel(title = h5("P-value Calculators"), value = "detpval",
                                                        fluidRow(column(width = 10,
                                                                        h2(tags$b("How do effect size and sample size affect the p-value?")), br()
                                                        )
                                                        ),
                                                        
                                                        fluidRow(column(offset = 1, width = 10,
                                                                        box(width = 7,background = "blue",
                                                                            h3(tags$b("Test Hypotheses:")),
                                                                            br(),
                                                                            
                                                                            HTML(paste(tags$b("H", tags$sub(0)), ": Giving customers candy does not affect the waiter's tip percentage.")),
                                                                            br(),
                                                                            "(Candy Tip = No Candy Tip)",
                                                                            br(), br(),
                                                                            
                                                                            HTML(paste(tags$b("H", tags$sub(1)), ": Giving customers candy does affect the waiter's tip percentage.")), 
                                                                            br(),
                                                                            "(Candy Tip > No Candy Tip)",
                                                                            br(), br()
                                                                        )
                                                        )
                                                        
                                                        ),
                                                        
                                                        fluidRow(column(offset = 1, width = 10,
                                                                        h4(tags$b("Move the sliders for effect size and sample size to see how changes in each affect the p-value.")),
                                                                        #br(),
                                                                        h4(tags$b("Observe whether researchers are more or less likely to detect a difference in average tip percentage
                                                                                  when you change the effect size and sample size.")),
                                                                        br(),
                                                                        
                                                                        sliderInput(inputId = "ef_size", "Adjust the observed effect size", min = 0 , max = 50, value = 0, width = 600),
                                                                        sliderInput(inputId = "samp_size", "Adjust the sample size", min = 10, max = 100, value = 10, width = 600),
                                                                        br(),
                                                                        
                                                                        #tags$head(tags$style("#pvalue{color: black; font-size: 30px; font-style: bold; background-color: #96c5e1}")),
                                                                        #tags$head(tags$style("#pvalue{color: black; font-size: 30px; font-style: bold; background-color: #2d6da4}")),
                                                                        tags$head(tags$style("#pvalue{color: white; font-size: 30px; font-style: bold;")),
                                                                        
                                                                        box(width = 8, background = "blue", htmlOutput("pvalue")),
                                                                        br()
                                                                        )
                                                        ),
                                                        
                                                        fluidRow(column(width = 12, offset = 9,
                                                                        br(), br(),
                                                                        actionButton(inputId = "totest", HTML(paste(tags$b("Test Your Understanding"))), style = "color: #fff; background-color: #1E407C;", width = 200, icon = icon("arrow-right"))
                                                        )
                                                        )
                                               ),
                                               
                                               tabPanel(title = h5("Matching Game"), value = "matching",
                                                        fluidRow(
                                                          fluidRow(column(12,
                                                                          h2(tags$b("Test Your Understanding:")),
                                                                          br(),
                                                                          h4("Match the appropriate p-values to each question according to the effect size and sample size. 
                                                                             Try to do this without doing any calculations using the P-value calculator"),
                                                                          #HTML(paste0("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;", tags$b("Test Your Understanding:"))),
                                                                          br()
                                                          )
                                                          ),
                                                          
                                                          #Question 1
                                                          box(width = 10, style = "color: #000000; background-color: #b6d0df", background = "blue", 
                                                              fluidRow(column(width = 6,
                                                                              h2(tags$b("1.)")),
                                                                              br(),
                                                                              tableOutput("table1")
                                                              ),
                                                              
                                                              column(width = 5,
                                                                     br(), br(), br(), br(),
                                                                     htmlOutput("choices1")
                                                                     #textOutput("check1")
                                                              )
                                                              )
                                                          ),
                                                          
                                                          #for the check or X
                                                          column(width = 2,
                                                                 br(), br(),
                                                                 htmlOutput("pic1")
                                                          ),
                                                          
                                                          
                                                          #Question 2
                                                          box(width = 10, style = "color: #000000; background-color: #dae1e5", background = "blue",
                                                              fluidRow(column(width = 6,
                                                                              h2(tags$b("2.)")),
                                                                              br(),
                                                                              tableOutput("table2")
                                                              ),
                                                              
                                                              column(width = 5,
                                                                     br(), br(), br(), br(),
                                                                     htmlOutput("choices2")
                                                                     #textOutput("check2")
                                                              )
                                                              )
                                                          ),
                                                          
                                                          column(width = 2,
                                                                 br(), br(),
                                                                 htmlOutput("pic2")
                                                          ),
                                                          
                                                          
                                                          #Question 3
                                                          box(width = 10, style = "color: #000000; background-color: #b6d0df", background = "blue", 
                                                              fluidRow(column(width = 6,
                                                                              h2(tags$b("3.)")),
                                                                              br(),
                                                                              tableOutput("table3")
                                                              ),
                                                              
                                                              column(width = 5,
                                                                     br(), br(), br(), br(),
                                                                     htmlOutput("choices3")
                                                                     #textOutput("check3")
                                                              )
                                                              )
                                                          ),
                                                          
                                                          column(width = 2,
                                                                 br(), br(),
                                                                 htmlOutput("pic3")
                                                          ),
                                                          
                                                          
                                                          #Question 4
                                                          box(width = 10, style = "color: #000000; background-color: #dae1e5", background = "blue",
                                                              fluidRow(column(width = 6,
                                                                              h2(tags$b("4.)")),
                                                                              br(),
                                                                              tableOutput("table4")
                                                              ),
                                                              
                                                              column(width = 5,
                                                                     br(), br(), br(), br(),
                                                                     htmlOutput("choices4")
                                                                     #textOutput("check4")
                                                              )
                                                              )
                                                          ),
                                                          
                                                          column(width = 2,
                                                                 br(), br(),
                                                                 htmlOutput("pic4")
                                                          ),
                                                          
                                                          
                                                          #Submit Button
                                                          fluidRow(column(width = 12, offset = 4,
                                                                          actionButton(inputId = "submit", HTML(paste(tags$b("Submit"))), style = "color: #fff; background-color: #1E407C;", width = 200),
                                                                          br(), br(),
                                                                          uiOutput("feedback"),
                                                                          br(),
                                                                          uiOutput("showtry"),
                                                                          br(),
                                                                          actionButton(inputId = "playagain", HTML(paste(tags$b("Play Again"))), style = "color: #fff; background-color: #1E407C;", width = 200)
                                                          )
                                                          )
                                                          
                                                        )
                                                        
                                               )
                                               
                                               )
                                   )
                                  
                          )
                          
                        ) #end of TabItems()
                        
                      ) #end of DashboardBody()
))
