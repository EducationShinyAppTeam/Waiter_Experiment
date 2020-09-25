#Difference Tests
#Waiter Experiment
#http://onlinelibrary.wiley.com/doi/10.1111/j.1559-1816.2002.tb00216.x/abstract

library(shiny)
library(shinydashboard)
library(shinyDND)
library(truncnorm)
library(V8)
library(shinyBS)
library(boastUtils)

jsResetCode <- "shinyjs.reset0 = function() {history.go(0)}"

# App Meta Data----------------------------------------------------------------
APP_TITLE  <<- "[Waiter Experiment]"
APP_DESCP  <<- paste(
  "This app will let students making use of general t-test to slove a problem of waiter's tip",
  "First, students need to review the concept of 'Two Sample T-Test' and 'Test Statistics & P-Values', 
   which we will use in the following exploration and game",
  "Second, students can use the game of waiter's tips to conduct a general t-test",
  "Third, students need to finish two games: 'p-value calculator' and 'matching games' to strength what they learned from this app"
)
# End App Meta Data------------------------------------------------------------

# Define UI for App

shinyUI(
  dashboardPage(
    skin = "purple",
    
    #Create the header
    dashboardHeader(
      title = "The Waiter Experiment", 
      titleWidth = 250,
      tags$li(class = "dropdown",actionLink("info", icon("info"))),
      tags$li(class = "dropdown",
        tags$a(href='https://shinyapps.science.psu.edu/', icon("home")))
      ),
    
    #Create the sidebar
    dashboardSidebar(
      width = 250,
      sidebarMenu(
        id = "tabs",
        menuItem("Overview", tabName = "overview", icon = icon("tachometer-alt")),
        menuItem("Prerequisites", tabName = "prereq", icon = icon("book")),
        menuItem("Simulation", tabName = "first", icon = icon("wpexplorer")),
        menuItem("Determining P-Values", tabName = "second", icon = icon("gamepad")),
        menuItem("References", tabName = "refs", icon = icon("leanpub"))), 
      tags$div(class = "sidebar-logo", boastUtils::psu_eberly_logo("reversed"))
      ),
    
    #Create the body
    dashboardBody(
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css",
                  href = "https://educationshinyappteam.github.io/Style_Guide/theme/boast.css")
        ),
      
      ##Overview Page
      tabItems(
        tabItem(
          tabName = "overview",
          h1("The Waiter Experiment"),
          p("This app demonstrates the reasoning of a two-sample t-test to determine
          whether a waiter giving a table candy or not affects their tip percentages.
          The null hypothesis is that the average tip percentages for tables receiving
          candy and tables not receiving candy are equal. The alternative hypothesis is 
          that the average tip percentages for tables receiving candy are greater than
            the average tip percentages for tables not receiving candy."),
          br(), 
          h2("Instructions:"),
          tags$li("Use the sliders to select a desired average tip percentage for tables receiving
          candy and tables not receiving candy. Click Randomly Assign and move the sliders
          to observe how the results change based on selected tip percent values. Is there
          a difference in a waiter's average tip percentage depending on whether they give
          a table candy or not? Each time the sliders are moved a new simulated sample is taken 
          for the group of tables involved and the test results are printed at the bottom."),
          tags$li("The Determining P-values section of the app allows the user to see how the p-value changes 
          for different observed effect sizes and different sample sizes. 
          Finally, a quiz tests your knowledge of how effect size and sample size affect the p-value."),
          div(style = "text-align: center",
              bsButton(
                inputId = "go",
                label = "Explore",
                icon = icon("bolt"),
                size = "large")),
          br(),
          h2("Acknowledgements:"),
          p("This application's the waiter experiment simulation was conceived and programmed by David Robinson 
            and then redesigned with the matching game added by Angela Ting. The latest version was modified by Gonghao Liu.",
            br(),
            br(),
            br(),
            div(class = "updated", "Last Update: 9/18/2020 by GL.")
            )
          ),
        
        #Prepreq Page 
        tabItem(
          tabName = "prereq",
          fluidRow(
            column(
              width = 10,
              h2("Prerequisites:"),
              box(
                title = strong("Two Sample T-Test"),
                status = "primary",
                collapsible = TRUE,
                collapsed = TRUE,
                width = '100%',
                "A two sample t-test is a commonly used hypothesis test to determine 
                whether the average difference between two groups can be explained 
                by random chance when the data arise from normally distributed populations. 
                Two sample t-tests can help answer a question like whether the test results of 
                patients who received a drug are significantly better than test results of those who received a placebo, 
                or whether any observed difference can be explained by chance."),
              withMathJax(),
              box(
                title = strong("Test Statistics & P-Values"),
                status = "primary",
                collapsible = TRUE,
                collapsed = TRUE,
                width = '100%',
                "The test statistic involves the difference between group averages standardized by an estimate of the 
                standard deviation of the difference. The statistic is compared to a t distribution to determine the p-value 
                (degrees of freedom = \\(n_{grp1}+n_{grp2} – 2\\)). 
                The p-value represents the probability of obtaining a difference at least as extreme as 
                the one in your sample data. Thus, a large p-value indicates the null hypothesis of no difference in means 
                provides a reasonable explanation of the data. Small p-values (typically taken to be below 0.05) 
                indicate the null hypothesis is a poor explanation of the data.")
              )
            )
          ),
        
        ##Waiter's Experiment Page
        tabItem(
          tabName = "first",
          fluidRow(column(width = 4,
                          "A study published in the Journal of Applied Social Psychology claims that 
                          giving candy to customers can increase a waiter's tip by about 23%.",
                          br(),
                          br(),
                          "Click below to randomly assign which tables receive candy.",
                          br(), 
                          br(),
                          shinyjs::useShinyjs(),
                          div(
                            id = "form",
                            div(style = "text-align: left",
                                bsButton(
                                  inputId = "rand",
                                  label = "Randomly Assign",
                                  size = "middle")
                                ),
                            br(), 
                            sliderInput(
                              inputId = "avgtipc", 
                              label = "Enter an average tip percentage for tables assigned to receive candy.", 
                              min = 0, 
                              max = 50, 
                              value = 0),
                            sliderInput(
                              inputId = "avgtipnc", 
                              label = "Enter an average tip percentage for tables assigned to receive no candy.", 
                              min = 0, 
                              max = 25, 
                              value = 0),
                            actionButton("reset", "Reset")
                            )
                          ),
                   column(width = 2,
                          htmlOutput("img1"),
                          textInput(
                            inputId = "tabtip1", 
                            label = NULL, 
                            value = "", 
                            width = '72px', 
                            placeholder = NULL),
                          htmlOutput("img2"),
                          textInput(
                            inputId = "tabtip2", 
                            label = NULL, 
                            value = "", 
                            width = '72px', 
                            placeholder = NULL),
                          htmlOutput("img3"),
                          textInput(
                            inputId = "tabtip3", 
                            label = NULL, 
                            value = "", 
                            width = '72px', 
                            placeholder = NULL),
                          htmlOutput("img4"),
                          textInput(
                            inputId = "tabtip4", 
                            label = NULL, 
                            value = "", 
                            width = '72px', 
                            placeholder = NULL),
                          htmlOutput("img5"),
                          textInput(
                            inputId = "tabtip5", 
                            label = NULL, 
                            value = "", 
                            width = '72px', 
                            placeholder = NULL),
                          htmlOutput("img6"),
                          textInput(
                            inputId = "tabtip6", 
                            label = NULL, 
                            value = "", 
                            width = '72px', 
                            placeholder = NULL)
                          ),
                   column(width = 2,
                          htmlOutput("img7"),
                          textInput(
                            inputId = "tabtip7", 
                            label = NULL, 
                            value = "", 
                            width = '72px', 
                            placeholder = NULL),
                          htmlOutput("img8"),
                          textInput(
                            inputId = "tabtip8", 
                            label = NULL, 
                            value = "", 
                            width = '72px', 
                            placeholder = NULL),
                          htmlOutput("img9"),
                          textInput(
                            inputId = "tabtip9", 
                            label = NULL, 
                            value = "", 
                            width = '72px', 
                            placeholder = NULL),
                          htmlOutput("img10"),
                          textInput(
                            inputId = "tabtip10", 
                            label = NULL, 
                            value = "", 
                            width = '72px', 
                            placeholder = NULL),
                          htmlOutput("img11"),
                          textInput(
                            inputId = "tabtip11", 
                            label = NULL, 
                            value = "", 
                            width = '72px', 
                            placeholder = NULL),
                          htmlOutput("img12"),
                          textInput(
                            inputId = "tabtip12", 
                            label = NULL, 
                            value = "", 
                            width = '72px', 
                            placeholder = NULL)
                          ),
                   column(width = 2,
                          htmlOutput("img13"),
                          textInput(
                            inputId = "tabtip13", 
                            label = NULL, 
                            value = "", 
                            width = '72px', 
                            placeholder = NULL),
                          htmlOutput("img14"),
                          textInput(
                            inputId = "tabtip14", 
                            label = NULL, 
                            value = "", 
                            width = '72px', 
                            placeholder = NULL),
                          htmlOutput("img15"),
                          textInput(
                            inputId = "tabtip15", 
                            label = NULL, 
                            value = "", 
                            width = '72px', 
                            placeholder = NULL),
                          htmlOutput("img16"),
                          textInput(
                            inputId = "tabtip16", 
                            label = NULL, 
                            value = "", 
                            width = '72px', 
                            placeholder = NULL),
                          htmlOutput("img17"),
                          textInput(
                            inputId = "tabtip17", 
                            label = NULL, 
                            value = "", 
                            width = '72px', 
                            placeholder = NULL),
                          htmlOutput("img18"),
                          textInput(
                            inputId = "tabtip18", 
                            label = NULL, 
                            value = "", 
                            width = '72px', 
                            placeholder = NULL)
                          ),
                   column(width = 2,
                          htmlOutput("img19"),
                          textInput(
                            inputId = "tabtip19", 
                            label = NULL, 
                            value = "", 
                            width = '72px', 
                            placeholder = NULL),
                          htmlOutput("img20"),
                          textInput(
                            inputId = "tabtip20", 
                            label = NULL, 
                            value = "", 
                            width = '72px',
                            placeholder = NULL),
                          htmlOutput("img21"),
                          textInput(
                            inputId = "tabtip21", 
                            label = NULL, 
                            value = "", 
                            width = '72px', 
                            placeholder = NULL),
                          htmlOutput("img22"),
                          textInput(
                            inputId = "tabtip22", 
                            label = NULL, 
                            value = "", 
                            width = '72px', 
                            placeholder = NULL),
                          htmlOutput("img23"),
                          textInput(
                            inputId = "tabtip23", 
                            label = NULL, 
                            value = "", 
                            width = '72px', 
                            placeholder = NULL),
                          htmlOutput("img24"),
                          textInput(
                            inputId = "tabtip24", 
                            label = NULL, 
                            value = "", 
                            width = '72px', 
                            placeholder = NULL),
                          br(), 
                          br()
                          )
                   ),
          fluidRow(
            column(
              width = 8, 
              offset = 1,
              wellPanel(
                h2(tags$b("Test Hypotheses:")),
                br(),
                HTML(paste(tags$b("H", tags$sub(0)), 
                           ": Giving customers candy does not affect the waiter's tip percentage")),
                br(),
                "(Candy Tip = No Candy Tip)",
                br(), 
                br(),
                HTML(paste(tags$b("H", tags$sub(1)), 
                           ": Giving customers candy does affect the waiter's tip percentage")),
                br(),
                "(Candy Tip > No Candy Tip)"
                ),
              conditionalPanel(
                condition = "input.rand",
                wellPanel(
                  h2(tags$b("Test Values:")),
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
              )
            )
          ),
        
        ##Game Page
        tabItem(
          tabName = "second",
          (tabsetPanel(
            id = "tabs2",
            tabPanel(
              title = "P-value Calculators", 
              value = "detpval",
              fluidRow(
                column(
                  width = 10,
                  h2(tags$b("How do effect size and sample size affect the p-value?")), 
                  br()
                  )
                ),
              fluidRow(
                column(
                  offset = 1, 
                  width = 10,
                  box(
                    width = 7,
                    background = "purple",
                    h3(tags$b("Test Hypotheses:")),
                    br(),
                    HTML(paste(tags$b("H", tags$sub(0)), 
                               ": Giving customers candy does not affect the waiter's tip percentage.")),
                    br(),
                    "(Candy Tip = No Candy Tip)",
                    br(), 
                    br(),
                    HTML(paste(tags$b("H", tags$sub(1)), 
                               ": Giving customers candy does affect the waiter's tip percentage.")), 
                    br(),
                    "(Candy Tip > No Candy Tip)",
                    br(), 
                    br()
                    )
                  )
                ),
              fluidRow(
                column(
                  offset = 1, 
                  width = 10,
                  h4(tags$b("Move the sliders for effect size and sample size to see how changes in each affect the p-value.")),
                  h4(tags$b("Observe whether researchers are more or less likely to detect a difference in average tip percentage
                            when you change the effect size and sample size.")),
                  br(),
                  sliderInput(
                    inputId = "ef_size", 
                    "Adjust the observed effect size", 
                    min = 0 , 
                    max = 50, 
                    value = 0, 
                    width = 600),
                  sliderInput(
                    inputId = "samp_size", 
                    "Adjust the sample size", 
                    min = 10, 
                    max = 100, 
                    value = 10, 
                    width = 600),
                  br(),
                  tags$head(tags$style("#pvalue{color: white; font-size: 30px; font-style: bold;")),
                  box(
                    width = 8, 
                    background = "purple", 
                    htmlOutput("pvalue")),
                  br()
                  )
                ),
              fluidRow(
                column(
                  width = 12, 
                  offset = 9,
                  br(), 
                  br(),
                  bsButton(
                    inputId = "totest",
                    label = "Test Your Understanding",
                    icon = icon("bolt"),
                    size = "medium")
                  )
                )
              ),
            tabPanel(
              title = "Matching Game", 
              value = "matching",
              fluidRow(
                fluidRow(
                  column(
                    12,
                    h2(tags$b("Test Your Understanding:")),
                    br(),
                    h4("Match the appropriate p-values to each question according to the effect size and sample size. 
                       Try to do this without doing any calculations using the P-value calculator"),
                    br()
                    )
                  ),
                
                #Question 1
                box(
                  width = 10, 
                  style = "color: #000000; background-color: #b6d0df", 
                  background = "blue", 
                  fluidRow(
                    column(
                      width = 6,
                      h2(tags$b("1.)")),
                      br(),
                      tableOutput("table1")
                      ),
                    column(
                      width = 5,
                      br(), 
                      br(), 
                      br(), 
                      br(),
                      htmlOutput("choices1")
                      )
                    )
                  ),
                column(
                  width = 2,
                  br(), 
                  br(),
                  htmlOutput("pic1")
                  ),
                #Question 2
                box(
                  width = 10, 
                  style = "color: #000000; background-color: #dae1e5", 
                  background = "blue",
                  fluidRow(
                    column(
                      width = 6,
                      h2(tags$b("2.)")),
                      br(),
                      tableOutput("table2")
                      ),
                    column(
                      width = 5,
                      br(),
                      br(), 
                      br(), 
                      br(),
                      htmlOutput("choices2")
                      )
                    )
                  ),
                column(
                  width = 2,
                  br(), 
                  br(),
                  htmlOutput("pic2")
                  ),
                #Question 3
                box(
                  width = 10, 
                  style = "color: #000000; background-color: #b6d0df", background = "blue", 
                  fluidRow(
                    column(
                      width = 6,
                      h2(tags$b("3.)")),
                      br(),
                      tableOutput("table3")
                      ),
                    column(
                      width = 5,
                      br(), 
                      br(), 
                      br(), 
                      br(),
                      htmlOutput("choices3")
                      )
                    )
                  ),
                column(
                  width = 2,
                  br(), 
                  br(),
                  htmlOutput("pic3")
                  ),
                #Question 4
                box(
                  width = 10, 
                  style = "color: #000000; background-color: #dae1e5", 
                  background = "blue",
                  fluidRow(
                    column(
                      width = 6,
                      h2(tags$b("4.)")),
                      br(),
                      tableOutput("table4")
                      ),
                    column(
                      width = 5,
                      br(), 
                      br(), 
                      br(), 
                      br(),
                      htmlOutput("choices4")
                      )
                    )
                  ),
                column(
                  width = 2,
                  br(), 
                  br(),
                  htmlOutput("pic4")
                  ),
                
                ###Submit Button
                fluidRow(
                  column(
                    width = 12, 
                    offset = 4,
                    actionButton(
                      inputId = "submit", 
                      HTML(paste(tags$b("Submit"))), 
                      style = "color: #fff; background-color: #1E407C;",
                      width = 200),
                    br(), 
                    br(),
                    uiOutput("feedback"),
                    br(),
                    uiOutput("showtry"),
                    br(),
                    actionButton(
                      inputId = "playagain", 
                      HTML(paste(tags$b("Play Again"))), 
                      style = "color: #fff; background-color: #1E407C;", 
                      width = 200)
                    )
                  )
                )
              )
            )
           )
          ),
        
        #Reference Page
        tabItem(
          tabName = "refs",
          withMathJax(),
          h2("References"),
          p(
            class = "hangingindent",
            "Bailey, E. (2015). shinyBS: Twitter bootstrap components for shiny.
            (v0.61). [R package]. Available from
            https://CRAN.R-project.org/package=shinyBS"
            ),
          p(
            class = "hangingindent",
            "Carey, R. (2019). boastUtils: BOAST Utilities. (v0.1.0). [R Package].
            Available from https://github.com/EducationShinyAppTeam/boastUtils"
            ),
          p(
            class = "hangingindent",
            "Chang, W. and Borges Ribeio, B. (2018). shinydashboard: Create
            dashboards with 'Shiny'. (v0.7.1) [R Package]. Available from
            https://CRAN.R-project.org/package=shinydashboard"
            ),
          p(
            class = "hangingindent",
            "Chang, W., Cheng, J., Allaire, J., Xie, Y., and McPherson, J. (2019).
            shiny: Web application framework for R. (v1.4.0) [R Package].
            Available from https://CRAN.R-project.org/package=shiny"
            ),
          p(
            class = "hangingindent",
            "Aaron Hoffer (2016). shinyDND: Shiny Drag-n-Drop. R package version 0.1.0.
            https://CRAN.R-project.org/package=shinyDND"
            ),
          p(
            class = "hangingindent",
            "Jeroen Ooms (2020). V8: Embedded JavaScript and WebAssembly Engine for R. 
            R package version 3.2.0.https://CRAN.R-project.org/package=V8"
            )
          )
        ) 
      ) 
    )
  )
