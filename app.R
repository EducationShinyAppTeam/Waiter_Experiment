# Load packages ----
library(shiny)
library(shinydashboard)
library(shinyDND)
library(truncnorm)
library(V8)
library(shinyBS)
library(boastUtils)
library(shinyjs)
library(shinyWidgets)
library(ggplot2)

# App Meta Data----------------------------------------------------------------
APP_TITLE  <<- "Waiter Experiment"
APP_DESCP  <<- paste(
  "This app will let students making use of general t-test to slove a problem of
    waiter's tip",
  "First, students need to review the concept of 'Two Sample T-Test' and 'Test
    Statistics & P-Values',
   which we will use in the following exploration and game",
  "Second, students can use the game of waiter's tips to conduct a general t-test",
  "Third, students need to finish two games: 'p-value calculator' and 'matching
    games' to strength what they learned from this app"
)
# End App Meta Data------------------------------------------------------------

# Define global constants, functions, and load data ----
jsResetCode <- "shinyjs.reset0 = function() {history.go(0)}"

#disable actionButton function
disableActionButton <- function(id,session) {
  session$sendCustomMessage(
    type = "jsCode",
    list(
      code =  paste("$('#",id,"').prop('disabled',true)",
                  sep = "")))
}

# Define the UI ----
ui <- list(
  dashboardPage(
    skin = "purple",
    ## Header ----
    dashboardHeader(
      title = "The Waiter Experiment",
      titleWidth = 250,
      tags$li(class = "dropdown",actionLink("info", icon("info"))),
      tags$li(
        class = "dropdown",
        boastUtils::surveyLink(name = "The_Waiter_Experiment")
      ),
      tags$li(
        class = "dropdown",
        tags$a(href = 'https://shinyapps.science.psu.edu/',
               icon("home")
        )
      )
    ),
    ## Sidebar ----
    dashboardSidebar(
      width = 250,
      sidebarMenu(
        id = "pages",
        menuItem("Overview", tabName = "overview", icon = icon("tachometer-alt")),
        menuItem("Prerequisites", tabName = "prereq", icon = icon("book")),
        menuItem("Simulation", tabName = "first", icon = icon("wpexplorer")),
        menuItem("Determining P-Values", tabName = "second", icon = icon("gamepad")),
        menuItem("References", tabName = "refs", icon = icon("leanpub"))),
      tags$div(
        class = "sidebar-logo",
        boastUtils::sidebarFooter()
      )    
    ),
    #Body ----
    dashboardBody(
      ### Overview Page ----
      tabItems(
        tabItem(
          tabName = "overview",
          h1("The Waiter Experiment"),
          p("This app demonstrates the reasoning of a two-sample t-test to determine
          whether a waiter giving a table candy or not affects their tip percentages.
          The null hypothesis is that the mean tip percentages for tables receiving
          candy and tables not receiving candy are equal. The alternative hypothesis is
          that the mean tip percentages for tables receiving candy are greater than
            the mean tip percentages for tables not receiving candy."),
          br(),
          h2("Instructions"),
          tags$ul(
            tags$li("Click Randomly Assign to choose which tables get the candy
                    and which do not. Use the sliders to select a desired average
                    tip percentage for tables receiving candy and tables not
                    receiving candy and observe how the results change based on
                    selected tip percent values."),
            tags$li("The Determining P-values section of the app allows the user 
                    to see how the p-value changes for different observed effect
                    sizes and different sample sizes."), 
            tags$li("Finally, a quiz tests your knowledge of how effect size and
                    sample size affect the p-value.")
          ), 
          br(), 
          div(style = "text-align: center",
              bsButton(
                inputId = "go",
                label = "Prerequisites!",
                icon = icon("book"),
                size = "large")),
          br(),
          h2("Acknowledgements"),
          p("This application's the waiter experiment simulation was conceived 
            and programmed by David Robinson and then redesigned with the matching
            game added by Angela Ting. The latest version was modified by Gonghao
            Liu and Shravani Samala in 2021. A special thanks to Dr. Neil Hatfield
            for creating the graph seen in the Determining P-Values page.",
            br(),
            br(),
            br(),
            div(class = "updated", "Last Update: 7/14/2021 by SJS.")
          )
        ),
        ### Prepreq Page ----
        tabItem(
          tabName = "prereq",
          fluidRow(
            column(
              width = 10,
              h2("Prerequisites"),
              box(
                title = strong("Two Sample T-Test"),
                status = "primary",
                collapsible = TRUE,
                collapsed = FALSE,
                width = 12,
                p("A two sample t-test is a commonly used hypothesis test to
                  determine whether the mean difference between two groups
                  can be explained by random chance when the data arise from
                  normally distributed populations. Two sample t-tests can help
                  answer a question like whether the test results of patients who
                  received a drug are significantly better than test results of
                  those who received a placebo, or whether any observed difference
                  can be explained by chance.")),
              withMathJax(),
              box(
                title = strong("Test Statistics & P-Values"),
                status = "primary",
                collapsible = TRUE,
                collapsed = FALSE,
                width = 12,
                p("The test statistic involves the difference between group means
                  standardized by an estimate of the standard deviation of the 
                  difference (degrees of freedom = \\(n_{grp1}+n_{grp2} - 2\\)). The p-value
                  represents the probability of obtaining a difference at least
                  as extreme as the one in your sample data. Thus, a large p-value
                  indicates the null hypothesis of no difference in means provides
                  a reasonable explanation of the data. Small p-values (typically
                  taken to be below 0.05) indicate the null hypothesis is a poor
                  explanation of the data."))
            )
          ), 
          br(), 
          div(style = "text-align: center",
              bsButton(
                inputId = "go2",
                label = "Simulation!",
                icon = icon("book"),
                size = "large"))
        ),
        ##Waiter's Experiment Page ----
        tabItem(
          tabName = "first",
          h2("Waiter Simulation"), 
          p("A study published in the Journal of Applied Social Psychology
              claims that giving candy to customers can increase a waiter's
              tip by about 23%."),
          p("In the Waiter Simulation Input tab, FIRST click the button to randomly
            assign which tables receive candy and then use the sliders to set the
            mean tip percentage for tables with candy and no candy. Then, check
            the Waiter Simulation Output tab to see the test hypotheses and test
            values."),
          br(), 
          tabsetPanel(
            tabPanel(
              title = "Random Assignment",
              br(),
              fluidRow(
                column(
                  width = 5,
                  wellPanel(
                    br(),
                    shinyjs::useShinyjs(),
                    div(
                      id = "form",
                      div(
                        style = "text-align: left",
                        bsButton(
                          inputId = "rand",
                          label = "Randomly Assign",
                          size = "large"
                        )
                      ),
                      br(),
                      sliderInput(
                        inputId = "avgtipc",
                        label = "Enter a mean tip percentage for tables assigned
                            to receive candy.",
                        min = 0,
                        max = 50,
                        post = "%", 
                        value = 0),
                      sliderInput(
                        inputId = "avgtipnc",
                        label = "Enter a mean tip percentage for tables assigned
                            to receive no candy.",
                        min = 0,
                        max = 25,
                        post = "%", 
                        value = 0),
                      bsButton(
                        inputId = "reset", 
                        label = "Reset", 
                        size = "large")
                    )
                  )
                ),
                column(
                  width = 1,
                  htmlOutput("img1"),
                  textInput(
                    inputId = "tabtip1",
                    label = "% Tip",
                    value = "",
                    width = '72px',
                    placeholder = NULL),
                  htmlOutput("img2"),
                  textInput(
                    inputId = "tabtip2",
                    label = "% Tip",
                    value = "",
                    width = '72px',
                    placeholder = NULL),
                  htmlOutput("img3"),
                  textInput(
                    inputId = "tabtip3",
                    label = "% Tip",
                    value = "",
                    width = '72px',
                    placeholder = NULL),
                  htmlOutput("img4"),
                  textInput(
                    inputId = "tabtip4",
                    label = "% Tip",
                    value = "",
                    width = '72px',
                    placeholder = NULL
                  )
                ), 
                column(
                  width = 1, 
                  htmlOutput("img5"),
                  textInput(
                    inputId = "tabtip5",
                    label = "% Tip",
                    value = "",
                    width = '72px',
                    placeholder = NULL),
                  htmlOutput("img6"),
                  textInput(
                    inputId = "tabtip6",
                    label = "% Tip",
                    value = "",
                    width = '72px',
                    placeholder = NULL), 
                  htmlOutput("img7"),
                  textInput(
                    inputId = "tabtip7",
                    label = "% Tip",
                    value = "",
                    width = '72px',
                    placeholder = NULL),
                  htmlOutput("img8"),
                  textInput(
                    inputId = "tabtip8",
                    label = "% Tip",
                    value = "",
                    width = '72px',
                    placeholder = NULL),
                ), 
                column(
                  width = 1,   
                  htmlOutput("img9"),
                  textInput(
                    inputId = "tabtip9",
                    label = "% Tip",
                    value = "",
                    width = '72px',
                    placeholder = NULL),
                  htmlOutput("img10"),
                  textInput(
                    inputId = "tabtip10",
                    label = "% Tip",
                    value = "",
                    width = '72px',
                    placeholder = NULL),
                  htmlOutput("img11"),
                  textInput(
                    inputId = "tabtip11",
                    label = "% Tip",
                    value = "",
                    width = '72px',
                    placeholder = NULL),
                  htmlOutput("img12"),
                  textInput(
                    inputId = "tabtip12",
                    label = "% Tip",
                    value = "",
                    width = '72px',
                    placeholder = NULL)
                ),
                column(
                  width = 1,
                  htmlOutput("img13"),
                  textInput(
                    inputId = "tabtip13",
                    label = "% Tip",
                    value = "",
                    width = '72px',
                    placeholder = NULL),
                  htmlOutput("img14"),
                  textInput(
                    inputId = "tabtip14",
                    label = "% Tip",
                    value = "",
                    width = '72px',
                    placeholder = NULL),
                  htmlOutput("img15"),
                  textInput(
                    inputId = "tabtip15",
                    label = "% Tip",
                    value = "",
                    width = '72px',
                    placeholder = NULL),
                  htmlOutput("img16"),
                  textInput(
                    inputId = "tabtip16",
                    label = "% Tip",
                    value = "",
                    width = '72px',
                    placeholder = NULL)
                ), 
                column(
                  width = 1, 
                  htmlOutput("img17"),
                  textInput(
                    inputId = "tabtip17",
                    label = "% Tip",
                    value = "",
                    width = '72px',
                    placeholder = NULL),
                  htmlOutput("img18"),
                  textInput(
                    inputId = "tabtip18",
                    label = "% Tip",
                    value = "",
                    width = '72px',
                    placeholder = NULL), 
                  htmlOutput("img19"),
                  textInput(
                    inputId = "tabtip19",
                    label = "% Tip",
                    value = "",
                    width = '72px',
                    placeholder = NULL),
                  htmlOutput("img20"),
                  textInput(
                    inputId = "tabtip20",
                    label = "% Tip",
                    value = "",
                    width = '72px',
                    placeholder = NULL),
                ), 
                column(
                  width = 1, 
                  htmlOutput("img21"),
                  textInput(
                    inputId = "tabtip21",
                    label = "% Tip",
                    value = "",
                    width = '72px',
                    placeholder = NULL),
                  htmlOutput("img22"),
                  textInput(
                    inputId = "tabtip22",
                    label = "% Tip",
                    value = "",
                    width = '72px',
                    placeholder = NULL),
                  htmlOutput("img23"),
                  textInput(
                    inputId = "tabtip23",
                    label = "% Tip",
                    value = "",
                    width = '72px',
                    placeholder = NULL),
                  htmlOutput("img24"),
                  textInput(
                    inputId = "tabtip24",
                    label = "% Tip",
                    value = "",
                    width = '72px',
                    placeholder = NULL),
                  br(),
                  br()
                )
              )
            ), 
            tabPanel(
              title = "Test Info", 
              br(), 
              fluidRow(
                column(
                  width = 6,
                  offset = 0,
                    wellPanel(
                      style = "background: white",
                      h2("Test Hypotheses:"),
                      br(),
                      HTML(paste("H", tags$sub(0),
                                 ": Giving customers candy does not affect the waiter's
                             tip percentage")),
                      br(),
                      p("(Candy Tip = No Candy Tip)"),
                      br(),
                      HTML(paste("H", tags$sub(1),
                                 ": Giving customers candy does affect the waiter's tip
                             percentage")),
                      br(),
                      p("(Candy Tip > No Candy Tip)"), 
                    )
                  ),
                column(
                  width = 6,
                  offset = 0, 
                  wellPanel(
                    style = "background: white",
                  h2("Test Values:"),
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
          )
        ),
        ### Game Page ----
        tabItem(
          tabName = "second",
          h2("Determining P-Values"), 
          br(), 
          tabsetPanel(
            id = "pages2",
            tabPanel(
              title = "P-value Calculators",
              value = "detpval",
              
              br(), 
              h2("How do effect size and sample size affect the p-value?"),
              br(), 
              tags$ul(
                tags$li("Move the sliders for effect size and sample size to see how
                   changes in each affect the p-value."),
                tags$li("Observe whether researchers are more or less likely to
                   detect a difference in mean tip percentage when you change
                   the effect size and sample size."),
              ), 
              fluidRow(
                column(
                  width = 6,
                  wellPanel(
                    br(),
                    sliderInput(
                      inputId = "ef_size",
                      "Adjust the observed effect size",
                      min = -10,
                      max = 20, 
                      post = "%",
                      value = 0,
                      width = 600, 
                      animate = animationOptions(interval = 1500, loop = FALSE)
                    ),
                    sliderInput(
                      inputId = "samp_size",
                      "Adjust the sample size",
                      min = 10,
                      max = 100,
                      value = 10,
                      width = 600, 
                      animate = animationOptions(interval = 1500, loop = FALSE)),
                    br(),
                    br()
                  )
                ), 
                column(
                  width = 6,
                  box(
                    width = 12,
                    # background = "purple",
                    h3("Test Hypotheses:"),
                    br(),
                    HTML(paste("H", tags$sub(0),
                               ": Giving customers candy does not affect the
                               waiter's tip percentage.")),
                    br(),
                    "(Candy Tip = No Candy Tip)",
                    br(),
                    br(),
                    HTML(paste("H", tags$sub(1),
                               ": Giving customers candy does affect the waiter's
                               tip percentage.")),
                    br(),
                    "(Candy Tip > No Candy Tip)",
                    br(),
                    br()
                  ), 
                  tags$head(tags$style("#pvalue{color: black; font-size: 20px")),
                  box(
                    width = 12,
                    htmlOutput("pvalue"))
                )
              ), 
              plotOutput("ef_sizeGraph"), 
              fluidRow(
                column(
                  width = 12,
                  br(),
                  br(),
                  style = "text-align: center",
                  bsButton(
                    inputId = "totest",
                    label = "Test Your Understanding",
                    icon = icon("bolt"),
                    size = "large", 
                    style = "default")
                )
              )
            ),
            tabPanel(
              title = "Matching Game",
              value = "matching",
              br(), 
              h2("Test Your Understanding:"),
              br(),
              p("Match the appropriate p-values to each question according
                       to the effect size and sample size. Try to do this without
                       doing any calculations using the P-value calculator"),
              br(), 
                #Question 1
                box(
                  width = 10,
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
                  uiOutput("pic1")
                  #htmlOutput("pic1")
                ),
                #Question 2
                box(
                  width = 10,
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
                  uiOutput("pic2")
                  #htmlOutput("pic2")
                ),
                #Question 3
                box(
                  width = 10,
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
                  uiOutput("pic3")
                  #htmlOutput("pic3")
                ),
                #Question 4
                box(
                  width = 10,
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
                  uiOutput("pic4")
                  #htmlOutput("pic4")
                ),

                ###Submit Button
                fluidRow(
                  column(
                    width = 12,
                    offset = 4,
                    bsButton(
                      inputId = "submit",
                      HTML(paste("Submit")),
                      style = "default",
                      size = "large"),
                    br(),
                    br(),
                    uiOutput("feedback"),
                    br(),
                    uiOutput("showtry"),
                    br(),
                    bsButton(
                      inputId = "playagain",
                      HTML(paste("Play Again")),
                      style = "default",
                      size = "large")
                  )
                )
              )
            )
          ), 
        
        #Reference Page ----
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
          ), 
          p(
            class = "hangingindent", 
            "Image: https://webstockreview.net/image/clipart-restaurant-restaurant
            -booth/2478454.html"
          ), 
          p(
            class = "hangingindent", 
            "Study:  http://onlinelibrary.wiley.com/doi/10.1111/j.1559-1816.2002.tb00216.
            x/abstract."
          ), 
          p(
            class = "hangingindent", 
            "Study:  http://onlinelibrary.wiley.com/doi/10.1111/j.1559-1816.2002.tb00216.
            x/abstract."
          ), 
          br(),
          br(),
          br(),
          boastUtils::copyrightInfo()
        )
      )
    )
  )
)

# Define the server ----
server <- function(input, output, session) {
  
  #info button
  observeEvent(input$info, {
    sendSweetAlert(
      session = session,
      title = "Instructions:",
      text = "This app demonstrates the reasoning of a two-sample t-test to
      determine whether a waiter giving a table candy or not affects their
      tip percentages.",
      type = "info"
    )
  })

  #move from Prerequisites to Overview
  observeEvent(
    eventExpr = input$next.page, 
    handlerExpr = {
      updateTabItems(
        session = session, 
        inputId = "pages", 
        selected = "overview")
  })

  #move from Overview to Part 1
  observeEvent(
    eventExpr = input$go, 
    handlerExpr = {
      updateTabItems(
        session = session, 
        inputId = "pages", 
        selected = "prereq")
  })
  
  observeEvent(
    eventExpr = input$go2, 
    handlerExpr = {
      updateTabItems(
        session = session, 
        inputId = "pages", 
        selected = "first")
    })

  #create reactive variables for the candy/no candy avg tip percentages
  avg.tip.c = reactive({(input$avgtipc)})
  avg.tip.nc = reactive({(input$avgtipnc)})

  #select 12 random tables to receive candy
  #create a reactive variable accessible throughout the server: samp$sample
    #(vector of 12 randomly selected numbers ranging from 1-24)
  #disable the Randomly Assign Button after the first time it is clicked
  samp = reactiveValues(sample = 0)
  observeEvent(
    input$rand,({
      samp$sample = sample(1:24, 12)
      shinyjs::disable("rand")
    })
  )

  #when the slider is moved for average tip percentage of tables receiving candy,
    #generate a percentage from rtruncnorm for each table in the sample
  #since avg.tip.c() is reactive, whenever 'mean' changes, 'val' will change,
    #and thus updateTextInput will change, executing each time the mean tip
    #percent for candy tables is changed
  observeEvent({input$avgtipc}, {
    for (i in samp$sample) {
      val = rtruncnorm(
        n = 1,
        a = 0,
        b = 100,
        mean = avg.tip.c(),
        sd = 1.75)

      #only assign random values if the Randomly Assign button has been pressed
        #(this stops random values being generated in the text boxes when the
        #app first launches)
      if (input$rand != 0) {
        updateTextInput(
          session,
          paste0("tabtip", i),
          value = as.character(round(val, 2)))
      }
    }

  })

  #when the slider is moved for average tip percentage of tables receiving not
    #candy, generate a percentage from rtruncnorm for each table NOT in the sample
  observeEvent({input$avgtipnc}, {
    for (i in 1:24) {
      if (!(i %in% samp$sample)) {
        val = rtruncnorm(
          n = 1,
          a = 0,
          b = 100,
          mean = avg.tip.nc(),
          sd = 1.75)

        #only assign random values if the Randomly Assign button has been pressed
          #(this stops random values being generated in the text boxes when the
          #app first launches)
        if (input$rand != 0) {
          updateTextInput(
            session, paste0("tabtip", i),
            value = as.character(round(val, 2))
          )
        }
      }
    }
  })

  #reset button
  observeEvent(input$reset,
               {shinyjs::reset("form")})

  ######TEST VALUES CALCULATIONS######

  #tips is a reactive object consisting of the list: tip.list
  tips = reactiveValues(tip.list = c())

  observe({
    tips$tip.list[1] = as.numeric(input$tabtip1)
    tips$tip.list[2] = as.numeric(input$tabtip2)
    tips$tip.list[3] = as.numeric(input$tabtip3)
    tips$tip.list[4] = as.numeric(input$tabtip4)
    tips$tip.list[5] = as.numeric(input$tabtip5)
    tips$tip.list[6] = as.numeric(input$tabtip6)
    tips$tip.list[7] = as.numeric(input$tabtip7)
    tips$tip.list[8] = as.numeric(input$tabtip8)
    tips$tip.list[9] = as.numeric(input$tabtip9)
    tips$tip.list[10] = as.numeric(input$tabtip10)
    tips$tip.list[11] = as.numeric(input$tabtip11)
    tips$tip.list[12] = as.numeric(input$tabtip12)
    tips$tip.list[13] = as.numeric(input$tabtip13)
    tips$tip.list[14] = as.numeric(input$tabtip14)
    tips$tip.list[15] = as.numeric(input$tabtip15)
    tips$tip.list[16] = as.numeric(input$tabtip16)
    tips$tip.list[17] = as.numeric(input$tabtip17)
    tips$tip.list[18] = as.numeric(input$tabtip18)
    tips$tip.list[19] = as.numeric(input$tabtip19)
    tips$tip.list[20] = as.numeric(input$tabtip20)
    tips$tip.list[21] = as.numeric(input$tabtip21)
    tips$tip.list[22] = as.numeric(input$tabtip22)
    tips$tip.list[23] = as.numeric(input$tabtip23)
    tips$tip.list[24] = as.numeric(input$tabtip24)

  })

  #average tip percent for tables receiving candy (reactive variable)
  avg.c = reactive({
    sum.c = 0
    for (i in 1:24) {
      if (i %in% samp$sample) {
        sum.c = sum.c + tips$tip.list[i]
      }
    }
    round(sum.c/12, 2)
  })

  #average tip percent for tables not receiving candy (reactive variable)
  avg.nc = reactive({
    sum.nc = 0
    for (i in 1:24) {
      if (!(i %in% samp$sample)) {
        sum.nc = sum.nc + tips$tip.list[i]
      }
    }
    round(sum.nc/12, 2)
  })

  #effect size (reactive variable)
  effect = reactive({
    eff.size = avg.c() - avg.nc()
    round(eff.size, 2)
  })

  #standard deviation of tips for tables with candy (reactive variable)
  sd.c = reactive({
    vec.c = c()
    for (i in 1:24) {
      if (i %in% samp$sample) {
        vec.c = c(vec.c, tips$tip.list[i])
      }
    }
    sd(vec.c)
  })

  #standard deviation of tips for tables without candy (reactive variable)
  sd.nc = reactive({
    vec.nc = c()
    for (i in 1:24) {
      if (!(i %in% samp$sample)) {
        vec.nc = c(vec.nc, tips$tip.list[i])
      }
    }
    sd(vec.nc)
  })

  #test statistic (reactive variable)
  z.stat = reactive({
    stand.err = sqrt(((sd.c()*sd.c())/12) + ((sd.nc()*sd.nc())/12))
    stat = effect()/stand.err
    round(stat, 2)
  })

  #p-value (reactive variable)
  p.val = reactive({
    pval = 1 - pnorm(abs(z.stat()))
    rpval = round(pval, 4)

    #must use ifelse function rather than if and else statements to avoid logical
      #input error
    ifelse(rpval == 0, "approximately 0", rpval)
  })


  #render HTML; call avg.c()
  output$average.c = renderUI({
    HTML(paste0("The average tip for the 12 tables receiving candy in our sample is: ",
                tags$b(avg.c()),
                tags$b(" %"), "."))
  })

  #render HTML, call avg.nc()
  output$average.nc = renderUI({
    HTML(paste0("The mean tip for the 12 tables not receiving candy in our sample is: ",
                tags$b(avg.nc()),
                tags$b(" %"),"."))
  })

  #render HTML, call effect()
  output$effect.size = renderUI({
    HTML(paste0("The observed effect size is: ",
                tags$b(avg.c()), tags$b(" % - "),
                tags$b(avg.nc()), tags$b(" % = "),
                tags$b(effect()), tags$b( " %"), "."))
  })

  #render HTML, call test.stat()
  output$test.stat = renderUI({
    HTML(paste0("The test statistic is: ",
                tags$b("T = "),
                tags$b(z.stat()), "."))
  })

  #render HTML, call p.val()
  output$p_value = renderUI({
    HTML(paste0("The p-value is: ",
                tags$b(p.val()), "."))
  })

  ####################################################
  #           For rendering the 24 images            #
  ####################################################
  output$img1 = renderUI({
    if (1 %in% samp$sample) {
      tags$img(
        src = 'tablecandy.PNG',
        alt = "A table without candy",
        width = 70,
        height = 60)
    }
    else{
      tags$img(
        src = 'dinnerTable.jpg',
        alt = "A table with candy",
        width = 70,
        height = 60)
    }
  })

  output$img2 = renderUI({
    if (2 %in% samp$sample) {
      tags$img(
        src = 'tablecandy.PNG',
        alt = "A table without candy",
        width = 70,
        height = 60)
    }
    else{
      tags$img(
        src = 'dinnerTable.jpg',
        alt = "A table with candy",
        width = 70,
        height = 60)
    }
  })

  output$img3 = renderUI({
    if (3 %in% samp$sample) {
      tags$img(
        src = 'tablecandy.PNG',
        alt = "A table without candy",
        width = 70,
        height = 60)
    }
    else{
      tags$img(
        src = 'dinnerTable.jpg',
        alt = "A table with candy",
        width = 70,
        height = 60)
    }
  })

  output$img4 = renderUI({
    if (4 %in% samp$sample) {
      tags$img(
        src = 'tablecandy.PNG',
        alt = "A table without candy",
        width = 70,
        height = 60)
    }
    else{
      tags$img(
        src = 'dinnerTable.jpg',
        alt = "A table with candy",
        width = 70,
        height = 60)
    }
  })

  output$img5 = renderUI({
    if (5 %in% samp$sample) {
      tags$img(
        src = 'tablecandy.PNG',
        alt = "A table without candy",
        width = 70,
        height = 60)
    }
    else{
      tags$img(
        src = 'dinnerTable.jpg',
        alt = "A table with candy",
        width = 70,
        height = 60)
    }
  })

  output$img6 = renderUI({
    if (6 %in% samp$sample) {
      tags$img(
        src = 'tablecandy.PNG',
        alt = "A table without candy",
        width = 70,
        height = 60)
    }
    else{
      tags$img(
        src = 'dinnerTable.jpg',
        alt = "A table with candy",
        width = 70,
        height = 60)
    }
  })

  output$img7 = renderUI({
    if (7 %in% samp$sample) {
      tags$img(
        src = 'tablecandy.PNG',
        alt = "A table without candy",
        width = 70,
        height = 60)
    }
    else{
      tags$img(
        src = 'dinnerTable.jpg',
        alt = "A table with candy",
        width = 70,
        height = 60)
    }
  })

  output$img8 = renderUI({
    if (8 %in% samp$sample) {
      tags$img(
        src = 'tablecandy.PNG',
        alt = "A table without candy",
        width = 70,
        height = 60)
    }
    else{
      tags$img(
        src = 'dinnerTable.jpg',
        alt = "A table with candy",
        width = 70,
        height = 60)
    }
  })

  output$img9 = renderUI({
    if (9 %in% samp$sample) {
      tags$img(
        src = 'tablecandy.PNG',
        alt = "A table without candy",
        width = 70,
        height = 60)
    }
    else{
      tags$img(
        src = 'dinnerTable.jpg',
        alt = "A table with candy",
        width = 70,
        height = 60)
    }
  })

  output$img10 = renderUI({
    if (10 %in% samp$sample) {
      tags$img(
        src = 'tablecandy.PNG',
        alt = "A table without candy",
        width = 70,
        height = 60)
    }
    else{
      tags$img(
        src = 'dinnerTable.jpg',
        alt = "A table with candy",
        width = 70,
        height = 60)
    }
  })

  output$img11 = renderUI({
    if (11 %in% samp$sample) {
      tags$img(
        src = 'tablecandy.PNG',
        alt = "A table without candy",
        width = 70,
        height = 60)
    }
    else{
      tags$img(
        src = 'dinnerTable.jpg',
        alt = "A table with candy",
        width = 70,
        height = 60)
    }
  })

  output$img12 = renderUI({
    if (12 %in% samp$sample) {
      tags$img(
        src = 'tablecandy.PNG',
        alt = "A table without candy",
        width = 70,
        height = 60)
    }
    else{
      tags$img(
        src = 'dinnerTable.jpg',
        alt = "A table with candy",
        width = 70,
        height = 60)
    }
  })

  output$img13 = renderUI({
    if (13 %in% samp$sample) {
      tags$img(
        src = 'tablecandy.PNG',
        alt = "A table without candy",
        width = 70,
        height = 60)
    }
    else{
      tags$img(
        src = 'dinnerTable.jpg',
        alt = "A table with candy",
        width = 70,
        height = 60)
    }
  })

  output$img14 = renderUI({
    if (14 %in% samp$sample) {
      tags$img(
        src = 'tablecandy.PNG',
        alt = "A table without candy",
        width = 70,
        height = 60)
    }
    else{
      tags$img(
        src = 'dinnerTable.jpg',
        alt = "A table with candy",
        width = 70, height = 60)
    }
  })

  output$img15 = renderUI({
    if (15 %in% samp$sample) {
      tags$img(
        src = 'tablecandy.PNG',
        alt = "A table without candy",
        width = 70,
        height = 60)
    }
    else{
      tags$img(
        src = 'dinnerTable.jpg',
        alt = "A table with candy",
        width = 70,
        height = 60)
    }
  })

  output$img16 = renderUI({
    if (16 %in% samp$sample) {
      tags$img(
        src = 'tablecandy.PNG',
        alt = "A table without candy",
        width = 70,
        height = 60)
    }
    else{
      tags$img(
        src = 'dinnerTable.jpg',
        alt = "A table with candy",
        width = 70,
        height = 60)
    }
  })

  output$img17 = renderUI({
    if (17 %in% samp$sample) {
      tags$img(
        src = 'tablecandy.PNG',
        alt = "A table without candy",
        width = 70,
        height = 60)
    }
    else{
      tags$img(
        src = 'dinnerTable.jpg',
        alt = "A table with candy",
        width = 70,
        height = 60)
    }
  })

  output$img18 = renderUI({
    if (18 %in% samp$sample) {
      tags$img(
        src = 'tablecandy.PNG',
        alt = "A table without candy",
        width = 70,
        height = 60)
    }
    else{
      tags$img(
        src = 'dinnerTable.jpg',
        alt = "A table with candy",
        width = 70,
        height = 60)
    }
  })

  output$img19 = renderUI({
    if (19 %in% samp$sample) {
      tags$img(
        src = 'tablecandy.PNG',
        alt = "A table without candy",
        width = 70,
        height = 60)
    }
    else{
      tags$img(
        src = 'dinnerTable.jpg',
        alt = "A table with candy",
        width = 70,
        height = 60)
    }
  })

  output$img20 = renderUI({
    if (20 %in% samp$sample) {
      tags$img(
        src = 'tablecandy.PNG',
        alt = "A table without candy",
        width = 70,
        height = 60)
    }
    else{
      tags$img(
        src = 'dinnerTable.jpg',
        alt = "A table with candy",
        width = 70,
        height = 60)
    }
  })

  output$img21 = renderUI({
    if (21 %in% samp$sample) {
      tags$img(
        src = 'tablecandy.PNG',
        alt = "A table without candy",
        width = 70, height = 60)
    }
    else{
      tags$img(
        src = 'dinnerTable.jpg',
        alt = "A table with candy",
        width = 70,
        height = 60)
    }
  })

  output$img22 = renderUI({
    if (22 %in% samp$sample) {
      tags$img(
        src = 'tablecandy.PNG',
        alt = "A table without candy",
        width = 70,
        height = 60)
    }
    else{
      tags$img(
        src = 'dinnerTable.jpg',
        alt = "A table with candy",
        width = 70,
        height = 60)
    }
  })

  output$img23 = renderUI({
    if (23 %in% samp$sample) {
      tags$img(
        src = 'tablecandy.PNG',
        alt = "A table without candy",
        width = 70,
        height = 60)
    }
    else{
      tags$img(
        src = 'dinnerTable.jpg',
        alt = "A table with candy",
        width = 70,
        height = 60)
    }
  })

  output$img24 = renderUI({
    if (24 %in% samp$sample) {
      tags$img(
        src = 'tablecandy.PNG',
        alt = "A table without candy",
        width = 70,
        height = 60)
    }
    else{
      tags$img(
        src = 'dinnerTable.jpg',
        alt = "A table with candy",
        width = 70,
        height = 60)
    }
  })
  ##################P-VALUE TAB########################
  #effect size (reactive variable)
  esize = reactive({input$ef_size})

  #sample size (reactive variable)
  sampsize = reactive({input$samp_size})

  #test statistic (reactive variable)
  zstat = reactive({
    sd1 = 16
    sd2 = 16
    stand.err = sqrt(((sd1*sd1)/(sampsize()/2)) + ((sd2*sd2)/(sampsize()/2)))
    stat = esize()/stand.err
    round(stat, 2)

  })

  #p-value (reactive variable)
  #note: could have passed this to "p.val" but we will create a new reactive
    #p-value variable for clarity
  pval = reactive({
    #I actually invalidated this condition in the UI by starting the sample size at 10
    if ((esize() == 0) | (sampsize() == 0)) {
      return("0.5")
    }
    else{
      pval = 1 - pnorm(abs(zstat()))
      rpval = round(pval, 4)
      return(ifelse(rpval == 0, "approximately 0", rpval))
    }

  })

  #render HTML; call pval()
  output$pvalue = renderUI({
    HTML(paste0(p("The p-value is: ", pval(), ".")))
  })

  #move from Determining P-Values to Test Your Understanding
  observeEvent(input$totest, {
    updateTabsetPanel(session, "pages2", selected = "matching")
  })

  ##################MATCHING TAB#######################

  #the Play Again button should be hidden when the app is first launched
  shinyjs::hideElement("playagain")


  #set working directory to source file location
  data = read.csv("P-Value Question Bank.csv", header = TRUE)

  #split entire data frame into 5* blocks
  dflist = split(data, data$Block)

  block = eventReactive({input$playagain | TRUE},
                        {sample(1:(nrow(data)/4), 1)})

  order = eventReactive({input$playagain | TRUE},
                        {sample(1:4)})

  #function to create the matrix of effect size and sample size
  create.matrix = function(i){
    efsize = dflist[[block()]][order()[i], 3]
    sampsize = dflist[[block()]][order()[i], 4]
    mat = matrix(c(efsize, sampsize), 1, 2)
    colnames(mat) = c("Effect Size", "Sample Size")
    return(mat)
  }

  #render all matrix tables
  output$table1 = renderTable(border = TRUE, width = "300px", {
    create.matrix(1)
  })

  output$table2 = renderTable(border = TRUE, width = "300px", {
    create.matrix(2)
  })

  output$table3 = renderTable(border = TRUE, width = "300px", {
    create.matrix(3)
  })

  output$table4 = renderTable(border = TRUE, width = "300px", {
    create.matrix(4)
  })


  #note: each choices variable shows p-values from least to greatest (as formatted
    #in csv file)
  #concatenate an empty string to choices so it is possible to use NULL as the
    #default value
  output$choices1 = renderUI({
    selectInput(
      inputId = "menu1",
      label = "P-Value",
      choices = c("", dflist[[block()]][, 5]), selected = NULL)
  })

  output$choices2 = renderUI({
    selectInput(
      inputId = "menu2",
      label = "P-Value",
      choices = c("", dflist[[block()]][, 5]), selected = NULL)
  })

  output$choices3 = renderUI({
    selectInput(
      inputId = "menu3",
      label = "P-Value",
      choices = c("", dflist[[block()]][, 5]), selected = NULL)
  })

  output$choices4 = renderUI({
    selectInput(
      inputId = "menu4",
      label = "P-Value",
      choices = c("", dflist[[block()]][, 5]), selected = NULL)
  })

  ##Check Answers##

  #qchoices is a vector of the current input value for each question
  #I probably should have just used a plain reactive variable instead: qchoices =
    #reactive({c(input$menu1, input$menu2, input$menu3, input$menu4)})
  qchoices = reactiveValues(qvec = c())

  observe({
    qchoices$qvec[1] = input$menu1
    qchoices$qvec[2] = input$menu2
    qchoices$qvec[3] = input$menu3
    qchoices$qvec[4] = input$menu4
  })

  check = function(i){
    if (!is.null(qchoices$qvec[i])) {
      if (qchoices$qvec[i] == dflist[[block()]][order()[i], 5]) {
        #CORRECT answer
        #return a neutral 0; we will later take a sum; 0 will not affect a sum;
          #we only perform the upcoming action if the sum is nonzero (at least one
          #incorrect answer)
        return(0)
      }
      else{
        if (qchoices$qvec[i] != "") {
          #INCORRECT answer
          #return the index (aka question #) so we have a way to know which questions
            #were answered incorrectly; incorrect return values will later be
            #concatenated into a vector
          return(i)
        }
      }
    }
  }

  #Prints out either 0 or the question number (index i)
  #This is just to check correct answers without pressing submit
  #Comment out in final version
  output$check1 = renderText({
    check(1)
  })

  output$check2 = renderText({
    check(2)
  })

  output$check3 = renderText({
    check(3)
  })

  output$check4 = renderText({
    check(4)
  })

  #reactive vector that holds the 0/index values (qchoices$qvec holds the INPUT values)
  answers = reactive({c(check(1), check(2), check(3), check(4))})

  observeEvent(input$submit, {
    #check if there are null values in the answers() vector (the user did not
      #select a p-value for each question)
    #we check this by seeing if the length of answers() is 4
    #if there are indeed unselected p-values, then answers() would concatenate
      #NULL values to the vector (which is basically concatenating nothing);
      #the length would not be 4

    nullvals = FALSE
    if (length(answers()) != 4) {
      nullvals = TRUE
    }

    #CASE 1: The user did not select a p-value for each question
    if (nullvals == TRUE) {
      output$feedback = renderUI({
        "Please select a p-value for each question."
      })
    }
    #CASE 2: All answers are correct (We don't need to worry that sum(answers())
      #== 0 is a vector of all NULL values because the previous if statement already
      #invalidates that possibility)
    else if (sum(answers()) == 0) {
      output$feedback = renderUI({
        paste0("CORRECT! Play again!")
        
      })
        #### Correct/Wrong symbols ----
        output$pic1 <- renderIcon(
          icon = ifelse(
            test = answers()[1] == 0,
            yes = "correct",
            no = "incorrect"
          )
        )
        
        output$pic2 <- renderIcon(
          icon = ifelse(
            test = answers()[2] == 2,
            yes = "incorrect",
            no = "correct"
          )
        )
        
        output$pic3 <- renderIcon(
          icon = ifelse(
            test = answers()[3] == 3,
            yes = "incorrect",
            no = "correct"
          )
        )
        
        output$pic4 <- renderIcon(
          icon = ifelse(
            test = answers()[4] == 4,
            yes = "incorrect",
            no = "correct"
          )
        )
      

      shinyjs::disable("submit")
      shinyjs::showElement("playagain")
      shinyjs::hideElement("tryagain")
    }
    #CASE 3: There is at least one incorrect answer
    else if (sum(answers()) != 0) {
      output$feedback = renderUI({
        #wrong will eventually be a vector stating which questions are incorrect
          #(we must remove 0s)
        wrong = c()
        for (i in 1:4) {
          if (answers()[i] != 0) {
            wrong = c(wrong, paste0("Question ", answers()[i], " is incorrect"))
          }
        }

        #Return the wrong vector with each element on a separate line
        if (length(wrong) == 1) {
          HTML(paste(wrong[1], sep = "<br/>"))
        }
        else if (length(wrong) == 2) {
          HTML(paste(wrong[1], wrong[2], sep = "<br/>"))
        }
        else if (length(wrong) == 3) {
          HTML(paste(wrong[1], wrong[2], wrong[3], sep = "<br/>"))
        }
        else if (length(wrong) == 4) {
          HTML(paste(wrong[1], wrong[2], wrong[3], wrong[4], sep = "<br/>"))
        }
        
        
      })

      #the Try Again button is re-rendered each time there is at least one incorrect
        #answer (with the default being that the button is enabled and not yet pressed)
      #therefore, it is not necessary to update the button to "disabled = FALSE"
      
      #### Correct/Wrong symbols ----
      output$pic1 <- renderIcon(
        icon = ifelse(
          test = answers()[1] == 0,
          yes = "correct",
          no = "incorrect"
        )
      )

      output$pic2 <- renderIcon(
        icon = ifelse(
          test = answers()[2] == 2,
          yes = "incorrect",
          no = "correct"
        )
      )

      output$pic3 <- renderIcon(
        icon = ifelse(
          test = answers()[3] == 3,
          yes = "incorrect",
          no = "correct"
        )
      )

      output$pic4 <- renderIcon(
        icon = ifelse(
          test = answers()[4] == 4,
          yes = "incorrect",
          no = "correct"
        )
      )

      output$showtry = renderUI({
        bsButton(
          inputId = "tryagain",
          HTML(paste("Try Again")),
          style = "default", 
          size = "large")
      })

      shinyjs::disable("submit")
    }

    #Re-show the elements that were hidden when playagain was pressed
    shinyjs::showElement("pic1")
    shinyjs::showElement("pic2")
    shinyjs::showElement("pic3")
    shinyjs::showElement("pic4")
  })


  observeEvent(input$tryagain, {
    shinyjs::enable("submit")

    #only reset the p-value drop down for the answers that are incorrect
    for (i in 1:4) {
      if (answers()[i] != 0) {
        reset(paste0("menu", i))
      }
    }

    shinyjs::disable("tryagain")

    #clear the previous feedback
    output$feedback = renderUI({
      " "
    })

    #hide the x for incorrect questions in which you want to try to answer again
    for (i in answers()) {
      if (i != 0) {
        shinyjs::hideElement(paste0("pic", i))
      }
    }
  })


  observeEvent(input$playagain, {
    shinyjs::enable("submit")

    output$feedback = renderUI({
      " "
    })

    shinyjs::hideElement("playagain")

    #When starting a new game, we must clear all the checks (the play again button
      #only appears when all questions are correct/all images of checks appear)
    shinyjs::hideElement("pic1")
    shinyjs::hideElement("pic2")
    shinyjs::hideElement("pic3")
    shinyjs::hideElement("pic4")
  })
  
  pval1 <- function(s){
    #x = input$ef_size, z = input$samp_size, s = zstat()
    #I actually invalidated this condition in the UI by starting the sample size at 10
    1 - pnorm(abs(s))
  } 

  ### Create Time Plot ----
  # pval1 <- function(x,z,m,v){
  #   1 - pnorm(x, z, mean = m, sd = sqrt(v))
  # }
  
  # Function to get p-value
  pValue <- function(x, n, sdCandy, sdNull){
    # x will be the difference in means
    # n will be the total sample size (assuming equal sizes of groups)
    # sdCandy will be sample standard deviation of the candy treatment group
    # sdNull will be the sample standard deviation of the null treatment group
    # Calculate Pooled SD
    sdPool <- sqrt(((n/2 - 1) * sdCandy^2 + (n/2 - 1) * sdNull^2)/(n - 2))
    # Calculate t
    tValue <- x / (sdPool * sqrt(4/n))
    return(1 - pt(q = tValue, df = (n - 2)))
  }
  
  # Constants
  # sample SDs from prompting article https://onlinelibrary-wiley-com.ezaccess.libraries.psu.edu/doi/epdf/10.1111/j.1559-1816.2002.tb00216.x
  sdCandy <- 3.06
  sdNull <- 1.89
  observeEvent(
    eventExpr = c(input$ef_size, pval1),
    handlerExpr = {
      output$ef_sizeGraph <- renderPlot(
        # Create plot
        ggplot() +
          xlim(-10, 20) + # Play with these limits-find the best to correspond with diff slider
          ylim(0, 1) +
          stat_function(
            fun = pValue,
            args = list(n = input$samp_size, sdCandy = sdCandy, sdNull = sdNull),
            color = "blue",
            size = 1
          ) +
          geom_point(
            mapping = aes(
              x = input$ef_size,
              y = pValue(
                x = input$ef_size,
                n = input$samp_size,
                sdCandy = sdCandy,
                sdNull = sdNull
              )
            ),
            color = "red",
            size = 3
          ) +
          geom_text( # Play around with this and see if we should add or drop this
            x = 15,
            y = 0.85,
            label = paste("Total Sample Size:", input$samp_size)
          ) +
          theme_bw() +
          xlab("Difference in Mean Tip Percentage") +
          ylab("P-value") +
          labs(
            title = "P-value vs. Actual Effect"
          ) +
          theme(
            text = element_text(size = 18) # See Chapter 13, Section 3 of Style Guide
          )

      )
  })
}

# Boast app call ----
boastUtils::boastApp(ui = ui, server = server)