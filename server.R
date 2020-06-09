library(shinyjs)
library(shiny)
library(shinyDND)
library(truncnorm)
library(V8)

jsResetCode <- "shinyjs.reset0 = function() {history.go(0)}"
#disable actionButton function
disableActionButton <- function(id,session) {
  session$sendCustomMessage(type="jsCode",
                            list(code= paste("$('#",id,"').prop('disabled',true)"
                                             ,sep="")))
}


shinyServer(function(input, output, session) {
  
  #move from Prerequisites to Overview
  observeEvent(input$next.page, {
    updateTabItems(session, "tabs", "overview")
  })
  
  #move from Overview to Part 1
  observeEvent(input$go, {
    updateTabItems(session, "tabs", "first")
  })

  #create reactive variables for the candy/no candy avg tip percentages
  avg.tip.c = reactive({(input$avgtipc)})
  avg.tip.nc = reactive({(input$avgtipnc)})
  
  #select 12 random tables to receive candy
  #create a reactive variable accessible throughout the server: samp$sample (vector of 12 randomly selected numbers ranging from 1-24)
  #disable the Randomly Assign Button after the first time it is clicked
  samp = reactiveValues(sample = 0)
  observeEvent(input$rand,({
    samp$sample = sample(1:24, 12)
    shinyjs::disable("rand")
  }))
  
  ##########################################################NOT NECESSARY####################################################################  
  #random assignment button: takes a sample of size 12 and assigns candy vs. no candy tables; disable button after pressed once
  #observeEvent({input$rand} ,({
  
  #  for(i in 1:24){
  #    if(i %in% samp$sample){
  #      #val = rtruncnorm(n = 1, a = 0, b = 100, mean = avg.tip.c())
  #      #updateTextInput(session, paste0("tabtip", i), value = as.character(round(val, 2)))
  #      updateTextInput(session, paste0("tabtip", i), value = "")
  
  #    }
  #    else{
  #      #val = rtruncnorm(n = 1, a = 0, b = 100, mean = avg.tip.nc())
  #      #updateTextInput(session, paste0("tabtip", i), value = as.character(round(val, 2)))
  #      updateTextInput(session, paste0("tabtip", i), value = "")
  #    }
  
  #  }
  
  #  shinyjs::disable("rand")
  
  #}))
  #############################################################################################################################################  
  
  
  #when the slider is moved for average tip percentage of tables receiving candy, generate a percentage from rtruncnorm for each table in the sample
  #since avg.tip.c() is reactive, whenever 'mean' changes, 'val' will change, and thus updateTextInput will change, executing each time the mean tip percent for candy tables is changed
  observeEvent({input$avgtipc}, {
    for(i in samp$sample){
      val = rtruncnorm(n = 1, a = 0, b = 100, mean = avg.tip.c(), sd = 1.75)
      
      #only assign random values if the Randomly Assign button has been pressed (this stops random values being generated in the text boxes when the app first launches)
      if(input$rand != 0){
        updateTextInput(session, paste0("tabtip", i), value = as.character(round(val, 2)))
      }
    }
    
  })
  
  #when the slider is moved for average tip percentage of tables receiving not candy, generate a percentage from rtruncnorm for each table NOT in the sample
  observeEvent({input$avgtipnc}, {
    for(i in 1:24){
      if(!(i %in% samp$sample)){
        val = rtruncnorm(n = 1, a = 0, b = 100, mean = avg.tip.nc(), sd = 1.75)
        
        #only assign random values if the Randomly Assign button has been pressed (this stops random values being generated in the text boxes when the app first launches)
        if(input$rand != 0){
          updateTextInput(session, paste0("tabtip", i), value = as.character(round(val, 2)))
        }
      }
    }
    
  })
  
  #reset button
  observeEvent(input$reset_button,
               {js$reset0()})
  
  
  
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
    for(i in 1:24){
      if(i %in% samp$sample){
        sum.c = sum.c + tips$tip.list[i]
      }
    }
    round(sum.c/12, 2)
  })
  
  #average tip percent for tables not receiving candy (reactive variable)
  avg.nc = reactive({
    sum.nc = 0
    for(i in 1:24){
      if(!(i %in% samp$sample)){
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
    for(i in 1:24){
      if(i %in% samp$sample){
        vec.c = c(vec.c, tips$tip.list[i])
      }
    }
    sd(vec.c)
  })
  
  #standard deviation of tips for tables without candy (reactive variable)
  sd.nc = reactive({
    vec.nc = c()
    for(i in 1:24){
      if(!(i %in% samp$sample)){
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
    pval = 1-pnorm(abs(z.stat()),lower.tail = FALSE)
    rpval = round(pval, 4)
    
    #must use ifelse function rather than if and else statements to avoid logical input error
    ifelse(rpval == 0, "approximately 0", rpval)
  })
    
  #render HTML; call avg.c()
  output$average.c = renderUI({
    HTML(paste0("The average tip for the 12 tables receiving candy in our sample is: ", tags$b(avg.c()), tags$b(" %"), "."))
  })
  
  #render HTML, call avg.nc()
  output$average.nc = renderUI({
    HTML(paste0("The average tip for the 12 tables not receiving candy in our sample is: ", tags$b(avg.nc()), tags$b(" %"),"."))
  })
  
  #render HTML, call effect()
  output$effect.size = renderUI({
    HTML(paste0("The effect size is: ", tags$b(avg.c()), tags$b(" % - "), tags$b(avg.nc()), tags$b(" % = "), tags$b(effect()), tags$b( " %"), "."))
  })
  
  #render HTML, call test.stat()
  output$test.stat = renderUI({
    HTML(paste0("The test statistic is: ", tags$b("T = "), tags$b(z.stat()), "."))
  })
  
  #render HTML, call p.val()
  output$p_value = renderUI({
    HTML(paste0("The p-value is: ", tags$b(p.val()), "."))
  })
  
  ####################################################
  #           For rendering the 24 images            #
  ####################################################
  output$img1 = renderUI({
    if(1 %in% samp$sample){
      tags$img(src = 'tablecandy.PNG', width = 70, height = 60)
    }
    else{
      tags$img(src = 'dinnerTable.jpg', width = 70, height = 60)
    }
  })
  
  output$img2 = renderUI({
    if(2 %in% samp$sample){
      tags$img(src = 'tablecandy.PNG', width = 70, height = 60)
    }
    else{
      tags$img(src = 'dinnerTable.jpg', width = 70, height = 60)
    }
  })
  
  output$img3 = renderUI({
    if(3 %in% samp$sample){
      tags$img(src = 'tablecandy.PNG', width = 70, height = 60)
    }
    else{
      tags$img(src = 'dinnerTable.jpg', width = 70, height = 60)
    }
  })
  
  output$img4 = renderUI({
    if(4 %in% samp$sample){
      tags$img(src = 'tablecandy.PNG', width = 70, height = 60)
    }
    else{
      tags$img(src = 'dinnerTable.jpg', width = 70, height = 60)
    }
  })
  
  output$img5 = renderUI({
    if(5 %in% samp$sample){
      tags$img(src = 'tablecandy.PNG', width = 70, height = 60)
    }
    else{
      tags$img(src = 'dinnerTable.jpg', width = 70, height = 60)
    }
  })
  
  output$img6 = renderUI({
    if(6 %in% samp$sample){
      tags$img(src = 'tablecandy.PNG', width = 70, height = 60)
    }
    else{
      tags$img(src = 'dinnerTable.jpg', width = 70, height = 60)
    }
  })
  
  output$img7 = renderUI({
    if(7 %in% samp$sample){
      tags$img(src = 'tablecandy.PNG', width = 70, height = 60)
    }
    else{
      tags$img(src = 'dinnerTable.jpg', width = 70, height = 60)
    }
  })
  
  output$img8 = renderUI({
    if(8 %in% samp$sample){
      tags$img(src = 'tablecandy.PNG', width = 70, height = 60)
    }
    else{
      tags$img(src = 'dinnerTable.jpg', width = 70, height = 60)
    }
  })
  
  output$img9 = renderUI({
    if(9 %in% samp$sample){
      tags$img(src = 'tablecandy.PNG', width = 70, height = 60)
    }
    else{
      tags$img(src = 'dinnerTable.jpg', width = 70, height = 60)
    }
  })
  
  output$img10 = renderUI({
    if(10 %in% samp$sample){
      tags$img(src = 'tablecandy.PNG', width = 70, height = 60)
    }
    else{
      tags$img(src = 'dinnerTable.jpg', width = 70, height = 60)
    }
  })
  
  output$img11 = renderUI({
    if(11 %in% samp$sample){
      tags$img(src = 'tablecandy.PNG', width = 70, height = 60)
    }
    else{
      tags$img(src = 'dinnerTable.jpg', width = 70, height = 60)
    }
  })
  
  output$img12 = renderUI({
    if(12 %in% samp$sample){
      tags$img(src = 'tablecandy.PNG', width = 70, height = 60)
    }
    else{
      tags$img(src = 'dinnerTable.jpg', width = 70, height = 60)
    }
  })
  
  output$img13 = renderUI({
    if(13 %in% samp$sample){
      tags$img(src = 'tablecandy.PNG', width = 70, height = 60)
    }
    else{
      tags$img(src = 'dinnerTable.jpg', width = 70, height = 60)
    }
  })
  
  output$img14 = renderUI({
    if(14 %in% samp$sample){
      tags$img(src = 'tablecandy.PNG', width = 70, height = 60)
    }
    else{
      tags$img(src = 'dinnerTable.jpg', width = 70, height = 60)
    }
  })
  
  output$img15 = renderUI({
    if(15 %in% samp$sample){
      tags$img(src = 'tablecandy.PNG', width = 70, height = 60)
    }
    else{
      tags$img(src = 'dinnerTable.jpg', width = 70, height = 60)
    }
  })
  
  output$img16 = renderUI({
    if(16 %in% samp$sample){
      tags$img(src = 'tablecandy.PNG', width = 70, height = 60)
    }
    else{
      tags$img(src = 'dinnerTable.jpg', width = 70, height = 60)
    }
  })
  
  output$img17 = renderUI({
    if(17 %in% samp$sample){
      tags$img(src = 'tablecandy.PNG', width = 70, height = 60)
    }
    else{
      tags$img(src = 'dinnerTable.jpg', width = 70, height = 60)
    }
  })
  
  output$img18 = renderUI({
    if(18 %in% samp$sample){
      tags$img(src = 'tablecandy.PNG', width = 70, height = 60)
    }
    else{
      tags$img(src = 'dinnerTable.jpg', width = 70, height = 60)
    }
  })
  
  output$img19 = renderUI({
    if(19 %in% samp$sample){
      tags$img(src = 'tablecandy.PNG', width = 70, height = 60)
    }
    else{
      tags$img(src = 'dinnerTable.jpg', width = 70, height = 60)
    }
  })
  
  output$img20 = renderUI({
    if(20 %in% samp$sample){
      tags$img(src = 'tablecandy.PNG', width = 70, height = 60)
    }
    else{
      tags$img(src = 'dinnerTable.jpg', width = 70, height = 60)
    }
  })
  
  output$img21 = renderUI({
    if(21 %in% samp$sample){
      tags$img(src = 'tablecandy.PNG', width = 70, height = 60)
    }
    else{
      tags$img(src = 'dinnerTable.jpg', width = 70, height = 60)
    }
  })
  
  output$img22 = renderUI({
    if(22 %in% samp$sample){
      tags$img(src = 'tablecandy.PNG', width = 70, height = 60)
    }
    else{
      tags$img(src = 'dinnerTable.jpg', width = 70, height = 60)
    }
  })
  
  output$img23 = renderUI({
    if(23 %in% samp$sample){
      tags$img(src = 'tablecandy.PNG', width = 70, height = 60)
    }
    else{
      tags$img(src = 'dinnerTable.jpg', width = 70, height = 60)
    }
  })
  
  output$img24 = renderUI({
    if(24 %in% samp$sample){
      tags$img(src = 'tablecandy.PNG', width = 70, height = 60)
    }
    else{
      tags$img(src = 'dinnerTable.jpg', width = 70, height = 60)
    }
  })
  #################################################
  
  
  
  #####################################################
  ##################P-VALUE TAB########################
  #####################################################
  
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
  #note: could have passed this to "p.val" but we will create a new reactive p-value variable for clarity
  pval = reactive({
    #I actually invalidated this condition in the UI by starting the sample size at 10
    if((esize() == 0) | (sampsize() == 0)){
      return("0.5")
    }
    else{
      pval = 1-pnorm(abs(zstat()))
      rpval = round(pval, 4)
      return(ifelse(rpval == 0, "approximately 0", rpval))
    }
    
  })
  
  #render HTML; call pval()
  output$pvalue = renderUI({
    HTML(paste0("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;The p-value is: ", tags$b(pval()), "."))
  })
  
  #move from Determining P-Values to Test Your Understanding
  observeEvent(input$totest, {
    updateTabsetPanel(session, "tabs2", selected = "matching")
  })
  
  #####################################################
  ##################MATCHING TAB#######################
  #####################################################
  
  #the Play Again button should be hidden when the app is first launched
  shinyjs::hideElement("playagain")
  
  
  #set working directory to source file location
  data = read.csv("P-Value Question Bank.csv", header = TRUE)
  
  #split entire data frame into 5* blocks
  dflist = split(data, data$Block)
  
  #choose a random block (of the 5*)
  #This is weird**: We need to render all tables and p-value input dropdowns when the app first starts. (I don't want outputs to be rendered on some condition like pressing a button)
  #                 The tables/p-value dropdowns will only be rendered if block (and order) are given values.
  #                 If the condition does not evaluate to TRUE when the app first starts, no value for block (or order) will even be assigned.
  #                 So, to get the condition to evaluate to TRUE by default (when the app first starts), we use the idea that (anything | TRUE) is always TRUE.
  #                 Thus, we can get a value for block (and order) even though input$playagain has not been pressed.
  #                 By using {input$playagain | TRUE}, block will recalculate only when the "playagain" button is pressed.
  #
  #                 Note that we cannot just merely use {TRUE} because we must establish a dependency on input$playagain for the recalculation of block.
  #                 Also note that we cannot just merely use {input$playagain} because then no value for block (or order) will be assigned when the app first starts (and resultingly, no outputs will be rendered)
  #
  #                 We use ideas like this when we have reactive variables that need to change based on conditions (like the press of a button) other than user input values.
  block = eventReactive({input$playagain | TRUE},
                   {sample(1:(nrow(data)/4), 1)})
  #we want sample(1:5, 1) for this case if there are 5* blocks
 
  
  #create a random order of the 4 questions in each block
  #see the note above for why we use {input$playagain | TRUE}
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
  
  
  #note: each choices variable shows p-values from least to greatest (as formatted in csv file)
  #concatenate an empty string to choices so it is possible to use NULL as the default value
  output$choices1 = renderUI({
    selectInput(inputId = "menu1", label = "P-Value", choices = c("", dflist[[block()]][, 5]), selected = NULL)
  })
  
  output$choices2 = renderUI({
    selectInput(inputId = "menu2", label = "P-Value", choices = c("", dflist[[block()]][, 5]), selected = NULL)
  })
  
  output$choices3 = renderUI({
    selectInput(inputId = "menu3", label = "P-Value", choices = c("", dflist[[block()]][, 5]), selected = NULL)
  })
  
  output$choices4 = renderUI({
    selectInput(inputId = "menu4", label = "P-Value", choices = c("", dflist[[block()]][, 5]), selected = NULL)
  })
  
  #print(block())
  #print(order())
  #print(input$playagain)
  #print(input$playagain)
  
  
  ##Check Answers##
  
  #qchoices is a vector of the current input value for each question
  #I probably should have just used a plain reactive variable instead: qchoices = reactive({c(input$menu1, input$menu2, input$menu3, input$menu4)})
  qchoices = reactiveValues(qvec = c())
  
  observe({
    qchoices$qvec[1] = input$menu1
    qchoices$qvec[2] = input$menu2
    qchoices$qvec[3] = input$menu3
    qchoices$qvec[4] = input$menu4
  })
  
  #function to check answer for each question
  #note: the redundant (!is.null) and (!= "") is necessary because a NULL value used with logical operators produces an argument of length 0 (error)
  #      if(!is.null(qchoices$qvec[i])){} ensures that an argument of length 0 is not passed to the != or == logical operator (ignores empty default input value; ensures it does not get passed through other functions but does not get rid of)
  #      if(qchoices$qvec[i] != ""){} ensures that the empty default input value does not get assigned a value in the summing process
  
  #NOTE: This is an example full function that is written for checking the correct p-value of Question 1
  # check = function(){
  #   if(!is.null(input$menu1)){
  #     if(input$menu1 == as.character(dflist[[block]][order[1], 5])){
  #       "CORRECT"
  #     }
  #     else{
  #       if(input$menu1 != ""){
  #         "INCORRECT"
  #       }
  #     }
  #   }
  # }  
  check = function(i){
    if(!is.null(qchoices$qvec[i])){
      if(qchoices$qvec[i] == dflist[[block()]][order()[i], 5]){
        #CORRECT answer
        #return a neutral 0; we will later take a sum; 0 will not affect a sum; we only perform the upcoming action if the sum is nonzero (at least one incorrect answer)
        return(0)
      }
      else{
        if(qchoices$qvec[i] != ""){
          #INCORRECT answer
          #return the index (aka question #) so we have a way to know which questions were answered incorrectly; incorrect return values will later be concatenated into a vector
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
  
  
  ##TRACE STATEMENT##
  # observeEvent(input$submit, {
  #   print(answers())
  # })
  

  observeEvent(input$submit, {
    ###I think using the method below instead of the validate() function is better: this way we avoid a missing argument where TRUE/FALSE is needed error###
    # output$feedback = renderUI({
    #   validate(need(input$menu1 != "" & input$menu2 != "" & input$menu3 != "" & input$menu4 != "", message = "Please select a p-value for each question."))
    # })
    
    #check if there are null values in the answers() vector (the user did not select a p-value for each question)
    #we check this by seeing if the length of answers() is 4
    #if there are indeed unselected p-values, then answers() would concatenate NULL values to the vector (which is basically concatenating nothing); the length would not be 4
    
    nullvals = FALSE
    if(length(answers()) != 4){
      nullvals = TRUE
    }

    #CASE 1: The user did not select a p-value for each question
    if(nullvals == TRUE){
      output$feedback = renderUI({
        "Please select a p-value for each question."
      })
    }
    #CASE 2: All answers are correct (We don't need to worry that sum(answers()) == 0 is a vector of all NULL values because the previous if statement already invalidates that possibility)
    else if(sum(answers()) == 0){
      output$feedback = renderUI({
        "CORRECT"
      })

      shinyjs::disable("submit")
      shinyjs::showElement("playagain")
      shinyjs::hideElement("tryagain")
    }
    #CASE 3: There is at least one incorrect answer
    else if (sum(answers()) != 0){
      output$feedback = renderUI({
        #wrong will eventually be a vector stating which questions are incorrect (we must remove 0s)
        wrong = c()
        for(i in 1:4){
          if(answers()[i] != 0){
            wrong = c(wrong, paste0("Question ", answers()[i], " is incorrect"))
          }
        }

        #Return the wrong vector with each element on a separate line
        if(length(wrong) == 1){
          HTML(paste(wrong[1], sep = "<br/>"))
        }
        else if(length(wrong) == 2){
          HTML(paste(wrong[1], wrong[2], sep = "<br/>"))
        }
        else if(length(wrong) == 3){
          HTML(paste(wrong[1], wrong[2], wrong[3], sep = "<br/>"))
        }
        else if(length(wrong) == 4){
          HTML(paste(wrong[1], wrong[2], wrong[3], wrong[4], sep = "<br/>"))
        }
      })

      #the Try Again button is re-rendered each time there is at least one incorrect answer (with the default being that the button is enabled and not yet pressed)
      #therefore, it is not necessary to update the button to "disabled = FALSE"
      output$showtry = renderUI({
        actionButton(inputId = "tryagain", HTML(paste(tags$b("Try Again"))), style = "color: #fff; background-color: #074467", width = 200)
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
    for(i in 1:4){
      if(answers()[i] != 0){
        reset(paste0("menu", i))
      }
    }
    
    shinyjs::disable("tryagain")
    
    #clear the previous feedback
    output$feedback = renderUI({
      " "
    })
    
    #hide the x for incorrect questions in which you want to try to answer again
    for(i in answers()){
      if(i != 0){
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
    
    #When starting a new game, we must clear all the checks (the play again button only appears when all questions are correct/all images of checks appear)
    shinyjs::hideElement("pic1")
    shinyjs::hideElement("pic2")
    shinyjs::hideElement("pic3")
    shinyjs::hideElement("pic4")
  })
  
  
  
  #Render pic1
  #isolate the function so that it only recalculates (pictures are only re-rendered) when the submit button is pressed
  #if all questions have user input, then render the proper images, else don't render any images at all
  output$pic1 = renderUI({
    input$submit
    isolate(
      if(length(answers()) == 4){
        if(answers()[1] == 0){
          tags$img(src = "check.png", width = 90)
        }
        else if(answers()[1] == 1){
          tags$img(src = "x.png", width = 90)
        }
      }
    )
  })

  #Render pic2
  output$pic2 = renderUI({
    input$submit
    isolate(
      if(length(answers()) == 4){
        if(answers()[2] == 0){
          tags$img(src = "check.png", width = 90)
        }
        else if(answers()[2] == 2){
          tags$img(src = "x.png", width = 90)
        }
      }
    )
  })

  #Render pic3
  output$pic3 = renderUI({
    input$submit
    isolate(
      if(length(answers()) == 4){
        if(answers()[3] == 0){
          tags$img(src = "check.png", width = 90)
        }
        else if(answers()[3] == 3){
          tags$img(src = "x.png", width = 90)
        }
      }
    )
  })

  
  #Render pic4
  output$pic4 = renderUI({
    input$submit
    isolate(
      if(length(answers()) == 4){
        if(answers()[4] == 0){
          tags$img(src = "check.png", width = 90)
        }
        else if(answers()[4] == 4){
          tags$img(src = "x.png", width = 90)
        }
      }
    )
  })
  
 
  
  # #The first way is probably better than this one
  # #the tryCatch basically tries to perform the function inside {}, then returns "" (nothing) for the error
  # #This way goes through SO many if/else statements to render the proper image for each possible combination of inputs
  # output$pic1 = renderUI({
  #   input$submit
  #   isolate(
  #     tryCatch({
  # 
  #     if(is.null(qchoices$qvec)){
  #       tags$img(src = "white.png", width = 90)
  #     }
  #     else if(length(answers()) == 4 & answers()[1] == 0){
  #       tags$img(src = "check.png", width = 90)
  #     }
  #     else if(length(answers()) == 4 & answers()[1] == 1){
  #       tags$img(src = "x.png", width = 90)
  #     }
  #     else if(length(answers()) != 4 & input$menu1 == ""){
  #       tags$img(src = "white.png", width = 90)
  #       #may need to re-enable tryagain
  #     }
  #     else if(length(answers()) != 4 & input$menu1 != dflist[[block()]][order()[1], 5]){
  #       tags$img(src = "white.png", width = 90)
  #       #maybe change to white.png?
  #     }
  #     else if(length(answers()) != 4 & input$menu1 == dflist[[block()]][order()[1], 5] & input$tryagain != 0){
  #       tags$img(src = "white.png", width = 90)
  #     }
  #     else if(length(answers()) != 4 & input$menu1 == dflist[[block()]][order()[1], 5]){
  #       tags$img(src = "white.png", width = 90)
  #     }
  # 
  #     }, error=function(e) return(""))
  #   )
  # })
  # 
  # output$pic2 = renderUI({
  #   input$submit
  #   isolate(
  #     tryCatch({
  # 
  #     if(is.null(qchoices$qvec)){
  #       tags$img(src = "white.png", width = 90)
  #     }
  #     else if(length(answers()) == 4 & answers()[2] == 0){
  #       tags$img(src = "check.png", width = 90)
  #     }
  #     else if(length(answers()) == 4 & answers()[2] == 2){
  #       tags$img(src = "x.png", width = 90)
  #     }
  #     else if(length(answers()) != 4 & input$menu2 == ""){
  #       tags$img(src = "white.png", width = 90)
  #       #may need to re-enable tryagain
  #     }
  #     else if(length(answers()) != 4 & input$menu2 != dflist[[block()]][order()[2], 5]){
  #       tags$img(src = "white.png", width = 90)
  #       #maybe change to white.png?
  #     }
  #     else if(length(answers()) != 4 & input$menu2 == dflist[[block()]][order()[2], 5] & input$tryagain != 0){
  #       tags$img(src = "white.png", width = 90)
  #     }
  #     else if(length(answers()) != 4 & input$menu2 == dflist[[block()]][order()[2], 5]){
  #       tags$img(src = "check.png", width = 90)
  #     }
  # 
  #     }, error=function(e) return(""))
  #   )
  # })
  # 
  # output$pic3 = renderUI({
  #   input$submit
  #   isolate(
  #     tryCatch({
  # 
  #     if(is.null(qchoices$qvec)){
  #       tags$img(src = "white.png", width = 90)
  #     }
  #     else if(length(answers()) == 4 & answers()[3] == 0){
  #       tags$img(src = "check.png", width = 90)
  #     }
  #     else if(length(answers()) == 4 & answers()[3] == 3){
  #       tags$img(src = "x.png", width = 90)
  #     }
  #     else if(length(answers()) != 4 & input$menu3 == ""){
  #       tags$img(src = "white.png", width = 90)
  #       #may need to re-enable tryagain
  #     }
  #     else if(length(answers()) != 4 & input$menu3 != dflist[[block()]][order()[3], 5]){
  #       tags$img(src = "white.png", width = 90)
  #       #maybe change to white.png?
  #     }
  #     else if(length(answers()) != 4 & input$menu3 == dflist[[block()]][order()[3], 5] & input$tryagain != 0){
  #       tags$img(src = "white.png", width = 90)
  #     }
  #     else if(length(answers()) != 4 & input$menu3 == dflist[[block()]][order()[3], 5]){
  #       tags$img(src = "check.png", width = 90)
  #     }
  # 
  #     }, error=function(e) return(""))
  #   )
  # })
  # 
  # output$pic4 = renderUI({
  #   input$submit
  #   isolate(
  #     tryCatch({
  # 
  #     if(is.null(qchoices$qvec)){
  #       tags$img(src = "white.png", width = 90)
  #     }
  #     else if(length(answers()) == 4 & answers()[4] == 0){
  #       tags$img(src = "check.png", width = 90)
  #     }
  #     else if(length(answers()) == 4 & answers()[4] == 4){
  #       tags$img(src = "x.png", width = 90)
  #     }
  #     else if(length(answers()) != 4 & input$menu4 == ""){
  #       tags$img(src = "white.png", width = 90)
  #       #may need to re-enable tryagain
  #     }
  #     else if(length(answers()) != 4 & input$menu4 != dflist[[block()]][order()[4], 5]){
  #       tags$img(src = "white.png", width = 90)
  #       #maybe change to white.png?
  #     }
  #     else if(length(answers()) != 4 & input$menu4 == dflist[[block()]][order()[4], 5] & input$tryagain != 0){
  #       tags$img(src = "white.png", width = 90)
  #     }
  #     else if(length(answers()) != 4 & input$menu4 == dflist[[block()]][order()[4], 5]){
  #       tags$img(src = "check.png", width = 90)
  #     }
  # 
  #     }, error=function(e) return(""))
  #   )
  # })
  
})