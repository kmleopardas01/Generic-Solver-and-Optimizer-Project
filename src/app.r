library(shiny)
library(rhandsontable)
library("shinythemes")


source("QSI.r")
source("SimplexProject.r")
source("PolynomialRegression.r")
source("GaussJordanMethod.r")
source("GaussJordanMethodMod1.r")



ui <- fluidPage(theme = shinytheme("slate"),
  
  titlePanel( #title panel
    h1 ("PROJECT IN CMSC150", align ="center"),
  ),
  tabsetPanel(type="tabs",
              
              # TAB PANEL FOR POLYNOMIAL REGRESSION
              tabPanel("Polynomial Regression",
                       # INPUT (PR)
                       sidebarPanel(
                         tags$hr(),
                         fileInput('datafile', 'Choose file to upload',
                                   accept = c(
                                     'text/csv',
                                     'text/comma-separated-values',
                                     '.csv'
                                   )
                         ),
                         tags$hr(), #gets the input for the Polynomial Regression
                         numericInput("degreeVal", 
                                      "Enter degreeVal:", 
                                      value = 1),
                         numericInput("estimate", 
                                      "Estimate [N]:", 
                                      value = 1),
                         checkboxInput("check1_PolyReg", "Show results", FALSE) #checkbox that will show and unshow the results
                         
                       ),
                       # main panel 
                       mainPanel(
                         br(),
                         div(align = "left", #prints the data table to the left
                             tableOutput("RegressionTable")
                             ),
                         
                         br(),
                         
                         tableOutput("Generated_PolynomialEQ"), #polynomial equations
                         tableOutput("PolyReg_estimate"), #estimated value
                         br(),
                         tableOutput("Show_PolyRegMatrix") #sshows the final polynomial regression matrix
                         
                         
                         
                       )
              ),
              
              # TAB PANEL FOR QSI
              tabPanel("Quadractic Spline Interpolation", 
                       
                       # INPUT (QSI)
                       sidebarPanel(
                         tags$hr(), #accepts a csv
                         fileInput('QSI_csv', 'Choose file to upload',
                                   accept = c(
                                     'text/csv',
                                     'text/comma-separated-values',
                                     '.csv'
                                   )
                         ),
                         tags$hr(),
                         #all inputs needed for QSI
                         numericInput("QSI_input", 
                                      "Estimate [N]:", 
                                      value = 5),
                         checkboxInput("check2_Estimated", "Show Interpolated Value", FALSE),
                         checkboxInput("check2_Intervals", "Show Interval Equations", FALSE),
                         checkboxInput("check2_QSImatrix", "Show Final Matrix", FALSE)
                         
                         
                       ),
                       
                       # OUTPUT (QSI)
                       mainPanel(
                         br(),
                         
                         tableOutput("QSITable"), # data table
                         tableOutput("QSIValue"), #interpolated value
                         tableOutput("QSIEquations"), #interval equations
                         tableOutput("QSImatrix") #final resulting matrix
                         
                       )
                       
              ),
              
              # TAB PANEL FOR SIMPLEX
              tabPanel("Simplex Implementation",
                       sidebarPanel( 
                         tags$hr(),
                        #inputs needed for simplex
                         numericInput("checkIter", "Current Iteration at [N]: ", value=1, width = "300" ),
                         actionButton("Min", "Show Minimum Cost", icon = icon("microchip")),
                         actionButton("showSimplexMat", "Show Tableaus  Per Iteration", icon = icon("table")),
                         actionButton("finalTab", "Final Tableau/Matrix", icon = icon("table"))
                        
                        
                         
                       ),
                       mainPanel(
                         br(), # the two essential matrices: suppy and demand matrix
                         rHandsontableOutput("Matrix_Demand"),
                         br(),
                         rHandsontableOutput("Matrix_Suppy"), 
                        
                         br(),
                         tableOutput("Minimumcost"), #minimum cost
                         br(),
                         rHandsontableOutput("Tableaus"), #tableau per iteration
                         br(),
                         rHandsontableOutput("showFinalTableau")  #final tableau
                         
                        
                       ),
              )

  )
  
)
###########################################################################################################################################################################################3
#These are the initial values for the handsontable in the simplex method implementation

Matrix_Demand = matrix(c(431,332,350,450,400), nrow = 1, ncol=5, byrow=TRUE, dimnames = list(c("DEM:"),c("W1","W2","W3","W4","W5")))
Matrix_Supply = matrix(c(1400,30,29,31,35,33,400,26,24,23,25,27,200,11,13,15,20,17), nrow=3,ncol=6,byrow=TRUE, dimnames = list(c("P1","P2","P3"),c("SUPPLY","W1","W2","W3","W4","W5")))
server <- function(input,output){
 
###########################################################################################################################################################################################  
  
#######################################################################################################################################################3
 #POLYNOMIAL REGRESSION OUTPUTS
  
  #outputs the data table of the polynomial regression method
  output$RegressionTable <- renderTable(hover = TRUE, spacing = c("m"), width = "50", {
    req(input$datafile)
    values = read.table(input$datafile$datapath, header=TRUE, sep=",")
    values
    
    
  })
  #outputs the generated polynomial equation
   output$Generated_PolynomialEQ <-  renderTable(hover = TRUE, spacing = c("m"), width = "560", align = "c",{
   #all requirements for this output
    req(input$datafile)
    req(input$degreeVal)
    req(input$estimate)
    
    #initialization of values
    estimate = input$estimate
    degreeVal = input$degreeVal
   
    if (is.null(input$datafile) && is.null(degreeVal)) return (NULL)
    
    #reads the value from the input csv and calls the Polynomial Regression Method
    values = read.table(input$datafile$datapath, header = TRUE, sep = ",")
    PolyReg <- PolynomialRegression(degreeVal,values[,1], values[,2], estimate)
    if(is.null(PolyReg)){
      return()
    }
    #final polynomial equation
    finalEquation = matrix(nrow = 1, ncol=1, byrow = TRUE, dimnames= list(c(),c("Generated Polynomial Function")))
    finalEquation[1,1] = PolyReg$polynomialString

    
    if (input$check1_PolyReg) return(finalEquation) #if the checkbox is clicked, ouputs the final polynomial equation
    else return(NULL)
    
    
   })
   
   #this outputs the estimated valut using the method
   output$PolyReg_estimate <-  renderTable(hover = TRUE, spacing = c("m"), width = "560", align = "c",{
     
     #all the requirements
     req(input$datafile)
     req(input$degreeVal)
     req(input$estimate)
     
     estimate = input$estimate
     degreeVal = input$degreeVal
    
     if (is.null(input$datafile) && is.null(degreeVal)) return (NULL)
     
     #reading from the scv and calling the method
     values = read.table(input$datafile$datapath, header = TRUE, sep = ",")
     PolyReg <- PolynomialRegression(degreeVal,values[,1], values[,2], estimate)
     
     if(is.null(PolyReg) == TRUE){
       return()
     }
     
     #final matrix for the estimated value
     finalVal = matrix(nrow = 1, ncol=1, byrow = TRUE, dimnames= list(c(),c("Result After Polynomial Regression")))
     finalVal[1,1] = PolyReg$estimate
     
     
     if (input$check1_PolyReg) return(finalVal) #if the checkbox is clicked, this outputs the finalVal
     else return(NULL)
     
     
   })
   
   #outputs the polynomial regression matrix
   output$Show_PolyRegMatrix <-  renderTable(hover = TRUE, spacing = c("m"), width = "560", align = "c", colnames = TRUE,{
     
     req(input$datafile)
     req(input$degreeVal)
     req(input$estimate)
     
     estimate = input$estimate
     
     degreeVal = input$degreeVal
    
     if (is.null(input$datafile) && is.null(degreeVal)) return (NULL)
     #does the method
     values = read.table(input$datafile$datapath, header = TRUE, sep = ",")
     PolyReg <- PolynomialRegression(degreeVal,values[,1], values[,2], estimate)
    if(is.null(PolyReg) == TRUE){
      return()
    }
     
     finalMat = PolyReg$augcoeffmatrix
     
     #if the checkbox is clicked, returns the final matrix
     if (input$check1_PolyReg) return(finalMat)
     else return(NULL)
     
     
   })
   
########################################################################################################################################  
  
  # QUADRATIC SPLINE INTERPOLATION METHOD OUTPUTS
  
   #outputs the data table of the QSI from the csv
  output$QSITable <- renderTable({
    req(input$QSI_csv)
    values = read.table(input$QSI_csv$datapath, header = TRUE, sep =",")
    values
    print(values)
    
    
  })
   #outputs the interpolated value and from what interval is was derived from
   output$QSIValue <- renderTable({
     req(input$QSI_csv)
     req(input$QSI_input)
     
     if (is.null(input$QSI_csv) && is.null(input$QSI_input)) return (NULL)
     
     QSI_input = input$QSI_input
     
     values = read.table(input$QSI_csv$datapath, header = TRUE, sep = ",")
     if(is.null(toQSI <- QSI(values[,1], values[,2], QSI_input))){
       print("NULL")
       return()
     }
     
     #initialized a answer table that will contain both the interpolated value and the interval it was derived from
     AnswerTable = matrix(nrow=1, ncol=2, byrow = TRUE, dimnames = list(c(),c("QSI Estimated Value", "Under f(x) Interval")))
     
     AnswerTable[1,1] = toQSI$QSI_Value
     AnswerTable[1,2] = toQSI$IntervalEquations[[toQSI$IntervalIndex]]
     
     if (input$check2_Estimated) return(AnswerTable) #if the checkbox is clicked, shows the answer table
     else return (NULL)
   })
   
   #outputs all the interval equations
  output$QSIEquations <- renderTable({
    req(input$QSI_csv)
    req(input$QSI_input)
    
    
    if (is.null(input$QSI_csv) && is.null(input$QSI_input)) return (NULL)
    #reads the csv
    values = read.table(input$QSI_csv$datapath, header = TRUE, sep = ",")
    
    toQSIval = input$QSI_input
    toQSI <- QSI(values[,1],values[,2],toQSIval)
    if(is.na(toQSI)){
      print("TRUE")
      return(output)
    }

    #initialized a matrix that wil contain all the interval equations
    answerMatrix = matrix(nrow = length(toQSI$IntervalEquations), ncol = 2, dimnames=list(c(), c("Interval", "Function")))
    
    #for loop that populated the matrix
    for( i in 1:nrow(answerMatrix)){
      leftBoundary = values[i,1] #left boundary for the interval
      rightBoundary = values[i+1,1] #right boundary for the interval
      answerMatrix[i,1] = paste(leftBoundary,"<=", "x", "<=", rightBoundary)
      answerMatrix[i,2] = toQSI$IntervalEquations[[i]]
      
    }
    
    if (input$check2_Intervals) return(answerMatrix) #if the checkbox is checked, outputs the interval equations
    else return(NULL)
  })
  
  #outpupts the final QSI matrix
  output$QSImatrix <- renderTable({
    req(input$QSI_csv)
    req(input$QSI_input)
    
    
    if (is.null(input$QSI_csv) && is.null(input$QSI_input)) return (NULL)
    values = read.table(input$QSI_csv$datapath, header = TRUE, sep = ",")
    
    toQSIval = input$QSI_input
    print(input$QSI_input)
    toQSI <- QSI(values[,1],values[,2],toQSIval)
    #if the checkbox is clicked, outputs the matrix
    if(input$check2_QSImatrix) return(toQSI$Matrix)
    else return(NULL)
  })
  
  
  
  
  
########################################################################################################################################  
  
  #SIMPEX METHOD OUTPUTS
  
  #initialized the matrix demand for later use
  output$Matrix_Demand <- renderRHandsontable({
    rhandsontable(Matrix_Demand, width = 3500)
  })
  #initialized the matrix suppy for later use
  output$Matrix_Suppy <- renderRHandsontable({
    rhandsontable(Matrix_Supply, width = 2000)
  })
  
  
  #event reactive call that does the simplex method
  finalTab <- eventReactive(input$finalTab, {
    Matrix_Demand <- as.matrix(hot_to_r(input$Matrix_Demand))
    Matrix_Supply <- as.matrix(hot_to_r(input$Matrix_Suppy))
  #simplexMat sotres the list returned by the method
    SimplexMat <- SimplexMethod(Matrix_Supply[1:3, 2:6], Matrix_Demand, Matrix_Supply[1:3, 1])
    return(SimplexMat$TableauPerIteration[[SimplexMat$noOfIterations]]$Matrix)
  })
  
  #event reactive call that returns the respective iteration chosen by the user
  showSimplexMat <- eventReactive(input$showSimplexMat, {
    
    req(input$checkIter) #this input is from the numeric input
  
    iterNo = input$checkIter

    Matrix_Demand <- as.matrix(hot_to_r(input$Matrix_Demand))
    Matrix_Supply <- as.matrix(hot_to_r(input$Matrix_Suppy))
    
    SimplexMat <- SimplexMethod(Matrix_Supply[1:3, 2:6], Matrix_Demand, Matrix_Supply[1:3, 1])
   
    if(iterNo > SimplexMat$noOfIterations){
      print("Out of Bounds")
      return(NULL)
    }
   #returns the specific tableau corresponding the chosen iteration number
    return(SimplexMat$TableauPerIteration[[iterNo]]$Matrix)
  })
  

 #outputs the minimnum cost computed using the simplex method
  output$Minimumcost <- renderTable({
    Matrix_Demand <- as.matrix(hot_to_r(input$Matrix_Demand))
    Matrix_Supply <- as.matrix(hot_to_r(input$Matrix_Suppy))
    SimplexMat <- SimplexMethod(Matrix_Supply[1:3, 2:6], Matrix_Demand, Matrix_Supply[1:3, 1])
    
    minCost = SimplexMat$Solution
    finalMinCost = matrix(nrow = 1, ncol = 2, byrow = TRUE, dimnames = list(c(),c("Minimized Cost: ", "No of Iterations: ")))
    finalMinCost[1,1] = minCost
    finalMinCost[1,2] = SimplexMat$noOfIterations
    # 
    if(input$Min)  return(finalMinCost) #returns the final Minimized cost
    else return(NULL)
  })
  #renders the handsontable of the final tableau
  output$showFinalTableau <- renderRHandsontable({
    rhandsontable(finalTab(), readOnly = TRUE)
  })
  #renders the handsontable of the tableaus per iteration
  output$Tableaus <- renderRHandsontable({
    rhandsontable(showSimplexMat(), readOnly = TRUE, width = 550)
  })
 
 
 
  
  
  
########################################################################################################################################  
  
  
}

shinyApp(ui = ui, server = server)