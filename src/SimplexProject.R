


#SAMPLE OUTPUS FOR


# Matrix_Demand = matrix(c(180,80,200,160,220), nrow = 1, ncol=5, byrow=TRUE, dimnames = list(c("DEMAND:"),c("W1","W2","W3","W4","W5")))
# Matrix_Supply = matrix(c(310,10,8,6,5,4,260,6,5,4,3,6,280,3,4,5,5,9), nrow=3,ncol=6,byrow=TRUE, dimnames = list(c("P1","P2","P3"),c("SUPPLY","W1","W2","W3","W4","W5")))

# 
# Matrix_Demand = matrix(c(431,332,350,450,400), nrow = 1, ncol=5, byrow=TRUE, dimnames = list(c("DEMAND:"),c("W1","W2","W3","W4","W5")))
# Matrix_Supply = matrix(c(1400,30,29,31,35,33,400,26,24,23,25,27,200,11,13,15,20,17), nrow=3,ncol=6,byrow=TRUE, dimnames = list(c("P1","P2","P3"),c("SUPPLY","W1","W2","W3","W4","W5")))


# Matrix_Demand = matrix(c(100,100,100,100,100), nrow = 1, ncol=5, byrow=TRUE, dimnames = list(c("DEMAND:"),c("W1","W2","W3","W4","W5")))
# Matrix_Supply = matrix(c(200,5,6,7,8,9,200,6,7,8,9,10,200,3,5,7,11,13), nrow=3,ncol=6,byrow=TRUE, dimnames = list(c("P1","P2","P3"),c("SUPPLY","W1","W2","W3","W4","W5")))

# Matrix_Demand = matrix(c(20,20,20,20,20), nrow = 1, ncol=5, byrow=TRUE, dimnames = list(c("DEMAND:"),c("W1","W2","W3","W4","W5")))
# Matrix_Supply = matrix(c(100,5,5,5,5,5,100,5,5,5,5,5,100,5,5,5,5,5), nrow=3,ncol=6,byrow=TRUE, dimnames = list(c("P1","P2","P3"),c("SUPPLY","W1","W2","W3","W4","W5")))
# # 
# Matrix_Demand = matrix(c(20,25,90,60,70), nrow = 1, ncol=5, byrow=TRUE, dimnames = list(c("DEMAND:"),c("W1","W2","W3","W4","W5")))
# Matrix_Supply = matrix(c(50,30,29,31,35,33,50,26,24,23,25,27,50,11,13,15,20,17), nrow=3,ncol=6,byrow=TRUE, dimnames = list(c("P1","P2","P3"),c("SUPPLY","W1","W2","W3","W4","W5")))


#function the generates the augmented coefficient matrix
generateAugCoeff <- function(Matrix_Objective,Matrix_Demand,Matrix_Supply){
  #generates the dimnames
  dimnamecol = c(paste("x",1:15,sep=""),paste("S",1:8,sep=""),"Z","RHS") 
  dimnamerow = c(paste("S",1:8,sep=""),"Z")

  initialize_tableau = matrix(0,nrow = 9, ncol = 25, dimnames = list(dimnamerow,dimnamecol)) #all-0 matrix that will be populated as the initial tableau
  slackVar_count = 1 #count of the slack variables
  
  #initialize in the matrix the demand constraints
  constraints_Demand = matrix(0,nrow = length(Matrix_Demand), ncol=ncol(initialize_tableau),dimnames=list(dimnamerow[1:5],dimnamecol))
  startcol = 1
  for(i in 1:length(Matrix_Demand)){
    endcol = startcol+2
    constraints_Demand[i,startcol:endcol] = 1
    startcol = startcol+3
   
  #multiplies the constraints by negative 1 to change the inequality
    constraints_Demand[[i,ncol(initialize_tableau)]] = Matrix_Demand[i]
    constraints_Demand[i,] = constraints_Demand[i,]*-1 
    #adds the slack variables
    constraints_Demand[i,(i+15)] = 1
    slackVar_count = slackVar_count+1
  
  }
 #adds the demand coefficients to the initial matrix
  initialize_tableau[1:5,1:ncol(initialize_tableau)] = constraints_Demand
  
  #initialize in the matrix the supply constraints
  constraints_Supply = matrix(0,nrow = length(Matrix_Supply), ncol(initialize_tableau), dimnames=list(dimnamerow[6:8],dimnamecol))
  si = 0
  for(i in 1:length(Matrix_Supply)){
    startIndex = si+1
    endIndex = si+13
    constraints_Supply[i,startIndex:endIndex] = c(1,0,0,1,0,0,1,0,0,1,0,0,1)
    
    #adds the RHS
    constraints_Supply[i,ncol(initialize_tableau)] = Matrix_Supply[i]
    
    #adding slack variables
    constraints_Supply[i,(slackVar_count+15)] = 1
    slackVar_count = slackVar_count+1 #updating the number of slack variables used
    si = si+1 #updates the starting index
  }
  initialize_tableau[6:8,1:ncol(initialize_tableau)] = constraints_Supply #adds the supply coefficients to the initialize matrix
  
  #adding in the matrix the objective function
  objective_function = matrix(0,nrow=1,ncol(initialize_tableau),dimnames = list(dimnamerow[9],dimnamecol))
  si_2 = 0

  for(i in 1:ncol(Matrix_Objective)){
    startIndex2 = (si_2+1)
    endIndex2 = (si_2+3)
    objective_function[1, startIndex2:endIndex2] = c(Matrix_Objective[1,i],Matrix_Objective[2,i],Matrix_Objective[3,i])
  
    si_2 = si_2 + 3 #updates the starting index
  }
  objective_function[1,ncol(initialize_tableau)-1] = 1
  initialize_tableau[9,1:ncol(initialize_tableau)] = objective_function #adds the coefficients of the objective function to the initial matrix
  
  
  return(list(AugCoeff = initialize_tableau, SlackVariableCount = slackVar_count)) #returns a list containing an initialized tableau and the number of slack variables added
  
}

#this function searches if there is still a negative value in the RHS
IsThereNegative <- function(checkMatrix){
 
  # http://www.datasciencemadesimple.com/min-and-max-function-in-r/
  #min() function in R computes the minimum value of a vector or data frame.
  
  
  minValue = min(checkMatrix[,ncol(checkMatrix)][1:nrow(checkMatrix)-1]) #the minvalue using the min method to find the minimum in the RHS
  if(!is.na(minValue) && !is.infinite(minValue) && !is.nan(minValue)){
    if(minValue >= 0){ 
      return(FALSE) #if the minvalue >= 0 meaning there are no more negative in the RHS, thus returns false and then move to the next method
    }else{
      return(TRUE) #otherwise returns true and then the process continues
    }
  }else{
    return(FALSE) #if the minvalue is one of these nan, null, inf, etc. returns false
  }
    
  # }
}

#checks the RHS for the negative number with the highest magnitude and normalizes the row and then eliminate the other values in the pivot column
DoPhase1 <- function(checkMatrix){
  
  #https://www.rdocumentation.org/packages/raster/versions/3.0-7/topics/which.min
  #Which cells have the minumum / maximum value (for a RasterLayer), or which layer has the minimum/maximum value 
  #which.min and which.max return the index of the first layer that has the min or max value for a cell. 
  
  
  
  #computes for the pivot row index, pivot column index and the index of the pivot element
  PivotRowIndex = which.min(checkMatrix[,ncol(checkMatrix)][1:nrow(checkMatrix)-1])  
  # print(paste("prow: ",PivotRowIndex))
  PivotColumnIndex = which.min(checkMatrix[PivotRowIndex,1:ncol(checkMatrix)-1])
  # print(paste("pcol: ",PivotColumnIndex))
  getPivotElement = checkMatrix[PivotRowIndex,PivotColumnIndex]
  # print(paste("pElemm: ",getPivotElement))

  for(i in 1:ncol(checkMatrix)){ #normalizing the pivot row
    checkMatrix[PivotRowIndex,i] = checkMatrix[PivotRowIndex,i]/getPivotElement
  }
  for(i in 1:nrow(checkMatrix)){
    if(i != PivotRowIndex) { #if it not the same row
      toEliminate = checkMatrix[i,PivotColumnIndex]
      for(j in 1:ncol(checkMatrix)){
       #elimination process
        multiplyer = checkMatrix[PivotRowIndex,j]
        additiveInverse = -1*(multiplyer*toEliminate)
        checkMatrix[i,j] = checkMatrix[i,j] + additiveInverse
      }
    }
    
  }
  return(checkMatrix) #returns the matrix
}
#returns index of the smallest positive
getIndex_SmallestPositive <- function(testRatio) {
  indexOfsmallest = 1; compareTo = 999999; #comparison values
  for(i in 1:length(testRatio)) {
    if(!is.na(testRatio) && !is.null(testRatio) && !is.nan(testRatio[[i]]) && !is.infinite(testRatio[[i]])) { #checks if it is not nan,null,inf, or etc
      if(testRatio[i] < compareTo  && testRatio[i]>0){
          indexOfsmallest = i; #set i as an initial smallest positive index in the objective row
          compareTo = testRatio[[i]];
      }
    }
  }
  
  return (indexOfsmallest); #returns the smalles index 
}
Do_ModifiedGaussJord <- function(checkMatrix, newPivotRow, newPivotCol){
  #do the normalization
  checkMatrix[newPivotRow,] = checkMatrix[newPivotRow,]/checkMatrix[newPivotRow,newPivotCol]
  # print(checkMatrix)
  #do the elimination
  for(i in 1:nrow(checkMatrix)){
    if(i != newPivotRow){
      multiplyer = checkMatrix[i,newPivotCol]
      tempVal = (checkMatrix[newPivotRow,]*multiplyer)
      checkMatrix[i,] = checkMatrix[i,] - tempVal 
    }
  }
  return(checkMatrix) #returns the matrix
}

#this function implements the simplex method
SimplexMethod <- function(Matrix_Objective,Matrix_Demand,Matrix_Supply){
  generatedMatrix = generateAugCoeff(Matrix_Objective,Matrix_Demand,Matrix_Supply) #generates the augcoeff matrix
  checkMatrix = generatedMatrix$AugCoeff #checkmatrix now holds the augcoeff matrix generated above
  view_iterations = list() #list of each iteration that will be stored along the proces
  iteration_count = 0 #iteration count set to 0 as a start
  #adds the first matrix as the initial tableau
  view_iterations[[iteration_count+1]] = list(label = "Initial Tableau", Current_Iteration = iteration_count+1, Matrix = generatedMatrix$AugCoeff)
  iteration_count = iteration_count+1 #increments the iteration count
  
 # print(checkMatrix)
  
  while(IsThereNegative(checkMatrix)){ #as long as there is a negative number in the RHS
    #continue to do the phase1
    checkMatrix = DoPhase1(checkMatrix)
    view_iterations[[iteration_count+1]] = list(label="Phase One",Current_Iteration = iteration_count+1, Matrix = checkMatrix)
    iteration_count = iteration_count+1
  }
  # print(view_iterations)
  
  NegativeInLastRow = any(checkMatrix[nrow(checkMatrix), 1:(ncol(checkMatrix)-2)] < 0)
  if(is.na(NegativeInLastRow) == TRUE || is.infinite(NegativeInLastRow)==TRUE || is.nan(NegativeInLastRow)==TRUE || is.null(NegativeInLastRow)==TRUE){
    print("No Feasible Solution") #if the least negative in the last row is either of the nan, null, or inf 
    #there will be no feasible solution, since the test ratio will not be succesfully computed
    return(NA)
  }
  while(NegativeInLastRow){ #whle there are negative values in the objective row
    min_in_lastrow = min(checkMatrix[nrow(checkMatrix),1:(ncol(checkMatrix)-2)]) #searches for the minimimum in the objective row
    #gets the index of the min in the last row
    in_what_index = match(min_in_lastrow,checkMatrix[nrow(checkMatrix),1:(ncol(checkMatrix)-2)]) #https://g4greetz.wordpress.com/2017/03/01/match-function-in-r/
    #match returns the index where the first paramerter is found in the second parameter
    #then get the test ratio
    testRatio = c()
    for(i in 1:nrow(checkMatrix)-1){ #gets the test ration by dividing RHS by the column of the min in the last row excluding the Zvalue
      testRatio[i] = checkMatrix[i,ncol(checkMatrix)]/checkMatrix[i,in_what_index]
    }
    
    smallestIndex_testratio = getIndex_SmallestPositive(testRatio) #gets the smallest positive index in the test ratio 
    checkMatrix = Do_ModifiedGaussJord(checkMatrix,smallestIndex_testratio,in_what_index) #sends the smallest index as the new pivot row
    # print(checkMatrix)
    view_iterations[[iteration_count+1]] = list(label="Phase2",CurrentIteration=iteration_count+1, Matrix=checkMatrix);
    iteration_count = iteration_count + 1; #increments the iteration count

    NegativeInLastRow = any(checkMatrix[nrow(checkMatrix), 1:(ncol(checkMatrix)-2)] < 0) #check again if there are any negative in the objective function row
      
  }
  view_iterations[[iteration_count+1]] = list(label="Final Matrix", Matrix = checkMatrix) #make the last iteration's tableau be the final matrix
  iteration_count=iteration_count+1 #fincrement the iteration count for the last time
  Zvalue = checkMatrix[nrow(checkMatrix),ncol(checkMatrix)]*-1 #gets the zvalue in the last row,column and multiiply it by negative 1
  
  #returns the final simplex list that contains the following:
  finalSimplex = list(TableauPerIteration = view_iterations[1:(iteration_count-1)], FinalSimplexMatrix = view_iterations[iteration_count], noOfIterations = iteration_count-1,Solution=Zvalue)
}

a = SimplexMethod(Matrix_Supply[1:3,2:6],Matrix_Demand,Matrix_Supply[1:3,1])

