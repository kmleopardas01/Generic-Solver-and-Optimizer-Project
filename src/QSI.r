# source('C:/Users/KIRBY M. LEOPARDAS/Desktop/CMSC150PROJECT/QSI/GaussJordanMethod.r')
source("GaussJordanMethod.r")
# 
x = c(3, 4.5, 7, 9)
y = c(2.5, 1, 2.5, 0.5)
dataPoints = data.frame(x=x, y=y)
dataPoints = dataPoints[order(dataPoints$x),]



toQSIval = 3
a = QSI(dataPoints$x, dataPoints$y, toQSIval)



condition1 <- function(dataPoints){
  xval = dataPoints$x
  yval = dataPoints$y
  
  #this function generates all the equations under the first condition of the quadratic spline interpolation
  n = length(xval)-1 #no of intervals
  functionString = "function(x)"
  eq1 = eq2 = condition1List = list() #initialization of all the necessary lists
  for(i in 2:n){
    # xval = as.numeric(xval[i]) #xvalue
    x = xval[i]
    
    part_a = paste(x^2,paste("a",i-1,sep=""),sep=" * ") #will generate the first term of the first equation from condition1
    #will generate the final first equation from condition1
    final = paste(functionString,paste(paste(part_a,(paste(x,paste("b",i-1,sep=""),sep=" * ")),paste("c",i-1,sep = ""),sep=" + "),yval[i],sep=" = "),sep=" ") 
    condition1List = c(condition1List,final) #will append to the list
    
    part_a2 = paste(x^2,paste("a",i,sep=""),sep=" * ") #will generate the second equation of the second equation from condition1
    #will generate the final second equation from condition1
    final2 = paste(functionString,paste(paste(part_a2,(paste(x,paste("b",i,sep=""),sep=" * ")),paste("c",i,sep = ""),sep=" + "),yval[i],sep=" = "),sep=" ")
    condition1List = c(condition1List,final2) #will append to the list
  }
  return (condition1List) #will return the list of the equations from condition1
}
condition2 <- function(dataPoints){
  xval = dataPoints$x
  yval = dataPoints$y
  #this function generates all the equations under the second condition of the quadratic spline interpolation
  end = length(xval) #will serve as the last index
  condition2List = list() #initialized list
  functionString = "function(x)"
  
  x = xval[1] #xvalue at the first index
  x2= (xval[end]) #xvalue at the last index
  
  
  part_a = paste(x^2,paste("a",1,sep=""),sep=" * ") #will generate the first term of the first equation from condition2
  part_a2 = paste(x2^2,paste("a",end-1,sep=""),sep=" * ") #will generate the first term of the second  equation from condition2
  #will complete the final first equation under condition2
  con2Eq1 =   paste(functionString,paste(paste(part_a,(paste(x,paste("b",1,sep=""),sep=" * ")),paste("c",1,sep = ""),sep=" + "),yval[1],sep=" = "),sep=" ")
  condition2List = c(condition2List,con2Eq1) #will append to the list
  #wwill generate the final second equation under condition2
  con2Eq2 =   paste(functionString,paste(paste(part_a2,(paste(x2,paste("b",end-1,sep=""),sep="  * ")),paste("c",end-1,sep = ""),sep=" + "),yval[end],sep=" = "),sep=" ")
  condition2List = c(condition2List,con2Eq2) #will append to the list
  
  return(condition2List) #will return the list of the equations from condition2
}
condition3 <- function(dataPoints){
  xval = dataPoints$x
  yval = dataPoints$y
  #this function generates all the equations under the third condition of the quadratic spline interpolation
  n = length(xval)-1 # no of intervals which is totaldatapoints - 1
  functionString = "function(x)"
  condition3List = list() #initialized list
  
  for(i in 2:n){
    x = xval[i] #getting the xVal
    LHS = paste(paste(x*2,paste("a",i-1,sep=""),sep=" * "),paste("b",i-1,sep=""),sep= " + ") #the left side of the equation
    RHS = paste(paste((-x)*2,paste("a",i,sep=""),sep=" * "),paste("-1 b",i,sep=""),sep=" + ") #the right side of the equation
    cond3eqs = paste(functionString,paste(paste(LHS,RHS,sep=" + "),0,sep=" = "),sep=" ") #final condition3 equations
    condition3List = c(condition3List,cond3eqs) #appending everything in the list
  }
  return(condition3List) #will return the list of the equations from condition3
}

is_Bounded <- function(toQSIval,minInterval, maxInterval){
  #this function checks wether the given value is within a particular interval from the generated set of equations
  #basically ensures that the value is less than the maxInterval and greater than the minInterval
  if (minInterval <= toQSIval && maxInterval >= toQSIval) {
    return (TRUE) #returns true
  }
  return (FALSE) #otherwise, false
}

generate_MinMaxInterval <- function(dataPoints,maxPossibeRange,toQSIval){
  xval = dataPoints$x
  minInterval = maxInterval = 0
  
  for(i in 1:maxPossibeRange){
    candidateRange = is_Bounded(toQSIval, as.numeric(xval[i]), as.numeric(xval[i+1])) #will accept a boolean value if the given value is bounded by the current prospect range
    if(candidateRange == TRUE) { #if there is a valid range
      minInterval = i #set the current index as the candidate minInterval
      maxInterval = i+1 #set the next index as the candidate maxInterval
      break #will break after the range is found
    }
    
  }
  if(minInterval== 0 && maxInterval == 0) return (-1) #if the given value is not within the rangle of the dependent variables, will returnn false
  return (minInterval) #returns the index of the minInterval
}

QSI <- function(x, y, toQSIval){
  
  dataPoints = data.frame(x=x, y=y)
  
  # sort by x 
  dataPoints = dataPoints[order(dataPoints$x),]
  
  xval = dataPoints$x
  
  yval = dataPoints$y
  
  
  if (length(xval) != length(yval)){
    print("Length of independent and dependent variable do not match")
    return (NA)
  }
  
  total_datapoints = length(xval) #total nunmber of data points in the set
  n = maxPossibleRange = length(xval)-1 #no of intervals // #also serves as the last index 
  equations_per_Interval = list() #will store the final equations of the intervals
  counter = 1 #initialize counter
  
  mattrix = matrix(0L,nrow=3*n-1, ncol=3*n, byrow = FALSE) #initialize a 0-matrix with 3n-1 x 3n dimenstion
  
  minIndex = generate_MinMaxInterval(dataPoints,maxPossibleRange,toQSIval) #accepting the index of the minInterval from the function call
  
  if(minIndex == -1) {
    print("Invalid Input!")
    return(NULL)
  }else{
    print("QSI SUCCESFUL!")
  }


  # if(is.na(minIndex)) return(NA)
  current_col = 1; #set the current column to 1
  #this for loop will add the coeffients of the first equation under the condition1
  for(i in 3:total_datapoints){ #one indexing
    if(i == 3){ #if will add to the first row, adding only the coefficients of b1 and c1 because a1 == 0
      end_col = current_col+1 #since there are two values to insert, will set the index of the last column 1 more the current column
      mattrix[counter,current_col:end_col] = c(as.numeric(xval[i-1]),1) # adding b1 and c1 from the current column to the end column
      mattrix[counter, ncol(mattrix)] = as.numeric(yval[i-1]) #adds the RHs to the last column of the matrix
      #the pattern is that after adding the first line, there will be two-step jump for the next insertion, next insert will start at a2
      current_col= current_col + 2 #two cells are filled successfully, move two slots away from the current column
    }else { 
      #succeeding iterations will come to this block
      end_col = current_col+2 #since there are three values to insert ai,bi,and ci, will set the index of the last column 2 more than the current column
      mattrix[counter, current_col:end_col] = c(as.numeric(xval[i-1])^2, as.numeric(xval[i-1]), 1) #adding ai-1,bi-1,and ci-1 to the matrix
      mattrix[counter, ncol(mattrix)] = as.numeric(yval[i-1]) #adding the RHS to the last column of the matrix
      #this time, the pattern is that after the successful insert, there will be a three-step jump for the next insertion
      current_col = current_col + 3 #three cells are filled succesfully, move three slots away from the current column
    }
    counter = counter + 1 #update counter
    #---------------------- this block wil generate the second equation under the condition1------------------------#
    end_col = current_col + 2 #since there are three slots to fill, set the index of the last column 2 more than the current column
    #from the currentcol to the endcol, will add the coefficients of  ai,bi,ci
    mattrix[counter, current_col:end_col] = c(as.numeric(xval[i-1])^2, as.numeric(xval[i-1]), 1)
    mattrix[counter, ncol(mattrix)] = as.numeric(yval[i-1]) #adding the RHS to the last column of the matrix
    counter = counter + 1 #update again the counter for the next conditions
  }
  
  
  
  #------------------condition 2 where the first and the last function must pass through the end points-----------------------------#
  mattrix[counter, 1:2] = c(as.numeric(xval[1]),1) #the coefficients are from a1 b1 and c1
  mattrix[counter, ncol(mattrix) ] = c(as.numeric(yval[1])) #the coefficients are from an bn and cn
  counter = counter+1 #update the counter for the next condition
  frontC = ncol(mattrix)-3
  
  mattrix[counter, frontC:ncol(mattrix)] = c(as.numeric(xval[total_datapoints])^2,as.numeric(xval[total_datapoints]),1,as.numeric(yval[total_datapoints]))
  counter = counter+1
  
  
  
  
  
  #-------------------condition3 The first derivative at the interior knots must be equal.-----------------------------------------#
  current_col2 =1
  for(i in 3:total_datapoints){ #one indexing
    if(i == 3){ #if will add to the first row, adding only the coefficients of b1 and c1 because a1 == 0
      end_col2 = current_col2 + 3;  #since there will be 4 values to insert, set the index of the end column 3 more than the current column
      mattrix[counter, current_col2:end_col2] = c(1,0,-2*as.numeric(xval[i-1]),-1) #adding the coeffecients of the first equation undenr cond3
      #the pattern here is that after a successful insert, there will be two-step jump because the next insert will start at a2
      current_col2 = current_col2 + 2 #two cells are filled succesfully, move two slots away from the current column
    }else{
      #succeeding insertions will come to this block
      end_col2 = current_col2 + 4 #there will be five coeffecients to insert, set the index of the last column 4 more than the current column
      #add the coeffecient of ai-1, bi-1, ci-1, ai, and bi   
      #ci is even though not included in the range is initialized as 0, so no problem
      mattrix[counter, current_col2:end_col2] = c(2*as.numeric(xval[i-1]),1,0,-2*as.numeric(xval[i-1]),-1)
      current_col2 = current_col2 + 4 #since there are five inserts, move 4 slots away from the current column
    }
    counter = counter+1 #update counter
    
  }
  
  #-----optional print resulting augcoeff matrix------#
  # print(mattrix)
  solutionSet = GaussJordanMethod(mattrix) #solve using gauss jordan for the solution set
  solutionFinal = solutionSet$solution[1:length(solutionSet$solution)]
  
  
  #generating the equation of each Interval
  functionString = "function(x) "
 
  
  eqIndex = 3
  for(i in 1:n){ #will generate the fn(x) where n is the number of intervals
    if(i == 1){
      equations_per_Interval[i] = paste(functionString, paste(paste(paste(solutionFinal[1], " x + ",sep=" * "),solutionFinal[2],sep="")))
    }else{
      equations_per_Interval[i] = paste(functionString, paste(paste(paste(solutionFinal[eqIndex], " x^2 +  ",sep=" * "),paste(solutionFinal[eqIndex+1], " x + ",sep=" * "),solutionFinal[eqIndex+2],sep="")))
      eqIndex = eqIndex+3
    }
    
    #update the index of the value to be accessed in the solution set by 3
  }
  
  
  QSI_FUNCTION = eval(parse(text = equations_per_Interval[minIndex])) #pasrse the equation where the given x value is bounded
  QSI_val = QSI_FUNCTION(toQSIval) #use the QSI FUNCTION to solve for the estimated value using the Quadratic Spline Interpolation Method
  
  #a list that containts all the generated 3n equations
  cond4eq = paste("function(x) a1", 0, sep=" = ")
  
  ConditionEquations = list(Condition1 = condition1(dataPoints), Condition2 = condition2(dataPoints), Condition3 = condition3(dataPoints), condition4 = cond4eq )
  
  return(list(Equations = ConditionEquations, IntervalEquations = equations_per_Interval, Matrix = solutionSet$GaussJordanMatrix, Solutions = solutionSet$solution, QSI_Value = QSI_val, IntervalIndex = minIndex, hanap = toQSIval))
}

