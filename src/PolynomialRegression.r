# source('C:/Users/KIRBY M. LEOPARDAS/Desktop/CMSC150PROJECT/PolyReg/GaussJordanMethodMod1.r')
source("GaussJordanMethodMod1.r")

x <- c(1,2,3,4,5,6,7)
y <- c(0.5,2.5,2,4,3.5,6,5.5)

degreeVal = 2
estimate = 5

dataPoints = data.frame(x=x,y=y)
d = PolynomialRegression(degreeVal,dataPoints$x,dataPoints$y, estimate)




#computes for the Nth Order elements
computeNthOrder <- function(degreeVal,x,y){
  augcoeffmatrix = matrix(0, nrow = (degreeVal+1), ncol = (degreeVal+2), byrow=TRUE) #initializes the matrix
  for(inRow in 1:nrow(augcoeffmatrix)){ #traverses the matrix 
    for(inCol in 1:ncol(augcoeffmatrix)){
      if(inCol == ncol(augcoeffmatrix)){ #if the iteration is on the RHS
        #the element will be solved by multiplying the sum of all x^row-1 by the y value
        augcoeffmatrix[inRow,inCol] = sum(((dataPoints[[1]]^(inRow-1)))* dataPoints[[2]])
      }else{
        #otherwise the elements will be computed by getting the summation of x raised to a current row-1
        augcoeffmatrix[inRow,inCol] = sum(((dataPoints[[1]]^((inRow-1)+(inCol -1)))))
      }
    }
  }
 
  return (augcoeffmatrix) #returns the augcoeffmatrix
 
}

#main polynomial regression function that accepts the degreeVal and all the data points
PolynomialRegression <- function(degreeVal, x,y,estimate){

  dataPoints = data.frame(x=x,y=y)
  colnames(dataPoints)[1] = "Ind_Variable"
  colnames(dataPoints)[2] = "Dep_Variable"

  
  if(degreeVal < 1){ #if the degreeVal passed is less than 1
    #will print invalid degreeVal
    print("Invalid degreeVal. degreeVal >= 1!")
    return(NULL)
  }else{
    
    #otherwise proceed with the methods
    #the final matrix will return the labeled list
    
    MatrixL = computeNthOrder(degreeVal,x,y) #temporary matrix that will be sent to the gaussian function
    getSolution = GaussJordanMethod(MatrixL) #getSolution receives the solution from the Gaussian Method

    unknowns = getSolution$solution
    
    functionString = "function(x)" #function(x) string form

    expression = c() #empty vector for the expression
   for(i in 1:length(getSolution$solution)){ #traverses the loop til the last element of the solution vector
      getVariables = paste("x^",deparse(i-1,control=NULL),sep=""); #will return x^p where p is the current degreeVal
      
      connectVariables = paste(deparse(getSolution$solution[i]),getVariables, sep=" * ") #conects the variables to the coefficients
      
      expression = c(expression,connectVariables) #appends the term in the vector expression
      
   }

   polyString1 = expression[1] #gets the first solution
   for(i in 2:length(expression)){
    polyString1 = paste(polyString1,expression[i],sep =" + ") #connects each terms in the vector by the plus operator
 
   }

   polynomialString = paste(functionString,polyString1,sep=" ") #connect the function string and the first term
  

   polynomial_function = eval(parse(text = polynomialString)) #parse the plynomial string into a polynomial function
   answer = polynomial_function(estimate)
    #finalMAtrix that contains the augcoeffmatrix, the unknowns, the polynomialString and lastly the polynomial function
    FinalMatrix = list(augcoeffmatrix = computeNthOrder(degreeVal,dataPoints), unknowns = unknowns, polynomialString = polynomialString, polynomialFunction = polynomial_function, estimate = answer, degree=degreeVal)
    return(FinalMatrix)
    
  }
  
}


