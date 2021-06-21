isPivotNeeded <- function(matrix, current_i, no_of_row){
  #this function checks if there is a need to pivot, meaning the current row element is not the largest among others
  getMax = abs(matrix[current_i, current_i]) #getting the initial max element
  index_of_max = current_i #set the current max index to current value of i
  
  checkRow = current_i+1 #will start to the row below the current
  if (checkRow <= no_of_row){ #checks if the current row is the last one, if yes, will not proceed 
    i = checkRow #otherwise will look for possible max elements
    while (i <= no_of_row){ #traverses the remaining rows below the current row
      if (getMax < abs(matrix[i,current_i])){ #if the initial max is less then the current element
        index_of_max = i #will update the index of the new initial max element
      }
      i = i + 1 #will increment i until the loop finishes
    }
  }
  return(index_of_max) #returns the index of the maximum element among the rows
}
doPivot <- function(matrix, candidate_i_for_pivot, i){
  #this function does the pivoting or the swapping of rows to have the element with largest magnitude to be the pivot row
  temp_matrix = matrix #initialize a temporary matrix for swapping
  
  #have the current row to be swapped with the candidate pivot row (containing max element)
  matrix[i, ] = temp_matrix[candidate_i_for_pivot, ] 
  matrix[candidate_i_for_pivot, ] = temp_matrix[i,]

  return(matrix) #returns the resulting matrix
}

GaussJordanMethod <- function(matrix){
  no_of_row = nrow(matrix)
  for(i in 1:no_of_row){
    candidate_i_for_pivot = isPivotNeeded(matrix,i,no_of_row)
    
    if(candidate_i_for_pivot != i){
      matrix = doPivot(matrix, candidate_i_for_pivot, i)
    }
    #normalizing the row by dividing the current row to the normalizing factor
    row_to_normalize = matrix[i,] #current row to be normalized
    normalizing_factor = matrix[i,i] #current diagonal element of the row
    normalized = row_to_normalize/normalizing_factor #normalizing the whole row                                                        
    matrix[i,] = normalized  #the row now is normalized    
    

    for (j in 1:nrow(matrix)){ #will now do the elimination
      
      if (j == i) next #if j == i meaning it is the diagonal, do nothing
      toEliminate = matrix[j, i] #eliminate the value at j,i
      additiveInverse = -1*(normalized * toEliminate) #to make it zero, the additive inverse must be added to the value to eliminate
      matrix[j, ] = matrix[j, ] + additiveInverse
    }
  }
  solution_vector = c(1:ncol(matrix)-1)
  solution_vector[] = 0L
  
  # solution_vector[1] = 0 #since a1 is always 0
  RHS_index = ncol(matrix) #gets the index of the RHS
  LastCol_index = RHS_index-1 #last col to access for the
  for(i in 1:LastCol_index){
    j = i+1 #start adding the solutions from the 2nd index because index 1 == 0
    solution_vector[j] = matrix[i,RHS_index]
  }

  GaussJordanList = list(GaussJordanMatrix = matrix, solution = solution_vector) #labled list containing the final matrix and the solution set
 
  return(GaussJordanList) #returns the labeled list
}
