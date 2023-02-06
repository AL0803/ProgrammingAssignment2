
#This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()){
	
        #set the matrix and set inverse matrix as null
	setMatrix <- function(m){
		m <<- x
		inverse <- NULL
	}
        
        #get the matrix
	getMatrix <- function(){
		return(m)
	}
	
	#set the inverse
	setInverse <- function(i){
		inverse <<- i
	}
	
        #get the inverse
	getInverse <- function(){
		return(inverse)
	}
	
        #return list of functions
	return(list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse))

}

#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache
cacheSolve <- function(x, ...){
	
        #if inverse is already calculated, return inverse
	if(!is.null(x$getInverse())){
		return(x$getInverse)
	}
	
        #calculate the inverse
	inverse <- solve(x$get())
        
        #set inverse to the matrix
	x$setInverse(inverse)	
        
        #returns inverse
	return(inverse)
}
