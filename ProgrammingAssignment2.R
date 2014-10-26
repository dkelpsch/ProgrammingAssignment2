##These functions take a matrix imput from the user, compute the inverse of the
##matrix, and store it to object m called by the get functions. The cached inverse
##matrix is simply called again as needed instead of being computed over and over. 


#The function makeCacheMatrix takes a matrix and caches the inverse of this matrix 
#to the object m
makeCacheMatrix <- function(x = matrix()) {  
    m <- NULL
    set <- function(y) {  #creates the set function allowing a new matrix to be applied and resets m
        x <<- y
        m <<- NULL
    }
    get <- function() x  #creates function get that returns the matrix when called
    setmatrix <- function(solve) m <<- solve  #creates function setmatrix, utalized in cacheSolve to 
                                              #apply the inverse matrix to m
    getinverse <- function() m  #creates function get inverse which will return the inverse matrix m
    list(set = set, get = get,
         setmatrix = setmatrix,
         getinverse = getinverse)
}

#functhion cacheSolve will compute the inverse matrix for the matrix imputted by 
#the user and apply it to object m, if object m contains a matrix (is not set at 
# NULL) the function will return m
cacheSolve <- function(x, ...) { #
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setmatrix(m)
    m
}