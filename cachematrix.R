#Work for the demo
makeVector <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmean <- function(mean) m <<- mean
    getmean <- function() m
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
}

cachemean <- function(x, ...) {
    m <- x$getmean()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
    m
}


#Now Test the functions
x <- makeVector(c(1,2,3,4,5))
y <- cachemean(x)
z <- cachemean(x)
z


#-------------------------------------------------------------------------
#Answering question of making two functions

## The functions in this file manage the cache of matrices and makes it available when required
## This facilitates that the inverse calculation need not be done everytime for the same object
## functions do

## The function makeCacheMatrix creates an instance of the matrix and the inverse of the matrix in the memory 




makeCacheMatrix <- function(input_matrix = matrix())
{
    return_matrix <- NULL
    set <- function(matrix_store)
    {
        input_matrix <<- matrix_store 
        return_matrix <<- NULL            
    }
    get <- function() input_matrix
    set_inverse <- function(solve) return_matrix <<- solve
    get_inverse <- function() return_matrix
    list(set = set, get = get, 
         set_inverse = set_inverse, 
         get_inverse = get_inverse)
}


## The function cacheSolve checks the cache for the value of the object passed. If available it shall return the inverse, else calculate 
## the inverse and set the same in the matrix object

cache_solve <- function(matrix_to_check, ...)
{
    matrix_store <- matrix_to_check$get_inverse()
    if(!is.null(matrix_store))
    {
        message("Getting Cached Matrix")
        return(matrix_store)
    }
    data <- matrix_to_check$get()
    matrix_store <- solve(data, ...)
    matrix_to_check$set_inverse(matrix_store)
    matrix_store
}


#Now Test the functions


B <- matrix( c(2, 4, 3, 1, 5, 7, 1, 2, 3),  nrow=3, ncol=3) 
B_inverse <- solve(B)
B_inverse

xm <- makeCacheMatrix(B)
xm_1 <- cache_solve(xm)
xm_1
xm_2 <- cache_solve(xm)
xm_2