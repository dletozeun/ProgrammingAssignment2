## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix())
{
    xinv <- NULL
    
    set <- function( y )
    {
        x <<- y
        xinv <<- NULL
    }
    
    get <- function() x
    
    setinverse <- function( inv ) xinv <<- inv
    
    getinverse <- function() xinv
    
    list( set = set, get = get, setinverse = setinverse, getinverse = getinverse )
}


## Write a short comment describing this function

cacheSolve <- function(x, ...)
{
    inv = x$getinverse()
    
    if( !is.null( inv ) )
    {
        message( "getting cached data" );
        return( inv )
    }
    
    data <- x$get()
    inv = solve( data, ... )
    x$setinverse( inv )
    inv
}
