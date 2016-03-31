## This script declares two functions, summariezed as follows:
##	- makeCacheMatrix - A function that populates a list
##			    with a collection of functions related 
##			    to the capture/exposure of square/inverse 
##			    matricies
##	- cacheSolve      - A function that employs those supplied
##			    by the function makeCacheMatrix to
##			    expose/cache inverse matricies

## My Test script...

## matrixfunc <- makeCacheMatrix ()
## matrixfunc$setinputsq (matrix(nrow = 2, ncol = 2, 1:4))
## cacheSolve (matrixfunc)
## cacheSolve (matrixfunc)
## matrixfunc$setinputsq (matrix(nrow = 2, ncol = 2, 5:8))
## cacheSolve (matrixfunc)
## cacheSolve (matrixfunc)

## Function declarations...

## makeCacheMatrix...
##
##	A function that returns a list of functions 
##	described as follows:
##
##		- getinputsq	   - Retrieve the most recently 
##				     inputted square matrix
##		- getcachedsq	   - Retrieve the previous captured 
##				     square matrix
##		- getcachedinverse - Retrieve the previously 
##				     captured inverse matrix
##		- setinputsq	   - Cache the supplied input matrix
##		- setcachedinverse - Cache the supplied inverse 
##				     matricies, and the square matrix 
##				     from which it was derived

makeCacheMatrix <- function()
{
	# The functions declarations noted above...

	input_sq_matrix <- NULL
	cached_inverse_matrix <- NULL
	cached_sq_matrix <- NULL
	
	setcachedinverse <- function (p_inverse_matrix, p_sq_matrix)
			{
			cached_inverse_matrix <<- p_inverse_matrix
			cached_sq_matrix <<- p_sq_matrix
			}
	
	list(
	    getinputsq = function () input_sq_matrix
	    , getcachedsq = function () cached_sq_matrix
	    , getcachedinverse = function() cached_inverse_matrix
	    , setinputsq = function (p_input_sq) input_sq_matrix <<- p_input_sq
	    , setcachedinverse = setcachedinverse
	    )
}


## cacheSolve...
##
##	A function that accepts a vector of functions, and
##	uses those functions to effect the following:
##		- Retrieve the previously cached inverse matrix
##		- Retrieve the most recently inputted square matrix
##		- Retrieve the previously cached sq matrix, what
##		  the cached inverse matrix was derived from
##		- Cache the aforementioned inverse matrix, and the square
##		  matrix from which it was derived
##		- Return the cached/newly-calculated inverse matrix

cacheSolve <- function(x, ...)
{
        ## Return a matrix that is the inverse of 'x'

	capture_inverse <- FALSE
	
	cached_inverse_matrix <- x$getcachedinverse()
	input_sq_matrix <- x$getinputsq ()
	
	if (is.null (cached_inverse_matrix))
	{
	    message ("Capturing inverse cache")
	    capture_inverse <- TRUE
	}
	else
	{
	    cached_sq_matrix <- x$getcachedsq()
	    
	    if (identical (input_sq_matrix, cached_sq_matrix))
	    {
		message ("Returning cached inverse")
		inverse_matrix <- cached_inverse_matrix
	    }
	    else
	    {
		message ("Capturing inverse to cache")
		capture_inverse <- TRUE
	    }
	}

	if (capture_inverse == TRUE)
	{
	    if (!is.null (input_sq_matrix))
	    {
		inverse_matrix <- solve (input_sq_matrix)
		x$setcachedinverse(inverse_matrix, input_sq_matrix)
	    }
	    else
	        warning ("No input matrix supplied")
	}	

	return (inverse_matrix);
}
