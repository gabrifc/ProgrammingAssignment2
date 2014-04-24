## File: cachematrix.R, a function for the 2nd Assignment of the R programming course 
## Web: https://class.coursera.org/rprog-002/
## Authors: rdpeng, gabrielfc
## Date: 2014/04
## Description: This R script has 2 functions: makeCacheMatrix and cacheSolve, used to 
## cache the inverse of a matrix in order to reduce the cost of computing it repeatedly.

## Use Example: 
## m<-makeCacheMatrix(matrix(8:11,nrow=2))
## cacheSolve(m) ## Inverse calculated
## cacheSolve(m) ## Inverse retrieved from cache, not calculated

## makeCacheMatrix creates a special matrix object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  
  ## Iniatialize the variable that will store the inverse matrix.
  invertedMatrix <- NULL
  
  ## 1.- If a (new) matrix (y) is passed to the function
  setMatrixCache <- function(y) {
  
    ## Set the matrix value to the cache
    x <<- y
    
    ## Restart the variable to NULL.
    invertedMatrix <<- NULL
  }
  
  ## 2.- Retrieve the matrix stored in the cache
  getMatrixCache <- function() x
  
  ## 3.- Save the inverted matrix to the cache
  setInvertedMatrixCache <- function(solve) invertedMatrix <<- solve
  
  ## 4.- Get the inverted matrix from the cache
  getInvertedMatrixCache <- function() invertedMatrix
  
  ## A simple list of the 4 functions.
  list(setMatrixCache=setMatrixCache,
       getMatrixCache=getMatrixCache,
       setInvertedMatrixCache=setInvertedMatrixCache,
       getInvertedMatrixCache=getInvertedMatrixCache)
  
}

## cacheSolve is a function that checks if the inverted matrix (solved 
## matrix) for a given matrix (x) is stored in the cache. If it is, 
## it retrieves the previous results. If not, it solves the matrix.
cacheSolve <- function(x, ...) {
  
  ## Try to get the result of the previous inverted matrix from the cache.
  invertedMatrix <- x$getInvertedMatrixCache()
  
  ## If the results is not NULL, means the matrix has already been inverted 
  ## and the script ends returning it form cache. No solving this time.
  if(!is.null(invertedMatrix)) {
    message("getting inverse matrix from cache")
    return(invertedMatrix)
  } else {
    
    ## If the invertedMatrix has not been solved yet, get the new matrix
    inputMatrix <- x$getMatrixCache()
    
    ## And solve (inverse) the matrix.
    invertedMatrix <- solve(inputMatrix)
    
    ## Save the inverted matrix to the cache.
    x$setInvertedMatrixCache(invertedMatrix)
    
    ## Return the result.
    return(invertedMatrix)
  }
}