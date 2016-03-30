## Instructions

In this assignment, a function to cache potentially time-consuming computations has been constructed. For example, taking the mean of a numeric vector is typically a fast operation. However, for a very long vector, it may take too long to compute the mean, especially if it has to be computed repeatedly (e.g. in a loop). If the contents of a vector are not changing, it may make sense to cache the value of the mean so that when we need it again, it can be looked up in the cache rather than recomputed. This Programming Assignment takes advantage of the scoping rules of the R language
and how they can be manipulated to preserve state inside of an R object.

### Assignment: Caching the Inverse of a Matrix

Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly. The purpose of the assignment is to write a pair of functions that cache the inverse of a matrix.
Write the following functions:
1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
2. cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated
(and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
Assume that the matrix supplied is always invertible.


