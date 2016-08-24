
#' Compute the inverse matrix by basic column operations
#' @param X  A matrix. Must be inverted.
#' @param verbose  Logical. If true, print the operation content
#' @return  Inverse matrix of X.
FindInverse <- function(X, verbose = FALSE)
{
  if (!is.matrix(X)) stop("X must be a matrix")
  if (det(X) == 0) stop("X is not invertible")
  
  Flip <- function(i, j)
  {
    X[, c(i, j)] <<- X[, c(j, i)]
    if (verbose)
    {
      cat(sprintf("\nFliped %d and %d\n", i, j))
      print(X)
    }
    
    A <- diag(N)
    A[i,i] <- 0
    A[j,j] <- 0
    A[i,j] <- 1
    A[j,i] <- 1
    return(A)
  }
  
  Multiply <- function(i, a)
  {
    X[, i] <<- X[, i] * a
    if (verbose)
    {
      cat(sprintf("\nMultiplied %f on %d\n", a, i))
      print(X)
    }
    
    A <- diag(N)
    A[i,i] <- a
    return(A)
  }
  
  MultAndSubtract <- function(i, j, a)
  {
    X[, j] <<- X[, j] - a * X[, i]
    if (verbose)
    {
      cat(sprintf("\nMultiplied %f on %d and subtracted from %d\n", a, i, j))
      print(X)
    }
    
    A <- diag(N)
    B <- matrix(0, nrow = N, ncol = N)
    B[i, j] <- a
    return(A - B)
  }
  
  N <- nrow(X)
  
  out <- diag(N)
  for (i in 1:N)
  {
    if (X[i,i] == 0) 
    {
      k <- min(which(X[i,] != 0 & 1:N > i))
      A <- Flip(i, k)
      out <- out %*% A
    }
    
    A <- Multiply(i, 1/X[i,i])
    out <- out %*% A
    
    for (j in setdiff(1:N, i))
    {
      A <- MultAndSubtract(i, j, X[i,j])
      out <- out %*% A
    }
  }
  
  return(out)  
}



# test 1
N <- sample(1:10, 1)
while (TRUE)
{
  X <- matrix(rnorm(N^2), nrow = N, ncol = N)
  if (det(X) != 0) break
}
print(X)
X %*% FindInverse(X) 

# test 2
N <- sample(1:10, 1)
while (TRUE)
{
  X <- matrix(sample(c(-2:2), N^2, replace = TRUE), nrow = N, ncol = N)
  if (det(X) != 0) break
}
print(X)
print(X %*% FindInverse(X, TRUE))

