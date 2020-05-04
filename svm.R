linear.kernel <- function(x1, x2) {
  return (x1%*%x2)
}
svm.results <- function(X, y, kernel=linear.kernel, C=NULL,testData) {
  n.samples <- nrow(X)
  n.features <- ncol(X)
  # Gram matrix
  K <- matrix(rep(0, n.samples*n.samples), nrow=n.samples)
  for (i in 1:n.samples){
    for (j in 1:n.samples){
      K[i,j] <- kernel(X[i,], X[j,])
    }
  }
  Dmat <- outer(y[,1],y[,1]) * K
  Dmat <- as.matrix(nearPD(Dmat)$mat) # convert Dmat to nearest pd matrix
  dvec <- rep(1, n.samples)
  if (!is.null(C)) { # soft-margin
    Amat <- rbind(y, diag(n.samples), -1*diag(n.samples))
    bvec <- c(0, rep(0, n.samples), rep(-C, n.samples))
  } else {           # hard-margin
    Amat <- rbind(t(y), diag(n.samples))
    bvec <- c(0, rep(0, n.samples))
  }
  res <- solve.QP(Dmat,dvec,t(Amat),bvec=bvec, meq=1)
  a = res$solution # Lagrange multipliers
  # Support vectors have non-zero Lagrange multipliers
  # ...
  sv = a > 1e-5
  ind = c(1:length(a))[sv]
  results.a = a[sv]

  results.sv <- matrix(nrow = length(ind) ,ncol = n.features )
  h <-0
  for(i in 1:length(sv))
  {
    if(sv[i] == TRUE)
    {
      h=h+1
      results.sv[h,] <- c(X[i,])
    }
  }
 # results.sv = X[sv,]

  results.sv_y = y[sv]
  results.b = 0
  
  for( i in 1:length(results.a))
  {
    results.b = results.b + results.sv_y[i]
    results.b = results.b - sum(results.a * results.sv_y * K[ind[i],sv])
  }

  results.b = results.b /length(results.a)
  results.w <- rep(0,n.features)
  
  for( i in 1:length(results.a))
  {
    results.w[i] = results.w[i] + ((results.a[i] * results.sv_y[i]) * results.sv[i,])
  }
 print(results.w)
  predResults <- vector(length = nrow(testData))
  for(i in 1:nrow(testData) )
  {
    predResults[i] <- rbind(results.w) %*% testData[i,] + results.b
  }
  return(predResults)
}
