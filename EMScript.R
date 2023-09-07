E.step <- function(x, tau, Mu, covariance){  #tau is mixture proportion, Mu is mean S2 is Covariance
  
  x <- as.matrix(x)
  n <- dim(x)[1]
  K <- length(tau)
  p <- dim(x)[2]
  Pi <- matrix(NA, n, K)
  
  for (i in 1:n){
    
    for (k in 1:K){
      Pi[i,k] <- tau[k] * mvtnorm::dmvnorm(t(x[i,]), mean = (Mu[,k]), sigma = covariance[, , k])  #dnorm means normal distribution
      
    } 
    Pi[i,] <- Pi[i,] / sum(Pi[i,])
  }
  return(Pi)
  
}


# Mstep
M.step <- function(x, Pi){
  x <- as.matrix(x)
  K <- dim(Pi)[2]
  n <- dim(x)[1]
  p <- dim(x)[2]
  Mu = matrix(0, nrow = p, ncol = K)
  covariance = array(0, dim = c(p,p,K))
  
  Sum.Pi <- apply(Pi, 2, sum)
  tau <- Sum.Pi / n
  
  for (k in 1:K){
    
    for (i in 1:n){
      Mu[,k] <- Mu[,k] + Pi[i,k] %*% t(x[i,])
    }
    Mu[,k] <- Mu[,k] / Sum.Pi[k]
    
    
    for (i in 1:n){   
      covariance[, , k] <- covariance[, , k] + (Pi[i,k] *  ( as.matrix(x[i,] - Mu[,k])) %*%  t(as.matrix(x[i,] - Mu[,k])))
    }
    covariance[, , k] <- covariance[, , k] / Sum.Pi[k]
    
  }
  
  return(list(tau = tau, Mu = Mu, covariance = covariance))
  
}



# Log Likelihood
logL <- function(x, tau, Mu, covariance){
  
  x <- as.matrix(x)
  n <- dim(x)[1]
  K <- length(tau)
  
  ll <- 0
  
  for (i in 1:n){
    
    ll2 <- 0
    
    for (k in 1:K){
      ll2 <- ll2 + tau[k] * mvtnorm::dmvnorm(t(x[i,]), mean = (Mu[,k]), sigma = covariance[, , k])
    }
    
    ll <- ll + log(ll2)
  }
  
  return(ll)
  
}


# EM Algorithm
EM <- function(x, tau, Mu, covariance, eps){
  
  x <- as.matrix(x)
  K <- length(tau)
  n <- dim(x)[1]
  p <- dim(x)[2]
  b <- 0
  ll.old <- -Inf
  ll <- logL(x, tau, Mu, covariance)
  repeat{
    
    b <- b + 1
    
    if ((ll - ll.old) / abs(ll) < eps) break
    ll.old <- ll
    Pi <- E.step(x, tau, Mu, covariance)
    M <- M.step(x,Pi)
    
    tau <- M$tau
    Mu <- M$Mu
    covariance <- M$covariance
    
    ll <- logL(x, tau, Mu, covariance) 
    #cat("Iteration", b, "logL =", ll, "\n")
  }
  
  id <- apply(Pi, 1, which.max)
  
  M <- 3 * K - 1
  BIC <- -2 * ll + M * log(n)
  AIC <- -2 * ll + M * 2
  
  return(list(tau = tau, Mu = Mu, covariance = covariance, logL = ll, BIC = BIC, Pi = Pi , id = id, AIC = AIC))
  
}

# x <- read.table("/Users/alhajidot/Documents/BGSU/Project/gaussian.txt", quote="\"", comment.char="")
# head(x)



# t_test = c(0.3,0.2,0.5)
# # Mean
# m1 = c(3,1,4,7)
# m2 = c(1,1,6,2)
# m3 = c(3,5,7,6)
# 
# mu_test = matrix(c(m1,m2,m3), nrow = 4, ncol = 3 )
# mu_test
# 
# 
# # Covariance
# e1 = c(2,1,2,5)
# e2 = c(9,5,8,3)
# e3 = c(3,7,3,7)
# 
# cov_e1 <- matrix(cov(as.matrix(e1)), nrow = 1, ncol = 4)
# cov_e1 = as.vector(cov_e1)
# cv_1 = diag(cov_e1, nrow =  4, ncol = 4)
# cv_1
# 
# 
# cov_e2 <- matrix(cov(as.matrix(e2)), nrow = 1, ncol = 4)
# cov_e2 = as.vector(cov_e2)
# cv_2 = diag(cov_e2, nrow =  4, ncol = 4)
# cv_2
# 
# 
# cov_e3 <- matrix(cov(as.matrix(e3)), nrow = 1, ncol = 4)
# cov_e3 = as.vector(cov_e3)
# cv_3 = diag(cov_e3, nrow =  4, ncol = 4)
# cv_3
# 
# 
# cov_test = array(c(cv_1, cv_2, cv_3), dim = c(4,4,3))
# cov_test
# 
# A2 <- EM(x,tau = t_test, Mu = mu_test, covariance = cov_test, eps = 1e-4)
# 
# A2$logL
# A2$BIC
# 
# A2$Pi
# 
# A2$Mu
# A2$covariance
#   