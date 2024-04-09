

#' Ftilde Function
#'
#' This function computes a transformed value based on the input parameters.
#'
#' @param y A numeric vector representing the input data.
#' @param t A numeric value indicating the target value.
#' @param ystar A numeric value representing an additional parameter.
#'
#' @return A transformed value computed based on the input parameters.
#'
#' @details
#' The Ftilde function computes a transformed value based on the input data vector `y`, a target value `t`, and an additional parameter `ystar`.
#'
#' The algorithm sorts the input vector `y` and computes a new vector `ytilde` using a specific formula.
#' Then, it computes the transformed value based on the relationship between `t` and the elements of `ytilde`.
#'
#' If `t` is greater than or equal to the maximum value in `ytilde`, the function returns a constant value.
#' If `t` is less than or equal to the minimum value in `ytilde`, the function returns 0.
#' Otherwise, it computes the transformed value based on the position of `t` relative to the elements of `ytilde`.
#'
#' @keywords transformation, algorithm, vector
#'
#' @export
#'
#' @examples
#' # Example usage of the Ftilde function
#' y <- c(1, 2, 3, 4, 5)
#' t <- 3.5
#' ystar <- 1
#' Ftilde(y, t, ystar)
#'
## interpolated distribution function estimator
Ftilde = function(y, t, ystar){
  y = sort(y)
  n = length(y)
  ytilde = rep(0, n + 1)

  ytilde[n+1] = y[n] + ystar
  ytilde[2:n] = unlist(lapply(2:n, function(j){
    (y[j]+y[j-1])/2
  }))

  if (t >= ytilde[n+1]) {
    1 - 0.1^7
  } else if (t <= ytilde[1]) {
    0
  } else {
    j = length(which(ytilde < t))
    (j - 1) / n + (t - ytilde[j]) / (n*(ytilde[j+1] - ytilde[j]))
  }

}

#' aptitude_nonpara Function
#'
#' This function extracts talent values from percentiles.
#'
#' @param p A numeric vector representing percentiles of talent values.
#' @param alpha A numeric value representing the shape parameter of the Pareto distribution. Default is 1.16.
#' @param npop A numeric value representing the population size.
#'
#' @return A numeric vector of talent values extracted from percentiles.
#'
#' @details
#' The aptitude_nonpara function takes a vector of percentiles `p`, along with optional parameters `alpha` and `npop`,
#' and extracts talent values based on a non-parametric method.
#'
#' The function first converfs order statistics to their percentiles using the `order_pbino` function.
#' Then, it transforms these percentiles to Pareto values corresponding to a general population of a greater than or equal size.
#'
#' The parameter `alpha` represents the shape parameter of the Pareto distribution, with a default value of 1.16.
#' The parameter `npop` represents the population size, which is used in the transformation process.
#'
#' @keywords talent, percentiles
#'
#' @export
#'
#' @examples
#'
#'
## This function extracts talent values from percentiles
aptitude_nonpara = function(p, alpha = 1.16, npop){

  # converts order stats to their percentiles
  order_pbino = function(p = 0, k = 1, n = 1e4){
    pbinom(k - 1, prob = p, size = n, lower.tail = FALSE)
  }

  # converts a vector of order stats
  # to their percentiles. This vector should be the entire
  # sample sorted in increasing order
  p = sort(p) # just in case
  n = length(p)
  u = unlist(lapply(1:n, function(j){
    order_pbino(p[j], k = j, n = n)
  }))

  # transforms percentiles from order stats (in increasing order)
  # to Pareto values corresponding to the general population
  # of a greater than or equal to size
  # default alpha is that of the Pareto principle 80-20
  n = length(u)
  #if(length(npop) == 1) npop = rep(npop, n)
  unlist(lapply(1:n, function(j){
    qPareto(qbeta(u[j], j + npop -n , n + 1 - j), t = 1, alpha = alpha)
  }))
}

#' k_finder Function
#'
#' This function finds the optimal value of k based on input data
#'
#' @param x A numeric vector representing the input data
#' @param stab A numeric value representing the stability parameter
#'
#' @return A numeric vector containing the optimal value of k, along with values K1 and K2
#'
#' @details
#' The k_finder function computes the optimal value of k based on the input vector `x` and a stability parameter `stab`.
#' It performs linear and quadratic modeling to evaluate the optimal value of k that maximizes the explained variance in the data.
#'
#' The function initializes quantities for linear approximation and then evaluates various values of k within specific ranges.
#' It uses a combination of linear and quadratic models to estimate the optimal value of k
#'
#' @keywords optimal value, modeling, linear approximation
#'
#' @export
#'
#' @examples
#'
##
 
library(dplyr) 
k_finder_optimized <- function(x, stab = 0.0001) {
  Y <- sort(as.matrix(x))
  n <- length(Y)
  Y[n] <- Y[n] + stab
  
  pi <- 1 - (n:1 - 1/3)/(n + 1/3)
  W <- log(pi/(1-pi))
  K1 <- max(5, floor(1.3 * sqrt(n)))
  K2 <- 2 * floor(log10(n) * sqrt(n))
  
  k_selector <- mclapply(K1:min(c(K1 + 500, K2, n)), function(k) {
    Ytil <- Y - median(Y)
    Ztil <- tail(Ytil, k)
    M1k <- 1/(k - 1) * sum(log(Ztil[2:k]/Ztil[1]))
    M2k <- 1/(k - 1) * sum(log(Ztil[2:k]/Ztil[1])^2)
    ck <- M1k + 1 - 0.5 * (1 - M1k^2/M2k)^{-1}
    fck <- ((-n * log(pi))^(-ck) - 1)/ck
    
    Sigma <- outer(1:k, 1:k, function(i, j) {
      min(i, j)^(-ck)
    })
    Sigma <- t(apply(Sigma, 1, rev))
    
    eig <- eigen(Sigma)
    C <- eig$vec %*% diag(1/sqrt(eig$val)) %*% t(eig$vec)
    Zk <- C %*% tail(Y, k)
    Xk <- cbind(1, tail(fck, k))
    Wk <- C %*% Xk
    
    m3 <- lm(Zk ~ -1 + Wk)
    Tk <- coef(m3)[2] / summary(m3)$sigma
    
    kappa <- sqrt(solve(crossprod(Wk))[2,2])
    I0 <- kappa * qt(c(0.25, 0.75), df = k - 2, ncp = 1/kappa)
    I1 <- kappa * qt(c(0.05, 0.95), df = k - 2, ncp = 1/kappa)
    I0int <- as.integer(Tk >= I0[1] & Tk <= I0[2])
    I1int <- as.integer(Tk >= I1[1] & Tk <= I1[2])
    
    c(k, Tk, I0int, I1int, summary(m3)$sigma^2)
  })
  
  k_selector <- do.call(rbind, k_selector)
  colnames(k_selector) <- c("k", "Tk", "I0", "I1", "delta_sq")
  
  k_selector <- as.data.frame(k_selector) %>%
    filter(I0 == 1) %>%
    mutate(R.sq = summary(lm(Tk ~ k))$adj.r.squared,
           Rquad.sq = summary(lm(Tk ~ k + I(k^2)))$adj.r.squared)
  
  ind <- which.max(c(k_selector$R.sq, k_selector$Rquad.sq))
  k <- k_selector[ind, "k"]
  
  return(list(k = k, K1 = K1, K2 = K2))
}



#' compute_ystarstar Function
#'
#' This function computes the optimal value of ystarstar based on input data
#'
#' @param x A numeric value representing the input data.
#' @param k A numeric value representing the parameter k
#' @param stab A numeric value representing the stability parameter. Default is 0.0001.
#'
#' @return A list containing the computed optimal value of ystarstar, along with auxiliary information.
#'
#' @details
#' The compute_ystarstar function computes the optimal value of ystarstar based on the input vector `x`,
#' parameter `k`, and stability parameter `stab`
#'
#' The function performs various linear and quadratic modeling techniques to estimate the optimal value of ystarstar
#' that maximizes the explained variance in the data
#'
#' @keywords
#'
#' @export
#'
#' @examples
#'
compute_ystarstar = function(x, k, stab = 0.0001) {
  Y = sort(as.matrix(x))
  n = length(Y)
  Y[n] = Y[n] + stab # for stability
  pi = 1 - (n:1 - 1/3)/(n + 1/3)
  X = W = log(pi/(1-pi))
  ystar = 10; ub = 0.999

  models = list(
    m1 = lm(tail(Y, k) ~ tail(W, k)),
    m2 = lm(tail(Y, k) ~ tail(W, k) + I(tail(W, k)^2))
  )

  models = models[names(sort(sapply(models, BIC)))]
  selected_model = models[[1]]
  f = function(w) {
    max(Y) - predict(selected_model, newdata = data.frame(W = w))
  }
  flag = try({
    ub_w = uniroot(f, c(mean(c(tail(W, 2)[1], max(W))), max(W)+2), tol = 1e-10)$root
    ub = 1/(1 + exp(-ub_w))
  }, silent = TRUE)

  if(class(flag) != "try-error"){
    try({
      g = function(ystar) ub - Ftilde(y = Y, t = max(Y), ystar = ystar)
      bar = uniroot(g, c(0, 100), tol = 1e-10)
      ystar = bar$root
    }, silent = TRUE)
  }

  if(ystar == 10) {
    selected_model = models[[2]]
    f = function(w) {
      max(Y) - predict(selected_model, newdata = data.frame(W = w))
    }
    flag = try({
      ub_w = uniroot(f, c(mean(c(tail(W, 2)[1], max(W))), max(W)+2), tol = 1e-10)$root
      ub = 1/(1 + exp(-ub_w))
    }, silent = TRUE)

    if(class(flag) != "try-error"){
      try({
        g = function(ystar) ub - Ftilde(y = Y, t = max(Y), ystar = ystar)
        bar = uniroot(g, c(0, 100), tol = 1e-10)
        ystar = bar$root
      }, silent = TRUE)
    }
  }

  models = list(
    m1 = lm(tail(Y, k) ~ log(tail(W, k))),
    m2 = lm(tail(Y, k) ~ log(tail(W, k)) + I(log(tail(W, k))^2))
  )

  if(any(BIC(selected_model) > sapply(models, BIC))) {
    models = models[names(sort(sapply(models, BIC)))]
    selected_model = models[[1]]
    f = function(w) {
      max(Y) - predict(selected_model, newdata = data.frame(W = w))
    }
    flag = try({
      ub_w = uniroot(f, c(max(W)-0.5, max(W)+2), tol = 1e-10)$root
      ub = 1/(1 + exp(-ub_w))
    }, silent = TRUE)

    if(class(flag) != "try-error"){
      try({
        g = function(ystar) ub - Ftilde(y = Y, t = max(Y), ystar = ystar)
        bar = uniroot(g, c(0, 100), tol = 1e-10)
        ystar = bar$root
      }, silent = TRUE)
    }
    if(ystar == 10) {
      selected_model = models[[2]]
      f = function(w) {
        max(Y) - predict(selected_model, newdata = data.frame(W = w))
      }
      flag = try({
        ub_w = uniroot(f, c(mean(c(tail(W, 2)[1], max(W))), max(W)+2), tol = 1e-10)$root
        ub = 1/(1 + exp(-ub_w))
      }, silent = TRUE)

      if(class(flag) != "try-error"){
        try({
          g = function(ystar) ub - Ftilde(y = Y, t = max(Y), ystar = ystar)
          bar = uniroot(g, c(0, 100), tol = 1e-10)
          ystar = bar$root
        }, silent = TRUE)
      }
    }
  }

  if(ystar == 10 ) {
    selected_model = lm(tail(Y, k) ~ tail(W, k) + I(tail(W, k)^2) +
                          I(tail(W, k)^3))
    f = function(w) {
      max(Y) - predict(selected_model, newdata = data.frame(W = w))
    }
    flag = try({
      ub_w = uniroot(f, c(mean(c(tail(W, 2)[1], max(W))), max(W)+2), tol = 1e-10)$root
      ub = 1/(1 + exp(-ub_w))
    }, silent = TRUE)

    if(class(flag) != "try-error"){
      try({
        g = function(ystar) ub - Ftilde(y = Y, t = max(Y), ystar = ystar)
        bar = uniroot(g, c(0, 100), tol = 1e-10)
        ystar = bar$root
      }, silent = TRUE)
    }
  }

  ## output
  out = list(ystar = ystar,
             model = selected_model,
             Y = tail(Y, k),
             pi = tail(pi, k),
             W = tail(W, k))
  out
}

#' talent_computing_nonpara Function
#'
#' This function estimates underlying talent values using a non-parametric method.
#'
#' @param ystar A numeric value representing an additional parameter for talent estimation.
#' @param y A numeric vector representing the observed statistic under study. This vector will be arranged from highest to lowest.
#' @param npop A numeric value representing the population size.
#' @param alpha A numeric value representing the shape parameter of the Pareto distribution. Default is 1.16.
#'
#' @return A vector containing estimated talent values based on the inputs.
#'
#' @details
#' The talent_computing_nonpara function estimates underlying talent values using a non-parametric approach based on the input data.
#'
#' It computes talent values for the input vector `y` using the Ftilde and aptitude_nonpara functions.
#' The parameter `ystar` is an additional parameter used in talent estimation.
#'
#' @keywords talent estimation, non-parametric, Pareto distribution
#'
#' @export
#'
#' @examples
#'
## This function estimates underlying talent values
talent_computing_nonpara = function(ystar, y, npop, alpha = 1.16){

  ## make sure that y is arranged from highest to lowest
  y = sort(y, decreasing = TRUE)

  ## latent talent
  latent_talent = aptitude_nonpara(p = unlist(lapply(y, function(xx)
    Ftilde(y = y, t = xx, ystar = ystar))), npop = npop)
  sort(latent_talent, decreasing = TRUE)
}


