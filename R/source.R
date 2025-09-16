

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
#' @examples
#' # Example usage of the Ftilde function
#' y <- c(1, 2, 3, 4, 5)
#' t <- 3.5
#' ystar <- 1
#' Ftilde(y, t, ystar)
#'
#' @export
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
#' @param npop A numeric value representing the population size.
#' @param cores Number of cores to be used for parallel processing. Default is 1.
#' @return A numeric vector of talent values extracted from percentiles.
#'
#' @details
#' The aptitude_nonpara function takes a vector of percentiles `p`, along with optional parameter `npop`,
#' and extracts talent values based on a non-parametric method.
#'
#' The function first converts order statistics to their percentiles using the `order_pbino` function.
#' Then, it transforms these percentiles to Gaussian values corresponding to a general population of a greater than or equal size.
#'
#' The parameter `npop` represents the population size, which is used in the transformation process.
#'In this implementation, the number of cores is set to 1 to ensure the code runs faster by avoiding the overhead
#' associated with parallel processing.
#'
#' @keywords talent, percentiles
#'
#' @export
aptitude_nonpara = function(p, npop, cores = 1){

  #converts order stats to their percentiles
  order_pbino = function(p = 0, k = 1, n = 1e4) {
    pbinom(k - 1, prob = p, size = n, lower.tail = FALSE)
  }

  #converts a vector of order stats
  #to their percentiles. this vector should be the entire
  #sample sorted in increasing order
  order_p = order(p)
  p = sort(p) #just in case
  n = length(p)

  #parallelize the computation of u
  u <- unlist(lapply(1:n, function(j) {
    order_pbino(p[j], k = j, n = n)
  }))

  #transforms percentiles from order stats
  n = length(u)
  #parallelize the computation of latent_talent
  latent_talent <- unlist(lapply(1:n, function(j) {
    qnorm(pbeta(u[j], j + npop - n, n + 1 - j))
  }))

  # match with corresponding original ordering
  latent_talent[order(order_p)]
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
#' @keywords optimization, modeling, linear approximation, order statistics
#'
#' @export
k_finder = function(x, stab = 0.0001) {
  # obtain initial quantities for linear approximation
  Y = sort(as.matrix(x))
  n = length(Y)
  Y[n] = Y[n] + stab # for stability
  pi = 1 - (n:1 - 1/3)/(n + 1/3)
  W = log(pi/(1-pi))
  K1 = max(5, floor(1.3*sqrt(n)))
  K2 = 2*floor(log10(n)*sqrt(n))

  k_selector = try({
    do.call(rbind, lapply(K1:min(c(K1+500,K2,n)), function(k){
      # Following Scholz (1995) Section 4
      Ytil = Y - median(Y)
      Ztil = tail(Ytil, k)
      # Dekkers et al (1989)
      M1k = 1/(k-1) * sum( log(Ztil[2:k]/Ztil[1]) )
      M2k = 1/(k-1) * sum( log(Ztil[2:k]/Ztil[1])^2 )
      ck = M1k + 1 - 0.5*(1 - M1k^2/M2k)^{-1}

      # Gumbel domain of attraction for functional form fck
      # and the matrix Sigma
      fck = ((-n*log(pi))^{-ck} - 1)/ck
      Sigma = matrix(0, k, k)
      for(i in 1:k){
        for(j in 1:i){
          Sigma[i,j] = i^{-ck-1} * j^{-ck}
        }
      }
      for(j in 1:k){
        for(i in 1:(j-1)){
          Sigma[i,j] = j^{-ck-1} * i^{-ck}
        }
      }
      rotate = function(x) t(apply(x, 2, rev))
      Sigma = rotate(rotate(Sigma))
      eig = eigen(Sigma)
      C = eig$vec %*% diag(1/sqrt(eig$val)) %*% t(eig$vec)
      Zk = C %*% tail(Y, k)
      Xk = cbind(1, tail(fck, k))
      Wk =  C %*% Xk

      # try linear and quadratic model as a means of determining k
      m1 = lm(tail(Y, k) ~ tail(fck, k))
      m2 = lm(tail(Y, k) ~ tail(fck, k) + I(tail(fck, k)^2))
      m3 = lm(Zk ~ -1 + Wk)
      delta.sq = summary(m3)$sigma^2
      Tk = coef(m3)[2] / summary(m3)$sigma

      # Following Scholz (1995) Section 5
      kappa.sq = solve(crossprod(Wk))[2,2]
      kappa = sqrt(kappa.sq)
      I0 = c(kappa * qt(0.25, df = k - 2, ncp = 1/kappa),
             kappa * qt(0.75, df = k - 2, ncp = 1/kappa))
      I1 = c(kappa * qt(0.05, df = k - 2, ncp = 1/kappa),
             kappa * qt(0.95, df = k - 2, ncp = 1/kappa))
      I0int = ifelse(I0[1] <= Tk && Tk <= I0[2], 1, 0)
      I1int = ifelse(I1[1] <= Tk && Tk <= I1[2], 1, 0)
      c(k, Tk, I0int, I1int, summary(m1)$adj.r.squared,
        summary(m2)$adj.r.squared)

    }))
  }, silent = TRUE)

  # If the try statement failed, then simply set k as K2 and finish.
  if (inherits(k_selector, "try-error")) {
    k = min(K2, floor(n/2) - 1)
  }
  else {
    # restrict attention to all k values such that Tk in I0
    # (see Section 5 of Scholz (1995) for details).
    # pick k that has best "fit" as judged by the maximum
    # candidate values from the best fitting linear and
    # quadratic models
    colnames(k_selector) = c("k", "Tk", "I0", "I1", "R.sq", "Rquad.sq")
    k_selector = as.data.frame(k_selector)
    k_selector_I0 = k_selector[which(k_selector$I0 == 1), ]
    a = which.max(k_selector_I0$R.sq)
    b = which.max(k_selector_I0$Rquad.sq)
    ind = which.max(c(k_selector_I0[a, ]$R.sq,
                      k_selector_I0[b, ]$Rquad.sq))
    k = k_selector_I0[c(a,b)[ind] , 1]
  }

  c(k, K1, K2)

}


#' compute_ystarstar Function
#'
#' This function computes the optimal value of ystarstar based on input data and a tail
#' probability modeling algorithm described in the Details section.
#'
#' @param x A numeric value representing the input data.
#' @param k A numeric value representing the parameter k
#' @param stab A numeric value representing the stability parameter. Default is 0.0001.
#'
#' @return A list containing the computed optimal value of ystarstar, along with auxiliary information.
#'
#' @details
#' With a model \eqn{h} selected (more on this below), we find \eqn{Y_i^{**}} as the solution
#' of the following optimization problem
#' \deqn{
#' Y_i^{**} = \text{argmin}_y|h^{-1}(Y_{i,(n_i)}) - \tilde{F}_{Y_i}(Y_{i,(n_i)};y)|,
#' }
#' where \eqn{\tilde{F}_{Y_i}(\cdot;y)} is \eqn{\tilde{F}_{Y_i}} with \eqn{y}
#' replacing \eqn{Y_i^{**}} in its construction.
#'
#' We select a tail probability model via the following steps:
#' 1. Fit the logistic and logistic quadratic models. Use BIC to choose among these models.
#' 2. Stop if the above optimization problem can be solved.
#' 3. If the above optimization problem cannot be solved, then fit the log-logistic and log-logistic quadratic models. Use BIC to choose among these models.
#' 4. Stop if the above optimization problem  can be solved.
#' 5. If the above optimization problem cannot be solved, then fit the logistic cubic model.
#'
#' @keywords optimization, modeling, linear approximation, order statistics
#'
#' @export
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
  #plot(tail(W, k), tail(Y, k))
  #lines(tail(W, k), predict(selected_model))
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
    #plot(tail(W, k), tail(Y, k))
    #lines(tail(W, k), predict(selected_model))
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
    #plot(log(tail(W, k)), tail(Y, k))
    #lines(log(tail(W, k)), predict(selected_model))
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
      #plot(tail(W, k), tail(Y, k))
      #lines(tail(W, k), predict(selected_model))
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
    #plot(tail(W, k), tail(Y, k))
    #lines(tail(W, k), predict(selected_model))
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

#' talent_computing_nonpara
#'
#' Estimate underlying talent values using a non-parametric method.
#'
#' @param ystar Numeric. Additional parameter for talent estimation.
#' @param y Numeric vector. Observed statistic, arranged from highest to lowest.
#' @param npop Numeric. Population size.
#'
#' @return Numeric vector of estimated talent values.
#'
#' @details
#' Computes talent values for `y` using `Ftilde()` and `aptitude_nonpara()`.
#'
#' @export
talent_computing_nonpara = function(ystar, y, npop){
  ## latent talent
  latent_talent = aptitude_nonpara(p = unlist(lapply(y, function(xx)
    Ftilde(y = y, t = xx, ystar = ystar))), npop = npop)
}
