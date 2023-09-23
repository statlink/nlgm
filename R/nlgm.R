nlgm <- function(y, ti, type, ini, ti.ahead = NULL) {

  suppressWarnings({

  if ( type == "richards" ) {
    mod <- nls( y ~ a * ( 1 + k * exp(  g / k * (ti - h) ) ),
                start = list( a = ini[1], k = ini[2], g = ini[3], h = ini[4]),
                nls.control(maxiter = 10000) )
    a <- coef(mod)[1]   ;   k <- coef(mod)[2]
    g <- coef(mod)[3]   ;   h <- coef(mod)[4]
    param <- c(a, b, g, h)
    names(param) <- c("alpha", "beta", "gamma", "eta")

  } else if ( type == "3logistic" ) {
    mod <- nls( y ~ a / ( 1 + exp( - g * (ti - h) ) ),
                start = list( a = ini[1], g = ini[2], h = ini[3]),
                nls.control(maxiter = 10000) )
    a <- coef(mod)[1]   ;   g <- coef(mod)[2]   ;   h <- coef(mod)[3]
    param <- c(a, g, h)
    names(param) <- c("alpha", "gamma", "eta")

  } else if (type == "4logistic" ) {
    mod <- nls( y ~ b + (a - b) / ( 1 + exp( - gama * (ti - eta) ) ),
               start = list( a = ini[1], b = ini[2], gama = ini[3], eta = ini[4]),
               nls.control(maxiter = 10000) )
    a <- coef(mod)[1]   ;   b <- coef(mod)[2]
    g <- coef(mod)[3]   ;   h <- coef(mod)[4]
    param <- c(a, b, g, h)
    names(param) <- c("alpha", "beta", "gamma", "eta")

  } else if ( type == "5logistic" ) {
    mod <- nls( y ~ a + (a0 - a) / ( 1 + ( 2^(1/k) - 1 ) * ( ti / h )^g )^k,
                start = list( a = ini[1], a0 = ini[2], k = ini[3], g = ini[4], h = ini[5]),
                nls.control(maxiter = 10000) )
    a <- coef(mod)[1]   ;   a0 <- coef(mod)[2]
    k <- coef(mod)[3]   ;   g <- coef(mod)[4]   ;   h <- coef(mod)[5]
    param <- c(a, a0, k, g, h)
    names(param) <- c("alpha", "alpha0", "kappa", "gamma", "eta")

  } else if (type == "gompertz") {
    mod <- nls( y ~ a + (a - a0) * exp( -exp(-g * (ti - h) ) ),
                start = list( a = ini[1], a0 = ini[2], g = ini[3], h = ini[4]),
                nls.control(maxiter = 10000) )
    a <- coef(mod)[1]   ;   a0 <- coef(mod)[2]
    g <- coef(mod)[3]   ;   h <- coef(mod)[4]
    param <- c(a, a0, g, h)
    names(param) <- c("alpha", "alpha0", "gamma", "eta")

  } else if (type == "weibull") {
    mod <- nls( y ~ a + (a - a0) * exp( -ti/h ),
                start = list( a = ini[1], a0 = ini[2], h = ini[3]),
                nls.control(maxiter = 10000) )
    a <- coef(mod)[1]   ;   a0 <- coef(mod)[2]   ;   h <- coef(mod)[3]
    param <- c(a, a0, h)
    names(param) <- c("alpha", "alpha0", "eta")

  }  ##  end  if (type == )
  })

  pred <- NULL
  if ( !is.null(ti.ahead) )  pred <- predict(mod, list(ti = max(ti) + 1:ti.ahead) )

  list(mod = mod, param = param, fit = predict(mod), pred = pred)
}
