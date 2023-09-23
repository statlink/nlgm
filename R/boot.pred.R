boot.pred <- function(mod, type, ti, ti.ahead = 10, B = 1000, conf = 0.95, seed = NULL) {
  fit <- mod$fit
  n <- length(fit)
  pred <- matrix(nrow = ti.ahead, ncol = B)
  if ( !is.null(seed) )  set.seed(seed)
  suppressWarnings({
  for (i in 1:B) {
    bootyi <- rpois(n, fit)
    pred[, i] <- nlgm::nlgm(bootyi, ti, type, ini = mod$param, ti.ahead = ti.ahead)$pred
  }
  })
  mp <- Rfast::rowmeans(pred)
  a <- (1 - 0.95)
  ci <- Rfast2::rowQuantile(pred, probs = c(a/2, 1 - a/2) )
  est <- cbind(mp, ci)
  colnames(est) <- c("boot mean", paste( c(a/2, 1 - a/2), "%", sep = "" ) )
  list(est = est, pred = pred)
}
