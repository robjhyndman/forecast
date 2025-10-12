# Author: srazbash and Rob J Hyndman
###############################################################################

checkAdmissibility <- function(
  opt.env,
  box.cox = NULL,
  small.phi = NULL,
  ar.coefs = NULL,
  ma.coefs = NULL,
  tau = 0,
  bc.lower = 0,
  bc.upper = 1
) {
  # Check the range of the Box-Cox parameter
  if (!is.null(box.cox)) {
    if ((box.cox <= bc.lower) || (box.cox >= bc.upper)) {
      return(FALSE)
    }
  }
  # Check the range of small.phi
  if (!is.null(small.phi)) {
    if (((small.phi < .8) || (small.phi > 1))) {
      return(FALSE)
    }
  }
  # Check AR part for stationarity
  if (!is.null(ar.coefs)) {
    arlags <- which(abs(ar.coefs) > 1e-08)
    if (length(arlags) > 0L) {
      p <- max(arlags)
      if (min(Mod(polyroot(c(1, -ar.coefs[1L:p])))) < 1 + 1e-2) {
        return(FALSE)
      }
    }
  }
  # Check MA part for invertibility
  if (!is.null(ma.coefs)) {
    malags <- which(abs(ma.coefs) > 1e-08)
    if (length(malags) > 0L) {
      q <- max(malags)
      if (min(Mod(polyroot(c(1, ma.coefs[1L:q])))) < 1 + 1e-2) {
        return(FALSE)
      }
    }
  }
  # Check the eigen values of the D matrix
  D.eigen.values <- eigen(
    opt.env$D,
    symmetric = FALSE,
    only.values = TRUE
  )$values

  all(abs(D.eigen.values) < 1 + 1e-2)
}
