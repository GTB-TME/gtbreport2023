
test.bounds <- function(best, lo, hi) {
  stopifnot(sum(is.na(best)) == 0)
  stopifnot(sum(is.na(lo)) == 0)
  stopifnot(sum(is.na(hi)) == 0)
  stopifnot(best >= lo)
  stopifnot(best <= hi)
  invisible(T)
}

test.ispos <- function(var) {
  stopifnot(sum(is.na(var)) == 0)
  stopifnot(var >= 0)
  invisible(T)
}

test.AgB <- function(A, B) {
  stopifnot(sum(is.na(A)) == 0)
  stopifnot(sum(is.na(B)) == 0)
  stopifnot(A > B)
  invisible(T)
}

test.AgeB <- function(A, B) {
  stopifnot(sum(is.na(A)) == 0)
  stopifnot(sum(is.na(B)) == 0)
  stopifnot(A >= B)
  invisible(T)
}

test.isbinom <- function(var) {
  test.AgeB(1, var) && test.AgeB(var, 0)
}


## returns Beta shape and scale params using the method of moments
##
get.beta <- function(ev, sd) {
  #' @param ev expected value.
  #' @param sd standard deviation.
  #' @export
  stopifnot(ev > 0 & ev < 1)
  stopifnot(sd > 0)

  S = (ev * (1 - ev) / sd ^ 2) - 1
  if (S < 0)
    stop('Not distributed Beta: sd^2 >= ev*(1-ev)')

  a = S * ev
  b = S * (1 - ev)
  return(c(a = a, b = b))
}

## generate low and high bounds assuming Beta distribution
lohi <- function(ev, sd) {
  #' @param ev expected value.
  #' @param sd standard deviation.
  #' @export
  stopifnot(ev > 0 & ev < 1)
  stopifnot(sd > 0)

  par <- get.beta(ev, sd)
  lo <- qbeta(0.025, par[1], par[2])
  hi <- qbeta(0.975, par[1], par[2])
  return(c(lo = lo, hi = hi))
}

vlohi <- Vectorize(lohi, c('ev', 'sd'))

cii <- function (size, x, precision, alpha = 0.05)
{
  success <- x
  if (missing(size)) {
    success1 <- success
    if (min(success, na.rm = TRUE) != 0 |
        max(success, na.rm = TRUE) !=
        1) {
      stop("This is not a binary vector.")
    }
    success <- length(na.omit(success1)[na.omit(success1) >
                                          0])
    size <- length(na.omit(success1))
  }
  reverse <- rep(FALSE, length(success))
  reverse[success / size > 0.5] <- TRUE
  success[reverse] <- size[reverse] - success[reverse]
  if (missing(precision)) {
    precision <- success / size / 10000
  }
  precision[success == 0 |
              success == size] <- 0.01 / size[success ==
                                                0 |
                                                success == size]
  probab <- success / size
  success1 <- success
  success1[success > 0] <- success[success > 0] - 1
  for (i in 1:length(success)) {
    while (pbinom(success1[i], size[i], probab[i], lower.tail = FALSE) >
           alpha / 2) {
      probab[i] <- probab[i] - precision[i]
    }
  }
  estimate <- success / size
  se <- sqrt(estimate * (1 - estimate) / size)
  ll <- probab
  probab <- success / size
  for (i in 1:length(success)) {
    while (pbinom(success[i], size[i], probab[i], lower.tail = TRUE) >
           alpha / 2) {
      probab[i] <- probab[i] + precision[i]
    }
  }
  ul <- probab
  data.frame.a <- data.frame(
    events = success,
    total = size,
    prob = estimate,
    se = se,
    ll = ll,
    ul = ul
  )
  data.frame.a[reverse, ] <- data.frame(
    events = size[reverse] -
      success[reverse],
    total = size[reverse],
    prob = 1 -
      estimate[reverse],
    se = se[reverse],
    ll = 1 - ul[reverse],
    ul = 1 - ll[reverse]
  )
  names(data.frame.a)[5] <- paste("lower", 100 * (1 -
                                                    alpha), "ci", sep = "")
  names(data.frame.a)[6] <- paste("upper", 100 * (1 -
                                                    alpha), "ci", sep = "")
  if (nrow(data.frame.a) == 1) {
    rownames(data.frame.a) <- ""
  }
  data.frame.a
}
