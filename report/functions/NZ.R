NZ <- function(x) {
  # Simple function to convert NA to zero

  ifelse(is.na(x),
         0,
         x)

}
