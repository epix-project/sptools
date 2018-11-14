# this function moves the coordinates to the first two variables of the df:
move_xy <- function(df) {
  nc <- ncol(df)
  sel <- c(nc - 1, nc)
  cbind(df[, sel], df[, -sel])
}
