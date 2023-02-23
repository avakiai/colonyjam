recolor <- function(x, which = NULL) {
  if (exp == "exp_1") {
    if (is.null(which)) {
      y <- x + 
        scale_fill_manual(values = pal) +
        scale_color_manual(values = pal)
    } else if (which=="c") {
      y <- x + 
        scale_color_manual(values = pal)
    } else if (which=="f") {
      y <- x + 
        scale_fill_manual(values = pal) }
  } else {
    y <- x
  }
  return(y)
}