# https://stackoverflow.com/questions/10296866/finding-the-column-number-and-value-the-of-second-highest-value-in-a-row

# a function that returns the position of n-th largest
maxn <- function(n) function(x) order(x, decreasing = TRUE)[n]
