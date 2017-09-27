bubblesort <- function(x) {
# x is initially the input vector and will be
# modified to form the output
if (length(x) < 2) return (x)
# last is the last element to compare with
for(last in length(x):2) {
for(first in 1:(last - 1)) {
if(x[first] > x[first + 1]) { # swap the pair
save <- x[first]
x[first] <- x[first + 1]
x[first + 1] <- save
}
}
}
return (x)
}
