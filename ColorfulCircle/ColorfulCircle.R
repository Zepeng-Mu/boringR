library(plotrix)

# This number can be changed from 1 to 99
jump = 30

plot(-2:2, -2:2, type  = "n", axes = F, xlab = NA, ylab = NA, asp = 1)
myCircle = draw.circle(0, 0, 2, border = "grey", lwd = 2)
myCol = rainbow(length(myCircle$x))
for (i in 1:length(myCircle$x)) {
  print(i+jump)
  if (i + jump <= length(myCircle$y)) {
    lines(x = myCircle$x[c(i, i + jump)],
          y = myCircle$y[c(i, i + jump)],
          col = myCol[i],
          lwd = 0.8)
  } else {
    lines(x = myCircle$x[c(i, i + jump - length(myCircle$y))],
          y = myCircle$y[c(i, i + jump - length(myCircle$y))],
          col = myCol[i],
          lwd = 0.8)
  }
}
