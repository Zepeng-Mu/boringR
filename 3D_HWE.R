library(dplyr)
library(lattice)

stepVec = seq(0, 1, by = 0.01)
a1Vec = rep(stepVec, times = length(stepVec))
a2Vec = rep(stepVec, each = length(stepVec))


freqDf = as.data.frame(cbind(a1 = a1Vec, a2 = a2Vec))
freqDf = filter(freqDf, a1 + a2 <= 1)
freqDf$a3 = 1  - freqDf$a1 - freqDf$a2
freqDf$a1_a1 = freqDf$a1 ^ 2
freqDf$a2_a2 = freqDf$a2 ^ 2
freqDf$a3_a3 = freqDf$a3 ^ 2
freqDf$a1_a2 = freqDf$a1 * freqDf$a2 * 2
freqDf$a1_a3 = freqDf$a1 * freqDf$a3 * 2
freqDf$a2_a3 = freqDf$a2 * freqDf$a3 * 2

a1 = rep(freqDf$a1, 6)
a2 = rep(freqDf$a2, 6)
z = c(freqDf$a1_a1, freqDf$a2_a2, freqDf$a3_a3, freqDf$a1_a2, freqDf$a1_a3, freqDf$a2_a3)
plotDf = as.data.frame(cbind(a1, a2, z))
plotDf$genotype = factor(rep(c("A1A1", "A2A2", "A3A3", "A1A2", "A1A3", "A2A3"), each = nrow(freqDf)))
wireframe(a1_a1 ~ a1 * a2,
          data = freqDf,
          zlim = c(0, 1),
          xlim = c(0, 1),
          ylim = c(0, 1),
          aspect=c(1,1),
          screen = list(z = 70, y = 0, x = -90),
          shade = T
          )
plotDf = filter(plotDf, a1+a2 <=1)

wireframe(z ~ a2 * a1 | genotype,
          data = filter(plotDf, genotype %in% c("A1A1", "A1A2", "A1A3", "A3A3")),
          #group = genotype,
          shade = T,
          col = NA,
          #col.group = rainbow(6),
          zlim = c(0, 1),
          xlim = c(0, 1),
          ylim = c(0, 1),
          xlab = list("P(A1)", rot = 0),
          ylab = list("P(A2)", rot = 90),
          cex.lab = 0.6,
          scales = list(arrows = FALSE, col = "black",font = 8, cex = 0.5),
          screen = list(z = 0, y = 0, x = 0)
          )

sorted = freqDf[order(freqDf$a1), ]

