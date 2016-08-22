install.packages("FBN")
library(FBN)

set.seed(4)
data <- read.csv("D:/webTraffic.csv", sep = ",", header = T)
days = as.numeric(data$Visite)
for (i in 1:45 ) {
  pos = floor(runif(1, 1, 50))
  days[i*15+pos] = days[i*15+pos]^1.2
}
days[510+pos] = 0

decomposed_days = decompose(ts(days, frequency = 7), "multiplicative")

random = decomposed_days$random
min = mean(random, na.rm = T) - 4*sd(random, na.rm = T)
max = mean(random, na.rm = T) + 4*sd(random, na.rm = T)

plot(as.ts(as.vector(random)), ylim = c(-0.5,2.5))
abline(h=max, col="#e15f3f", lwd=2)
abline(h=min, col="#e15f3f", lwd=2)