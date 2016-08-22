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

#############################################

install.packages("forecast")
library(forecast)
library(stats)

set.seed(4)
data <- read.csv("D:/webTraffic.csv", sep = ",", header = T)
days = as.numeric(data$Visite)
for (i in 1:45 ) {
  pos = floor(runif(1, 1, 50))
  days[i*15+pos] = days[i*15+pos]^1.2
}
days[510+pos] = 0

trend = runmed(days, 7)

detrend = days / as.vector(trend)
m = t(matrix(data = detrend, nrow = 7))
seasonal = colMeans(m, na.rm = T)
random = days / (trend * seasonal)
rm_random = runmed(random[!is.na(random)], 3)

min = mean(rm_random, na.rm = T) - 4*sd(rm_random, na.rm = T)
max = mean(rm_random, na.rm = T) + 4*sd(rm_random, na.rm = T)
plot(as.ts(random))
abline(h=max, col="#e15f3f", lwd=2)
abline(h=min, col="#e15f3f", lwd=2)