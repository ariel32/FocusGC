rm(list = ls())
library(ptw)
source("datfile_reader.r")

a <- ch.update("baselinecorrection.dat")
a <- a[20:length(a)]
plot(a, type = "l", ylim = c(0, 5000))

a.blc = baseline.corr(a)
lines(a.blc, col = 2)
lines(difsm(a, lambda = 1e5), col = 2)

ch.write(file.src = "baselinecorrection.dat", file.dst = "0.dat", a.blc)

new.ch <- c(a[1:10000]-median(a[1:10000]), a.blc[10001:length(a.blc)])
ch.write(file.src = "baselinecorrection.dat", file.dst = "0.dat", new.ch)
