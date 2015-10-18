library(ptw)

plot(a, type = "l", ylim = c(0, 25000))
a.blc = baseline.corr(a)
lines(a.blc, col = 2)
plot(a.blc, type = "l", ylim = c(0, 25000))

data(gaschrom)
ref <- gaschrom[1,]
samp <- gaschrom[16,]
gaschrom.ptw <- ptw(ref, samp)
summary(gaschrom.ptw)

## same with sticks (peak lists)
refst <- gaschrom.st[1]
sampst <- gaschrom.st[16]
gaschrom.st.ptw <- stptw(refst, sampst, trwdth = 100)
summary(gaschrom.st.ptw)
