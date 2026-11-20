# calculate Nb from Waples (2014)
# https://pmc.ncbi.nlm.nih.gov/articles/PMC4063931/

# use equation from Table 3
# increase a --> lower Nb
# increase l --> increase Nb
a = 4 # age at maturity
l = 16 # lifespan
ne = c(52.52, 45.33, 60.85) # Ariana's Ne, 90% CI
nb = ne * (0.485 + (0.758 * log10((l - a + 1) / a)))
# a = 3 > 52.10547 44.97222 60.36972
# a = 4 > 45.85036 39.57343 53.12251

# find good distribution
# use a = 4
sd = (0.5312 - 0.3957) / (2 * 1.645)
mean = 0.4585
curve(dnorm(x, mean = mean, sd = sd), 
      from = 0, to = 1, 
      main = "Standard Normal Distribution", 
      xlab = "Z-score", ylab = "Density", 
      lwd = 2, col = "steelblue")
qnorm(0.05, mean = mean, sd = sd)
qnorm(0.95, mean = mean, sd = sd)
