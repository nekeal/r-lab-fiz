S1 <- c(0.743, 0.661, 0.643, 0.669, 0.689, 0.709, 0.670)
S2 <- c(1.128, 0.985, 0.962, 1.08, 0.996, 1.072, 0.998)
S3 <- c(1.381, 1.257, 1.216, 1.324, 1.26, 1.347, 1.275)
S4 <- c(1.611, 1.448, 1.452, 1.537, 1.514, 1.557, 1.515)
S5 <- c(1.791, 1.685, 1.643, 1.728, 1.707, 1.773, 1.685)
S6 <- c(1.947, 1.858, 1.801, 1.914, 1.875, 1.939, 1.861)
S7 <- c(2.124, 2, 1.977, 2.082, 2.043, 2.107, 2.017)
S <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7)
h <- 0.033
l <- 0.86
s <- 0.7
sina <- h/l
data <- data.frame(S1, S2, S3, S4, S5, S6, S7)
colnames(data) <- c('S1', 'S2', 'S3', 'S4', 'S5', 'S6', 'S7')
mean_t_square <- unname(colMeans(data))^2

plot(mean_t_square, 2*S, main='2S=at^2', ylab='2S [m]', xlab='t^2', ylim=c(0,1.5), xlim=c(0,4))

linear_regression <- lm(2*S ~  -1 + mean_t_square )
abline(linear_regression, col='red')

print(summary(linear_regression))
View(mean_t_square)
View(data)
print(sprintf('g = %f', linear_regression$coefficients*l/h))

