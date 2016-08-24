library(splines)
library(dplyr)
library(reshape2)
library(ggplot2)

N <- 1e2
x <- runif(N)*0.9 + 0.05
dat <- bs(x, 7, Boundary.knots = c(0, 1)) %>% 
  as.data.frame() %>% 
  mutate(id = seq(N), x = x) %>% 
  melt(id.vars = c("id", "x"), variable.name = "h", value.name = "value")

ggplot(dat, aes(x, value, color = h)) + geom_line(size = 1.2) + xlim(c(0, 1))


Sig <- solve(cov(bs(x, 7, Boundary.knots = c(0, 1)))) / N

a <- bs(seq(0, 1, 0.01), 7) 

diag(a %*% diag(diag(Sig)) %*% t(a)) %>% plot()



# following code (qualitatively) reprodce Fig 5.3

N <- 1e+3
x <- runif(N)
y <- rnorm(N)

# cbic spline fits
h <- bs(x, 7, Boundary.knots = c(0, 1))
dat <- data.frame(h)
dat <- cbind(dat, y)
fit <- lm(y ~ ., data = dat)
newX <- seq(0, 1, 0.01)
newH <- bs(newX, 7, Boundary.knots = c(0, 1))
newdata <- data.frame(newH)
pred <- predict(fit, newdata, interval = "prediction")
plot(newX, pred[,3] - pred[,2], type = "l")

# lm fits
fit <- lm(y ~ x)
pred <- predict(fit, newdata = data.frame(x = newX), interval = "prediction")
lines(newX, pred[,3] - pred[,2], col = "blue")

# global cube
fit <- lm(y ~ poly(x, 3))
pred <- predict(fit, newdata = data.frame(x = newX), interval = "prediction")
lines(newX, pred[,3] - pred[,2], col = "red")

# natural cubic spline
n <- ns(x, 7, Boundary.knots = c(0, 1))
dat <- data.frame(n)
fit <- lm(y ~ ., data = dat)
newN <- ns(newX, 7, Boundary.knots = c(0, 1))
newdata <- data.frame(newN)
pred <- predict(fit, newdata, interval = "prediction")
lines(newX, pred[,3] - pred[,2], col = "green")



# Example 5.2.3
x <- read.csv("../Desktop/p.csv", as.is = TRUE)
x <- filter(x, g %in% c("aa", "ao"))
x <- select(x, -speaker)
x1 <- x[sample(seq(nrow(x)), 1000), ]

dat <- melt(x1, id.vars = c("row.names", "g"))
dat$f <- sub("[^0-9]+", "", dat$variable) %>% as.integer()
ggplot(dat, aes(f, value)) + 
  geom_point(alpha = 0.01) +
  facet_grid(g ~.)  

# generate natural cubic spline
H <- ns(1:256, 12)
Xf <- as.matrix(x1[, paste("x", 1:256, sep = ".")])
Hx <- Xf %*% H


  