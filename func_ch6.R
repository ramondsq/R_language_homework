u.conf.int<-function(x, sigma, conf.level = 0.95){
    n = length(x)
    xbar = mean(x)
    a = 1 - conf.level
    ua = qnorm(1 - a / 2)
    Se = sigma / sqrt(n)
    xbar + c(-ua * Se, ua * Se)
}

square <- function(x, conf.level) {
    n = length(x)
    eq = mean(x)
    stan = sd(x)
    down = stan / sqrt(n)
    a = 1 - conf.level
    t = qt(1 - a / 2, n - 1)
    eq + c(-t * down, t * down)
}