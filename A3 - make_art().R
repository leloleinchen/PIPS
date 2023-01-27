
# 3.2.1b

# Leonie Sindern
# 12696781


library("ggplot2")
library("devtools")


make_art <- function (myseed){
  
  set.seed(myseed)
  
  
  mydf <- data.frame(
    
    x = rep(x = seq(from = 0.1, to = 1.9, by = 0.2), 2),
    y = rep(x = c(0.5:1.5), each = 10),
    z = factor(rep(sample(1:100, 10, replace = FALSE), each = 2)),
    
    s = rnorm(n = 20,sd = 6, mean = 20),
    
    theta = runif(100, 0,2*pi),
    a = 0.8*cos(theta) + rnorm(100, 1, 0.04),
    b = 0.8*sin(theta) + rnorm(100, 1, 0.04),
    
    c = 0.5*cos(theta) + rnorm(100, 1, 0.04),
    d = 0.5*sin(theta) + rnorm(100, 1, 0.04),
    
    e = 0.2*cos(theta) + rnorm(100, 1, 0.04),
    f = 0.2*sin(theta) + rnorm(100, 1, 0.04)
    
  )
  
  ggplot(mydf, aes(x = x, y = y)) +
    geom_tile(aes(fill = z)) +
    geom_point(data=mydf, aes(x=a, y=b, size = s),color = "white", alpha = 0.5) +
    geom_point(data=mydf, aes(x=c, y=d, size = s),color = "lightgrey", alpha = 0.5) +
    geom_point(data=mydf, aes(x=e, y=f, size = s),color = "grey", alpha = 0.5) +
    theme_void() +
    theme(legend.position = "none")
  
}

myart(430764)



