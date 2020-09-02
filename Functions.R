
# Writing functions

# raising 2 to the 3rd power
Power <- function()
{
  print(2^3)
  
}

# raising x to the ath power
Power2 <- function(x, a){
  print(x^a)
}

Power3 <- function(x, a)
{
  return(x^a)
}

PlotPower <- function(x, a)
{
  plot(x, y = Power3(x, a),
       xlab = "x",
       ylab = "(y = x^a)",
       main = "f(x) = x^a")
  
}

PlotPower(1:10, 3)
# raising 3 to the 8th power
Power2(3, 8)
# raising 10 to the 3rd power
Power2(10, 3)
# raising 8 to the 17th power
Power2(8, 17)
# raising 131 to the 3rd power
Power2(131, 3)

(res <- Power3(10, 3))

x <- 1:10
y <- Power3(x, 2)
plot(log(x), log(y),
     xlab = "log(x) 1:10",
     ylab = "log(y = x^2)",
     main = "f(x) = x^2")

?plot
plot