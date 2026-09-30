
n <- 30
p <- 1/6
x <- 1:30


prob <- dbinom(x, size = n, prob = p)

plot(x, prob, 
     type = "h", 
     lwd= 3, 
     col= "firebrick",
     main = "Distribution of Number of Sixes in 30 Dice Throws",
     xlab = "Number of sixes", ylab = "Probability")
