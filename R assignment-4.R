#Question 1
x<-c(0,1,2,3,4)
prob_x<-c(0.41,0.37,0.16,0.05,0.01)
Expval<-sum(x*prob_x)
print(Expval)
#or
Exp_val<-weighted.mean(x,prob_x)
Exp_val
#or
Exp_val<-c(x%*%prob_x)
print(Exp_val)
Exp_val2<-sum(x^2*prob_x)
varx<-Exp_val2-Exp_val^2
cat("var(x)",varx,"\n")
var2x<-2^2*varx
cat("var(2x)",var2x,"\n")
var_x_min3<-varx
cat("var(x-3)",varx,"\n")

#Question 2
f1<-function(t){t*0.1*exp(-0.1*t)}
E_T<-integrate(f1,lower = 0, upper = Inf)$value
print(E_T)
fT<-function(t){
  ifelse(t>0,0.01*exp(-0.1*t),0)
}
ET<-integrate(function(t)t*fT(t),lower = 0, upper = Inf)$value
print(ET)

#Question 3
x<-c(0,1,2,3)
probx<-c(0.1,0.2,0.2,0.5)
y<-10*x-12
proby<-probx
EY<-sum(y*proby)
cat("Expected revenue:",EY,"\n")

#Question 4
f_x <- function(x) {
  0.5 * exp(-x)
}
mean_x <- integrate(function(x) x * f_x(x), lower = 1, upper = 10)$value
second_moment_x2 <- integrate(function(x) x^2 * f_x(x), lower = 1, upper = 10)$value
variance_x <- second_moment_x2 - mean_x^2
cat("Mean (E(X)): ", mean_x, "\n")
cat("Second moment (E(X^2)): ", second_moment_x2, "\n")
cat("Variance: ", variance_x, "\n")

#Question 5
f_x <- function(x) {
  (3/4) * (1/4)^(x - 1)
}
f_y <- function(y) {
  x <- sqrt(y)
  if (x %% 1 == 0) {
    return(f_x(x))
  } else {
    return(0) 
  }
}
x_value <- 3
y_value <- x_value^2
prob_y_for_x_3 <- f_y(y_value)
x_values <- 1:5
y_values <- x_values^2
expected_value_y <- sum(y_values * sapply(x_values, f_x))
E_Y2 <- sum(y_values^2 * sapply(x_values, f_x))
variance_y <- E_Y2 - expected_value_y^2
cat("Probability of Y for X = 3 (Y = 9): ", prob_y_for_x_3, "\n")
cat("Expected value of Y: ", expected_value_y, "\n")
cat("Variance of Y: ", variance_y, "\n")

#Submitted By
#Gurvinder Singh
#102203149
