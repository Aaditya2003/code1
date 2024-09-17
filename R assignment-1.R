#Question 1
x = c(5, 10, 15, 20, 25, 30)
print("Maximum value of the above Vector:")
print(max(x))
print("Minimum value of the above Vector:")
print(min(x))

#Question 2
num = as.integer(readline(prompt="Enter a number: "))
factorial = 1

if(num < 0) {
  print("Not possible for negative numbers")
} else if(num == 0) {
  print("The factorial of 0 is 1")
} else {
  for(i in 1:num) {
    factorial = factorial * i
  }
  print(paste("The factorial of", num ,"is",factorial))
}

#Question 3
fibonacci <- function(x){
  a<-0
  b<-1
  print(paste("Fibonacci Sequence:"))
  for(i in 1:x){
    print(paste(a))
    next_num<-a+b
    a<-b
    b<-next_num
  }
}
num = as.integer(readline(prompt="Enter number of terms to be printed: "))
fibonacci(num)


#Question 4
add<-function(x,y){
  return(x+y)
}
subtract<-function(x,y){
  return(x-y)
}
multiply<-function(x,y){
  return(x*y)
}
divide<-function(x,y){
  if(y!=0){
    return(x/y)
  }
}

print("1. Add")
print("2. Subtract")
print("3. Multiply")
print("4. Divide")
choice<-as.integer(readline(prompt="Enter the choice[1/2/3/4]"))
num1<-as.integer(readline(prompt="Enter the first number:"))
num2<-as.integer(readline(prompt="Enter the second number:"))
operator<-switch(choice,"+","-","*","/")
result<-switch(choice, add(num1,num2), subtract(num1,num2), multiply(num1,num2), divide(num1,num2))
print(paste(num1,operator,num2,"=",result))


#Question 5
#scattered plot
x<-rnorm(100)
y<-rnorm(100)
plot(x,y,pch=20,main = "scattered plot",lwd=4,xlab="X-axis",ylab = "Y=sin(x)")
plot(1:10,pch=4,lwd=5,main = "line plot")

#pie chart
x<-c(10,20,30,40,30)
mylabel<-c("Banana","apple","Orange","Grapes","Tomato")
colors<-c("blue","magenta","yellow","red","green")
pie(x,labels=mylabel,col=colors)

#barplot

fruits <- c("Banana", "Apple", "Orange", "Grapes", "Tomato")
counts <- c(20, 15, 30, 10, 25)

barplot(counts, 
        names.arg = fruits, 
        col = "skyblue", 
        main = "Fruit Count", 
        xlab = "Fruits", 
        ylab = "Count",
        border = "black")

text(x = barplot(counts), y = counts, label = counts, pos = 3, cex = 0.8, col = "black")
