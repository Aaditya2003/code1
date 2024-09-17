#Question 1
n = 12
x = pbinom(9,n,prob=1/6) - pbinom(6,n,prob=1/6)
print(x)

#Question 2
y = pnorm(84,mean=72,sd=15.2,lower.tail = F) - pnorm(100,mean=72,sd=15.2,lower.tail = F)
print(y)

#Question 3
v=ppois(0,lambda = 5)
print(v)

cust = c(47,50)
v <- ppois(cust[2], lambda = 50) - ppois(cust[1], lambda = 50)
print(v)

#Question 4
b=dhyper(3,17,233,5)
print(b)

#Question 5
n = 31
p = 0.447
x = 0:31

db = dbinom(x,n,p)
plot(x,dbinom(x,n,p),type="h")

plot(x,cumsum(dbinom(x,n,p)),type="l")

m = weighted.mean(x,db)
print(m)

var = weighted.mean((x-m)^2,db)
print(var)

sd = sqrt(var)
print(sd)

#Submitted By
#Gurvinder Singh
#Roll No.- 102203149