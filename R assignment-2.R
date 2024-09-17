#1a
chest=c(rep("Gold",20),rep("Silver",30),rep("Bronze",50))
sample(chest,10)

#1b
chance=c("Success","Failure")
sample(chance,10,replace = TRUE, prob=c(0.9,0.1))

#2
pbirthday(n=1000,classes=365,coincident = 2)
qbirthday(prob = 0.5,classes=365,coincident = 2)
total = 1000
count = 0
n = 20
for (i in 1:total){
  a = as.integer(any(duplicated(sample(365,n,replace = TRUE))))
  count = count + a
}
prob = count/total
prob

#3
func<-function(clouds,rain,c_r){
  r_c = (c_r*rain/clouds)
  return(r_c)
}
cloudy=0.4
rainy=0.2
cloud_rain=0.85
rain_cloud=func(cloudy,rainy,cloud_rain)
rain_cloud

#4
df=iris
head(df)
str(df)

range(df$Sepal.Length)
range(df$Sepal.Width)
range(df$Petal.Length)
range(df$Petal.Width)

mean(df$Sepal.Length)
mean(df$Sepal.Width)
mean(df$Petal.Length)
mean(df$Petal.Width)

median(df$Sepal.Length)
median(df$Sepal.Width)
median(df$Petal.Length)
median(df$Petal.Width)

a=quantile(df$Sepal.Length,p=c(0.25,0.75))
print(a)
IQR_Sepal.Length <- IQR(iris$Sepal.Length)
IQR_Sepal.Length

b=quantile(df$Sepal.Width,p=c(0.25,0.75))
print(b)
IQR_Sepal.Width <- IQR(iris$Sepal.Width)
IQR_Sepal.Width

c=quantile(df$Petal.Length,p=c(0.25,0.75))
print(c)
IQR_Petal.Length <- IQR(iris$Petal.Length)
IQR_Petal.Length

d=quantile(df$Petal.Width,p=c(0.25,0.75))
print(d)
IQR_Petal.Width <- IQR(iris$Petal.Width)
IQR_Petal.Width

sd(iris$Sepal.Length)
var(iris$Sepal.Length)
sd(iris$Sepal.Width)
var(iris$Sepal.Width)
sd(iris$Petal.Length)
var(iris$Petal.Length)
sd(iris$Petal.Width)
var(iris$Petal.Width)

summary(iris)

#5
find_mode<-function(v){
  a=unique(v)
  a[which.max(tabulate(match(v,unique(v))))]
}
v=c(1,2,3,4,5)
ans=find_mode(v)
ans

#Submitted by 
#Gurvinder Singh
#102203149