# Generisemo 60 elemnta iz N(0,1) raspodele
library(MASS)
set.seed(12345)
mu = c(2,5)
sigma = rbind(c(1.5,0.5), c(0.2,1.5))
n=60
data = mvrnorm(n, mu, sigma)
data

# Srednja vrednost bez autlajera

mean_value_without_outlier = c(mean(data[,1]), mean(data[,2]))
mean_value_without_outlier

#Medijana bez autlajera 

me_value_without_outlier = c((sort(data[,1])[n/2]+sort(data[,1])[n/2+1])/2, (sort(data[,2])[n/2]+sort(data[,2])[n/2+1])/2)
me_value_without_outlier

#Dodajemo autlajere
data1 = data
data1[53:60,] = rbind(c(10,15), c(15,18), c(8,5), c(13,14), c(17,12), c(13,20), c(-12,-12), c(15,19))


# Srednja vrednost sa autlajerima

mean_value_with_outlier = c(mean(data1[,1]), mean(data1[,2]))
mean_value_with_outlier

#Medijana sa autlajerima 

me_value_with_outlier = c((sort(data1[,1])[n/2]+sort(data1[,1])[n/2+1])/2, (sort(data1[,2])[n/2]+sort(data1[,2])[n/2+1])/2)
me_value_with_outlier

