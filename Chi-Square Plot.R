library(mvoutlier)
# Slucajni uzorak iz standardne normalne raspodele
x = rnorm(190)
x

# Dodavanje autlajera slucajnom uzorku
outlier = c(10,-15,12,8,-9, 11, 17,-12,6,-5)
x[191:200] = outlier
x

# Hi-kvadrat grafik
p=chisq.plot(data.frame(x), quan = 0.5, ylim=c(0,10))
abline(h=qchisq(p = 0.95 , df = ncol(data.frame(x))), col="red")
