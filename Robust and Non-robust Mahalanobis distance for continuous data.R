library(robustbase)
library(dplyr)
library(ggplot2)
library(robust)

data = airquality[c("Ozone" , "Temp")]
data
data = na.omit(data)
data

(MD = 1/(1 + mahalanobis(data, colMeans(data), cov(data))))

# Dodajemo kolonu m_depth
d = data.frame(data, MD)
d
d1 = d %>% arrange(desc(MD))
(MD_center = c(d1[1,1:2]$Ozone, d1[1,1:2]$Temp))


(MDrobust = covMcd(data,alpha = 0.75))
(MDrobust_center = MDrobust$center)
(MDrobust_cov = MDrobust$cov)

r = fastmcd(data)

rad  = sqrt(qchisq(p = 0.95 , df = ncol(data)))

Ellipse_plot = function(data,center, covariance, radius,title){
ellipse = car::ellipse(center = center , shape = covariance , radius = radius ,
                        segments = 150 , draw = FALSE)

ellipse = as.data.frame(ellipse)
colnames(ellipse) = colnames(data)

figure = ggplot(data , aes(x = Ozone , y = Temp)) +
  geom_point(size = 3) +
  geom_polygon(data = ellipse , fill = "lightgreen" , color = "green" , alpha = 0.5)+
  geom_point(aes(center[1] , center[2]) , size = 5 , color = "brown") +
  geom_text( aes(label = row.names(data)) , hjust = 1 , vjust = -1.5 ,size = 2.5 ) +
  ylab("Temperatura") + xlab("Ozon") +
  ggtitle(title) + theme(plot.title = element_text(hjust = 0.5))
return(figure)
}

Ellipse_plot(data,MD_center, cov(data), rad,"Nerobusno Mahalanobisovo rastojanje")
Ellipse_plot(data,MDrobust_center, MDrobust_cov, rad, "Robusno Mahalanobisovo rastojanje")

# Grafik zavisnosti robusnog i nerobusnog  Mahalanobisovog rastojanja

plot(mahalanobis(data, colMeans(data), cov(data)), MDrobust$mah,  
     ylab = "Robusno Mahalanobisovo rastojanje", xlab = "Mahalanobisovo rastojanje", col="dark red" )
