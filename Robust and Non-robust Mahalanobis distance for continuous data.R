library(robustbase)
library(dplyr)
library(ggplot2)

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
  ylab("Температура") + xlab("Озон") +
  ggtitle(title) + theme(plot.title = element_text(hjust = 0.5))
return(figure)
}

Ellipse_plot(data,MD_center, cov(data), rad,"Неробусно Махаланобисово растојање")
Ellipse_plot(data,MDrobust_center, MDrobust_cov, rad, "Робусно Махаланобисово растојање")


