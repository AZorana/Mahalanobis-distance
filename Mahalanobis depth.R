#Ucitamo potrebne biblioteke za rad
library(MASS)
library(matlib)
library(stats)
library(dplyr)
library(ggplot2)
set.seed(12345)


# Generisemo slucajni uzorak dimenzije 99 iz dvodimenzionalne normalne raspodele
mu = c(4,9)
sigma = rbind(c(1,0.6), c(0.4,1))
data = mvrnorm(99, mu, sigma)
# Uzorak
data
# Racunamo Mahalanobisovo rastojanje
# funkcija mahalanobis od agrumenata trazi podatke za koje racunamo
# Mahalanobisovo rastojanje, vektor srednje vrednosti svake kolone i
# kovarijacionu matricu

# Izracunali smo Mahalanobisovo rastojanje
m_distance = mahalanobis(data, mu, sigma)
# Racunamo Mahalanobisovu dubinu:1/[1+(x-mu)'(x-mu)/sigma]
# Vec smo izracunali Mahalanobisovo rastojanje u prethodnom koraku:
# (x-mu)'(x-mu)/sigma. Ostalo je izracunati jos 1/[1+m_distance]
m_depth = 1/(1 + m_distance)
# Dodajemo kolonu m_depth
d = data.frame(data, m_depth)
d
# Sortiramo vrednosti kolona od najvece m_depth vrednosti do najmanje,
# zadrzavajuci parove unutar vrste
# Tacka sa najvecom vrednoscu m_depth je Mahalanobisova dubina
d1 = d %>% arrange(desc(m_depth))


# Vizuelizacija podataka i Mahalanobisove dubine
# Koristicemo paket ggplot2

# Izdvojicemo tacku koja predstavlja Mahalanobisovu dubinu, jer ce  nam trebati kako bismo
# je obojili drugom bojom
MD_point = d1 %>% filter((X1==d1[1,1] & X2==d1[1,2]))
# Iskljucujemo eksponencijalni zapis poput 1e+06
options(scipen=999) 
#data("d1", package = "ggplot2")
ggplot(data.frame(data), aes(x = X1, y = X2))+geom_point(col="brown", size=3) +
  geom_point(data = MD_point, color='yellow', size=5) +
  ggtitle("Махаланобисова дубина") +
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("X-коодината") + ylab("Y-координата")
