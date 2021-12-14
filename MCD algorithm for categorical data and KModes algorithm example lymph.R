remotes::install_github("derekbeaton/GSVD")
Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS = TRUE)
remotes::install_github("derekbeaton/OuRS/OuRS")
library(GSVD)
library(ours)
library(klaR)
set.seed(12345)
lymph_csv = read.csv("C:/Users/Zorana/Desktop/Master kod u R/Baza lymph/data lymph/lymph_csv.csv")
lymph=lymph_csv[,-c(9, 10, 19)]

# Funkcija koja vraca one opservacije cija je razlika nerobusnog i 
# robusnog Mahalanobisovog rastojanja veca od 2 i crta grafik zavisnosti
# robusnog i nerobusnog Mahalanobisovog rastojanja i grafik razlike robusnog i nerobusnog 
# Mahalanobisovog rastojanja
razlika_robusnog_nerobusnog_MD = function(data, alpha){
  max_razlika = c()
  index = c()
  j=1
  cat_mcd = categorical_mcd(data, alpha = alpha)
  razlika = cat_mcd$dists$robust_mahal_dists - cat_mcd$dists$mahal_dists
  for (i in seq(1,nrow(data),1)) {
    if(razlika[i] > 2){
       max_razlika[j] = razlika[i]
       index[j] = i
       j = j+1
    }
      
  }
  plot(cat_mcd$dists$mahal_dists, cat_mcd$dists$robust_mahal_dists,  
       ylab = "Robusno kvadratno Mahalanobisovo rastojanje", xlab = "Kvadratno Mahalanobisovo rastojanje", col="dark orange" )
  plot(seq(1,nrow(data)),razlika,  
       ylab = "Razlika nerobusnog i robusnog Mahalanobisovog rastojanja", xlab = "Redni broj promenljive", 
       type = "p", col='brown')
  #abline(h=1, col = "orange")
 M = cbind(index,max_razlika)
  return(M[order(M[,2],decreasing = TRUE),])
} 

(r_0.5 = razlika_robusnog_nerobusnog_MD(lymph, 0.5))


(r_0.75 = razlika_robusnog_nerobusnog_MD(lymph, 0.75))


(r_0.9 = razlika_robusnog_nerobusnog_MD(lymph, 0.9))

optimalan_broj_K = function(data){
  sum_withindiff = c()
  broj_K = 0
for (i in 2:6) {
  cl = kmodes(data, modes = i)
  sum_withindiff[i-1] = sum(cl$withindiff)
}
  return(sum_withindiff)
}
(b = optimalan_broj_K(lymph))
# b = c(774, 723, 659, 650,624)
# Optimalan broj klastera bi bio 6, ali posto imamo 148 opservacija
# odlucicemo se da broj klastera bude 4

plot(c(2,3,4,5,6), b, xlab = "Broj klastera" , ylab = "Suma withindiff" ,type = "l", col = "blue" )

# Klasterovanje na osnovu optimalnog broja klastera
(cl = kmodes(lymph, modes = 4))
(sum_withindiff=sum(cl$withindiff))
# Baza bez autlajera
lymph1=lymph[-r_0.75[,1], ]

# Klasterovanje nad bazom bez autlajera
(cl_bez_autlajera = kmodes(lymph1,4))
(sum_withindiff1=sum(cl_bez_autlajera$withindiff))

