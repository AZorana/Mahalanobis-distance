library(GSVD)
library(ours)
library(klaR)
set.seed(12345)
# Baza se nalazi na adresi https://github.com/derekbeaton/OuRS/tree/master/OuRS/data
snps = SNPS


outliers_plot_generalizedMD = function(data, alpha){
  cat_mcd = categorical_mcd(data, alpha = alpha)

  plot(cat_mcd$dists$mahal_dists, cat_mcd$dists$robust_mahal_dists,  
       ylab = "Robusno kvadratno Mahalanobisovo rastojanje", xlab = "Kvadratno Mahalanobisovo rastojanje", col="dark green" )
  plot(sqrt(cat_mcd$dists$mahal_dists), sqrt(cat_mcd$dists$robust_mahal_dists),  
       ylab = "Robusno Mahalanobisovo rastojanje", xlab = " Mahalanobisovo rastojanje", col="dark green" )
  return(cbind(sort(cat_mcd$dists$mahal_dists), sort( cat_mcd$dists$robust_mahal_dists)))
} 

(r_0.75 = outliers_plot_generalizedMD(snps, 0.75))

snps_without_outliers = snps[c(-23,-53), ]

(r_0.75_without_outliers = outliers_plot_generalizedMD(snps_without_outliers, 0.75))



optimalan_broj_K = function(data){
  sum_withindiff = c()
  broj_K = 0
  for (i in 2:6) {
    cl = kmodes(data, modes = i)
    sum_withindiff[i-1] = sum(cl$withindiff)
  }
  return(sum_withindiff)
}
(b = optimalan_broj_K(snps))

plot(c(2,3,4,5,6), b, xlab = "Broj klastera" , ylab = "Suma withindiff" ,type = "l", col = "blue" )


(cl = kmodes(snps, modes = 4))
(sum_withindiff=sum(cl$withindiff))


(cl_bez_autlajera = kmodes(snps_without_outliers,4))
(sum_withindiff1=sum(cl_bez_autlajera$withindiff))

