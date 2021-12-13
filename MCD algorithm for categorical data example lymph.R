remotes::install_github("derekbeaton/GSVD")
Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS = TRUE)
remotes::install_github("derekbeaton/OuRS/OuRS")
library(GSVD)
library(ours)
set.seed(12345)
lymph_csv = read.csv("C:/Users/Zorana/Desktop/Master kod u R/Baza lymph/lymph_csv.csv")
lymph = lymph_csv[,1:8]
lymph[9:16] = lymph_csv[,11:18]



razlika_robusnog_nerobusnog_MD = function(data, alpha){
  max_razlika = c()
  index = c()
  j=1
  cat_mcd = categorical_mcd(data, alpha = alpha)
  razlika = cat_mcd$dists$robust_mahal_dists - cat_mcd$dists$mahal_dists
  for (i in seq(1,nrow(data),1)) {
    if(razlika[i] > 1){
       max_razlika[j] = razlika[i]
       index[j] = i
       j = j+1
    }
      
  }
  plot(cat_mcd$dists$mahal_dists, cat_mcd$dists$robust_mahal_dists,  
       ylab = "Robusno kvadratno Mahalanobisovo rastojanje", xlab = "Kvadratno Mahalanobisovo rastojanje" )
  plot(seq(1,nrow(data)),razlika,  
       ylab = "Razlika robusnog i nerobusnog kvadratnog Mahalanobisovog rastojanja", xlab = "Redni broj promenljive", 
       type = "p", col='brown')
  #abline(h=1, col = "orange")
 M = cbind(index,max_razlika)
  return(M[order(M[,2],decreasing = TRUE),])
} 

(r_0.5 = razlika_robusnog_nerobusnog_MD(lymph, 0.5))


(r_0.75 = razlika_robusnog_nerobusnog_MD(lymph, 0.75))


(r_0.9 = razlika_robusnog_nerobusnog_MD(lymph, 0.9))



