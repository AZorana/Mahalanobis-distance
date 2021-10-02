## Tea primer
# U ovoj biblioteci se nalazi potrebna funkcija MCA i potreba baza tea
library(FactoMineR)
# Ucitava bazu
data(tea)
# Primena visestruke analize korespodnecije (Multiple Correspondence Analysis (MCA)) na
# kategoricke podatke
# quanti.sup predstavlja dopunske kvantitativne promenljive
# quali.sup predstavlja dopunske kategoricke promenljive
res.mca = MCA(tea,ncp=18, quanti.sup=19,quali.sup=20:36)
# Levi singularni vektori
U = res.mca$svd$U
U
# Dimenzije matrice U
cat("Dimenzije matrice U su: (", nrow(U),",",ncol(U),")")
U_U_T = U%*%t(U)
U_U_T
# Dimenzije matrice U_U_T
cat("Dimenzije matrice U_U_T su: (", nrow(U_U_T),",",ncol(U_U_T),")")
# Mahalanobisovo rastojanje
Mahalanobis_distance=diag(U_U_T)
Mahalanobis_distance
# Mahalanobisova dubina kao najveca vrednost Mahalanobisovog rastojanja
# Sortirali smo u opadajucem nizu i zbog toga uzeli  prvi element niza
Mahalanobis_depth=sort(Mahalanobis_distance, decreasing = TRUE)[1]
Mahalanobis_depth



