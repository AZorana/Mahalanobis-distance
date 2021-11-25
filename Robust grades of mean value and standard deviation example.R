# Generisemo 100 elemnta iz N(0,1) raspodele
n=100
x = rnorm(n)
x = sort(x)
x
# Srednja vrednost 
mean_value_without_outlier = mean(x)
mean_value_without_outlier

# Medijana
me_value_without_outlier = (x[n/2]+x[n/2+1])/2
me_value_without_outlier

# Standardno odstupanje racunano nad celim skupom podataka
sd_value_without_outlier = sqrt(var(x))
sd_value_without_outlier

# Standardno odstupanje racunato nad 95-o procentnim skupom podataka, gde smo
# odbacili prvih 2.5% podatka i poslednjih 2.5% podataka
x_new = x[round(0.025*length(x)):round(0.975*length(x))]
x_new
sd_95_value_without_outlier = sqrt(var(x_new))
sd_95_value_without_outlier

# Dodavanje 4 nova elementa, 4 autlajera
x[97:100] = c(15,-6,8,-20)
x = sort(x)
x

# Srednja vrednost za podatke sa autlajerima
mean_value_with_outlier = mean(x)
mean_value_with_outlier

# Medijana za podatke sa autlajerima
me_value_with_outlier = (x[n/2]+x[n/2+1])/2
me_value_with_outlier

# Standardno odstupanje racunato nad celim skupom podataka (sa autlajerima)
sd_value_with_outlier = sqrt(var(x))
sd_value_with_outlier

# Standardno odstupanje racunato nad 95-o procentnim skupom podataka (sa autlajerima),
# gde smo odbacili prvih 2.5% podatka i poslednjih 2.5% podataka
x_new = x[round(0.025*length(x)):round(0.975*length(x))]
x_new
sd_95_value_with_outlier = sqrt(var(x_new))
sd_95_value_with_outlier

