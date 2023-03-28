print("Hello paradice!")

#on set la seed aléatoire sur 667
set.seed(667)
# 1) 
mu <- 10
sigma <- sqrt(0.5)

# Calcul de la probabilité
p_a <- pnorm(10, mean=mu, sd=sigma)
p_a
#La probabilité qu'un verre rempli par le serveur A contienne moins de 10cl de vin est d'environ 0.5
#2) 
mu <- 10.5
sigma <- sqrt(0.5)

# Calcul de la probabilité
p_b <- pnorm(10, mean=mu, sd=sigma)
p_b
#La probabilité qu'un verre rempli par le serveur B contienne moins de 10cl de vin est d'environ 0.0228.

#3) On peut simuler un échantillon en utilisant la fonction rnorm de R qui permet de générer des nombres aléatoires suivant une loi normale.
#On peut ainsi générer un échantillon de n valeurs suivant une loi N(10,0.5^2) avec :
n <- 20 # ou 40, 100, 200
echantillonn20 <- rnorm(n, 10, 0.5)
n <- 40
echantillonn40 <- rnorm(n, 10, 0.5)
n <- 100
echantillonn100 <- rnorm(n, 10, 0.5)
n <- 200
echantillonn200 <- rnorm(n, 10, 0.5)
echantillonn20
echantillonn40
echantillonn100
echantillonn200
#On peut simuler un échantillon en utilisant la fonction rnorm de R qui permet de générer des nombres aléatoires suivant une loi normale. On peut ainsi générer un échantillon de n valeurs suivant une loi N(10.5,0.5^2) avec :
  
  
 
 
n <- 20 # ou 40, 100, 200   
echantillon20 <- rnorm(n, 10.5, 0.5)
n <- 40 # ou 40, 100, 200
echantillon40 <- rnorm(n, 10.5, 0.5)
n <- 100 # ou 40, 100, 200
echantillon100 <- rnorm(n, 10.5, 0.5)
n <- 200 # ou 40, 100, 200
echantillon200 <- rnorm(n, 10.5, 0.5)
echantillon20
echantillon40
echantillon100
echantillon200
#On obtient ainsi un échantillon de n valeurs de volumes de vin dans les verres servis par le serveur B.







