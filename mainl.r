print("Logan is in the place !")

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




#2.2 question 5
#Representer les valeurs obtenues sur chaque échantillons par un graphique :
#echantillon A
hist(echantillonn20)
hist(echantillonn40)
hist(echantillonn100)
hist(echantillonn200)
#echantillon B
hist(echantillon20)
hist(echantillon40)
hist(echantillon100)
hist(echantillon200)

#2.2 question 6
#Déterminer la proportion de verre avec moins de 10cl de vin dans chaque échantillon.
#echantillon A
p_a20 <- sum(echantillonn20 < 10)/length(echantillonn20)
p_a20
p_a40 <- sum(echantillonn40 < 10)/length(echantillonn40)
p_a40
p_a100 <- sum(echantillonn100 < 10)/length(echantillonn100)
p_a100
p_a200 <- sum(echantillonn200 < 10)/length(echantillonn200)
p_a200
#echantillon B
p_b20 <- sum(echantillon20 < 10)/length(echantillon20)
p_b20
p_b40 <- sum(echantillon40 < 10)/length(echantillon40)
p_b40
p_b100 <- sum(echantillon100 < 10)/length(echantillon100)
p_b100
p_b200 <- sum(echantillon200 < 10)/length(echantillon200)
p_b200

#2.2 question 7
#Intervalle de confiance pour qu'un verre contienne moins de 10cl dans chaque échantillon.
#echantillon A
t.test(echantillonn20, alternative="less", mu=10, conf.level=0.95)
t.test(echantillonn40, alternative="less", mu=10, conf.level=0.95)
t.test(echantillonn100, alternative="less", mu=10, conf.level=0.95)
t.test(echantillonn200, alternative="less", mu=10, conf.level=0.95)
#echantillon B
t.test(echantillon20, alternative="less", mu=10, conf.level=0.95)
t.test(echantillon40, alternative="less", mu=10, conf.level=0.95)
t.test(echantillon100, alternative="less", mu=10, conf.level=0.95)
t.test(echantillon200, alternative="less", mu=10, conf.level=0.95)



#2.2 question 8
#Estimation de m et de sigma^2 pour chaque échantillon.
#echantillon A
#Estimation de m
m_a20 <- mean(echantillonn20)
m_a20
m_a40 <- mean(echantillonn40)
m_a40
m_a100 <- mean(echantillonn100)
m_a100
m_a200 <- mean(echantillonn200)
m_a200
#Estimation de sigma^2
sigma_a20 <- var(echantillonn20)
sigma_a20
sigma_a40 <- var(echantillonn40)
sigma_a40
sigma_a100 <- var(echantillonn100)
sigma_a100
sigma_a200 <- var(echantillonn200)
sigma_a200
#echantillon B
#Estimation de m
m_b20 <- mean(echantillon20)
m_b20
m_b40 <- mean(echantillon40)
m_b40
m_b100 <- mean(echantillon100)
m_b100
m_b200 <- mean(echantillon200)
m_b200
#Estimation de sigma^2
sigma_b20 <- var(echantillon20)
sigma_b20
sigma_b40 <- var(echantillon40)
sigma_b40
sigma_b100 <- var(echantillon100)
sigma_b100
sigma_b200 <- var(echantillon200)
sigma_b200


#2.2 question 9
#Calcul d'un intervalle de confiance pour une moyenne
#echantillon A
#On calcule l'intervalle de confiance à 95% pour la moyenne de l'échantillon
intervalle_a20 <- t.test(echantillonn20, conf.level = 0.95)
intervalle_a20
intervalle_a40 <- t.test(echantillonn40, conf.level = 0.95)
intervalle_a40
intervalle_a100 <- t.test(echantillonn100, conf.level = 0.95)
intervalle_a100
intervalle_a200 <- t.test(echantillonn200, conf.level = 0.95)
intervalle_a200
#echantillon B
#On calcule l'intervalle de confiance à 95% pour la moyenne de l'échantillon
intervalle_b20 <- t.test(echantillon20, conf.level = 0.95)
intervalle_b20
intervalle_b40 <- t.test(echantillon40, conf.level = 0.95)
intervalle_b40
intervalle_b100 <- t.test(echantillon100, conf.level = 0.95)
intervalle_b100
intervalle_b200 <- t.test(echantillon200, conf.level = 0.95)
intervalle_b200

#2.2 question 10
#Test de comparaison d'une moyenne a une reference pour verifier si m>10cl
#echantillon A
#On teste si la moyenne de l'échantillon est supérieure à 10cl
test_a20 <- t.test(echantillonn20, alternative="greater", mu=10, conf.level=0.95)
test_a20
test_a40 <- t.test(echantillonn40, alternative="greater", mu=10, conf.level=0.95)
test_a40
test_a100 <- t.test(echantillonn100, alternative="greater", mu=10, conf.level=0.95)
test_a100
test_a200 <- t.test(echantillonn200, alternative="greater", mu=10, conf.level=0.95)
test_a200
#echantillon B
#On teste si la moyenne de l'échantillon est supérieure à 10cl
test_b20 <- t.test(echantillon20, alternative="greater", mu=10, conf.level=0.95)
test_b20
test_b40 <- t.test(echantillon40, alternative="greater", mu=10, conf.level=0.95)
test_b40
test_b100 <- t.test(echantillon100, alternative="greater", mu=10, conf.level=0.95)
test_b100
test_b200 <- t.test(echantillon200, alternative="greater", mu=10, conf.level=0.95)
test_b200

#2.2 question 11
#faire un text de comparaison de deux moyennes pour savoir si les moyennes des deux serveurs sont les memes
t.test(echantillonn20, echantillon20, alternative="two.sided", conf.level=0.95)







