############################################################
####  Devoir 5 : Séries temporelle et reproductibilité  ####
############################################################

# Étudiante : Ariane Dionne 9 08 160 798 
# Date de remise : 31 juillet 2020 17h00


# Installation des modules et des librairies
## Installer module si le module n'est pas déjà installé. 
## Chargement des librairies en début de session. 
##install.packages("tidyverse") 
library("tidyverse") 

##install.packages("ggplot2") 
library("ggplot2")

##install.packages("lubridate")
library("lubridate")

##install.packages("forecast")
library("forecast")

##install.packages("fpp2")
library("fpp2")

##install.packages("cowplot")
library("cowplot")

##install.packages("e1071")
library("e1071")

## Importer le tableau hawai.csv
## CSV : séparateur virgule et séparateur décimal point 
hawai <- read.csv("data/hawai.csv") # données hawai(moyennes ppm-volume collectés de CO2)

## Inspection du tableau hawai 
head(hawai) # Inspection de l'entête du tableau
str(hawai)  # Inspection de la structure du tableau 
names(hawai)# Inspection des noms des colonnes du tableau

glimpse(hawai)

## Explorer graphiquement les moyenes de CO2  
hawai %>%
  ggplot(aes(x = time, y = `CO2`)) +
  geom_line()

# Créer une série temporelle 
### de type ts : enlève date, démarre 1er événement, incrément a une fréquence de 1/12  
hawai_ts <- ts(hawai %>% dplyr::select(-time),
                    start = c(hawai$time[1], 1),
                    frequency = 12)
hawai_ts
graph_ts <- autoplot(hawai_ts)
graph_ts

### Séparer la série en série d'entraînement et série test
indice_70 <- floor(length(hawai$time)*0.7) #indice qui arrivent à 70 % des données hawai
annee_70 <- floor(hawai$time[indice_70])   # année reliée à l'indice 70 %
hawai_ts_train <- window(hawai_ts, start = 1958, end = (annee_70-(1/12))) # 70% des 1ere données pour l'entraînement
hawai_ts_test <- window(hawai_ts, start = annee_70)                       # 30 % des dernières données pour le test 
### Vérification des séries d'entraînement et de test 
hawai_ts_train
hawai_ts_test

## Créer un modèle prévisionnel sur les données d'entraînement
### Prétraitement des données avec fonction Box-Cox
BoxCox.lambda(hawai_ts_train)

### Le modèle prévisionnel ARIMA 
hawai_arima <- hawai_ts_train %>% auto.arima(lambda = -0.0722)
summary(hawai_arima)

### Projection des prévisions pour les années 1988 à 2001 (les 30% de données) 
Graph_prev <- hawai_arima %>% forecast(h = 169) %>% autoplot()
Graph_prev
  
### Comparaison aux données test 
Graph_comp <- hawai_arima %>% forecast(h = 169) %>% autoplot()+
  autolayer(fitted(hawai_arima)) +
  autolayer(hawai_ts_test, color = "yellow") +
  labs(x = "Année", y = "CO2")
Graph_comp

### Compraisons des tableaux pour vérification 
plot_grid(Graph_prev, Graph_comp, graph_ts, ncol = 3, nrow = 1)


# Analyse des résidus 
checkresiduals(hawai_arima) # Distribution des Résidus, graphique d'autocorrélation et histogramme de normalité 
shapiro.test(residuals(hawai_arima)) # non-normal si p-value < seuil (0.05)
