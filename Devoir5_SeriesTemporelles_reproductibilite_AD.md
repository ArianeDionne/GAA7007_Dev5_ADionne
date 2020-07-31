Devoir 5 : Séries temporelles et reproductibilité
================
Ariane Dionne
31/07/2020

## 1 Installation des modules et librairies

Pour débuter la session, voici une liste des modules et libraries qui
seront nécessaires pour lancer le code R et ses différentes fonctions.
Si certains modules sont déjà installés sur votre ordinateur, il n’est
pas nécessaires de les installer de nouveau.

``` r
##install.packages("tidyverse") 
library("tidyverse") 
```

    ## 

    ## v ggplot2 3.3.2     v purrr   0.3.4
    ## v tibble  3.0.1     v dplyr   1.0.0
    ## v tidyr   1.1.0     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.5.0

    ## Warning: package 'ggplot2' was built under R version 4.0.2

    ## 
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
##install.packages("ggplot2") 
library("ggplot2")

##install.packages("lubridate")
library("lubridate")
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
##install.packages("forecast")
library("forecast")
```

    ## Warning: package 'forecast' was built under R version 4.0.2

    ## Registered S3 method overwritten by 'quantmod':
    ##   method            from
    ##   as.zoo.data.frame zoo

``` r
##install.packages("fpp2")
library("fpp2")
```

    ## Warning: package 'fpp2' was built under R version 4.0.2

    ## Loading required package: fma

    ## Warning: package 'fma' was built under R version 4.0.2

    ## Loading required package: expsmooth

    ## Warning: package 'expsmooth' was built under R version 4.0.2

``` r
##install.packages("cowplot")
library("cowplot")
```

    ## Warning: package 'cowplot' was built under R version 4.0.2

    ## 
    ## ********************************************************

    ## Note: As of version 1.0.0, cowplot does not change the

    ##   default ggplot2 theme anymore. To recover the previous

    ##   behavior, execute:
    ##   theme_set(theme_cowplot())

    ## ********************************************************

    ## 
    ## Attaching package: 'cowplot'

    ## The following object is masked from 'package:lubridate':
    ## 
    ##     stamp

``` r
##install.packages("e1071")
library("e1071")
```

    ## Warning: package 'e1071' was built under R version 4.0.2

## 2 Importer les données hawai

Importation du tableau **hawai** avec la fonction `read.csv`, car le
séparateur utilisé dans le tableau est une virgule et le séparateur
décimal est un point. Les données du tableau **hawai** comprennent les
données de temps dans la colonne `time` et les moyennes des mesures
mensuelles de CO2 atmosphérique dans la colonne `CO2`entrées en
ppm-volume collectées au Mauna Loa Observatory à Hawaii de mars 1958 à
décembre 2001, inclusivement.

``` r
hawai <- read.csv("data/hawai.csv") # données hawai(moyennes ppm-volume collectées de CO2)
glimpse(hawai) # aperçu du tableau 
```

    ## Rows: 526
    ## Columns: 2
    ## $ time <dbl> 1958.167, 1958.250, 1958.333, 1958.417, 1958.500, 1958.583, 19...
    ## $ CO2  <dbl> 316.1000, 317.2000, 317.4333, 317.4333, 315.6250, 314.9500, 31...

### 2.1 Exploration graphique

Il est plus aisé d’explorer graphiquement les données du tableau
**hawai** afin d’y déceler des structures. Les données de CO2
atmosphérique semblent présenter une **tendance** à la hausse d’années
en années. De plus, une **fluctuation saisonnière ou annuelle** semble
également être présente.

``` r
hawai %>%
  ggplot(aes(x = time, y = `CO2`)) +
  geom_line()
```

![](Devoir5_SeriesTemporelles_reproductibilite_AD_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

## 3 Création d’une série temporelle

Avec la fonction `ts`, le tableau **hawai\_ts** est créé en série
temporelle. Pour se faire, la colonne `time` du tableau **hawai** est
enlevée, la série temporelle débute au premier événement du tableau
**hawai** et l’incrémentation est mensuelle, c’est-à-dire douze fois en
un an.

``` r
### de type ts :   
hawai_ts <- ts(hawai %>% dplyr::select(-time),      # Enlever colonne time
                    start = c(hawai$time[1], 1),    # Démarrer au 1er événement
                    frequency = 12)                 # Incrément mensuel (fréquence de 1/12)
hawai_ts
```

    ##           Jan      Feb      Mar      Apr      May      Jun      Jul      Aug
    ## 1958                   316.1000 317.2000 317.4333 317.4333 315.6250 314.9500
    ## 1959 315.5000 316.7000 316.7333 317.6750 318.3250 318.0250 316.5250 314.9000
    ## 1960 316.3800 316.9750 317.5750 319.1200 319.9250 319.4500 318.0600 315.7750
    ## 1961 316.9250 317.6500 318.5250 319.4200 320.4750 319.7500 318.3200 316.7750
    ## 1962 317.9250 318.6250 319.6800 320.5500 320.9000 320.5000 319.4250 317.7333
    ## 1963 318.7250 318.9667 319.8600 321.2750 322.1000 321.4600 319.6000 317.6000
    ## 1964 319.4000 319.4000 319.1000 319.4000 322.0000 321.7500 320.3000 318.5000
    ## 1965 319.4000 320.4500 320.9250 322.0000 322.0600 321.7750 321.0400 318.7250
    ## 1966 320.5400 321.5750 322.3750 323.6600 324.0250 323.7000 322.8500 320.2000
    ## 1967 322.4000 322.4250 323.0000 324.2800 324.9500 324.0750 322.4800 320.8500
    ## 1968 322.5500 323.0500 323.9200 324.9750 325.4750 325.2600 324.0250 321.9400
    ## 1969 323.9250 324.2750 325.6000 326.5750 327.2800 326.5500 325.8000 323.5400
    ## 1970 325.0800 326.0250 327.0000 328.0750 327.9200 327.5500 326.3000 324.6600
    ## 1971 326.1400 326.6500 327.2000 327.6750 328.8200 328.4750 327.2200 325.2500
    ## 1972 326.7200 327.6250 327.7250 329.6400 330.0000 329.0750 328.0400 326.2500
    ## 1973 328.5500 329.4750 330.3800 331.5750 332.4000 331.9400 330.6500 329.3250
    ## 1974 329.3250 330.6250 331.5000 332.6250 332.9750 332.1600 331.0750 329.2200
    ## 1975 330.2500 331.3000 332.0000 333.2750 333.8600 333.4000 331.8000 329.9800
    ## 1976 331.7400 332.5500 333.4500 334.4500 334.7600 334.4000 332.8600 330.7250
    ## 1977 332.8800 333.4250 334.7000 336.0400 336.6750 336.2000 334.8000 332.8750
    ## 1978 335.0250 335.3500 336.6000 337.6400 337.9500 337.9250 336.5000 334.6500
    ## 1979 336.2250 336.7000 338.0000 338.8750 339.3750 339.2400 337.5250 336.0250
    ## 1980 337.9250 338.2250 340.0600 340.7500 341.3400 340.9500 339.3750 337.6000
    ## 1981 339.2200 340.4500 341.4000 342.4500 342.8200 342.1250 340.4250 338.4000
    ## 1982 340.7000 341.5500 342.6750 343.4750 343.9600 343.3000 341.9200 339.6250
    ## 1983 341.3400 342.5000 343.1500 344.9000 345.6500 345.2750 343.8200 342.1500
    ## 1984 343.6750 344.4250 345.1750 347.4000 347.3500 346.6600 345.2000 343.3000
    ## 1985 344.9250 345.8750 347.4400 348.3250 348.8250 348.1800 346.4500 344.3000
    ## 1986 346.2500 346.8250 347.7400 349.5250 350.0800 349.3750 347.8250 345.8200
    ## 1987 348.0000 348.5000 349.4750 350.8500 351.7400 351.1500 349.4500 348.0800
    ## 1988 350.4000 351.7750 352.1250 353.5800 354.1750 353.7500 352.2200 350.3000
    ## 1989 352.7750 353.0000 353.6000 355.3600 355.6000 355.1250 353.8600 351.5750
    ## 1990 353.6500 354.6500 355.4800 356.1750 357.0750 356.0800 354.6750 352.9000
    ## 1991 354.6750 355.6500 357.2000 358.6000 359.2500 358.1800 356.0500 353.8600
    ## 1992 355.9000 356.6800 357.9000 359.0750 359.5400 359.1250 357.0000 354.8600
    ## 1993 356.6800 357.1750 358.4250 359.3250 360.1800 359.5000 357.4200 355.3250
    ## 1994 358.3200 358.9000 359.9250 361.2200 361.6500 360.9000 359.4600 357.3750
    ## 1995 359.9750 360.9250 361.5750 363.3600 363.7000 363.2500 361.8000 359.3750
    ## 1996 362.0250 363.1750 364.0600 364.7000 365.3250 364.8800 363.4750 361.3200
    ## 1997 363.1250 363.8750 364.5600 366.3250 366.6800 365.4750 364.3750 362.4600
    ## 1998 365.3400 366.2000 367.3750 368.5250 369.1400 368.7500 367.6000 365.7200
    ## 1999 368.1200 368.8500 369.6000 370.9750 370.8400 370.2500 369.0000 366.7000
    ## 2000 369.0200 369.3750 370.4000 371.5400 371.6500 371.6250 369.9400 367.9500
    ## 2001 370.1750 371.3250 372.0600 372.7750 373.8000 373.0600 371.3000 369.4250
    ##           Sep      Oct      Nov      Dec
    ## 1958 313.5000 313.5000 313.4250 314.7000
    ## 1959 313.8250 313.4000 314.8750 315.5250
    ## 1960 314.1750 313.8400 315.0250 316.2000
    ## 1961 315.0400 315.3500 316.0500 316.9800
    ## 1962 316.0667 315.4750 316.5500 317.5250
    ## 1963 316.1500 316.0500 317.0000 318.3500
    ## 1964 316.6000 316.9600 317.7250 318.6750
    ## 1965 317.8250 317.3400 318.8750 319.3250
    ## 1966 318.7000 318.1800 319.9000 321.0600
    ## 1967 319.3000 319.4500 320.6750 321.9200
    ## 1968 320.2500 320.2750 321.3200 322.9250
    ## 1969 322.4000 321.8000 322.8200 324.1250
    ## 1970 323.1500 323.1400 324.0500 325.1000
    ## 1971 323.4000 323.5400 324.8250 325.9250
    ## 1972 324.8600 325.3000 326.4250 327.5400
    ## 1973 327.5000 327.2250 328.0500 328.6400
    ## 1974 327.3750 327.3750 328.4000 329.6750
    ## 1975 328.5250 328.3250 329.4400 330.7750
    ## 1976 329.3500 329.1000 330.3500 331.6250
    ## 1977 331.5750 331.2000 332.3500 333.8600
    ## 1978 332.7400 332.6500 333.8250 334.9600
    ## 1979 333.9800 333.9500 335.1500 336.6600
    ## 1980 335.8750 336.0250 337.0600 338.2000
    ## 1981 336.7000 336.9400 338.4250 339.6000
    ## 1982 338.0250 337.9000 339.2750 340.4250
    ## 1983 339.8750 340.0200 341.1500 342.9800
    ## 1984 341.1200 341.4750 342.8500 344.1400
    ## 1985 343.0000 342.8000 344.2200 345.5750
    ## 1986 344.8000 344.1000 345.6200 346.8750
    ## 1987 346.4000 346.4400 347.9250 348.9250
    ## 1988 348.7500 348.9600 350.0000 351.3600
    ## 1989 349.8600 350.0500 351.2000 352.4800
    ## 1990 350.9400 351.2250 352.7000 354.1400
    ## 1991 352.1250 352.2500 353.7400 355.0250
    ## 1992 353.0250 353.4200 354.2000 355.3500
    ## 1993 353.7750 354.0600 355.3500 356.7750
    ## 1994 355.9250 356.0200 357.5750 359.0600
    ## 1995 358.0000 357.8500 359.4750 360.7000
    ## 1996 359.4000 359.6250 360.7400 362.3750
    ## 1997 360.1500 360.7500 362.3800 364.2500
    ## 1998 363.9250 364.3200 365.5500 366.9250
    ## 1999 364.6750 365.1400 366.6500 367.9000
    ## 2000 366.5400 366.7250 368.1250 369.4400
    ## 2001 367.8800 368.0500 369.3750 371.0200

### 3.1 Vérification graphique de la série temporelle

Une vérification graphique confirme que toutes les données du tableau
**hawai** ont bien été transformées en série temporelle vers le tableau
**hawai\_ts**. Les données débutent en mars 1958 et se terminent en
décembre 2001 inclusivement.

``` r
graph_ts <- autoplot(hawai_ts)
graph_ts
```

![](Devoir5_SeriesTemporelles_reproductibilite_AD_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

## 4 Séries d’entraînement et de test

Avant de procéder à la modélisation prévisionnelle, il est important de
séparer les données temporelles en série d’entraînement ainsi qu’en
série de test afin de créer le modèle et de le vérifier par la suite.

``` r
indice_70 <- floor(length(hawai$time)*0.7) #indice qui arrivent à 70 % des données hawai
annee_70 <- floor(hawai$time[indice_70])   # année reliée à l'indice 70 %
hawai_ts_train <- window(hawai_ts, start = 1958, end = (annee_70-(1/12))) # 70% des 1ere données pour l'entraînement
```

    ## Warning in window.default(x, ...): 'start' value not changed

``` r
hawai_ts_test <- window(hawai_ts, start = annee_70)                       # 30 % des dernières données pour le test 
### Vérification des séries d'entraînement et de test 
hawai_ts_train
```

    ##           Jan      Feb      Mar      Apr      May      Jun      Jul      Aug
    ## 1958                   316.1000 317.2000 317.4333 317.4333 315.6250 314.9500
    ## 1959 315.5000 316.7000 316.7333 317.6750 318.3250 318.0250 316.5250 314.9000
    ## 1960 316.3800 316.9750 317.5750 319.1200 319.9250 319.4500 318.0600 315.7750
    ## 1961 316.9250 317.6500 318.5250 319.4200 320.4750 319.7500 318.3200 316.7750
    ## 1962 317.9250 318.6250 319.6800 320.5500 320.9000 320.5000 319.4250 317.7333
    ## 1963 318.7250 318.9667 319.8600 321.2750 322.1000 321.4600 319.6000 317.6000
    ## 1964 319.4000 319.4000 319.1000 319.4000 322.0000 321.7500 320.3000 318.5000
    ## 1965 319.4000 320.4500 320.9250 322.0000 322.0600 321.7750 321.0400 318.7250
    ## 1966 320.5400 321.5750 322.3750 323.6600 324.0250 323.7000 322.8500 320.2000
    ## 1967 322.4000 322.4250 323.0000 324.2800 324.9500 324.0750 322.4800 320.8500
    ## 1968 322.5500 323.0500 323.9200 324.9750 325.4750 325.2600 324.0250 321.9400
    ## 1969 323.9250 324.2750 325.6000 326.5750 327.2800 326.5500 325.8000 323.5400
    ## 1970 325.0800 326.0250 327.0000 328.0750 327.9200 327.5500 326.3000 324.6600
    ## 1971 326.1400 326.6500 327.2000 327.6750 328.8200 328.4750 327.2200 325.2500
    ## 1972 326.7200 327.6250 327.7250 329.6400 330.0000 329.0750 328.0400 326.2500
    ## 1973 328.5500 329.4750 330.3800 331.5750 332.4000 331.9400 330.6500 329.3250
    ## 1974 329.3250 330.6250 331.5000 332.6250 332.9750 332.1600 331.0750 329.2200
    ## 1975 330.2500 331.3000 332.0000 333.2750 333.8600 333.4000 331.8000 329.9800
    ## 1976 331.7400 332.5500 333.4500 334.4500 334.7600 334.4000 332.8600 330.7250
    ## 1977 332.8800 333.4250 334.7000 336.0400 336.6750 336.2000 334.8000 332.8750
    ## 1978 335.0250 335.3500 336.6000 337.6400 337.9500 337.9250 336.5000 334.6500
    ## 1979 336.2250 336.7000 338.0000 338.8750 339.3750 339.2400 337.5250 336.0250
    ## 1980 337.9250 338.2250 340.0600 340.7500 341.3400 340.9500 339.3750 337.6000
    ## 1981 339.2200 340.4500 341.4000 342.4500 342.8200 342.1250 340.4250 338.4000
    ## 1982 340.7000 341.5500 342.6750 343.4750 343.9600 343.3000 341.9200 339.6250
    ## 1983 341.3400 342.5000 343.1500 344.9000 345.6500 345.2750 343.8200 342.1500
    ## 1984 343.6750 344.4250 345.1750 347.4000 347.3500 346.6600 345.2000 343.3000
    ## 1985 344.9250 345.8750 347.4400 348.3250 348.8250 348.1800 346.4500 344.3000
    ## 1986 346.2500 346.8250 347.7400 349.5250 350.0800 349.3750 347.8250 345.8200
    ## 1987 348.0000 348.5000 349.4750 350.8500 351.7400 351.1500 349.4500 348.0800
    ##           Sep      Oct      Nov      Dec
    ## 1958 313.5000 313.5000 313.4250 314.7000
    ## 1959 313.8250 313.4000 314.8750 315.5250
    ## 1960 314.1750 313.8400 315.0250 316.2000
    ## 1961 315.0400 315.3500 316.0500 316.9800
    ## 1962 316.0667 315.4750 316.5500 317.5250
    ## 1963 316.1500 316.0500 317.0000 318.3500
    ## 1964 316.6000 316.9600 317.7250 318.6750
    ## 1965 317.8250 317.3400 318.8750 319.3250
    ## 1966 318.7000 318.1800 319.9000 321.0600
    ## 1967 319.3000 319.4500 320.6750 321.9200
    ## 1968 320.2500 320.2750 321.3200 322.9250
    ## 1969 322.4000 321.8000 322.8200 324.1250
    ## 1970 323.1500 323.1400 324.0500 325.1000
    ## 1971 323.4000 323.5400 324.8250 325.9250
    ## 1972 324.8600 325.3000 326.4250 327.5400
    ## 1973 327.5000 327.2250 328.0500 328.6400
    ## 1974 327.3750 327.3750 328.4000 329.6750
    ## 1975 328.5250 328.3250 329.4400 330.7750
    ## 1976 329.3500 329.1000 330.3500 331.6250
    ## 1977 331.5750 331.2000 332.3500 333.8600
    ## 1978 332.7400 332.6500 333.8250 334.9600
    ## 1979 333.9800 333.9500 335.1500 336.6600
    ## 1980 335.8750 336.0250 337.0600 338.2000
    ## 1981 336.7000 336.9400 338.4250 339.6000
    ## 1982 338.0250 337.9000 339.2750 340.4250
    ## 1983 339.8750 340.0200 341.1500 342.9800
    ## 1984 341.1200 341.4750 342.8500 344.1400
    ## 1985 343.0000 342.8000 344.2200 345.5750
    ## 1986 344.8000 344.1000 345.6200 346.8750
    ## 1987 346.4000 346.4400 347.9250 348.9250

``` r
hawai_ts_test
```

    ##          Jan     Feb     Mar     Apr     May     Jun     Jul     Aug     Sep
    ## 1988 350.400 351.775 352.125 353.580 354.175 353.750 352.220 350.300 348.750
    ## 1989 352.775 353.000 353.600 355.360 355.600 355.125 353.860 351.575 349.860
    ## 1990 353.650 354.650 355.480 356.175 357.075 356.080 354.675 352.900 350.940
    ## 1991 354.675 355.650 357.200 358.600 359.250 358.180 356.050 353.860 352.125
    ## 1992 355.900 356.680 357.900 359.075 359.540 359.125 357.000 354.860 353.025
    ## 1993 356.680 357.175 358.425 359.325 360.180 359.500 357.420 355.325 353.775
    ## 1994 358.320 358.900 359.925 361.220 361.650 360.900 359.460 357.375 355.925
    ## 1995 359.975 360.925 361.575 363.360 363.700 363.250 361.800 359.375 358.000
    ## 1996 362.025 363.175 364.060 364.700 365.325 364.880 363.475 361.320 359.400
    ## 1997 363.125 363.875 364.560 366.325 366.680 365.475 364.375 362.460 360.150
    ## 1998 365.340 366.200 367.375 368.525 369.140 368.750 367.600 365.720 363.925
    ## 1999 368.120 368.850 369.600 370.975 370.840 370.250 369.000 366.700 364.675
    ## 2000 369.020 369.375 370.400 371.540 371.650 371.625 369.940 367.950 366.540
    ## 2001 370.175 371.325 372.060 372.775 373.800 373.060 371.300 369.425 367.880
    ##          Oct     Nov     Dec
    ## 1988 348.960 350.000 351.360
    ## 1989 350.050 351.200 352.480
    ## 1990 351.225 352.700 354.140
    ## 1991 352.250 353.740 355.025
    ## 1992 353.420 354.200 355.350
    ## 1993 354.060 355.350 356.775
    ## 1994 356.020 357.575 359.060
    ## 1995 357.850 359.475 360.700
    ## 1996 359.625 360.740 362.375
    ## 1997 360.750 362.380 364.250
    ## 1998 364.320 365.550 366.925
    ## 1999 365.140 366.650 367.900
    ## 2000 366.725 368.125 369.440
    ## 2001 368.050 369.375 371.020

## 5 Modèle prévisionnel

Le modèle ARIMA (*auto-regressive integrated moving average*) a été
choisi pour effectuer les prévisions.

### 5.1 Prétraitement des données

La fonction `forecast::BoxCox.lambda()` estime pour nous la valeur
optimale de lambda λ. Ce lambda de `-0.07226294` sera utilisé dans le
modèle prévisionnel.

``` r
BoxCox.lambda(hawai_ts_train) # lambda optimale avec fonction BoxCox
```

    ## [1] -0.07226294

### 5.2 Modèle prévisionnel ARIMA

Comme la série temporelle semble démontrer une tendance et des
fluctuations saisonnière, le modèle ARIMA est approprié. La fonction
`forecast::auto.arima()` fournit le modèle prévisionnel. La fonction
optmise également les coefficients **ARIMA(p,d,q)(P,D,Q)m**.

``` r
hawai_arima <- hawai_ts_train %>% auto.arima(lambda = -0.0722) # modèle prévisionnel ARIMA
summary(hawai_arima)
```

    ## Series: . 
    ## ARIMA(1,1,1)(2,1,2)[12] 
    ## Box Cox transformation: lambda= -0.0722 
    ## 
    ## Coefficients:
    ##          ar1      ma1     sar1     sar2     sma1     sma2
    ##       0.3717  -0.7481  -0.3078  -0.1583  -0.5602  -0.2481
    ## s.e.  0.0934   0.0668   0.3033   0.0678   0.3046   0.2781
    ## 
    ## sigma^2 estimated as 1.285e-06:  log likelihood=2030.9
    ## AIC=-4047.79   AICc=-4047.46   BIC=-4020.89
    ## 
    ## Training set error measures:
    ##                      ME      RMSE       MAE         MPE     MAPE      MASE
    ## Training set 0.01790715 0.5405954 0.2793741 0.005020997 0.085311 0.2363958
    ##                  ACF1
    ## Training set 0.105587

Ainsi, avec un lambda = `-0.07226294`, le modèle prévisionnel arrive aux
coefficients **ARIMA(1,1,1)(2,1,2)\[12\]** détectant effectivement une
fluctuation saisonnière à une fréquence de 12 mois. Tous les
coefficients ont également été considérés par le modèle:
l’autorégression, l’intégration et la moyenne mobile.

### 5.3 Projection des prévisions

Voici la projection graphique des données prévues par le modèle ARIMA.

``` r
### Projection des prévisions pour les années 1988 à 2001 (les 30% de données test) 
Graph_prev <- hawai_arima %>% forecast(h = 169) %>% autoplot()
Graph_prev
```

![](Devoir5_SeriesTemporelles_reproductibilite_AD_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

La tendance et les fluctuations saisonnières ont bien été considérées
par le modèle ARIMA. La précision de la prédiction diminue évidemment
avec le temps. Une comparaison des prédictions avec la série de données
test permettra de juger de la fiabilité du modèle prévisionnel.

### 5.4 Comparaison des prédictions avec les donées test

Le modèle ARIMA semble avoir prédit de manière très acceptable les
données test. Les données test de couleur bleu foncée suivent assez
fidèlement les données test en jaune. Le modèle est donc **fiable** et
pourrait être amélioré avec plus de données d’entraînement et en
intégrant des **covariables** qui pourraient influencer les données
mesurées de CO2 atmosphérique (modèle dynamique).

``` r
### Comparaison aux données test 
Graph_comp <- hawai_arima %>% forecast(h = 169) %>% autoplot()+
  autolayer(fitted(hawai_arima)) +
  autolayer(hawai_ts_test, color = "yellow") +
  labs(x = "Année", y = "CO2")
Graph_comp
```

![](Devoir5_SeriesTemporelles_reproductibilite_AD_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

### 5.5 Analyse des résidus

La distribution des résidus ne semble pas démontrer de structure.
Toutefois, il semble y avoir une valeur aberrante collectée avant 1960.
Le graphique d’autocorrélation démontre cependant un quelques données
significativement corrélées à une fréquence de 12 mois. Il semble
également y avoir une structure sinusoïdale, mais qui n’est pas
significiative. Finalement, l’histrogramme de normalité semble indiquer
que les résidus ne suivent pas une distribution normale. Le test de
normalité de Shapiro-Wilk confirme que les résidus ne suivent pas une
distribution normale.

``` r
checkresiduals(hawai_arima) # Distribution des Résidus, graphique d'autocorrélation et histogramme de normalité 
```

![](Devoir5_SeriesTemporelles_reproductibilite_AD_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

    ## 
    ##  Ljung-Box test
    ## 
    ## data:  Residuals from ARIMA(1,1,1)(2,1,2)[12]
    ## Q* = 18.232, df = 18, p-value = 0.4404
    ## 
    ## Model df: 6.   Total lags used: 24

``` r
shapiro.test(residuals(hawai_arima)) # non-normal si p-value < seuil (0.05)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  residuals(hawai_arima)
    ## W = 0.52662, p-value < 2.2e-16

## 6\. Reproductibilité

Voici le [lien
github](https://github.com/ArianeDionne/GAA7007_Dev5_ADionne.git) pour
qui vous envoie vers le code reproductible du Devoir 5 sur les séries
temporelles et la reproductibilité

## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax
for authoring HTML, PDF, and MS Word documents. For more details on
using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that
includes both content as well as the output of any embedded R code
chunks within the document. You can embed an R code chunk like this:

``` r
summary(cars)
```

    ##      speed           dist       
    ##  Min.   : 4.0   Min.   :  2.00  
    ##  1st Qu.:12.0   1st Qu.: 26.00  
    ##  Median :15.0   Median : 36.00  
    ##  Mean   :15.4   Mean   : 42.98  
    ##  3rd Qu.:19.0   3rd Qu.: 56.00  
    ##  Max.   :25.0   Max.   :120.00

## Including Plots

You can also embed plots, for example:

![](Devoir5_SeriesTemporelles_reproductibilite_AD_files/figure-gfm/pressure-1.png)<!-- -->

Note that the `echo = FALSE` parameter was added to the code chunk to
prevent printing of the R code that generated the plot.
