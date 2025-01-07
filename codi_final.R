# PRÀCTICA 2: TIPOLOGIA I CICLE DE VIDA DE LES DADES

# Farem servir el paquet countrycode per obtenir el continent on es situa cada
# país a partir del codi ISO - alpha 2.
install.packages('countrycode')
install.packages('missForest')
install.packages('caret')
install.packages('performanceEstimation')
install.packages("dbscan")

library('caret')
library('dplyr')
library('e1071')
library('tidyverse')
library('missForest')
library('countrycode')
library('randomForest')
library('performanceEstimation')
library('dbscan')

# Guardem el dataset obtingut a la pràctica anterior:
setwd('/Users/ignasi/Desktop/Master/Tipologia i Cicle Vida Dades/PAC4')
data_frame <- read.csv('dataset.csv',na.strings='')
head(data_frame)

# 1. Descripció del dataset.
# 
# Fem una petita descripció del dataset amb els maxims minims i els valors nuls d'aquest:
summary(data_frame)

# 2. Integració i selecció de les dades:  
# 
# Primer de tot, ens quedarem amb unes quantes de les columnes del dataset original. Eliminarem les següents: SMI.Mon.Local, ja que aporta la mateixa informació que SMI (USD), i així els valors són comparables entre tots els paisos.
# 
# També eliminarem Deute.PIB, i Dèficit.PIB i ens quedarem amb les columnes Deute.Total, Deute.Per.Capita, Dèficit i PIB.anual. Fem això ja que les columnes Deute.PIB i Dèficit.PIB estan clarament correlacionades amb Duete, Dèficit i PIB.anual.
# 
# De moment mantindrem Codi, ja que ens anirà bé per a la preparació de les dades, però després el traurem, ja que dona la mateixa informació que Pais.

data <- data_frame[,c('Pais', 'Fecha', 'SMI', 'Deute.Total', 'Deute.Per.Capita', 'Dèficit', 'Atur', 'PIB.anual', 'Var..PIB....', 'IPC.Anual.General', 'Codi')]

# Simplificarem alguns dels noms de les columnes:
names(data)[names(data) == 'Deute.Total'] <- 'Deute'
names(data)[names(data) == 'Deute.Per.Capita'] <- 'Deute.Capita'
names(data)[names(data) == 'Dèficit'] <- 'Deficit'
names(data)[names(data) == 'PIB.anual'] <- 'PIB'
names(data)[names(data) == 'Var..PIB....'] <- 'PIB.Var'
names(data)[names(data) == 'IPC.Anual.General'] <- 'IPC'
head(data)

# Afegirem una columna per indicar el continent en el qual es troba cada país. Aquesta columna serà la variable objectiu

# Creem una nova columna, Continent, on a partir de la columna Codi
# extraiem el continent. Així, ndiquem que l'origen de les dades és iso2c
# i el destí continent.
data$Continent <- countrycode(sourcevar = data[, "Codi"],
                              origin = "iso2c",
                              destination = "continent")

# Eliminem les files corresponents a la zona Euro (EA), ja que
# només ens interessa estudiar paisos:

data <- data[ !(data$Pais == 'Zona Euro'), ]

# Mirem si hi ha algun país amb continent NA:
data[which(is.na(data$Continent)), ]

# Comprovem que el resultat ha sigut satisfactori:
head(data)

# Estudiem la distribució de continents:
barplot(table(data$Continent))

# Observem que Oceania té molts pocs països, en comparació a la resta.
# Sabem que això és problemàtic a l'hora d'analitzar les dades, tant per mètodes
# supervisats com no supervisats. Aplicar tècniques de random oversampling
# pot ser útil per augmentar les dades dels altres continents, i que
# tots tinguin com Àfrica, per exemple, però amb Oceania hi ha molta diferència.

# Per tant, el que farem serà eliminar aquests valors del dataframe.
data <- data[ !(data$Continent == 'Oceania'), ]

# Mirem quants nuls tenim:
colSums(is.na(data))

# Com que per algunes columnes tenim un nombre de nuls molt elevat,
# hem decidit que buscarem algunes de les dades que falten per afegir-les
# directament, i així tenir més valors fiables. Ho farem per les columnes
# SMI, Atur i IPC, que són les columnes amb més valors faltants.

# Pels altres valors faltants, després aplicarem altres mètodes d'imputació.
# Així tenim les dues

# Carreguem les dades noves:
data_smi <- read.csv('smi_data.csv', na.strings='', sep=';')
data_atur <- read.csv('atur_data.csv', na.strings='', sep=';')
data_ipc <- read.csv('ipc_data.csv', na.strings='', sep=';')

# Com que els noms dels països estan en anglès, afegirem una columna amb el
# codi de cada país, per poder-los associar després al nostre conjunt de dades.
data_smi$Codi <- countrycode(sourcevar = data_smi[, "Pais"],
                             origin = "country.name",
                             destination = "iso2c")
data_atur$Codi <- countrycode(sourcevar = data_atur[, "Pais"],
                              origin = "country.name",
                              destination = "iso2c")
data_ipc$Codi <- countrycode(sourcevar = data_ipc[, "Pais"],
                             origin = "country.name",
                             destination = "iso2c")

data_smi <- data_smi[,c('Codi', 'SMI')]
data_atur <- data_atur[,c('Codi', 'Atur')]
data_ipc <- data_ipc[,c('Codi', 'IPC')]

# Per tenir dades més consistents, en comptes d'agafar només els valors que
# falten, substituirem tots els valors de cada columna, així tindrem que
# cada columna prové de la mateixa font d'informació.
data <- dplyr::rows_update(data, data_smi, unmatched = 'ignore')
data <- dplyr::rows_update(data, data_atur, unmatched = 'ignore')
data <- dplyr::rows_update(data, data_ipc, unmatched = 'ignore')

# Comprovem que hem reduït molt el nombre de valors nuls per aquestes columnes:
colSums(is.na(data))

# Mirem la classe de cada columna:
sapply(data, class)

# Corregim la variable Atur perque interpreti com a numèric:
data$Atur <- gsub(",", ".", data$Atur)
data$Atur <- as.numeric(data$Atur)
sapply(data, class)

head(data)

# Hi ha files repetides idènticament, com la d'Alemanya. Eliminem-les:
data <- distinct(data)
head(data)

# Observem els útlims valors:
tail(data, 8)

# Veiem que hi ha files diferents que corresponen al mateix país, però
# diferents anys. Això és degut a que quan obteníem les dades de la web, sempre
# agafavem el valor més actualitzat. Per la majoria de països, les dades
# estaven actualitzades i tenim tota la informació de l'any
# 2023. Per altres països teníem columnes d'aquest any, i columnes d'altres
# anys, ja que era l'última informació disponible.

# Com que el que ens interessa és tenir les últimes dades disponibles, encara
# que potser no siguin totes de l'any 2023, el que farem serà ajuntar les files
# corresponents al mateix país, i eliminarem la columna Fecha. Així tindrem
# la informació més recent disponible per a cada país.

# Primer de tot eliminarem tots aquells paisos que no tinguin dades,
# és a dir, aquells que no tenen valor a la columna Fecha.
data <- data[!is.na(data$Fecha),]

# Com que ara no té sentit guardar la columna Fecha, ja que les dades no són
# del mateix any, si no de l'any disponible, eliminarem la columna Fecha.
# També eliminarem la columna Codi, ja que aporta la mateixa informació que
# la columna Pais.

# Eliminem les columnes Fecha i Codi:
data <- data[,c('Pais', 'SMI', 'Deute', 'Deute.Capita', 'Deficit', 'Atur', 'PIB', 'PIB.Var', 'IPC', 'Continent')]

# Procedim a ajuntar les files amb valors faltants.

# Solució extreta de:
# https://stackoverflow.com/questions/45515218/combine-rows-in-data-frame-containing-na-to-make-complete-row

coalesce_by_column <- function(data) {
  return(coalesce(data[1], data[2]))
}

data <- data %>%
  group_by(Pais, Continent) %>%
  summarise_all(coalesce_by_column)

# Comprovem que ara és correcte:
tail(data, 8)

# A falta de variables categoriques, en crearem a través de la discretització. Per afegir variables categòriques, substituirem els valors de les variables IPC, Atur i PIB.Var per valor categòrics.

colSums(is.na(data))

# Comencem per discretitzar la columna Atur. Veiem com es distribueixen els valors en aquesta columna, per entendre quins intervals té sentit definir:

# Fem un histograma:
hist(data$Atur, breaks= 13)

# Definirem 4 trams, Baix, Moderat, Elevat i Massiu:
tall_atur = c(0,5,10,20,Inf)
data$Atur <- cut(data$Atur, breaks = tall_atur, labels = c('Baix', 'Moderat', 'Elevat', 'Massiu'), right = F)

head(data)

# Estudiem el mateix per a l'IPC:
boxplot(data$IPC)

boxplot(data$IPC, outline = FALSE)

min(data$IPC, na.rm=T)

max(data$IPC, na.rm=T)

# Definirem 4 trams, Baix, Moderat, Elevat i Massiu:
tall_ipc = c(0,6,10,17,Inf)
data$IPC <- cut(data$IPC, breaks = tall_ipc, labels = c('Baix', 'Moderat', 'Elevat', 'Massiu'), right = F)

head(data)

# Estudiem el mateix per a PIB.Var:
boxplot(data$PIB.Var)

boxplot(data$PIB.Var, outline = F)

# Definirem 4 trams, Negatiu, Baix, Moderat i Elevat:
tall_pib = c(-Inf,0,3,6,Inf)
data$PIB.Var <- cut(data$PIB.Var, breaks = tall_pib, labels = c('Negatiu', 'Baix', 'Moderat', 'Elevat'), right = F)

# Es demana fer una descripció breu del dataset:
# head(data, n= 10)

# Observem que ja tenim les variables categòriques definides en format factor:
sapply(data, class)

## Fem els paisos com a indexos del df
data <- as.data.frame(data)
rownames(data) <- data$Pais
data <- data[,-1]
head(data)

# 3 Neteja de dades

# Ens hem quedat amb les següents variables:
#   - Pais
#   - Continent
#   - SMI
#   - Deute
#   - Deute.Capita
#   - Deficit
#   - Atur
#   - PIB
#   - PIB.Var
#   - IPC
# 
# A continuació, acabarem de fer-ne la neteja.
# 
# 3.2 Tipus dels atributs

# Mirem primer les classes de cada columna:
sapply(data, class)

# Ja s'han convertit moltes variables categòriques a factor al llarg del procés.
# Només ens falta convertir Continent a factor.
# La resta de variables ja tenen el format apropiat:
data$Continent <- as.factor(data$Continent)
# Comfirmem els canvis:
sapply(data, class)

#3.3 Valors extrems

# Anem observant les variables i veiem si ens trobem amb algun extrem.

# Mirem la variable SMI
hist((data$SMI), breaks=15)

# La deixarem tal com està.

# Mirem la columna Deute:
hist((data$Deute), breaks=15)

# Només tenim els dos següents valors extrems:
data[which(data$Deute > 1e7), ]

# Mirem el deute per càpita:
hist((data$Deute.Capita), breaks=15)

# Tenim els països seguents:
data[which(data$Deute.Capita > 80000), ]

# Mirem el dèficit:
hist((data$Deficit), breaks=15)

data[which(data$Deficit < -500000), ]

# Mirem l'atur:
barplot(table(data$Atur))

# Mirem el PIB:
hist((data$PIB), breaks=15)

data[which(data$PIB > 1e7), ]

# Mirem la variació del PIB:
barplot(table(data$PIB.Var))

# Mirem l'IPC:
barplot(table(data$IPC))

# Per tant, els valors que podríem considerar Outliers en alguna de les
# categories són Estats Units, China i Singapur.

# Com que són pocs, eliminem els valors outliers.
data$PIB[data$PIB > 1e7] <- NA
data$Deute[data$Deute > 1e7] <- NA
data$Deficit[data$Deficit < -500000] <- NA
data$Deute.Capita[data$Deute.Capita > 80000] <- NA

# 3.1 Omplir els 0s

# Amb les dades ja netes, el que queda per fer és aplicar alguns mètodes per
# tal de reduir la quantitat de valors nuls.

# Mirem quants valors nuls tenim:
colSums(is.na(data))
nrow(data)

# Pel que fa al SMI, els valors nuls corresponen a països que no tenen
# SMI definit: o bé no tenen legislació al respecte, o bé hi ha alguns convenis,
# però això no és SMI. En aquests casos, no tindria tant sentit substituir els
# nuls per algun valor mitjà. Per tant, considerarem que el SMI és 0.
data[which(is.na(data$SMI)), ]$SMI <- 0

# Per la resta de valors, sí que farem imputació de valors buits. Notem que ja
# hem millorat molt la presència de nuls en l'apartat anterior, ja que les
# columnes atur, IPC i SMI les hem canviat per valors nous, fiables i més
# complets. Així, les correccions que fem amb diversos mètodes seran més
# acurades, tenim més valors de referència correctes.

# Ja podem començar amb la imputació de valors buits, ho farem amb missForest.
# Fem la susbstitució usant la informació de totes les columnes, pero guardem
# els resultats a una nova variable data_no_NA:
data_no_NA <- missForest(data)$ximp

# Creem un dataset data_final sense modificar l'original, però no alterem
# de moment el conjunt data amb valors nuls.

# Ens interessa guardar el conjunt amb valors nuls, ja que per a l'aprenentatge
# supervisat, quan fem la divisió en conjunts de train i test, no volem
# que informació de les dades de train passi a les dades de test. Aleshores
# farem la divisió primer, i completarem els nuls per separat.
data_final <- data_no_NA

head(data_final)

# Comprovem que ja no tenim nuls:
colSums(is.na(data_final))

# A continuació, fem un resum de les dades finals:
summary(data_final)

# 3.4 Altres mètodes
# 
# A part dels mètodes mencionats explícitament en l'apartat 3, en l'apartat 2 també hem aplicat alguns passos per a la neteja de dades.
# 
# - Hem eliminat algunes columnes que són redundants.
# - Per exemple, hem canviat i simplificat el nom de les columnes.
# - Hem eliminat els països d'Oceania, per tenir un equilibri en les variables objetiu.
# - Hem eliminat files duplicades.
# - Hem eliminat files completament buides.
# - Hem ajuntat files corresponents al mateix pais per completar valors faltants.
# - Hem buscat la informació corresponent a les columnes amb més nuls, modificant l'original per millorar la qualitat del conjunt de dades.
# - Hem fet que el nom de la fila sigui el nom del pais.


# Creem un csv amb les dades finals
write.csv(data_final, "final_data.csv")

# 4. Anàlisi de dades
# 
# 4.1 Aplicar un model supervisat i un model no supervisat.
# 
# A. Model supervisat
# 
# Primer prepararem els conjunts d'entrenament i de test.

# Primer de tot, farem la divisió de les dades en un conjunt de test i de train:
train_ids <- createDataPartition(y=data$Continent, p=0.7, list=FALSE)
data_train <- data[train_ids,]
data_test <- data[-train_ids,]

# Comprovem que s'ha conservat la proporció de continents en cada conjunt:
table(data$Continent) / nrow(data) * 100
table(data_train$Continent) / nrow(data_train) * 100
table(data_test$Continent) / nrow(data_test) * 100

# Recordem que no hem fet la substitució dels valors nuls, ara és el
# moment de fer-ho amb els conjunts separats. Així no introduïm biaix a les
# dades.
colSums(is.na(data_train))
data_train_noNA <- missForest(data_train)$ximp
cols = c( 'Deute', 'Deute.Capita', 'Deficit', 'Atur', 'PIB', 'PIB.Var', 'IPC')
data_train[,cols] <- data_train_noNA[,cols]
colSums(is.na(data_train))

colSums(is.na(data_test))
data_test_noNA <- missForest(data_test)$ximp
cols = c( 'Deute', 'Deute.Capita', 'Deficit', 'Atur', 'PIB', 'PIB.Var', 'IPC')
data_test[,cols] <- data_test_noNA[,cols]
colSums(is.na(data_test))

# Ara augmentarem les classes minoritàries i que totes tinguin un nombre
# similar al d'Africa.
table(data_train$Continent)

# Incrementem en 15 els exemples d'Americas
data_train <- dplyr::bind_rows(data_train, tail(smote(Continent ~ ., data_train, perc.over = 1, k = 5, perc.under = 0), n=15))

# Incrementem en 10 els exemples d'Europe
data_train <- dplyr::bind_rows(data_train, tail(smote(Continent ~ ., data_train, perc.over = 1, k = 5, perc.under = 0), n=10))

# Incrementem en 6 els exemples d'Asia
data_train <- dplyr::bind_rows(data_train, tail(smote(Continent ~ ., data_train, perc.over = 1, k = 5, perc.under = 0), n=6))

# Incrementem en 1 els exemples d'Africa
data_train <- dplyr::bind_rows(data_train, tail(smote(Continent ~ ., data_train, perc.over = 1, k = 5, perc.under = 0), n=1))

table(data_train$Continent)

# Fem el mateix pel conjunt de test:
table(data_test$Continent)

# Incrementem en 6 els exemples d'Americas
data_test <- dplyr::bind_rows(data_test, tail(smote(Continent ~ ., data_test, perc.over = 1, k = 5, perc.under = 0), n=6))

# Incrementem en 4 els exemples d'Europe
data_test <- dplyr::bind_rows(data_test, tail(smote(Continent ~ ., data_test, perc.over = 1, k = 5, perc.under = 0), n=4))

# Incrementem en 2 els exemples d'Asia
data_test <- dplyr::bind_rows(data_test, tail(smote(Continent ~ ., data_test, perc.over = 1, k = 5, perc.under = 0), n=2))

# Incrementem en 1 els exemples d'Africa
data_test <- dplyr::bind_rows(data_test, tail(smote(Continent ~ ., data_test, perc.over = 1, k = 5, perc.under = 0), n=1))

table(data_test$Continent)

# Ara separem la variable a predir, Continent:
X_train <- subset(data_train, select = -Continent )
y_train <- data_train['Continent']
X_test <- subset(data_test, select = -Continent )
y_test <- data_test['Continent']

# El primer que farem serà aplicar un model randomForest.

# Creem el model amb 500 arbres, i la data d'entrenament:
rf_model <- randomForest(Continent~., data_train, ntree = 500)

# Mirem la importància de cada atribut. Sembla que el Deute.Capita i SMI
# són els valors més alts.
importance(rf_model)

# Apliquem el model a les dades d'entrenament.
rf_pred_train <- predict(rf_model, data_train)
confusionMatrix(rf_pred_train, data_train$Continent)

# Com és d'esperar, s'han obtingut resultats perfectes amb les dades
# d'entrenament. Sabem que els models basats en DecisionTrees són propensos
# a l'overfitting.

# Mirem que passa amb les dades de test:
rf_pred_test <- predict(rf_model, data_test)
confusionMatrix(rf_pred_test, data_test$Continent)

# Aconseguim predir correctament més de la meitat dels paisos de forma correcta.

# Ara provarem un altre model supervisat, Support-Vector Machines.
# Creem el model:
svm_model = svm(formula = Continent~.,
                data = data_train,
                type = 'C-classification',
                kernel = 'linear')

# L'apliquem a les dades d'entrenament:
svm_pred_train <- predict(svm_model, data_train)
confusionMatrix(svm_pred_train, data_train$Continent)

# No obtenim uns resultats tant bons com amb RandomForest.

# Veiem que passa amb el conjunt de test:
svm_pred_test <- predict(svm_model, data_test)
confusionMatrix(svm_pred_test, data_test$Continent)

# Tenim uns resultats similars als obtinguts amb RandomForest, en la majoria
# de execucions són una mica pitjor.

#B. Model no supervisat

# Per fer un model no supervisat en farem us del mètode de mètodes de partició
# per determinar si segons les propietats dels diferents valors numèrics són
# suficients per determinar si el continent el qual es representat es pot formar
# un cluster sobre aquest

# Primer de tot crearem un conjunt amb el total de valors numerics sense el
# continent
data_to_cluster <- data_final[,c("SMI","Deute","Deute.Capita","Deficit","PIB")]

# Un cop amb aquestes dades es pot aplicar el kmeans i k = 4 sent el numero de
# clusters que volem crear
kmeans.res <- kmeans(data_to_cluster,4)

# Un cop tenim els camps en clusters, calcularem quins seràn els clusters reals
# comparats amb els continents

table(data_final$Continent, kmeans.res$cluster)

# Un cop tenim els camps en clusters, calcularem quins seràn els clusters reals
# comparats amb els continents

table(data_final$PIB.Var, kmeans.res$cluster)

# Ho farem igualment per l'atur
table(data_final$Atur, kmeans.res$cluster)

# Ho fem igualment per l'IPC
table(data_final$IPC, kmeans.res$cluster)

# Veiem que per a aquest mètode no supervisat, de clustering, no arriba a fer
# cluster correcte del continent segons les dades numeriques que hem
# proporcionat, aquestes sent el SMI, el Deute, el deute per càpita, el dèficit
# i el PIB

# Farem servir igualment un altre tipus de model no supervisat, aquest sent
# aquests mètodes basats en la densitat de veïnts.
# Per això farem servir el mètode dbscan

#Primer de tot haurem de calucalr quin es el valor de eps, que es el que es
# defineix com la distancia promitja a la que haurien d'estar punts d'un
# matiex cluster
distcluster <- dbscan::kNNdist(data_to_cluster, k = 5)

# Graficar las distancias para observar el codo
plot(distcluster, type = "l")

# Veiem que hi ha dos punts on es podría explicitar els dos valors de eps, per
# tant, ho probarem amb els dos

dbscan.result <- dbscan(data_to_cluster, eps = 90000, minPts = 2)
dbscan.result
table(data_final$Continent, dbscan.result$cluster)
table(data_final$Atur, dbscan.result$cluster)
table(data_final$IPC, dbscan.result$cluster)

# Veiem amb aquest mètode tampoc hem aconseguit agrupar correctament les dades
# pel que es podría confirmar que aquestes variables no segueixen una
# distribucuó de clustering segons els diferents paràmteres que tenin al
# dataset

#4.2 proba de contrast d'hipotesis

# Volem fer ara un contrast d'hipòtesis sobre com el PIB es veu influit pel
# nivell d'atur del pais.

# Veient les dades, veiem que les dades no segueixen una distribució normal
# hem de compovar que les distribucions es troben en un altre distribució.

shapiro.test(data_final$PIB)

# Veiem que ambdos camp de PIB no compleixen distribucions normals,
# ja que el p-value < 0.05 fent que podem rebutjar la hipòtesi que aquesta
# variable segueix una distribució normal.


# Per comprovar l’homoscedasticitat d'aquesta variable no té una homogeneitat
# de variançes.

fligner.test(PIB ~ Atur, data = data_final)

# Veiem que el p-value es menor de 0.05 pel que podem afirmar que per aquestes
# dades es faran tests no paramètrics, com el test de Wilcox. Si analitzem
# aquestes variables segons els diferents valors veiem:

wilcox.test(PIB ~ Atur, data = data_final, subset = Atur %in% c('Baix', 'Massiu'))
wilcox.test(PIB ~ Atur, data = data_final, subset = Atur %in% c('Baix', 'Elevat'))
wilcox.test(PIB ~ Atur, data = data_final, subset = Atur %in% c('Baix', 'Moderat'))

# Si ara calculem les mitjes podem extreure informació de quins PIB són
# majors que altres.
mean(data_final$PIB[data_final$Atur == 'Baix'])
mean(data_final$PIB[data_final$Atur == 'Moderat'])
mean(data_final$PIB[data_final$Atur == 'Elevat'])
mean(data_final$PIB[data_final$Atur == 'Massiu'])
