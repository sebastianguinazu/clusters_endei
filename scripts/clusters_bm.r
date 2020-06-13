
# CLSUTERS -------------------------------------------------------------------------------

# cargo librerias
# source("librerias.r")

library(magrittr)
library(purrr)
library(dplyr)  
library(ggplot2)
library(tidyr)

library(factoextra)
library(NbClust)
library(amap)
library(cluster)
library(Hotelling)
library(clustertend)

library(ranger)

semilla = 1234
set.seed(semilla) 

setwd('C:/Users/Sebastian/Desktop/CAPACIDADES/cluster_endei_i')

# PREPROCESAMIENTO Y EDA -----------------------------------------------------------------

wmsdata = readr::read_csv("dat/wmsdata_2004_2015.csv")

str(wmsdata)
head(wmsdata)

## filtro argentina
table(wmsdata$country)

wmsdata_argentina = wmsdata %>% 
  filter(country == "Argentina")

table(wmsdata_argentina$N)

### Estadisticas descrpitvas (pre-norm) 
skimr::skim(wmsdata_argentina)

## Normalizo atributos entre 0 y 1
normalize = function(x){
    return ((x-min(x))/(max(x)-min(x)))
}

wmsdata_arg_orig = wmsdata_argentina

wmsdata_arg_mat = wmsdata_argentina %>% 
  select(-c(country, N, employment, sic2, management, operations,
            monitor, target, people))

# wmsdata_arg_mat = wmsdata_arg_mat %>% 
#     mutate_all(funs(normalize)) 

## Matriz de correlaciones
cor = cor(wmsdata_arg_mat) 
round(cor,2)

GGally::ggcorr(wmsdata_arg_mat)


# TENDENCIA DE CLUSTER -------------------------------------------------------------------

## Hopkins

# genero una muestra mas chica para que tarde menos

# hopkins alternativa 1
get_clust_tendency(wmsdata_arg_mat, n = nrow(wmsdata_arg_mat)-1,
                   gradient = list(low = "steelblue",  high = "white"))

# hopkins alternativa 2
set.seed(semilla) 
clustertend::hopkins(data = df.new.sample, n = nrow(df.new.sample) - 1)


# ELECCION K OPTIMO ----------------------------------------------------------------------

# https://rpubs.com/Nitika/kmeans_Iris
# https://rpubs.com/Joaquin_AR/310338

## Elbow method
fviz_nbclust(wmsdata_arg_mat, FUNcluster = kmeans, method = "wss", diss = dist(wmsdata_arg_mat, method = "manhattan")) +
    geom_vline(xintercept = 2, linetype = 2)+
    labs(subtitle = "Elbow method")

## Silhouette method
fviz_nbclust(wmsdata_arg_mat, FUNcluster = kmeans, method = "silhouette", diss = dist(wmsdata_arg_mat, method = "manhattan"))+
    labs(subtitle = "Silhouette method")

## Gap statistic
set.seed(semilla) 

fviz_nbclust(wmsdata_arg_mat, FUNcluster = kmeans,  method = "gap_stat", k.max=10, 
             nstart = 50, nboot = 50, diss = dist(wmsdata_arg_mat, method = "manhattan"),
             print.summary = TRUE) +
    labs(subtitle = "Gap statistic method") 

## NbClust() function: 30 indices for choosing the best number of clusters
nb = NbClust(wmsdata_arg_mat, distance = "euclidean", min.nc = 2,
             max.nc = 10, method = "kmeans", index="all")
fviz_nbclust(nb) +
  ggtitle("Numero optimo de clusters - K=2") + 
  xlab("Numero de clusters K") + 
  ylab("Frecuencia entre todos los Indices")


# K-MEANS ALGORTITHM ---------------------------------------------------------------------

# Kmeans{amap} algorithm
K = Kmeans(wmsdata_arg_mat, centers=2, method="manhattan", iter.max= 500, nstart = 50) 

table(K$cluster)
cent = t(K$centers)

wss = sum(K$withinss)

df = data.frame(wmsdata_arg_orig,K$cluster)

### Grafico de la distribucion de las variables x cluster

gdat_c = df %>%
  select(-c(country, N, employment, sic2, management, operations,
         monitor, target, people)) %>% 
  mutate(id = row_number()) %>%
  pivot_longer(-c(id,K.cluster), names_to="variable", values_to="value")

ggplot(gdat_c) +
  geom_density(aes(x=value, fill=factor(K.cluster)), alpha=0.5) +
  facet_wrap(~variable, scales = "free") +
  NULL

# correlaciones
GGally::ggpairs(df, aes(color=factor(K.cluster)))


# RELATIVE IMPORTANCE VARIABLES ----------------------------------------------------------

set.seed(semilla)

clustering.rf = ranger(factor(K.cluster) ~ ., data=df, mtry=5, min.node.size=50, num.trees=500
       , probability=T, importance="impurity")

# feature importance
varimp = tibble(
  variable = names(clustering.rf$variable.importance)
  ,importance = clustering.rf$variable.importance
) %>% arrange(-importance)

# feature importance plot
ggplot(varimp, aes(x=reorder(variable,importance), y=importance))+
  geom_bar(stat="identity", position="dodge") +
  coord_flip() +
  guides(fill=F)

# explicacion gini index = https://www.r-bloggers.com/variable-importance-plot-and-variable-selection/
