
# ENDEI II - Clusters segun capacidades empresariales -------------------------

source('scripts/libraries.r')

semilla = 123

endei = readRDS('working/endei_db')


# Seleccion de variables ------------------------------------------------------

# variables input
var.inp = c('ide_endei_ii', 'capac_prod',	'prac_empre'
            # ,'rotacion_pla',	'part_perso', 'estimul_emp'
            # ,'eval_des'
            ,'abs_ih',	'prop_prof',	'prop_capac',	'gest_rrhh'
            ,'vinc_inst')

# variables output
var.oup = c('ide_endei_ii','Tam_nue','Rama_act','calif.ocde','exporta', 'exp.hincome', 'exp.dest','innovo', 'k.inac','ing',
            'empleo','va.tr', 'dnorm.calidad', 'joven', 'inno.ventas', 'id.ventas', 'dai','did', 'inno.tot', 'inno.id',
            'tc.va.tr', 'tc.empleo','tc.ventas')

# pierdo muchas - revisar NA
colna = colSums(is.na(endei %>% select(var.inp)))

df_varsel =  endei %>% 
  select(var.inp) %>% 
  drop_na()

ide = df_varsel$ide_endei_ii

df_varsel = df_varsel %>%
  select(-ide_endei_ii)


# Tratamiento de variables para cluster -------------------------------------------------

# Normalizo atributos entre 0 y 1
normalize = function(x){
  return ((x-min(x))/(max(x)-min(x)))
}

df_orig = df_varsel
df_varsel = df_varsel %>%
  mutate_all(funs(normalize))

df_varsel %>% summary()

saveRDS(df_varsel, 'working/df_endei2.rds')

# Genero matriz de distancias
df_varsel_dist = dist(df_varsel, method = 'manhattan')

# genero una muestra mas chica para que tarde menos
df_varsel_sample = df_varsel[sample(nrow(df_varsel), size = 300, replace = FALSE),]


# Matriz de correlaciones  ----------------------------------------------------
cor = cor(df_varsel) 
round(cor,2)
write.csv(cor, "res/tabla-matcor.csv")

GGally::ggcorr(df_varsel)


# Analisis de tendencia de cluster --------------------------------------------

# Hopkins
hopkins = get_clust_tendency(df_varsel, n = 300, graph = FALSE, 
                             seed = semilla)
hopkins$hopkins_stat %>% round(3)
# 0.802

# Heatmap -> calcular distancia con manhatan
fviz_dist(dist(df_varsel_sample, method = "manhattan"), 
          show_labels = FALSE,
          gradient = list(low = "steel blue", high = "white")) + 
  labs(title = "Heatmap")


# Eleccion de K optimo ------------------------------------------------------------------

# https://rpubs.com/Nitika/kmeans_Iris
# https://rpubs.com/Joaquin_AR/310338

# Elbow method
fviz_nbclust(df_varsel_sample, FUNcluster = kmeans, method = "wss", 
             diss = dist(df_varsel_sample, method = "manhattan")) +
  geom_vline(xintercept = 3, linetype = 2) +
  labs(subtitle = "Elbow method")

# Silhouette method
fviz_nbclust(df_varsel_sample, FUNcluster = kmeans, method = "silhouette", 
             diss = dist(df_varsel_sample, method = "manhattan")) +
  labs(subtitle = "Silhouette method")

# Gap statistic
set.seed(semilla) 
fviz_nbclust(df_varsel_sample, FUNcluster = kmeans,  method = "gap_stat", 
             k.max=10, nstart = 50, nboot = 50, 
             diss = dist(df_varsel_sample, method = "manhattan"),
             print.summary = F) +
  labs(subtitle = "Gap statistic method") 

gap_stat = clusGap(df_varsel_sample, FUN = kmeans, K.max = 10, B = 20)
fviz_gap_stat(gap_stat) +
  theme_minimal() + ggtitle("fviz_gap_stat: Gap Statistic")

# NbClust() function: 30 indices for choosing the best number of clusters
nb = NbClust(df_varsel_sample, distance = "manhattan", min.nc = 2,
             max.nc = 10, method = "kmeans", index="all")
fviz_nbclust(nb) +
  ggtitle("Numero optimo de clusters - K=2") + 
  xlab("Numero de clusters K") + 
  ylab("Frecuencia entre todos los Indices")


# k-means algorithm -----------------------------------------------------------

set.seed(123)

# 2K
K2 = Kmeans(df_varsel, centers=2, method="manhattan", iter.max= 1000, nstart = 100) 

table(K2$cluster)
t(K2$centers)

wss = sum(K2$withinss)

df2 = data.frame(df_varsel, cluster = K2$cluster)

# Silhuette 2K
df_spl2 = df2[sample(nrow(df2), 250), ]
df_k2_silhouette = silhouette(df_spl2$cluster, 
                                  dist(df_spl2 %>% select(-cluster), 
                                  method = 'manhattan')) 
fviz_silhouette(df_k2_silhouette)

# prueba2 = df2 %>% filter(cluster==1) %>% select(-cluster)
# K = Kmeans(prueba2, centers=2, method="manhattan", iter.max= 1000, nstart = 100) 
# table(K$cluster)
# t(K$centers)

# 3K
K3 = Kmeans(df_varsel, centers=3, method="manhattan", iter.max= 1000, nstart = 100) 

table(K3$cluster)
t(K3$centers)

wss = sum(K2$withinss)

df3 = data.frame(df_varsel, cluster = K3$cluster)

# Silhuette 3K
df_spl3 = df3[sample(nrow(df3), 250), ]
df_k3_silhouette = silhouette(df_spl3$cluster, 
                                  dist(df_spl3 %>% select(-cluster), 
                                       method = 'manhattan')) 
fviz_silhouette(df_k3_silhouette)


# Analisis cruzado

table(K2$cluster, K3$cluster)



# Analizo resultados ----------------------------------------------------------

# TSNE

# https://www.analyticsvidhya.com/blog/2017/01/t-sne-implementation-r-python/

# 2Dim
tsne = Rtsne::Rtsne(df_varsel_dist, dims = 2, 
                    verbose=TRUE, max_iter = 500, is_distance = T)
tsne$Y %>% as.data.frame() %>% 
  ggplot(aes(x = V1, y = V2, color = K2$cluster)) +
  geom_point()

# 3Dim
tsne3d = Rtsne::Rtsne(df_varsel_dist, dims = 3,  verbose=TRUE, max_iter = 400)
car::scatter3d(x=tsne3d$Y[,1], y=tsne3d$Y[,2],z=tsne3d$Y[,3],
          groups=as.factor(K3$cluster)
          # ,grid = FALSE
          ,surface = FALSE
          # ,surface.col = c("#0000FF", "#FF3333")
          ,ellipsoid = TRUE
)

# uno base completa con columna de cluster ------------------------------------

# aplico mismo filtro que para cluster
endei = endei[!endei %>% select(var.inp) %>% is.na() %>% rowSums(),]

endei_clu = cbind(endei, cluster = K$cluster)

saveRDS(endei_clu, 'working/endei_clu')
