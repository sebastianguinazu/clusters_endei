
# ENDEI II - Clusters segun capacidades empresariales -------------------------

source('scripts/libraries.r')

endei = readRDS('working/endei_db')

semilla = 888

endei = endei %>% filter(outlier == 0)


# Seleccion de variables ------------------------------------------------------

# variables input 
var_inp = c('capac_prod',	'prac_empre', 'abs_ih',	'prop_calif',	
            'gest_rrhh', 'orga_tec', 'prop_capac', 'vinc_inst')

# me quedo con las variables para cluster
df_varsel =  endei %>% 
  select(var_inp)

# guardo obs NA para filtrar despues
index_na = which(rowSums(is.na(df_varsel)) > 0)

# elimino filas con na
df_varsel = df_varsel %>% drop_na()


# Tratamiento de variables para cluster -------------------------------------------------

# Normalizo atributos entre 0 y 1
normalize = function(x) {
  return ((x-min(x))/(max(x)-min(x)))
}

df_orig = df_varsel
df_varsel = df_varsel %>%
  mutate_all(funs(normalize))

df_varsel %>% summary()

saveRDS(df_varsel, 'working/df_endei2.rds')

# Descriptivos del dataset final
df_final_stats = rbind(
  df_varsel %>% summarise(across(everything(), median))
  ,df_varsel %>% summarise(across(everything(), mean))
  ,df_varsel %>% summarise(across(everything(), sd))
  )
df_final_stats = as.data.frame(t(as.matrix(df_final_stats)))  
df_final_stats %>% round(3)


# Genero matriz de distancias
df_varsel_dist = dist(df_varsel, method = 'manhattan')

# genero una muestra mas chica para que tarde menos
set.seed(semilla)
df_varsel_sample = df_varsel[sample(nrow(df_varsel), size = 400, replace = FALSE),]


# Matriz de correlaciones  ----------------------------------------------------
cor = cor(df_varsel) 
round(cor,2)
write.csv(cor, "res/tabla-matcor.csv")

GGally::ggcorr(df_varsel)


# Analisis de tendencia de cluster --------------------------------------------

# Hopkins
hopkins(df_varsel, n = 400)
# 0.828

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
  labs(subtitle = "Elbow method")

# Silhouette method
fviz_nbclust(df_varsel_sample, FUNcluster = kmeans, method = "silhouette", 
             diss = dist(df_varsel_sample, method = "manhattan")) +
  labs(subtitle = "Silhouette method")

# Gap statistic
set.seed(semilla) 
gap_stat = clusGap(df_varsel_sample, FUN = kmeans, K.max = 10, B = 20)
gap_stat %>% fviz_gap_stat() +
  theme_minimal() + ggtitle("Gap Statistic")


# k-means algorithm -----------------------------------------------------------

# 2K
set.seed(semilla)
K2 = Kmeans(df_varsel, centers=2, method="manhattan", iter.max= 800, nstart = 100) 

nclu2=table(K2$cluster)
t(K2$centers)

wss = sum(K2$withinss)

df2 = data.frame(df_varsel, cluster = as.factor(K2$cluster)) %>% 
  mutate(cluster=case_when(cluster == which.max(nclu2) ~ 1,
                           TRUE ~ 2))
table(df2$cluster)

# Silhuette 2K
df_spl2 = df2[sample(nrow(df2), 300), ]
df_k2_silhouette = silhouette(df_spl2$cluster, 
                                  dist(df_spl2 %>% select(-cluster), 
                                  method = 'manhattan')) 
fviz_silhouette(df_k2_silhouette)

# 3K
set.seed(semilla)
K3 = Kmeans(df_varsel, centers=3, method="manhattan", iter.max= 800, nstart = 100) 

nclu3 = table(K3$cluster)
t(K3$centers)

wss = sum(K2$withinss)

df3 = data.frame(df_varsel, cluster = as.factor(K3$cluster)) %>% 
  mutate(cluster=case_when(cluster == 2 ~ 1,
                           cluster == 1 ~ 2,
                           TRUE ~ 3))
table(df3$cluster)

# Silhuette 3K
df_spl3 = df3[sample(nrow(df3), 400), ]
df_k3_silhouette = silhouette(df_spl3$cluster, 
                                  dist(df_spl3 %>% select(-cluster), 
                                       method = 'manhattan')) 
fviz_silhouette(df_k3_silhouette) +
  theme_classic() +
  ylim(-0.1, 0.75)

# 4K
set.seed(semilla)
K4 = Kmeans(df_varsel, centers=4, method="manhattan", iter.max= 800, nstart = 100) 

nclu4 = table(K4$cluster)
t(K4$centers)

wss = sum(K4$withinss)

df4 = data.frame(df_varsel, cluster = K4$cluster) %>% 
  mutate(cluster=case_when(cluster == 3 ~ 1,
                           cluster == 2 ~ 2,
                           cluster == 1 ~ 3,
                           TRUE ~ 4))
table(df4$cluster)

# Silhuette 4K
df_spl4 = df4[sample(nrow(df4), 400), ]
df_k4_silhouette = silhouette(df_spl4$cluster, 
                              dist(df_spl4 %>% select(-cluster), 
                                   method = 'manhattan')) 
fviz_silhouette(df_k4_silhouette) +
  theme_classic() +
  ylim(-0.1, 0.75)


# Analisis cruzado
table(df2$cluster, df3$cluster)
# Analisis cruzado
table(df2$cluster, df4$cluster)
# Analisis cruzado
table(df3$cluster, df4$cluster)


# Analizo resultados ----------------------------------------------------------

# TSNE

# https://www.analyticsvidhya.com/blog/2017/01/t-sne-implementation-r-python/

set.seed(semilla)

df4_smpl = df4[sample(nrow(df3), 800), ]
df4_smpl_dist = dist(df4_smpl %>% select(-cluster), method = 'manhattan')

# 2Dim
tsne = Rtsne::Rtsne(df4_smpl_dist, dims = 2, 
                    verbose=TRUE, max_iter = 400, is_distance = T)
tsne = data.frame(V1 = tsne$Y[,1], V2 = tsne$Y[,2], cluster = df4_smpl$cluster) 
tsne_m = tsne %>% group_by(cluster) %>% 
  summarise(V1 = mean(V1), V2 = mean(V2))
ggplot(tsne, aes(x = V1, y = V2, color = as.factor(cluster))) +
  geom_point() +
  geom_point(data = tsne_m, size = 4) + theme_classic() +
  scale_color_grey(start=0.8, end=0.2)

# 3Dim
tsne3d = Rtsne::Rtsne(df4_smpl_dist, dims = 3,  verbose=TRUE, max_iter = 400)
car::scatter3d(x=tsne3d$Y[,1], y=tsne3d$Y[,2],z=tsne3d$Y[,3],
          groups=as.factor(df4_smpl$cluster)
          # ,grid = FALSE
          ,surface = FALSE
          # ,surface.col = c("#0000FF", "#FF3333")
          ,ellipsoid = TRUE
)


# Uno base completa con columna de cluster ------------------------------------

# aplico mismo filtro que para cluster
endei_clu = endei[-index_na,]

endei_clu = cbind(endei_clu, cluster = as.factor(df4$cluster))

saveRDS(endei_clu, 'working/endei_clu')

