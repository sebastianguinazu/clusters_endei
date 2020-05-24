
# ENDEI II - Clusters segun capacidades empresariales -----------------------------------

path = getwd()
source('scripts/libraries.r')

semilla = 123

# Seleccion de variables ----------------------------------------------------------------

endei = readRDS('working/endei_db')

# variables input
var.inp = c('ide_endei_ii','dvinc.firmas','dvinc.pub', 'svinc.firmas', 'svinc.pub', 'depto.id', 'dorganiza', 
            'dnorm.calidad', 'd.especific', 'd.gesproydis','d.diseno','d.mejoracont','d.problemas','d.trazabilidad',
            'prop.prof', 'prop.tec', 'prop.ing', 'scap.organiza', 'd.rotacion', 'capacit.jer', 'capacit.sup', 
            'capacit.nojer', 'part.personal', 'd.evaldes', 'dcap.func', 'difu.bpracticas', 'd.basedatos', 'd.estimul')

# variables output
var.oup = c('ide_endei_ii','Tam_nue','Rama_act','calif.ocde','exporta', 'exp.hincome', 'exp.dest','innovo', 'k.inac','ing',
            'empleo','va.tr', 'joven', 'inno.ventas', 'id.ventas', 'dai','did', 'inno.tot', 'inno.id',
            'tc.va.tr', 'tc.empleo','tc.ventas')

# pierdo muchas - revisar NA
colna = colSums(is.na(endei %>% select(var.inp)))

df.new =  endei %>% 
  select(var.inp) %>% 
  drop_na()

ide = df.new$ide_endei_ii

df.new = df.new %>%
  select(-ide_endei_ii)


# Tratamiento de variables para cluster -------------------------------------------------

# Orden de variables
df.new = df.new[c('d.especific','d.trazabilidad','d.problemas','d.mejoracont','d.gesproydis',
                  'dnorm.calidad','d.rotacion','part.personal','depto.id','prop.prof','prop.tec',
                  'capacit.jer','capacit.nojer','dvinc.firmas','dvinc.pub',
                  'd.evaldes', 'dcap.func', 'difu.bpracticas', 'd.basedatos', 'd.estimul')]

# Normalizo atributos entre 0 y 1
normalize = function(x){
  return ((x-min(x))/(max(x)-min(x)))
}

df.orig = df.new
df.new = df.new %>% 
  mutate_all(funs(normalize)) 

# genero una muestra mas chica para que tarde menos
df.new.sample = df.new[sample(nrow(df.new), size = 500, replace = FALSE),]


# Matriz de correlaciones  ------------------------------------------------------
cor = cor(df.new) 
round(cor,2)
write.csv(cor, "res/tabla-matcor.csv")

GGally::ggcorr(df.new)


# Analisis de tendencia de cluster ------------------------------------------------------

# Hopkins

# hopkins alternativa 1
get_clust_tendency(df.new.sample, n = nrow(df.new.sample)-1,
                   gradient = list(low = "steelblue",  high = "white"))
# hopkins alternativa 2
set.seed(semilla) 
hopkins(data = df.new.sample, n = nrow(df.new.sample) - 1)


# Eleccion de K optimo ------------------------------------------------------------------

# https://rpubs.com/Nitika/kmeans_Iris
# https://rpubs.com/Joaquin_AR/310338

# Elbow method
fviz_nbclust(df.new, FUNcluster = kmeans, method = "wss", diss = dist(df.new, method = "manhattan")) +
  geom_vline(xintercept = 2, linetype = 2)+
  labs(subtitle = "Elbow method")

# Silhouette method
fviz_nbclust(df.new, FUNcluster = kmeans, method = "silhouette", diss = dist(df.new, method = "manhattan"))+
  labs(subtitle = "Silhouette method")

# Gap statistic
set.seed(semilla) 
fviz_nbclust(df.new.sample, FUNcluster = kmeans,  method = "gap_stat", k.max=10, 
             nstart = 20, nboot = 20, diss = dist(df.new, method = "manhattan"),
             print.summary = TRUE) +
  labs(subtitle = "Gap statistic method") 

# NbClust() function: 30 indices for choosing the best number of clusters
nb = NbClust(df.new, distance = "manhattan", min.nc = 2,
             max.nc = 10, method = "kmeans", index="all")
fviz_nbclust(nb) +
  ggtitle("Numero optimo de clusters - K=2") + 
  xlab("Numero de clusters K") + 
  ylab("Frecuencia entre todos los Indices")
