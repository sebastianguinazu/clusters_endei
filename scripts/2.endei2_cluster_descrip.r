
# ENDEI II - descriptivas de los clusters -------------------------------------

source('scripts/libraries.r')

semilla = 123

endei_clu = readRDS('working/endei_clu')


# 0. Distribucion de clusters -------------------------------------------------

# Proporcion de firmas por cluster

endei_clu %>% ggplot(aes(x = cluster, fill = cluster)) +
  geom_bar() +
  scale_fill_grey(start=0.8, end=0.4) + theme_classic()


# 1. Descriptivas variables que se usaron para crear los clusters -------------

# variables usadas
var_inp = c('capac_prod',	'prac_empre', 'abs_ih',	'prop_calif',	
            'gest_rrhh', 'orga_tec', 'prop_capac', 'vinc_inst')

endei_vi = endei_clu %>% select(all_of(c('cluster', var_inp)))

# a.capacidades productivas

endei_vi %>% ggplot(aes(x=capac_prod, group = cluster, fill = cluster)) + 
  geom_bar(position="dodge", aes(y=..prop..)) +
  scale_fill_grey(start=0.8, end=0.4) + theme_classic()

endei_vi %>% ggplot(aes(x=prac_empre, group = cluster, fill = cluster)) + 
  geom_bar(position="dodge", aes(y=..prop..)) +
  scale_fill_grey(start=0.8, end=0.4) + theme_minimal()

# b.capacidades de absorcion

endei_vi %>% ggplot(aes(x=abs_ih, group = cluster, fill = cluster)) + 
  geom_bar(position="dodge", aes()) +
  scale_fill_grey(start=0.7, end=0.4) + theme_minimal()

endei_vi %>% ggplot(aes(x=prop_calif, group = cluster, fill = cluster)) + 
  geom_density(alpha = 0.6) +
  scale_fill_grey(start=0.7, end=0.6) + theme_minimal()

# c. capacidades organizacionales

endei_vi %>% ggplot(aes(x=gest_rrhh, group = cluster, fill = cluster)) + 
  geom_bar(position="dodge", aes()) +
  scale_fill_grey(start=0.7, end=0.4) + theme_minimal()

endei_vi %>% ggplot(aes(x=orga_tec, group = cluster, fill = cluster)) + 
  geom_bar(position="dodge", aes()) +
  scale_fill_grey(start=0.7, end=0.4) + theme_minimal()

# d. capacidades de aprendizaje

endei_vi %>% ggplot(aes(x=prop_capac, group = cluster, fill = cluster)) + 
  geom_density(alpha = 0.6) +
  scale_fill_grey(start=0.7, end=0.4) + theme_minimal()

endei_vi %>% ggplot(aes(x=abs_ih, group = cluster, fill = cluster)) + 
  geom_bar(position="dodge", aes()) +
  scale_fill_grey(start=0.7, end=0.4) + theme_minimal()

  
# 2. Descriptivas de variables originales de ENDEI para cluster --------------
  
var_orig = c('d_especinsum', 'd_especproce', 'd_trazabilidad', 'd_problemas',
             'd_mejoracont', 'd_diseno', 'd_deptoid', 'd_montcomp', 'd_montec', 
             'prop_prof', 'prop_tec', 'd_rrhhdto', 'd_perffor', 'd_evaldes',
             'd_sistemas', 'd_sapoydec', 'd_softgest', 
             'capacit_nojer', 'capacit_ger', 'capacit_sup', 
             'd_vincpub', 'd_vincfir', 'd_vincase')

endei_vo = endei_clu %>% select(all_of(c('cluster', var_orig)))

clust_mean = endei_vo %>% group_by(cluster) %>%
  summarise(across(everything(), mean))

clust_mean = as.data.frame(t(as.matrix(clust_mean)))  
clust_mean  
  

# 3. Descriptivas de variables estructurales de las firmas --------------------

# variables output
var_oup = c('tam_mue', 'joven', 'k_inac', 'rama_act', 'calif_ocde', 'exporta', 
            'innovo', 'ing', 'va.tr', 'dnorm.calidad',  
            'inno.ventas', 'id.ventas', 'dai','did', 'inno.tot', 'inno.id',
            'empleo', 'tc.va.tr', 'tc.empleo','tc.ventas')

endei_vo = endei_clu %>% select(all_of(c('cluster', var_oup)))


# 4. Obstáculos ---------------------------------------------------------------

endei_ob = endei_clu %>% select(all_of(c('cluster', vars_obst)))

clust_ob_mean = endei_ob %>% group_by(cluster) %>%
  summarise(across(everything(), mean))

clust_ob_mean = as.data.frame(t(as.matrix(clust_ob_mean)))  
