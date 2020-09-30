
# ENDEI II - descriptivas de los clusters -------------------------------------

source('scripts/libraries.r')

semilla = 123

endei_clu = readRDS('working/endei_clu')


# 1. Descriptivas variables que se usaron para crear los clusters -------------

# variables usadas
var_inp = c('d_especinsum', 'd_especproce', 'd_trazabilidad',
  'd_problemas', 'd_mejoracont', 'd_diseno', 
  'd_deptoid', 'd_sistemas', 'prop_prof', 'prop_tec',
  
  )

endei_vi = endei_clu %>% select(all_of(c('cluster', var_inp)))

# opcion 1
endei_vi %>% ggplot(aes(x=prac_empre, group = cluster, fill = cluster)) + 
  geom_bar(aes(y = ..prop..)) +
  facet_wrap(~cluster) +
  scale_fill_grey(start=0.7, end=0.4) + theme_minimal()
  NULL

# opcion 2
endei_vi %>% ggplot(aes(x=prac_empre, group = cluster, fill = cluster)) + 
  geom_bar(position="dodge", aes()) +
  scale_fill_grey(start=0.7, end=0.4) + theme_minimal()
  NULL

# opcion 3
endei_vi %>% ggplot(aes(x=prac_empre, group = cluster, fill = cluster)) + 
  geom_bar(position="fill", aes(y = ..prop..)) +
  scale_fill_grey(start=0.7, end=0.4) + theme_minimal()
  NULL


# 2. Descriptivas de variables estructurales de las firmas --------------------

# variables output
var_oup = c('ide_endei_ii','Tam_nue','Rama_act','calif.ocde','exporta', 'exp.hincome', 'exp.dest','innovo', 'k.inac','ing',
            'empleo','va.tr', 'dnorm.calidad', 'joven', 'inno.ventas', 'id.ventas', 'dai','did', 'inno.tot', 'inno.id',
            'tc.va.tr', 'tc.empleo','tc.ventas')



