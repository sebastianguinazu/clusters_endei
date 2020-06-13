
# ENDEI II - descriptivas de los clusters -------------------------------------

# levanto la base original con la marca del cluster

endei_clu = readRDS('working/endei_clu')


# 1. Descriptivas variables que se usaron para crear los clusters -------------







# 2. Descriptivas de variables estructurales de las firmas --------------------




# 3. Descriptivas de variables de innovacion y fontar -------------------------

# creo dummy para fontar
endei_clu = endei_clu %>% mutate(d_fontar = ifelse(finan_fontar>0,1,0),
                                 d_fontar = ifelse(is.na(finan_fontar),0,d_fontar))


# creo una tabla con info solo de los que usaron fontar
endei_fontar = endei_clu %>% filter(finan_fontar > 0)

# Clusters vs Fontar
table(endei_clu$d_fontar, endei_clu$cluster)
table(endei_clu$d_fontar, endei_clu$cluster) %>% prop.table(margin = 2)

# p.3.1.2 - Innovacion: Investigacion y Desarrollo (I+D) interna

table(endei_clu$p.3.1.2, endei_clu$cluster) %>% prop.table(margin = 2)
table(endei_fontar$p.3.1.2, endei_fontar$cluster) %>% prop.table(margin = 2)

# p.4.1.1
endei_clu$p.4.1.1 %>% table




