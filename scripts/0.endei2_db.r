
# ENDEI II - Creacion del Dataset ---------------------------------------------

path = getwd()
source('scripts/libraries.r')


# Read dataset ----------------------------------------------------------------

file = glue(path, '/raw/ENDEI II_anonimizada_Junio_2019.sav')

endei = read.spss(file, to.data.frame=TRUE)

endei = endei %>% 
  as_tibble() %>% 
  setNames(tolower(colnames(endei)))

endei_colnames = colnames(endei) %>% as.data.frame() %>% 
  setNames('variable') %>% mutate(flag_db = 1) %>% 
  mutate(column = variable)


# Pre procesamiento de variables ----------------------------------------------

# levanto el gsheet
# p_gsheet = 'https://docs.google.com/spreadsheets/d/1Jbj3heTwCzAKYwZ_egHeicJs29gedl7_imyUhxKCU8c/edit#gid=0'
# endei_vars = read_sheet(p_gsheet)
endei_vars = read_delim('endei_variables.csv', delim = ';')

nrow(endei_vars %>% filter(modelo == 'si')) # 682
nrow(endei_vars %>% filter(tipo == 'categorica')) # 507
nrow(endei_vars %>% filter(tipo == 'numeric')) # 174

table(endei_vars$familia)

# analizo las variables que coinciden base y gsheet
endei_vars = endei_vars %>% 
  left_join(endei_colnames, by = "variable") %>% 
  mutate(flag_db = ifelse(is.na(flag_db), 0, 1)) %>% filter(flag_db==1)

# seteo las variables en el formato correcto
to_char = endei_vars %>% filter(tipo == 'categorica') %>%
  select(variable)

to_num = endei_vars %>% filter(tipo == 'numeric') %>%
  select(variable)

endei = endei %>% 
  mutate_at(to_num$variable, funs(as.numeric(as.character(.)))) %>% 
  mutate_at(to_char$variable, funs(as.character(.))) %>%
  mutate_at(to_char$variable, funs(stri_trans_general(., "Latin-ASCII"))) %>% 
  select(endei_vars$variable)


# Creacion de variables estructurales de las firmas ---------------------------

tc.2014 = 3.9124
tc.2015 = 4.1297
tc.2016 = 4.5508

# Renombro variables
endei = endei %>% 
  rename(empleo_2014 = cant_nivtotal_2014) %>% 
  rename(empleo_2015 = cant_nivtotal_2015) %>% 
  rename(empleo_2016 = cant_nivtotal_2016) 

# Reemplazo NA por 0
endei = endei %>% 
  mutate_at(vars(starts_with("inno_total")), funs(replace(., is.na(.), 0))) %>% 
  mutate_at(vars(starts_with("prop_idint")), funs(replace(., is.na(.), 0))) %>%  
  mutate_at(vars(starts_with("prop_edu")), funs(replace(., is.na(.), 0))) 

# Creo variables usando otras
endei = endei %>% 
  mutate(inno.id.2014 = (prop_idint_2014 + prop_idext_2014) * inno_total_2014 / 100 / tc.2014, 
         inno.id.2015 = (prop_idint_2015 + prop_idext_2015) * inno_total_2015 / 100 / tc.2015,
         inno.id.2016 = (prop_idint_2016 + prop_idext_2016) * inno_total_2016 / 100 / tc.2016,
         dai = case_when((p.3.1.1 == "Si" | p.3.1.2 == "Si" | p.3.1.3 == "Si" |
                          p.3.1.4 == "Si" | p.3.1.5 == "Si" | p.3.1.6 == "Si" | 
                          p.3.1.7 == "Si" | p.3.1.8 == "Si") ~ 1,
                          TRUE ~ 0),        
         did = case_when((inno.id.2014 > 0 | inno.id.2015 > 0 | inno.id.2016 > 0) ~ 1,
                         TRUE ~ 0),
         exporta = case_when(!p.1.14 %in% c('0%', 'Ns/Nc') ~ 1,
                             TRUE ~ 0),
         # revisar esta
         innovo = case_when(p.4.1.1 == 'Si' ~ 1,
                            TRUE ~ 0)) %>% 
  mutate_at(vars(p.1.12.3:p.1.12.8), funs(case_when(!.  %in% c('No', 'Ns/Nc') ~ 1, 
                                                    TRUE ~ 0))) %>%
  rowwise() %>%
  mutate(exp.dest = sum(c(p.1.12.3,p.1.12.4,p.1.12.5,p.1.12.6,p.1.12.7,p.1.12.8))) 

# Dummies por tipo de informacion que tiene la empresa (ojo: pregunta solo disponible para las que gastan en AI)
endei = endei %>% 
  mutate_at(vars(starts_with("p.7.1")),funs(ifelse(. %in% c('Si'), 1, 0))) %>% # o matches instead of strarts_with; si hay mas variables "var1|var2"
  mutate(interna.info = case_when((p.7.1.1.a==1 | p.7.1.1.b==1 | p.7.1.1.c==1 | p.7.1.1.d==1 |
                                     p.7.1.1.e==1 | p.7.1.1.f==1) ~ 1,
                                TRUE ~ 0),
       mercado.info = case_when((p.7.1.2.a==1 | p.7.1.2.b==1 | p.7.1.2.c==1) ~ 1,
                                TRUE ~ 0),
       instituc.info = case_when((p.7.1.3.a==1 | p.7.1.3.b==1 | p.7.1.3.c==1 | p.7.1.3.d==1 |
                                    p.7.1.3.e==1 | p.7.1.3.f==1 | p.7.1.3.g==1) ~ 1,
                                 TRUE ~ 0))

# Capital intenacional 
endei = endei %>% 
  mutate(k_inac = ifelse(p.1.8 == 'Con presencia de capital internacional', 1, 0))

# Sectores de actividad
alta_tecnologia = c('Farmaceuticas', 'Productos quimicos', 'Material electrico, radio, television',
                    'Instrumentos medicos', 'Otros equipo de transporte')
media_alta_tecnologia = c('Carrocerias,  remolques y semirremolques', 'Autopartes', 
                          'Maquina herramienta en general', 'Maquinaria y equipo',
                          'Maquinaria Agropecuaria y Forestal')
media_baja_tecnologia = c('Otros minerales no metalicos', 'Otros productos de metal', 'Metales comunes',
                          'Productos de caucho y plastico', 'Aparatos de uso domestico')
baja_tecnologia = c('Otras', 'Madera', 'Papel', 'Alimentos', 'Edicion', 'Frigorificos', 'Productos lacteos',
                    'Vinos y otras bebidas fermentadas', 'Productos textiles', 'Confecciones', 'Cuero', 'Muebles')

endei = endei %>% 
  mutate(calif_ocde = factor(case_when(rama_act %in% alta_tecnologia ~ 'Alta Tecnologia',
                                       rama_act %in% media_alta_tecnologia ~ 'Media-Alta Tecnologia',
                                       rama_act %in% media_baja_tecnologia ~ 'Media-Baja Tecnologia',
                                       rama_act %in% baja_tecnologia ~ 'Baja Tecnologia'),
                             levels =c('Baja Tecnologia',
                                       'Media-Baja Tecnologia',
                                       'Media-Alta Tecnologia',
                                       'Alta Tecnologia')))

# obtuvo y solicito financiamiento para innovacion
# esto lo cambiaron, ver de armar variables de financiamiento


# a.Capacidad productiva ------------------------------------------------------

# 1. Capacidades productivas (capac_prod)
# 2. Practicas empresariales (prac_empre)

endei = endei %>% 
  mutate_at(vars(starts_with("p.2.8.")), funs(case_when(. == "Si" ~ 1, TRUE ~ 0))) %>%
  mutate(d_especinsum = p.2.8.1,
         d_especproce = p.2.8.2,
         d_trazabilidad = p.2.8.3,
         d_problemas = p.2.8.4,
         d_mejoracont = p.2.8.5,
         d_diseno = p.2.8.6) %>% 
  rowwise() %>%
  mutate(capac_prod = sum(c(p.2.8.1,p.2.8.2,p.2.8.3)),
         prac_empre = sum(c(p.2.8.4,p.2.8.5,p.2.8.6)))


# b.Capacidades de Absorcion --------------------------------------------------

# 3. Absorcion inhouse (abs_ih)
# 4. Nivel de profesionales/tecnicos (prop_prof)

endei = endei %>% 
  mutate_at(vars(p.2.3.2, p.2.3.5, p.2.7.1),
            funs(str_replace(., "SI", "Si"))) %>% 
  mutate(d_deptoid = ifelse(p.5.1.6 == 'Si' | p.5.1.5 == 'Si',1,0),
         d_montec = ifelse(p.2.3.2=="Si",1,0),
         d_montcomp = ifelse(p.2.3.5=="Si",1,0),
         d_moninfo = ifelse(p.2.7.1=="Si",1,0),
         abs_ih = sum(d_deptoid, d_montcomp, d_moninfo)) 

endei = endei %>%   
  mutate(prop_prof = mean(c(prop_calprof_2014, prop_calprof_2015, prop_calprof_2016)),
         prop_tec = mean(c(prop_caltec_2014, prop_caltec_2015, prop_caltec_2016)),
         prop_calif = mean(c(prop_prof, prop_tec)))


# c.Capacidades organizacionales ----------------------------------------------

# 5. Gestion de Recursos Humanos (gest_rrhh)
# 6. Aplica algun sistema de evaluacion de desempeno para el personal (eval_des)

endei = endei %>% 
  mutate(d_rrhhdto = ifelse(p.10.1=="Si",1,0),
           d_perffor = ifelse(p.10.2.1=="Si",1,0),
         d_evaldes = ifelse(p.10.4.1=="Si",1,0),
         gest_rrhh = sum(d_rrhhdto, d_perffor, d_evaldes))

endei = endei %>% 
  mutate(d_informat = ifelse(informatizada=="Si",1,0),
         d_sistemas = ifelse(sis_rrhhctablefinan=="Si",1,0),
         d_softgest = ifelse(area_sistemas=="Si",1,0),
         d_sapoydec = ifelse(sis_apoyodec=="Si",1,0), 
         orga_tec = sum(d_sapoydec, d_sistemas, d_softgest))


# d.Capacidades de aprendizaje ------------------------------------------------

# 7. Proporcion de personas capacitadas (prop_capac)
# 8. Vinculaciones con instituciones publicas y privadas (vinc_inst)

# Personal capacitado a nivel jerarquico, supervisores y nivel no-jerarquico (en %)
endei = endei %>% 
  mutate(capacit_ger   = ifelse(!is.na(p.10.8.1), p.10.8.1, 0),
         capacit_sup   = ifelse(!is.na(p.10.8.2), p.10.8.2, 0), 
         capacit_nojer = ifelse(!is.na(p.10.8.3), p.10.8.3, 0)) %>%
  rowwise() %>% 
  mutate(prop_capac = mean(c(capacit_ger, capacit_sup, capacit_nojer)))

# vinculaciones con firmas y con  spub (dummy y suma)
endei = endei %>% 
  mutate_at(vars(starts_with("p.9.")),funs(ifelse(. %in% c('Si'), 1, 0))) %>% # o matches instead of strarts_with; si hay mas variables "var1|var2"
  mutate(d_vincfir = ifelse(p.9.1.b == 1 | p.9.2.b == 1 | p.9.3.b == 1 | p.9.4.b == 1 |
                                 p.9.5.b == 1 | p.9.6.b == 1 | p.9.7.b == 1,1,0),
         d_vincpub = ifelse(p.9.1.c == 1 | p.9.2.c == 1 | p.9.3.c == 1 | p.9.4.c == 1 | 
                              p.9.5.c == 1 | p.9.6.c == 1 | p.9.7.c == 1 | p.9.1.d == 1 |
                              p.9.2.d == 1 | p.9.3.d == 1 | p.9.4.d == 1 | p.9.5.d == 1 |
                              p.9.6.d == 1 | p.9.7.d == 1,1,0),
         d_vincase = ifelse(p.9.1.f == 1 | p.9.2.f == 1 | p.9.3.f == 1 | p.9.4.f == 1 |
                              p.9.5.f == 1 | p.9.6.f == 1 | p.9.7.f == 1,1,0),
         vinc_inst = sum(d_vincfir, d_vincpub, d_vincase))


# Otras variables   -----------------------------------------------------------

# Obstaculos
vars_obst = endei %>% select(starts_with('p.7.3.')) %>% 
  select(-c(p.7.3.11, p.7.3.11.otros)) %>% names() 

endei = endei %>% 
  mutate_at(vars(all_of(vars_obst)), funs(str_replace(., "SI", "Si"))) %>% 
  mutate_at(vars(all_of(vars_obst)), funs(ifelse(.=="Si",1,0))) 

# Paso a usd
endei = endei %>% 
  mutate(ing_2014 = ingr_total_2014 / tc.2014,
         ing_2015 = ingr_total_2015 / tc.2015,
         ing_2016 = ingr_total_2016 / tc.2016,
         va.tr.2014 = va_tr14 / tc.2014,
         va.tr.2015 = va_tr15 / tc.2015,
         va.tr.2016 = va_tr16 / tc.2016,
         inno.tot.2014 = inno_total_2014 / tc.2014,
         inno.tot.2015 = inno_total_2015 / tc.2015,
         inno.tot.2016 = inno_total_2016 / tc.2016)

# Ratios actividades de innovacion sobre ventas
endei = endei %>% 
  mutate(inno.ventas.2014 = (inno.tot.2014 / ing_2014)*100,
         inno.ventas.2015 = (inno.tot.2015 / ing_2015)*100,
         inno.ventas.2016 = (inno.tot.2016 / ing_2016)*100,
         id.ventas.2014 = (inno.id.2014 / ing_2014)*100,
         id.ventas.2015 = (inno.id.2015 / ing_2015)*100,
         id.ventas.2016 = (inno.id.2016 / ing_2016)*100) 

# Promedios 2014-2016 (hacer funcion)
endei = endei %>% 
  rowwise() %>% 
  mutate(ing = mean(c(ing_2014, ing_2015, ing_2016)),
         empleo = mean(c(empleo_2014, empleo_2015, empleo_2016)),
         inno_tot = mean(c(inno.tot.2014, inno.tot.2015, inno.tot.2016)),
         inno_id = mean(c(inno.id.2014, inno.id.2015, inno.id.2016)),
         va_tr = mean(c(va.tr.2014, va.tr.2015, va.tr.2016)),
         inno_ventas = mean(c(inno.ventas.2014, inno.ventas.2015, inno.ventas.2016)),
         id_ventas = mean(c(id.ventas.2014, id.ventas.2015, id.ventas.2016))
  )

# TC 2014-2016
# promedios 2014-2016
endei = endei %>% 
  mutate(tc.va.tr = ((va.tr.2016-va.tr.2014)/va.tr.2014)*100,
         tc.empleo = ((empleo_2016-empleo_2014)/empleo_2014)*100,
         tc.ventas = ((ing_2016-ing_2014)/ing_2014)*100)


# Guardo base con todos los atributos 
saveRDS(endei,'working/endei_db')

