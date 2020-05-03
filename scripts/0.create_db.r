
# ENDEI II - Creacion del Dataset -------------------------------------------------------

path = getwd()
source('scripts/libraries.r')


# Read dataset --------------------------------------------------------------------------

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
  mutate_at(to_num$variable, funs(as.numeric(.))) %>% 
  mutate_at(to_char$variable, funs(as.character(.))) %>%
  mutate_at(to_char$variable, funs(replace(., . == 'Sí', 'Si'))) %>% 
  select(endei_vars$variable)

# exploratorio
explora = skim_to_list(endei)
explora$numeric
explora$factor


# Creacion de variables estructurales de las firmas -------------------------------------

tc.2014 = 3.9124
tc.2015 = 4.1297
tc.2016 = 4.5508

# Renombro variables
endei = endei %>% 
  rename(empleo.2014 = cant_nivtotal_2014) %>% 
  rename(empleo.2015 = cant_nivtotal_2015) %>% 
  rename(empleo.2016 = cant_nivtotal_2016) 

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
  mutate(k.inac = ifelse(p.1.8 == 'Con presencia de capital internacional', 1, 0))

# Sectores de actividad
alta_tecnologia = c('Farmaceuticas', 'Productos químicos', 'Material eléctrico, radio, televisión',
                    'Instrumentos médicos', 'Otros equipo de transporte')
media_alta_tecnologia = c('Carrocerías,  remolques y semirremolques', 'Autopartes', 
                          'Maquina herramienta en general', 'Maquinaria y equipo',
                          'Maquinaria Agropecuaria y Forestal')
media_baja_tecnologia = c('Otros minerales no metálicos', 'Otros productos de metal', 'Metales comunes',
                          'Productos de caucho y plástico', 'Aparatos de uso doméstico')
baja_tecnologia = c('Otras', 'Madera', 'Papel', 'Alimentos', 'Edición', 'Frigoríficos', 'Productos lácteos',
                    'Vinos y otras bebidas fermentadas', 'Productos textiles', 'Confecciones', 'Cuero', 'Muebles')

endei = endei %>% 
  mutate(calif.ocde = factor(case_when(rama_act %in% alta_tecnologia ~ 'Alta Tecnologia',
                                       rama_act %in% media_alta_tecnologia ~ 'Media-Alta Tecnologia',
                                       rama_act %in% media_baja_tecnologia ~ 'Media-Baja Tecnologia',
                                       rama_act %in% baja_tecnologia ~ 'Baja Tecnologia'),
                             levels =c('Baja Tecnologia',
                                       'Media-Baja Tecnologia',
                                       'Media-Alta Tecnologia',
                                       'Alta Tecnologia')),
         rama.num = factor(as.numeric(rama_act)))

# obtuvo y solicito financiamiento para innovacion
# esto lo cambiaron, ver de armar variables de financiamiento


# a.Capacidad productiva/practicas empresariales ----------------------------------------

# 1.d.especific:
# 2.d.trazabilidad: 
# 3.d.problemas:
# 4.d.mejoracont:
# 5.d.diseno:
# 6.dnorm.calidad: Implementa Normas ISO o Normas sectoriales o de prodcutos
# d.gesproydis:
# scap.organiza: suma actividades o herramientas que hacen a la capacidad organizativa                                                            #

endei = endei %>% 
  mutate(dnorm.calidad = case_when((p.2.8.8 == "Si" | p.2.8.9 == "Si")  ~ 1,
                                    TRUE ~ 0),
         d.especific = case_when((p.2.8.1 == "Si")  ~ 1,
                                    TRUE ~ 0),
         d.trazabilidad = case_when(p.2.8.3 == "Si"  ~ 1,
                                    TRUE ~ 0), 
         d.problemas = case_when(p.2.8.4 == "Si"  ~ 1,
                                    TRUE ~ 0), 
         d.mejoracont = case_when(p.2.8.5 == "Si"  ~ 1,
                                    TRUE ~ 0),
         d.diseno = case_when(p.2.8.6 == "Si"  ~ 1,
                                    TRUE ~ 0),         
         d.gesproydis = case_when(p.2.8.5 == "Si"  ~ 1,
                                    TRUE ~ 0),
         dorganiza = case_when((p.2.8.1 == "Si" | p.2.8.2 == "Si" | p.2.8.3 == "Si" | p.2.8.4 == "Si" | p.2.8.5 == "Si"  | p.2.8.6 == "Si" | p.2.8.7 == "Si") ~ 1,
                                   TRUE ~ 0)) %>% 
  mutate_at(vars(starts_with("p.2.8.")), funs(case_when(. == "Si"~ 1, TRUE ~ 0))) %>%
  rowwise() %>%
  mutate(scap.organiza = sum(c(p.2.8.1,p.2.8.2,p.2.8.3,p.2.8.4,p.2.8.5,p.2.8.6,p.2.8.7)))


# b.Organizacion del trabajo ------------------------------------------------------------

# 7. Implementa una rotacion planificada del personal.
# 8. Grado de participacion del personal para el desarrollo de actividades

# Participacion del personal (practicas de trabajo)
endei = endei %>%
  mutate_at(vars(starts_with("p.10.15.4.")),funs(case_when(. =="Si"~ 1, TRUE ~ 0)))  %>%
  rowwise() %>%
  mutate(part.personal = sum(c(p.10.15.4.1,p.10.15.4.2,p.10.15.4.3, p.10.15.4.4, p.10.15.4.5)))

# Rotacion personal (planificada)
endei = endei %>%
  mutate_at(vars(c("p.10.15.a", "p.10.15.b", "p.10.15.c")),funs(case_when(. =="Implementa, planificada"~ 1, TRUE ~ 0)))  %>%
  rowwise() %>%
  mutate(d.rotacion = max(c(p.10.15.a,p.10.15.b,p.10.15.c)))

# 11.3.4, 11.4.3?

# c.Absorcion acumulada -----------------------------------------------------------------

# 9. Tiene departamento formal de I+D. Variable binaria.
# 10. Porcentaje de profesionales en el personal total. 
# 11. Porcentaje de personal con calificacion tecnica en el personal total. 

endei = endei %>% 
  mutate(depto.id = ifelse(p.5.1.6 == 'Si',1,0)) %>% 
  rowwise() %>% 
  mutate(
         prop.prof   = mean(c(prop_calprof_2014, prop_calprof_2015, prop_calprof_2016)),
         prop.tec    = mean(c(prop_caltec_2014,prop_caltec_2015,prop_caltec_2016)),
         prop.ing    = mean(c(prop_eduing_2014,prop_eduing_2015,prop_eduing_2016))
  )

# agregar aca la de base de datos? p.2.7.7

# d.Absorcion potencial/capacidades potenciales  ----------------------------------------

# 12. Tiene un area responsable de orgranizar las actividades de capacitacion. Variable binaria.
# 13. Porcentaje de personas de la empresa que recibieron cursos de formacion a nivel jerarquico
# 14. Porcentaje de personas de la empresa que recibieron cursos de formacion a nivel no jerarquico

# scap.func: cantidad de aspectos atendidos por el area responsable de organizar actividades de capacitacion
# dcap.func: si hay algun area responsable de organizar actividades de capacitacion (captado por si se atiende algun aspecto)
# dcap.cursos: si atendio alguna temÃ¡tica en los cursos(capta si hizo cursos/ojo: antes esta pregunta de % de personal capacitado)
# scap.cursos: cantidad de cursos realizados a los empleados                                             

endei = endei %>% 
  mutate(dcap.func = case_when((p.10.5.1 == "Si" | p.10.5.2 == "Si" | p.10.5.3 == "Si" |
                                  p.10.5.4 == "Si" | p.10.5.5 == "Si"  | p.10.5.6 == "Si" |
                                  p.10.5.7 == "Si") ~ 1,
                               TRUE ~ 0),
         dcap.cursos = case_when((p.10.9.1 == "Si" | p.10.9.2 == "Si" | p.10.9.3 == "Si" |
                                    p.10.9.4 == "Si" | p.10.9.5 == "Si"  | p.10.9.6 == "Si" |
                                    p.10.9.7 == "Si" |  p.10.9.8 == "Si" |  p.10.9.9 == "Si"|
                                    p.10.9.12 == "Si" ) ~ 1, 
                                 TRUE ~ 0)) %>% 
  mutate_at(vars(starts_with("p.10.5."),starts_with("p.10.9.")),funs(case_when(. == "Si"~ 1, TRUE ~ 0)))  %>%
  rowwise() %>%
  mutate(scap.func = sum(c(p.10.5.1,p.10.5.2,p.10.5.3,p.10.5.4,p.10.5.5,p.10.5.6,p.10.5.7)),
         scap.cursos = sum(c(p.10.9.1,p.10.9.2,p.10.9.3,p.10.9.4,p.10.9.5,p.10.9.6,p.10.9.7,
                             p.10.9.8,p.10.9.12)))


# e.Incentivos --------------------------------------------------------------------------

# 15.Aplica algun sistema de evaluacion de desempeno para el personal

endei = endei %>% 
  mutate(d.incent  = case_when((p.10.4.1 == "Si" | p.10.4.2 == "Si" | p.10.4.3 == "Si") ~ 1,
                               TRUE ~ 0))

# endei$p.7.3.1 -> resistencia de empleados al cambio
# endei$p.11.3.1
# endei$p.11.3.4

# f.Vinculaciones  ----------------------------------------------------------------------

# 16. Tiene vinculaciones con otras firmas
# 17. Tiene vinculaciones con el sector publico

# vinculaciones con firmas y con  spub (dummy y suma)
endei = endei %>% 
  mutate_at(vars(starts_with("p.9.")),funs(ifelse(. %in% c('Si'), 1, 0))) %>% # o matches instead of strarts_with; si hay mas variables "var1|var2"
  mutate(dvinc.firmas = ifelse(p.9.1.b == 1 | p.9.2.b == 1 | p.9.3.b == 1 | p.9.4.b == 1 |
                               p.9.5.b == 1 | p.9.6.b == 1 | p.9.7.b == 1,1,0),
         dvinc.pub = ifelse(p.9.1.c == 1 | p.9.2.c == 1 | p.9.3.c == 1 | p.9.4.c == 1 | 
                             p.9.5.c == 1 | p.9.6.c == 1 | p.9.7.c == 1 | p.9.1.d == 1 |
                             p.9.2.d == 1 | p.9.3.d == 1 | p.9.4.d == 1 | p.9.5.d == 1 |
                             p.9.6.d == 1 | p.9.7.d == 1,1,0)) %>%
  rowwise() %>%
  mutate(svinc.firmas = sum(c(p.9.1.b, p.9.2.b, p.9.3.b, p.9.4.b, p.9.5.b, p.9.6.b, p.9.7.b)),
         svinc.pub = sum(c(p.9.1.c, p.9.2.c, p.9.3.c, p.9.4.c, p.9.5.c, p.9.6.c, p.9.7.c, 
                            p.9.1.d, p.9.2.d,p.9.3.d, p.9.4.d, p.9.5.d, p.9.6.d, p.9.7.d, 
                            p.9.1.f, p.9.2.f, p.9.3.f, p.9.4.f, p.9.5.f, p.9.6.f, p.9.7.f)))


# Otras variables   ---------------------------------------------------------------------

# Personal capacitado a nivel jerarquico, supervisores y nivel no-jerarquico (en %)
endei = endei %>% 
  mutate(capacit.ger   = p.10.8.1,
         capacit.sup   = p.10.8.2,
         capacit.nojer = p.10.8.3) %>%
  rowwise() %>% 
  mutate(capacit.jer  = mean(c(capacit.ger, capacit.sup)))

# Grado de autonomia del personal (respuesta frente a problemas - vble ordinal)
# 0: no resolver
# 1: llamar al supervisor
# 2: resolver y comunicar
# 3: resolver y documentar

endei = endei %>%
  filter(!p.10.15.3 %in% c('Ns/Nc', 'Otros.')) %>%
  mutate(autonom.personal = case_when(p.10.15.3 == "No resolver, llamar al supervisor para evaluar  la complejidad del problema" ~ 0,
                                      p.10.15.3 == "Resolver y luego comunicar al supervisor" ~ 1,
                                      p.10.15.3 == "Resolver sin que sea necesario comunicar al superv" ~ 2,
                                      p.10.15.3 == "Resolver y luego documentar lo ocurrido." ~ 3))

# Paso a usd
endei = endei %>% 
  mutate(ing.2014      = ingr_total_2014 / tc.2014,
         ing.2015      = ingr_total_2015 / tc.2015,
         ing.2016      = ingr_total_2016 / tc.2016,
         va.tr.2014    = va_tr14 / tc.2014,
         va.tr.2015    = va_tr15 / tc.2015,
         va.tr.2016    = va_tr16 / tc.2016,
         inno.tot.2014 = inno_total_2014 / tc.2014,
         inno.tot.2015 = inno_total_2015 / tc.2015,
         inno.tot.2016 = inno_total_2016 / tc.2016)

# Ratios actividades de innovacion sobre ventas
endei = endei %>% 
  mutate(inno.ventas.2014 = (inno.tot.2014 / ing.2014)*100,
         inno.ventas.2015 = (inno.tot.2015 / ing.2015)*100,
         inno.ventas.2016 = (inno.tot.2016 / ing.2016)*100,
         id.ventas.2014   = (inno.id.2014 / ing.2014)*100,
         id.ventas.2015   = (inno.id.2015 / ing.2015)*100,
         id.ventas.2016   = (inno.id.2016 / ing.2016)*100) 

# Promedios 2014-2016 (hacer funcion)
endei = endei %>% 
  rowwise() %>% 
  mutate(ing         = mean(c(ing.2014, ing.2015, ing.2016)),
         empleo      = mean(c(empleo.2014, empleo.2015, empleo.2016)),
         inno.tot    = mean(c(inno.tot.2014, inno.tot.2015, inno.tot.2016)),
         inno.id     = mean(c(inno.id.2014, inno.id.2015, inno.id.2016)),
         va.tr       = mean(c(va.tr.2014, va.tr.2015, va.tr.2016)),
         inno.ventas = mean(c(inno.ventas.2014, inno.ventas.2015, inno.ventas.2016)),
         id.ventas   = mean(c(id.ventas.2014, id.ventas.2015, id.ventas.2016))
  )

# TC 2014-2016
# promedios 2014-2016
endei = endei %>% 
  mutate(tc.va.tr  = ((va.tr.2016-va.tr.2014)/va.tr.2014)*100,
         tc.empleo = ((empleo.2016-empleo.2014)/empleo.2014)*100,
         tc.ventas = ((ing.2016-ing.2014)/ing.2014)*100)


# Guardo base con todos los atributos 
saveRDS(endei,'working/endei_db')

