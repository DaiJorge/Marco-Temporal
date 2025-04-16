# ÁREA ACUMULADO POR ANO --------------------------------------------------
s1 = resultado[, c( 8,15, 29, 31)]
s1$ano = as.Date (s1$Publicacao)
s1$ano = as.numeric(format(s1$ano, "%Y"))
s1 = s1[-496, ] # não informação sobre superficie 
s1 = s1 %>% mutate(anos = Publicacao)
s1 = as.data.frame(s1)
c1 = with(s1, s1[(Publicacao >= "1988-10-05" & classe == "nreg"), ])

c2 =  with(s1, s1[(Publicacao <= "1988-10-05" & classe == "reg"), ])

c3 =  with(s1, s1[(classe == "Não Homologada"), ])

# TODAS AS RESERVAS -------------------------------------------------------


#acumulo de reservas ao longo dos anos 
s2 = s1 %>% 
  group_by(ano) %>% 
  summarise(nti = length(ano)) %>% 
  mutate(acuanos = cumsum(nti))

#acumulo de superficie ao longo dos anos

s3 = s1 %>% 
  group_by(ano) %>% 
  summarise(nsu = sum(superficie)) %>% 
  mutate(acumulado = cumsum (nsu))

#juntando as informações
l1 = merge(s2, s3, by.x = "ano", by.y = "ano")
l1$ano = as.Date(l1$anos)
l1$ano = as.numeric(format(l1$ano, "%Y"))
# HOMOLOGADAS ANTES DE 1988 ----------------------------------------------
# TIS
cc = c1 %>% 
  group_by(ano) %>% 
  summarise(nti = length(ano)) %>% 
  mutate(acuanos = cumsum(nti))

# Superfície
cc1 = c1 %>% 
  group_by(ano) %>% 
  summarise(nsu = sum(superficie)) %>% 
  mutate(acumulado = cumsum (nsu))

cf1 = merge(cc, cc1,by.x = "ano", by.y = "ano")
cf1$ano = as.Date(cf1$anos)
cf1$ano = as.numeric(format(cf1$ano, "%Y"))

# HOMOLOGADAS DEPOIS DE 1988 -----------------------------------------------
cc2 = c2 %>% 
  group_by(ano) %>% 
  summarise(nti = length(ano)) %>% 
  mutate(acuanos = cumsum(nti))

# Superfície
cc22 = c2 %>% 
  group_by(ano) %>% 
  summarise(nsu = sum(superficie)) %>% 
  mutate(acumulado = cumsum (nsu))

cf2 = merge(cc2, cc22,by.x = "ano", by.y = "ano")
cf2$ano = as.Date(cf2$anos)
cf2$ano = as.numeric(format(cf2$ano, "%Y"))
# OUTRAS TERRAS -----------------------------------------------------------
cc3 = c3 %>% 
  group_by(ano) %>% 
  summarise(nti = length(ano)) %>% 
  mutate(acuanos = cumsum(nti))

# Superfície
cc33 = c3 %>% 
  group_by(ano) %>% 
  summarise(nsu = sum(superficie)) %>% 
  mutate(acumulado = cumsum (nsu))

cf3 = merge(cc3, cc33,by.x = "ano", by.y = "ano")
cf3$ano = as.Date(cf3$anos)
cf3$ano = as.numeric(format(cf3$ano, "%Y"))


# transformando ha em km²
l1 = l1 %>% 
  mutate(km2 = acumulado * 0.0001)

cf1 = cf1 %>% 
  mutate(km2 = acumulado * 0.0001)
cf2 = cf2 %>% 
  mutate(km2 = acumulado * 0.0001)
cf3 = cf3 %>% 
  mutate(km2 = acumulado * 0.0001)

