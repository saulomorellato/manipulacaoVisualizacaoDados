### PACOTES ###

library(tidyverse)
library(janitor)



### CARREGANDO DADOS ###

dados_originais<- read.delim("dass.csv")



### PRE PROCESSAMENTO ###

# LIMPANDO OS NOMES DA VARIAVEIS

dados<- clean_names(dados_originais)


# REMOVENDO QXXI E QXXE

dados<- dados %>% 
  select(!contains(c("0i", "1i", "2i", "3i", "4i",
                     "5i", "6i", "7i", "8i", "9i",
                     "0e", "1e", "2e", "3e", "4e",
                     "5e", "6e", "7e", "8e", "9e"))) 


# CALCULANDO OS ESCORES

dados<- dados %>% 
  mutate(dep = q3a + q5a + q10a + q13a + q16a + q17a + q21a + q24a + q26a + q31a + q34a + q37a + q38a + q42a,
         ans = q2a + q4a + q7a + q9a + q15a + q19a + q20a + q23a + q25a + q28a + q30a + q36a + q40a + q41a,
         est = q1a + q6a + q8a + q11a + q12a + q14a + q18a + q22a + q27a + q29a + q32a + q33a + q35a + q39a,
         tot = dep + ans + est) %>% 
  select(!starts_with("q"))


# DICOTOMIZAR VARIAVEIS TIPI

dados<- dados %>% 
  mutate(extrovertido = ifelse(tipi1>5,"sim","não"),
         critico = ifelse(tipi2>5,"sim","não"),
         confiavel = ifelse(tipi3>5,"sim","não"),
         ansioso = ifelse(tipi4>5,"sim","não"),
         aberto = ifelse(tipi5>5,"sim","não"),
         reservado = ifelse(tipi6>5,"sim","não"),
         simpatico = ifelse(tipi7>5,"sim","não"),
         desorganizado = ifelse(tipi8>5,"sim","não"),
         calmo = ifelse(tipi9>5,"sim","não"),
         convencional = ifelse(tipi10>5,"sim","não")) %>% 
  select(!starts_with("tipi"))


# REMOVENDO RESPOSTAS "NAO-CONFIAVEIS"

dados<- dados %>% 
  mutate(drop=vcl6+vcl9+vcl12) %>% 
  filter(drop==0) %>% 
  select(!starts_with("vcl")) %>% 
  select(-drop)


# CODIFICAR VARIAVEIS SOCIODEMOGRAFICAS

dados<- dados %>% 
  mutate(education)
