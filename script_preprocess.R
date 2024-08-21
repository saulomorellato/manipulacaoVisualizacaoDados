### PACOTES ###

library(tidyverse)
library(janitor)



### CARREGANDO DADOS ###

dados_originais<- read.delim("dass.csv")



### PRE PROCESSAMENTO ###

# LIMPANDO OS NOMES DA VARIAVEIS / REMOVENDO NA's

dados<- dados_originais %>% 
  clean_names() %>% 
  drop_na()


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

# dados<- dados %>% 
#   mutate(educacao=ifelse(education==1,
#                           "Fundamental",
#                           ifelse(education==2,
#                                  "Médio",
#                                  ifelse(education==3,
#                                         "Superior",
#                                         "Pós-Graduação"))),
#          moradia_infancia=ifelse(urban==1,"Rural",
#                                  ifelse(urban==2,
#                                         "Suburbio",
#                                         "Urbano")),
#          genero=ifelse(gender==1,
#                        "Masculino",
#                        ifelse(gender==2,
#                               "Feminino",
#                               "Outro")),
#          idioma=ifelse(engnat==1,"Inglês","Outro"),
#          idade=age,
#          lateralidade=ifelse(hand==1,
#                              "Destro",
#                              ifelse(hand==2,
#                                     "Canhoto",
#                                     "Ambidestro")),
#          religiao=ifelse(religion==1,
#                          "Agnótico",
#                          ifelse(religion==2,
#                                 "Ateu",
#                                 ifelse(religion==3,
#                                        "Budista",
#                                        ifelse(religion==4,
#                                               "Católico",
#                                               ifelse(religion==5,
#                                                      "Mórmon",
#                                                      ifelse(religion==6,
#                                                             "Protestante",
#                                                             ifelse(religion==7,
#                                                                    "Cristão/Outro",
#                                                                    ifelse(religion==8,
#                                                                           "Hindu",
#                                                                           ifelse(religion==9,
#                                                                                  "Judeu",
#                                                                                  ifelse(religion==10,
#                                                                                         "Muçulmano",
#                                                                                         ifelse(religion==11,
#                                                                                                "Sikh",
#                                                                                                "Outro"))))))))))),
#          orientacao=ifelse(orientation==1,
#                            "Heterossexual",
#                            ifelse(orientation==2,
#                                   "Bissexual",
#                                   ifelse(orientation==3,
#                                          "Homossexual",
#                                          ifelse(orientation==4,
#                                                 "Assexual",
#                                                 "Outro")))),
#          cor_raca=ifelse(race==10,
#                          "Asiático",
#                          ifelse(race==20,
#                                 "Árabe",
#                                 ifelse(race==30,
#                                        "Negro",
#                                        ifelse(race==40,
#                                               "Indígena Australiano",
#                                               ifelse(race==50,
#                                                      "Nativo Americano",
#                                                      ifelse(race==60,
#                                                             "Branco",
#                                                             "Outro")))))),
#          estado_civil=ifelse(married==1,
#                              "Solteiro",
#                              ifelse(married==2,
#                                     "Casado",
#                                     "Outro")),
#          irmaos=familysize-1,
#          pais=country,
#          dispositivo=ifelse(screensize==1,"Celular","Computador"),
#          fonte=ifelse(source==1,
#                       "Página DASS",
#                       ifelse(source==2,
#                              "Google",
#                              "Outro"))) %>% 
#   select(-c(education,
#             urban,
#             gender,
#             engnat,
#             age,
#             hand,
#             religion,
#             orientation,
#             race,
#             voted,
#             married,
#             familysize,
#             major,
#             country,
#             screensize,
#             uniquenetworklocation,
#             source))


dados<- dados %>% 
  mutate(educacao=recode(as.character(education),
                          "1"="Fundamental",
                          "2"="Médio",
                          "3"="Superior",
                          "4"="Pós-Graduação"),
         moradia_infancia=recode(as.character(urban),
                                 "1"="Rural",
                                 "2"="Suburbio",
                                 "3"="Urbano"),
         genero=recode(as.character(gender),
                       "1"="Masculino",
                       "2"="Feminino",
                       "3"="Outro"),
         idioma=recode(as.character(engnat),
                       "1"="Inglês",
                       "2"="Outro"),
         idade=age,
         lateralidade=recode(as.character(hand),
                             "1"="Destro",
                             "2"="Canhoto",
                             "3"="Ambidestro"),
         religiao=recode(as.character(religion),
                         "1"="Agnóstico",
                         "2"="Ateu",
                         "3"="Budista",
                         "4"="Católico",
                         "5"="Mórmon",
                         "6"="Protestante",
                         "7"="Cristão/Outro",
                         "8"="Hindu",
                         "9"="Judeu",
                         "10"="Muçulmano",
                         "11"="Sikh",
                         "12"="Outro"),
         orientacao=recode(as.character(orientation),
                           "1"="Heterossexual",
                           "2"="Bissexual",
                           "3"="Homossexual",
                           "4"="Assexual",
                           "5"="Outro"),
         cor_raca=recode(as.character(race),
                         "10"="Asiático",
                         "20"="Árabe",
                         "30"="Negro",
                         "40"="Indígena Australiano",
                         "50"="Nativo Americano",
                         "60"="Branco",
                         "70"="Outro"),
         estado_civil=recode(as.character(married),
                             "1"="Solteiro",
                             "2"="Casado",
                             "3"="Outro"),
         irmaos=familysize-1,
         pais=country,
         dispositivo=ifelse(screensize==1,"Celular","Computador"),
         fonte=recode(as.character(source),
                      "1"="Página DASS",
                      "2"="Google",
                      "0"="Outro")) %>% 
  select(-c(education,
            urban,
            gender,
            engnat,
            age,
            hand,
            religion,
            orientation,
            race,
            voted,
            married,
            familysize,
            major,
            country,
            screensize,
            uniquenetworklocation,
            source))



# REMOCAO DE VALORES INVALIDOS

dados<- dados %>% 
  filter(genero!=0,
         estado_civil!=0,
         religiao!=0)



# TRANSFORMAR NAO NUMERICAS EM FATOR

dados<- dados %>%
  unclass() %>%
  as.data.frame(stringsAsFactors=TRUE)



