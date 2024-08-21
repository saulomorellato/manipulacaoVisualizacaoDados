### PACOTES ###

library(tidyverse)
library(gtsummary)



### CARREGANDO DADOS ###

dados<- readRDS("dados_tratados.rds")



### TABELA ###


# idade, genero, estado civil

dados %>% 
  select(idade, genero, estado_civil) %>% 
  tbl_summary()


# reordenando estado civil, variavel em negrito

dados %>% 
  select(idade, genero, estado_civil) %>% 
  mutate(estado_civil=fct_relevel(estado_civil,
                                  "Solteiro",
                                  "Casado",
                                  "Outro")) %>% 
  tbl_summary() %>% 
  bold_labels()



# modificando cabecalho

tbl<- dados %>% 
  select(idade, genero, estado_civil) %>% 
  mutate(estado_civil=fct_relevel(estado_civil,
                                  "Solteiro",
                                  "Casado",
                                  "Outro")) %>% 
  tbl_summary() %>% 
  bold_labels()


tbl %>% show_header_names()

tbl<- tbl %>%
  modify_header(label="**Variável**",
                stat_0="**N = 34,334**")

tbl


# modificando o rodape

tbl<- tbl %>%
  modify_footnote(all_stat_cols() ~ "mediana (Q1, Q3); n (%)")

tbl


# para mais dicas sobre gtsummary acesse
# https://cran.r-project.org/web/packages/gtsummary/readme/README.html



### GRAFICOS ###


### idade x estado civil x genero

dados %>% 
  ggplot(aes(x=estado_civil,y=idade)) +
  geom_boxplot()


# removendo valores extremos e adicionando genero

dados %>% 
  filter(idade<100) %>% 
  ggplot(aes(x=estado_civil,y=idade,fill=genero)) +
  geom_boxplot()


# reordenar a variavel estado_civil

dados %>% 
  filter(idade<100) %>% 
  mutate(estado_civil=fct_relevel(estado_civil,
                                  "Solteiro",
                                  "Casado",
                                  "Outro"),
         genero=fct_relevel(genero,
                            "Feminino",
                            "Masculino",
                            "Outro")) %>% 
  ggplot(aes(x=estado_civil,y=idade,fill=genero)) +
  geom_boxplot()


# alterar titulo, legenda e tema

dados %>% 
  filter(idade<100) %>% 
  mutate(estado_civil=fct_relevel(estado_civil,
                                  "Solteiro",
                                  "Casado",
                                  "Outro"),
         genero=fct_relevel(genero,
                            "Feminino",
                            "Masculino",
                            "Outro")) %>% 
  ggplot(aes(x=estado_civil,y=idade,fill=genero)) +
  geom_boxplot() +
  theme_bw() +
  labs(x="Estado Civíl",
       y="Idade",
       fill="Gênero",
       title="Inserir Título") +
  theme(legend.position="right")    # "none", "left", "right", "bottom", "top", "inside"
  


### cor/raça

dados %>% 
  ggplot(aes(x=cor_raca)) + 
  geom_bar()


# colocar na horizontal

dados %>% 
  ggplot(aes(y=cor_raca)) + 
  geom_bar()


# modo alternativo de escrever

dados %>% 
  group_by(cor_raca) %>% 
  summarise(freq = n()) %>%
  ggplot(aes(x=freq,y=cor_raca)) + 
  geom_bar(stat="identity")


# reordenar conforme frequencia

dados %>% 
  group_by(cor_raca) %>% 
  summarise(freq = n()) %>%
  ggplot(aes(x=freq,y=reorder(cor_raca,freq))) + 
  geom_bar(stat="identity")


# adicionar cor

dados %>% 
  group_by(cor_raca) %>% 
  summarise(freq = n()) %>%
  ggplot(aes(x=freq,y=reorder(cor_raca,freq))) + 
  geom_bar(stat="identity",fill="blue")


# cor conforme classe

dados %>% 
  group_by(cor_raca) %>% 
  summarise(freq = n()) %>%
  ggplot(aes(x=freq,y=reorder(cor_raca,freq),fill=cor_raca)) + 
  geom_bar(stat="identity")


# cor conforme frequencia

dados %>% 
  group_by(cor_raca) %>% 
  summarise(freq = n()) %>%
  ggplot(aes(x=freq,y=reorder(cor_raca,freq),fill=freq)) + 
  geom_bar(stat="identity")


# cor conforme frequencia + tema + remover legenda

dados %>% 
  group_by(cor_raca) %>% 
  summarise(freq = n()) %>%
  ggplot(aes(x=freq,y=reorder(cor_raca,freq),fill=-freq)) + 
  geom_bar(stat="identity") +
  theme_bw() +
  labs(x="Frequência",
       y="",
       #fill="",
       title="Cor/Raça") +
  theme(legend.position="none")    # "none", "left", "right", "bottom", "top", "inside"



### Religiao (Pizza?)

dados %>% 
  group_by(religiao) %>% 
  summarise(freq = n()) %>%
  ggplot(aes(x=freq,y=reorder(religiao,freq),fill=-freq)) + 
  geom_bar(stat="identity") +
  theme_bw() +
  labs(x="Frequência",
       y="",
       #fill="",
       title="Religião") +
  theme(legend.position="none")    # "none", "left", "right", "bottom", "top", "inside"



### Escores

dados %>% 
  ggplot(aes(x=tot)) +
  geom_histogram()


# reduzindo o numero de "bins"

dados %>% 
  ggplot(aes(x=tot)) +
  geom_histogram(bins=15)


# escolhendo cores + tema

dados %>% 
  ggplot(aes(x=tot)) +
  geom_histogram(bins=15, colour="black", fill="white") +
  theme_bw()


# adicionando distribuicao empirica

dados %>% 
  ggplot(aes(x=tot)) +
  geom_histogram(aes(y=after_stat(density)),
                 bins=15,
                 colour="black",
                 fill="white") +
  geom_density(alpha=.2, fill="blue") + 
  theme_bw()


# mudando labels

dados %>% 
  ggplot(aes(x=tot)) +
  geom_histogram(aes(y=after_stat(density)),
                 bins=15,
                 colour="black",
                 fill="white") +
  geom_density(alpha=.2, fill="blue") + 
  theme_bw() +
  labs(x="Escore",
       y="Densidade",
       #fill="",
       title="")

