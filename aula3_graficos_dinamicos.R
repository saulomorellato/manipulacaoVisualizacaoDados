### PACOTES ###

library(plotly)



### GRAFICOS ###


### escore x idade x genero

plt<- dados %>% 
  filter(idade<100) %>% 
  mutate(genero=fct_relevel(genero,
                            "Feminino",
                            "Masculino",
                            "Outro")) %>% 
  ggplot(aes(x=idade,y=tot,colour=genero)) +
  geom_point() +
  theme_bw() +
  labs(x="Idade",
       y="Escore",
       colour="Gênero",
       title="Inserir Título") +
  theme(legend.position="right")

plt


# dinamico

ggplotly(plt)


# modificando a tip

plt<- dados %>% 
  filter(idade<100) %>% 
  mutate(genero=fct_relevel(genero,
                            "Feminino",
                            "Masculino",
                            "Outro")) %>% 
  ggplot(aes(x=idade,y=tot,colour=genero,
             text=paste("Escore:",tot,"\n",
                        "Idade:",idade,"\n",
                        "Gênero:",genero))) +
  geom_point() +
  theme_bw() +
  labs(x="Idade",
       y="Escore",
       colour="Gênero",
       title="Inserir Título") +
  theme(legend.position="right")

ggplotly(plt,
         tooltip=c("text"))


### escore x cor/raca

dados %>% 
  ggplot(aes(x=cor_raca,y=tot,fill=cor_raca)) +
  geom_boxplot() +
  theme_bw() +
  labs(x="Cor/Raça",
       y="Escore",
       fill="Cor/Raça",
       title="Inserir Título") +
  theme(legend.position="right")


# removendo elementos no eixo x

plt<- dados %>% 
  ggplot(aes(x=cor_raca,y=tot,fill=cor_raca)) +
  geom_boxplot() +
  theme_bw() +
  labs(x="Cor/Raça",
       y="Escore",
       fill="Cor/Raça",
       title="Inserir Título") +
  theme(legend.position="right") +
  theme(axis.title.x=element_blank(), # remove label
        axis.text.x=element_blank(),  # remove texto
        axis.ticks.x=element_blank()) # remove marcacoes

plt


# dinamico

ggplotly(plt)



### religiao (pizza)

dados %>%
  count(religiao) %>% 
  plot_ly(values=~n,
          labels=~religiao,
          type="pie")



# adicionar titulo e remover legenda

dados %>%
  count(religiao) %>% 
  plot_ly(values=~n,
          labels=~religiao,
          type="pie",
          textinfo="label+percent",
          showlegend=FALSE) %>% 
  layout(title="Inserir título")



### escores x estado civil x genero

plt<- dados %>% 
  mutate(estado_civil=fct_relevel(estado_civil,
                                  "Solteiro",
                                  "Casado",
                                  "Outro"),
         genero=fct_relevel(genero,
                            "Feminino",
                            "Masculino",
                            "Outro")) %>% 
  ggplot(aes(x=estado_civil,y=tot,fill=genero)) +
  geom_boxplot() +
  theme_bw() +
  labs(x="Estado Civíl",
       y="Escore",
       fill="Gênero",
       title="Inserir Título") +
  theme(legend.position="right")    # "none", "left", "right", "bottom", "top", "inside"

plt

ggplotly(plt)


# corrigindo o boxplot

ggplotly(plt) %>% layout(boxmode="group")




### escores x genero x extrovertido

plt<- dados %>% 
  mutate(genero=fct_relevel(genero,
                            "Feminino",
                            "Masculino",
                            "Outro")) %>% 
  ggplot(aes(x=genero,y=tot,fill=extrovertido)) +
  #geom_boxplot() +
  tidysdm::geom_split_violin() +
  theme_bw() +
  labs(x="Gênero",
       y="Escore",
       fill="Extrovertido",
       title="Inserir Título") +
  theme(legend.position="right")    # "none", "left", "right", "bottom", "top", "inside"

plt

ggplotly(plt)


# corrigindo

ggplotly(plt) %>% layout(boxmode="group")
