# ------------------------------------- #
# analise de redes - PPGCP UFPE 2023.1  #
# artigo final de disciplina            #
# grupo: georgia ribeiro, ma. gabriela  #
#        e vitoria beatriz              #
# ------------------------------------- #

# importando pacotes
library(igraph)
library(tidyverse)
library(ggraph)
library(MASS) # salvar layout
#install.packages('RColorBrewer')
library(RColorBrewer)

#install.packages('dendextend')
library(dendextend)

# importando bases
edge_list = read.csv("dados/edge_list.csv")

edge_list = edge_list %>%
  rename(from = pais, to = fundo, weight = contribuicao)  %>%
  mutate(weight = as.numeric(str_remove_all(weight, fixed(".")))) %>%
  filter(weight!=0)

node_list = read.csv("dados/node_list.csv")
node_list = node_list %>%
  mutate(pib2021_annualperc = as.numeric(pib2021_annualperc),
         pib2021_percapita = as.numeric(pib2021_percapita),
         income_group = case_when(income_group == "" ~ NA,
                                  .default = income_group))

# criando rede
g_funds_bip = graph_from_data_frame(edge_list,
                                    directed = FALSE,
                                    vertices = node_list)
g_funds_bip

#validar pesos
check_df = get.data.frame(g_funds_bip, what = "edges") #ok

######### GRAFOS ##########

# # # VISUALIZAÇÃO GERAL # # # 
# customizar a nivel de coordenada do layout
LDH = layout_with_dh(g_funds_bip)
CM = components(g_funds_bip)$membership
write.matrix(LDH,file="layout_dh_redegeral.csv")
LDH[,1] = LDH[,1] - mean(LDH[CM != 1,1])
LDH[,2] = LDH[,2] - mean(LDH[CM != 1,2])
LDH[CM != 1,][,1] = 120
LDH[CM == 1,] = LDH[CM == 1,]*2

par(mar = c(1, 1, 1, 1)) # Reduzir margem do plot
plot(g_funds_bip,
     layout = LDH, #layout_components, layout.lgl, layout.fruchterman.reingold, layout.kamada.kawai, layout.circle
     vertex.color = ifelse(components(g_funds_bip)$membership != 1, "red",
                           ifelse(V(g_funds_bip)$type==1, "lightblue2",
                                  "deepskyblue")),
     vertex.frame.color = "white",
     vertex.size = ifelse(
       V(g_funds_bip)$type==0, 6, 3),
     vertex.shape= ifelse(
       V(g_funds_bip)$type==0, "square", "circle"),
     vertex.label.cex= ifelse(components(g_funds_bip)$membership != 1, 0.7,
                              ifelse(V(g_funds_bip)$type==0, 1, 0.9)),
     vertex.label.family="Helvetica", 
     vertex.label.color= "black",
     vertex.label.font= ifelse(
       V(g_funds_bip)$type==0, 2, 1),
     
     vertex.label.dist = ifelse(components(g_funds_bip)$membership != 1, -0.9, 0),
     vertex.label.degree=ifelse(components(g_funds_bip)$membership != 1, -pi, pi),
     
     edge.width=E(g_funds_bip)$weight/500000,
     asp=0.7
)
#LayBip = layout_as_bipartite(g_funds_bip)
#LayBip = LayBip[,c(2,1)] #mudar a rotacao da rede (piorou)


# # # VISUALIZAÇÃO PARA IDENTIFICAR OS FUNDOS # # # 
plot(g_funds_bip,
     layout = layout_as_bipartite, #layout_components, layout.lgl, layout.fruchterman.reingold, layout.kamada.kawai, layout.circle
     vertex.color = "deepskyblue",
     vertex.frame.color = "white",
     vertex.size = ifelse(
       V(g_funds_bip)$type==0, 4, 2),
     vertex.shape= ifelse(
       V(g_funds_bip)$type==0, "square", "circle"),
     vertex.label.family="Helvetica", #windowsFonts
     vertex.label.color="black",
     vertex.label.font= ifelse(
       V(g_funds_bip)$type==0, 2, 1),
     vertex.label.cex= ifelse(
       V(g_funds_bip)$type==0, 1, 0.8),
     
     vertex.label.dist=ifelse(
       V(g_funds_bip)$label=="F_FRA", 0.6,
       ifelse(V(g_funds_bip)$label=="F_FIA", -0.6,
              ifelse(V(g_funds_bip)$label %in% c("F_ITL","F_CB"), 0, 1))),
     vertex.label.degree=ifelse(
       V(g_funds_bip)$type == 0, -pi, 1
     ),
     asp=-2,
     
     #edge.label=E(g_funds_bip)$weight
)


# # # VISUALIZAÇÃO PARA IDENTIFICAR OS PAISES NAO CONTRIBUINTES # # # 
plot(g_funds_bip,
     layout = layout_components, #layout_components, layout.lgl, layout.fruchterman.reingold, layout.kamada.kawai, layout.circle
     vertex.color = "deepskyblue",
     vertex.frame.color = "white",
     vertex.size = ifelse(
       V(g_funds_bip)$type==0, 6, 2),
     vertex.shape= ifelse(
       V(g_funds_bip)$type==0, "square", "circle"),
     vertex.label.family="Helvetica", #windowsFonts
     #vertex.label.color="black",
     vertex.label.color=ifelse(
       V(g_funds_bip)$label %in% c("USA", "CHN", "BRA"), "red",
       ifelse(V(g_funds_bip)$label %in% c("JPN", "GBR", "NLD"), "darkmagenta","#868786")),
     vertex.label.font= ifelse(
       V(g_funds_bip)$label %in% c("USA", "CHN", "BRA", "JPN", "GBR", "NLD"), 2, 1),
     vertex.label.cex= ifelse(
       V(g_funds_bip)$label %in% c("USA", "CHN", "BRA", "JPN", "GBR", "NLD"), 1, 0.8),
     asp=-0.4,
     #edge.label=E(g_funds_bip)$weight
)


# # # VISUALIZAÇÃO PARA IDENTIFICAR OS ATORES COM MAIORES CONTRIBUICOES # # # 
# customizar a nivel de coordenada do layout
LDH1 = layout_with_dh(g_funds_bip)
CM = components(g_funds_bip)$membership
write.matrix(LDH1,file="layout_dh1_redemaxcontrib.csv")

LDH1[,1] = LDH1[,1] - mean(LDH1[CM != 1,1])
LDH1[,2] = LDH1[,2] - mean(LDH1[CM != 1,2])
LDH1[CM != 1,][,1] = 120
LDH1[CM == 1,] = LDH1[CM == 1,]*2
#LDH[CM != 1,][,2] = -60 -> em linha reta no x e abaixo | #LDH[CM != 1,][,1] = 120 -> em linha reta no y e direita

par(mar = c(1, 1, 1, 1)) # Set the margin on all sides to 2
plot(g_funds_bip,
     layout = LDH1, #layout_components, layout.lgl, layout.fruchterman.reingold, layout.kamada.kawai, layout.circle
     vertex.color = ifelse(components(g_funds_bip)$membership != 1, "red","deepskyblue"),
     vertex.frame.color = "white",
     vertex.size = ifelse( V(g_funds_bip)$type==0, 6, 3),
     vertex.shape= ifelse(
       V(g_funds_bip)$type==0, "square", "circle"),
     vertex.label.cex= ifelse(
       V(g_funds_bip)$name %in% c("Germany","Japan","Spain",
                                  "BLOOMBERG PHILANTHROPIES",
                                  "United Kingdom of Great Britain and Northern Ireland",
                                  "France","Canada",
                                  "core_budget","int_transational_log",
                                  "participation_process","suppl_activities"), 0.8,
       ifelse(components(g_funds_bip)$membership != 1, 0.01, 0.5)),
     vertex.label.family="Helvetica", 
     vertex.label.color= "black",
     vertex.label.font= ifelse(
       V(g_funds_bip)$name %in% c("Germany","Japan","Spain",
                                  "BLOOMBERG PHILANTHROPIES",
                                  "United Kingdom of Great Britain and Northern Ireland",
                                  "France","Canada"), 2, 1),
     edge.color = ifelse(E(g_funds_bip)$weight > 1000000, "blue", "gray"),
     edge.label = ifelse(E(g_funds_bip)$weight > 1000000, E(g_funds_bip)$weight, " "),
     edge.label.cex=0.9,
     asp=0.9
)


# # # VISUALIZAÇÃO PARA IDENTIFICAR OS ATORES COM MAIORES GRAUS E/OU FORÇA # # # 
par(mar = c(1, 1, 1, 1)) # ajustar margem do plot
plot(g_funds_bip,
     layout = LDH1, 
     vertex.shape= ifelse(V(g_funds_bip)$type==0, "square", "circle"),
     vertex.color = ifelse(components(g_funds_bip)$membership != 1, "red",                    
                           ifelse(V(g_funds_bip)$name %in%
                                    c("Germany","Japan","Spain","BLOOMBERG PHILANTHROPIES"),
                                  "chartreuse","deepskyblue")),
     vertex.label.color=ifelse(
       V(g_funds_bip)$name %in% c("Austria","Denmark","Spain", "Germany"), "darkmagenta","black"),
     vertex.frame.color = "white",
     vertex.size = ifelse(V(g_funds_bip)$type==0|
                            V(g_funds_bip)$name %in%
                            c("Austria","Denmark","Spain", "Germany",
                              "Japan","Spain","BLOOMBERG PHILANTHROPIES"), 10, 3),
     vertex.label.family="Helvetica", 
     vertex.label.cex= ifelse(
       V(g_funds_bip)$name %in% c("Austria","Denmark","Spain", "Germany",
                                  "Japan","Spain","BLOOMBERG PHILANTHROPIES",
                                  "core_budget","int_transational_log",
                                  "participation_process","suppl_activities"), 0.9, 0.1),
     vertex.label.font= ifelse(
       V(g_funds_bip)$name %in% c("Austria","Denmark","Spain", "Germany",
                                  "Japan","Spain","BLOOMBERG PHILANTHROPIES"), 2, 1),
     edge.color = ifelse(E(g_funds_bip)$weight > 1000000, "cyan3", "gray"),
     #edge.label = ifelse(E(g_funds_bip)$weight > 1000000, E(g_funds_bip)$weight, " "),
     #edge.label.cex=0.9,
     edge.width=E(g_funds_bip)$weight/500000,
     asp=0.9
)

######### ANALISES ##########
### Grau
V(g_funds_bip)$grau = degree(g_funds_bip, normalized = F)

hist(V(g_funds_bip)$grau,
     main = 'Histograma de Grau',
     ylab = 'Frequência',
     xlab = 'Grau dos Vertices',
     breaks = 20)
axis(side=1, at=seq(0,100, 5), labels=seq(0,100,5))

which.max(V(g_funds_bip)$grau) # 1
V(g_funds_bip)$label[1] # F_CB = 93
V(g_funds_bip)$label[2] # F_ITL = 31

V(g_funds_bip)$name[degree(g_funds_bip)>5]

V(g_funds_bip)$name[degree(g_funds_bip)==4]


###Forca / Grau com peso 
V(g_funds_bip)$strength = strength(g_funds_bip,loops = F,weights = E(g_funds_bip)$weight)
which.max(V(g_funds_bip)$strength)
V(g_funds_bip)$label[1] # F_CB = 17591968

V(g_funds_bip)$name[strength(g_funds_bip)==1391666] #"France"
V(g_funds_bip)$strength[strength(g_funds_bip)>1391666]
V(g_funds_bip)$name[strength(g_funds_bip)==17591968] #"core_budget"
V(g_funds_bip)$name[strength(g_funds_bip)==13095816] #"suppl_activities"
V(g_funds_bip)$name[strength(g_funds_bip)==2000000] #"BLOOMBERG PHILANTHROPIES"

par(mar = c(5, 5, 5, 5)) # Ajustar margem do plot
hist(V(g_funds_bip)$strength/1000000,
     main = 'Histograma de Força',
     ylab = 'Frequência',
     xlab = 'Força dos vértices (em milhões)',
     breaks = 10,
     axes = FALSE)
axis(side=1, at=seq(0,20,by=2), labels=seq(0,20,by=2))

#FORÇA GERAL (NAO INTUITIVO)
hist(V(g_funds_bip)$strength/1000000,
     main = 'Histograma de Força',
     ylab = 'Frequência',
     xlab = 'Força dos vértices (em milhões)',
     breaks = 10,
     axes = FALSE)
axis(side=1, at=seq(0,20,by=2), labels=seq(0,20,by=2))

#FORÇA DOS FUNDOS = MONTANTE RECEBIDO
hist(V(g_funds_bip)$strength[V(g_funds_bip)$label %in% c("F_CB", "F_FRA", "F_FIA", "F_ITL")]/1000000,
     main = 'Histograma de Força dos Fundos',
     ylab = 'Frequência',
     xlab = 'Força dos vértices tipo 0 (em milhões)',
     #breaks = 10,
     #axes = FALSE
     )
#axis(side=1, at=seq(0,20,by=2), labels=seq(0,20,by=2))

#FORÇA DOS ATORES = MONTANTE CONTRIBUIDO
hist(V(g_funds_bip)$strength[!V(g_funds_bip)$label %in% c("F_CB", "F_FRA", "F_FIA", "F_ITL")]/1000000,
     main = 'Histograma de Força dos Atores',
     ylab = 'Frequência',
     xlab = 'Força dos vértices tipo 0 (em milhões)',
     #breaks = 10,
     #axes = FALSE
)

mean(V(g_funds_bip)$strength)

### Pesos
hist(E(g_funds_bip)$weight/1000000,
     main = 'Histograma de Peso',
     ylab = 'Frequência',
     xlab = 'Peso das arestas (em bilhões de euros)',
     #breaks = 10,
     axes = FALSE)
axis(side=1, at=seq(0,5,by=1), labels=seq(0,5,by=1))


### SALVANDO MEDIDAS EM UM DATAFRAME
funds_medidas <- data.frame(
  nome=V(g_funds_bip)$name,
  label=V(g_funds_bip)$label,
  grau=V(g_funds_bip)$grau,
  strength=V(g_funds_bip)$strength)




######### ANALISES UNIPARTIDAS ########## (script aula 12 - 07/06)

## projetando rede em unipartidas
bipartite_projection(g_funds_bip)

g_proj_unip_list = bipartite_projection(g_funds_bip)
g_fundos_fundos = g_proj_unip_list[[1]]
g_atores_atores = g_proj_unip_list[[2]]

#recalculando metricas para redes unipartidas
# ator-ator
par(mar = c(3, 3, 3, 3)) # Ajustar margem do plot
V(g_atores_atores)$grau = degree(g_atores_atores)

hist(degree(g_atores_atores, normalized = F),
     main = "Histograma de Grau",
     sub  = "Rede ator-ator",
     xlab = "Grau dos Vértices",
     ylab = "Frequência",
     xlim = range(c(0, 120)))

# não faz sentido na unipartida
#hist(strength(g_atores_atores,loops = F,weights = E(g_atores_atores)$weight),
#     main = "Histograma de Força dos Vértices",
#     sub  = "Rede ator-ator",
#     xlab = "Força dos Vértices",
#     ylab = "Frequência")
#V(g_atores_atores)$strength = strength(g_atores_atores,loops = F,weights = E(g_atores_atores)$weight)

betweenness(g_atores_atores, directed = F, weights = NULL, normalized = F)
V(g_atores_atores)$intermediacao = betweenness(g_atores_atores, directed = F, weights = NULL, normalized = F)

hist(V(g_atores_atores)$intermediacao,
     main = "Histograma de intermediação",
     sub  = "Rede ator-ator",
     xlab = "Centralidade de intermediação",
     ylab = "Frequência",
     xlim = range(c(0, 120))
     )

# salvando metricas em df
ator_ator_metricas <- data.frame(
  nome = V(g_atores_atores)$name,
  label = V(g_atores_atores)$label,
  grau = V(g_atores_atores)$grau,
  strength = V(g_atores_atores)$strength,
  intermediacao = V(g_atores_atores)$intermediacao)

ator_ator_metricas2 <- data.frame(
  nome= V(g_atores_atores)$label,
  grau=degree(g_atores_atores),
  intermediacao=betweenness(g_atores_atores),
  eig=evcent(g_atores_atores)$vector)

# entendendo métricas 
## grau
which.max(V(g_atores_atores)$grau) # 1
V(g_atores_atores)$label[11] 

teste_int = edge_list[edge_list$from %in% c("Canada","Japan","Republic of Korea (South Korea)", "Austria","Denmark","Germany","Spain"),]
teste_int %>% group_by(from) %>% summarise(total_weight = sum(weight)) %>% arrange(desc(total_weight))

# identificando atores mais conectados (grau e intermediação) na rede original (bipartida)
#"AUT","DNK","DEU","ESP" - MAIOR GRAU e 2a maior interm "CAN","JPN","KOR"  2o MAIOR GRAU e maior interm
par(mar = c(1, 1, 1, 1)) # Reduzir margem do plot
plot(g_funds_bip,
     layout = LDH, #layout_components, layout.lgl, layout.fruchterman.reingold, layout.kamada.kawai, layout.circle
     vertex.color = ifelse(components(g_funds_bip)$membership != 1, "red",
                           ifelse(V(g_funds_bip)$type==1, "lightblue2",
                                  "deepskyblue")),
     vertex.frame.color = "white",
     vertex.size = ifelse(
       V(g_funds_bip)$type==0, 6, 3),
     vertex.shape= ifelse(
       V(g_funds_bip)$type==0, "square", "circle"),
     vertex.label.cex= ifelse(components(g_funds_bip)$membership != 1, 0.7,
                              ifelse(V(g_funds_bip)$type==0|
                                       V(g_funds_bip)$label %in% c("AUT","DNK","DEU","ESP","CAN","JPN","KOR"), 1, 0.9)),
     vertex.label.family="Helvetica", 
     vertex.label.color= ifelse(V(g_funds_bip)$label %in% c("AUT","DNK","DEU","ESP"),"blue",
                                ifelse(V(g_funds_bip)$label %in% c("CAN","JPN","KOR"),"darkgreen",
                                "black")),
     vertex.label.font= ifelse(
       V(g_funds_bip)$type==0|
         V(g_funds_bip)$label %in% c("AUT","DNK","DEU","ESP","CAN","JPN","KOR"), 2, 1),
     
     vertex.label.dist = ifelse(components(g_funds_bip)$membership != 1, -0.9, 0),
     vertex.label.degree=ifelse(components(g_funds_bip)$membership != 1, -pi, pi),
     
     edge.width=E(g_funds_bip)$weight/500000,
     asp=0.7
)

# ajustar grafo anterior substituindo intermediação por maiores doações.
edge_list %>% group_by(from) %>% summarise(total_weight = sum(weight)) %>% arrange(desc(total_weight))

#"AUT","DNK","DEU","ESP" - MAIOR GRAU // "DEU","JPN","ESP","BLOOMBERG PHIL." MAIOR PESO
par(mar = c(1, 1, 1, 1)) # Reduzir margem do plot
plot(g_funds_bip,
     layout = LDH, #layout_components, layout.lgl, layout.fruchterman.reingold, layout.kamada.kawai, layout.circle
     vertex.color = ifelse(components(g_funds_bip)$membership != 1, "red",                    
                           ifelse(V(g_funds_bip)$label %in% c("DEU","JPN","ESP","BLOOMBERG PHIL."),
                                  "chartreuse","deepskyblue")),
     vertex.frame.color = "white",
     vertex.size = ifelse(V(g_funds_bip)$type==0|
                            V(g_funds_bip)$label %in% c("DEU","JPN","ESP","BLOOMBERG PHIL."), 6,
                                 3),
     vertex.shape= ifelse(V(g_funds_bip)$type==0, "square", "circle"),
     vertex.label.cex= ifelse(components(g_funds_bip)$membership != 1, 0.7,
                              ifelse(V(g_funds_bip)$type==0|
                                       V(g_funds_bip)$label %in% c("AUT","DNK","DEU","JPN","ESP","BLOOMBERG PHIL."), 1, 0.9)),
     vertex.label.family="Helvetica", 
     vertex.label.color= ifelse(V(g_funds_bip)$label %in% c("AUT","DNK","DEU","ESP"),"blue", "black"),
     vertex.label.font= ifelse(V(g_funds_bip)$type==0|
                                 V(g_funds_bip)$label %in% c("AUT","DNK","DEU","JPN","ESP","BLOOMBERG PHIL."), 2, 1),
     
     vertex.label.dist = ifelse(components(g_funds_bip)$membership != 1, -0.9, 0),
     vertex.label.degree=ifelse(components(g_funds_bip)$membership != 1, -pi, pi),
     
     edge.width=E(g_funds_bip)$weight/500000,
     asp=0.7
)


# reduzindo rede para atores com grau > 0 para melhor visualização
g_atores_atores_ind = delete.vertices(g_atores_atores, 
                                      V(g_atores_atores)[degree(g_atores_atores) == 0])

par(mar = c(1, 1, 1, 1)) # Ajustar margem do plot
plot(g_atores_atores_ind,
     vertex.frame.color = "white",
     vertex.size = 7,
     vertex.label.cex = 0.6,
     vertex.label.color = "black",
     vertex.label.family = "Helvetica",
     #edge.color = "gray",
     asp=1)

# fundo-fundo
par(mar = c(5, 5, 5, 5)) # Ajustar margem do plot
hist(degree(g_fundos_fundos, normalized = F),
     main = "Histograma de Grau",
     sub  = "Rede fundo-fundo",
     xlab = "Grau dos Vértices",
     ylab = "Frequência",
     xlim = range(c(0, 5)),
     ylim = range(c(0,5))
)

V(g_fundos_fundos)$grau = degree(g_fundos_fundos)

### IDENTIFICANDO COMUNIDADES ###
## aplicando louvain
cluster_louvain(g_atores_atores)

# salvando objeto communities
com_gatores_louv = cluster_louvain(g_atores_atores)

# recuperando:
# tamanho dos grupos
sizes(com_gatores_louv)

# membresia
membership(com_gatores_louv)
V(g_atores_atores)$com_louvain = membership(com_gatores_louv)
g_atores_atores

# modularidade
modularity(com_gatores_louv)

## aplicando outros algoritmos (ex. optimal) 
#usar rede reduzida para otimizar execução
plot(g_atores_atores_ind) #ok

com_gatores_optimal = cluster_optimal(g_atores_atores_ind)
plot(com_gatores_optimal,g_atores_atores_ind,
     vertex.frame.color = "white",
     vertex.size = 10,
     vertex.label.cex = 0.9,
     vertex.label.color = "black",
     vertex.label.family = "Helvetica",
     #edge.color = "gray",
     asp=1.2)

#modularity
modularity(com_gatores_optimal)

#membership
membership(com_gatores_optimal) # qual comunidade pertence cada vertice
V(g_atores_atores_ind)$com_optimal = membership(com_gatores_optimal) # salvar comunidade como variavel

### SALVANDO REDE COM COMUNIDADE OPTIMAL EM UM DATAFRAME
df_com_optimal <- as_long_data_frame(g_atores_atores_ind)

# DF SIMPLES (DF_COMUN)
list_att_full <- get.vertex.attribute(g_atores_atores_ind)
list_att_full$comun_opt <- as.numeric(list_att_full$comun_opt)
list_att_full$comun_fg <- as.numeric(list_att_full$comun_fg)
list_att_full$com_optimal <- as.numeric(list_att_full$com_optimal) # repetida
df_simple_analysis <- data.frame(list_att_full)
df_simple_analysis <-  df_simple_analysis[, !(names(df_simple_analysis) %in% c("com_optimal"))]
write.csv(df_simple_analysis,
          "C:/Users/gmr/Documents/paper_redes/results/df_att_communities.csv",
          row.names=TRUE)


# diferenças louvain x optimal?
#membership(com_gatores_optimal)
#membership(com_gatores_louv)

## aplicando um algoritmo hierárquico: fast and greedy
com_gatores_fg = (g_atores_atores)
com_gfundos_fg = cluster_fast_greedy(g_fundos_fundos)
comun_atores_ind_fg = cluster_fast_greedy(g_atores_atores_ind, weights = E(g_atores_atores_ind)$weight)

### testando fg com pesos
comun_atores_ind_fg = cluster_fast_greedy(g_atores_atores_ind)
comun_atores_ind_fg_pesos = cluster_fast_greedy(g_atores_atores_ind, weights = E(g_atores_atores_ind)$weight)

## verificando se o output é hierárquico
is_hierarchical(com_gatores_fg)
is_hierarchical(com_gatores_optimal)
is_hierarchical(com_gatores_louv)

modularity(com_gatores_optimal)
modularity(comun_atores_ind_fg)

##plotando comunidade fg
plot(comun_atores_ind_fg,g_atores_atores_ind,
     vertex.frame.color = "white",
     vertex.size = 10,
     vertex.label.cex = 0.9,
     vertex.label.color = "black",
     vertex.label.family = "Helvetica",
     #edge.color = "gray",
     asp=1.2)

##plotando comunidade fg - pesos
plot(comun_atores_ind_fg_pesos, g_atores_atores_ind)
plot(comun_atores_ind_fg, g_atores_atores_ind)
plot(comun_atores_ind_fg_pesos,g_atores_atores_ind,
     vertex.frame.color = "white",
     vertex.size = 10,
     vertex.label.cex = 0.9,
     vertex.label.color = "black",
     vertex.label.family = "Helvetica",
     #edge.color = "gray",
     asp=1.2)


## plotando o objeto communities - louvain
plot(com_gatores_louv,
     g_atores_atores,
     layout = layout_components,
     #vertex.color=V(g_atores_atores)$income_group,
     vertex.frame.color = "white",
     vertex.size = 5,
     vertex.label.cex = 0.6,
     vertex.label.color = "black",
     vertex.label.font = 2,
     edge.color = "gray",
     asp=0.9)
#legend("topleft",#bty = "n",
#       legend=V(g_atores_atores)$income_group,
#       fill=categorical_pal(5), border=NA)

# testando ajustes em outro layout
V(g_atores_atores)$com_louvain = membership(com_gatores_louv)
sizes(com_gatores_louv)[sizes(com_gatores_louv) >1] # 1, 4, 7

LN = layout_nicely(g_atores_atores)
LN[,1] = LN[,1] - mean(LN[(V(g_atores_atores)$com_louvain %in% c(1, 4, 7)), 1])
LN[,2] = LN[,2] - mean(LN[(V(g_atores_atores)$com_louvain %in% c(1, 4, 7)), 2])
LN[(V(g_atores_atores)$com_louvain %in% c(1, 4, 7)),] = LN[(V(g_atores_atores)$com_louvain %in% c(1, 4, 7)),]*7

pal_pastel2 = brewer.pal(5, 'Pastel2')
plot(com_gatores_louv,
     g_atores_atores,
     layout = LN,
     #col = V(g_atores_atores)$income_group, #https://stackoverflow.com/questions/58843666/how-can-i-plot-igraph-community-with-colors-according-to-attributes
     vertex.frame.color = "white",
     vertex.size = 5,
     vertex.label.cex = 0.6,
     vertex.label.color = "black",
     vertex.label.font = 2,
     edge.color = "gray",
     asp=0.9)

## plotando o objeto communities - fast and greedy
plot(com_gatores_fg,
     g_atores_atores,
     layout = LN,
     vertex.color=V(g_atores_atores)$income_group,
     vertex.frame.color = "white",
     vertex.size = 5,
     vertex.label.cex = 0.6,
     vertex.label.color = "black",
     vertex.label.font = 2,
     edge.color = "#868786",
     edge.lty=5,
     asp=0.9)

par(mar = c(3, 3, 3, 3))
plot(com_gfundos_fg,g_fundos_fundos,
     layout=layout_as_star,
     vertex.size = 40,
     vertex.frame.color = ifelse(
       V(g_fundos_fundos)$label %in% c("F_FIA", "F_FRA"), "orange", "lightblue"),
     vertex.label.cex = 1.4,
     vertex.label.color = "black",
     vertex.label.family = "Helvetica",
     edge.width=2
     )

## dendograma- visualizando a hierarquia
plot_dendrogram(com_gatores_fg, hang = -1, cex = 0.4) #impossivel visualizar, colocar label e aumentar?

par(mar = c(3, 3, 3, 3)) # Ajustar margem do plot
plot_dendrogram(com_gfundos_fg)

#dend <- as.dendrogram(com_gatores_fg)
#plot(dend)
#dend %>% set("labels", list(node_list$label[node_list$type == 1])) %>% plot #erro: lista n corresponde

## ajustando a altura do corte
com_gs10_fg
membership(com_gs10_fg)
cut_at(com_gs10_fg, steps = 4)
V(g_s10)$name


# checando outros atributos
## grupo de renda
plot(g_atores_atores,
     layout = LN,
     vertex.color = ifelse(V(g_atores_atores)$income_group == "Low income", "blue", "pink"),
     vertex.frame.color = "white",
     vertex.size = 5,
     vertex.label.cex = 0.6,
     vertex.label.color = "black",
     vertex.label.font = 2,
     edge.color = "gray",
     asp=0.9)

#pib #cor numerica https://rpubs.com/JanpuHou/337696
library(grDevices)
palet_heatcolor <- heat.colors(100)
plot(g_atores_atores,
     layout = LN,
     #vertex.color = V(g_atores_atores)$pib2021_percapita,
     vertex.frame.color = "white",
     vertex.size = 5, #V(g_atores_atores)$pib2021_percapita
     vertex.label.cex = 0.6,
     vertex.label.color = "black",
     vertex.label.font = 2,
     edge.color = "gray",
     asp=0.9)


# analisando comunidades e atributos
modularity(com_gatores_optimal)
modularity(comun_atores_ind_fg)

V(g_atores_atores_ind)$comun_opt = membership(com_gatores_optimal)
V(g_atores_atores_ind)$comun_fg = membership(comun_atores_ind_fg)

com <- cbind(V(g_atores_atores_ind)$name,
             V(g_atores_atores_ind)$label,
             com_gatores_optimal$membership,
             comun_atores_ind_fg$membership) #To get names if your vertices are labeled

df_comun = as.data.frame(com)
df_comun = df_comun %>%
  rename("name"="V1", "label"="V2", "comun_opt"="V3", "comun_fg"="V4")

df_att<-left_join(df_comun,node_list,by=c("name","label"))

edge_list_edit = edge_list %>% rename("name" = "from")
df_at_fundos<-left_join(df_comun,edge_list_edit,by=c("name"))

#count income group by comunity
df_att %>% group_by(comun_opt, income_group) %>%
  summarise(n = n())

#count fundos by comunity
df_at_fundos %>% group_by(comun_opt, to) %>%
  summarise(n = n())

#media de emissoes de co2
df_att %>% group_by(comun_opt ) %>%
  mutate(co2_emissions2 = case_when(is.na(co2_emissions2019) ~ 0,
                                  .default = co2_emissions2019)) %>%
summarise(media_co2 = mean(co2_emissions2))

map(df_att, ~sum(is.na(.)))

#count regiao by comunity
df_att %>% group_by(comun_opt, region) %>%
  summarise(n = n())

## detalhamento de comunidade para polmeth2023
str(df_simple_analysis)
num_cols = c(
  "divida_aberta", "compromisso_2023","cumpriu_compromisso_core_budget","cumpriu_compromisso_itl",
  "parties","pib2021_annualperc","pib2021_percapita","co2_emissions2019","grau",'intermediacao'
)

df_mean_community <- df_simple_analysis %>%
  group_by(comun_opt) %>% 
  summarise_at(vars(num_cols), mean)
write.csv(df_mean_community,
          "C:/Users/gmr/Documents/paper_redes/results/df_mean_communities.csv",
          row.names=TRUE)

# extra 
# triangle vertex shape
mytriangle <- function(coords, v=NULL, params) {
  vertex.color <- params("vertex", "color")
  if (length(vertex.color) != 1 && !is.null(v)) {
    vertex.color <- vertex.color[v]
  }
  vertex.size <- 1/200 * params("vertex", "size")
  if (length(vertex.size) != 1 && !is.null(v)) {
    vertex.size <- vertex.size[v]
  }
  
  symbols(x=coords[,1], y=coords[,2], bg=vertex.color,
          stars=cbind(vertex.size, vertex.size, vertex.size),
          add=TRUE, inches=FALSE)
}

add_shape("triangle", clip=shapes("circle")$clip, plot=mytriangle) # add shape
