# pacotes
library(tidyverse)
library(igraph)
set.seed(1234)

# carregar dados
load("datasets_s09.RData")

# há 2 objetos igraph prontos
## g_pls_sen: uma rede ponderada de senadores-senadores unidos por projetos de lei (fonte: Georgia Ribeiro)
## g_amizade: uma rede dirigida de amizade entre alunos franceses (fonte: http://www.sociopatterns.org/datasets/high-school-contact-and-friendship-networks/)

g_pls_sen
plot(g_pls_sen)
;.
# cliques
## rede de senadores
### maior clique
largest_cliques(g_pls_sen)

### considerando tamanho
clique_size_counts(g_pls_sen, min=10, max=14)
plot(clique_size_counts(g_pls_sen, min=10, max=14))

### considerando peso
largest_weighted_cliques(g_pls_sen)


# componentes
## em redes não direcionadas
### criando uma rede ER e vendo seus componentes
rede_rand = sample_gnp(n=50, p=0.1, directed = F, loops = F)
plot(rede_rand)

rede_rand2 = sample_gnm(n=50, m=100, directed = F, loops = F)
plot(rede_rand2)

components(rede_rand2) #argumento "mode" para redes dirigidas
rede_rand2_comps = components(rede_rand2) #salvar informações numa lista pra visualizar/analisar melhor
V(rede_rand2)$componente = rede_rand2_comps$membership
plot(rede_rand2,
     vertex.color = V(rede_rand2)$componente) # diferenciar componentes por cor
plot(rede_rand2_comps$csize) #distribuição de tamanho dos componentes

## em redes direcionadas
### usando a rede de amizades
g_amizade
plot(g_amizade)

#### quantos componentes fracamente conect há? quantos fortemente conectados?
components(g_amizade, mode = "weak")
components(g_amizade, mode = "strong")

amizade_compfr = components(g_amizade, mode = "weak")
V(g_amizade)$componente = amizade_compfr$membership
plot(g_amizade,
     vertex.color = V(g_amizade)$componente,
     vertex.size = 10)

# ajustes na visualização
#lo <- layout_with_kk(g_amizade) # create a layout
#lo <- norm_coords(lo, ymin=-1, ymax=1, xmin=-1, xmax=1)

amizade_compforte = components(g_amizade, mode = "strong")
V(g_amizade)$componente_str = amizade_compforte$membership
plot(g_amizade,
     vertex.color = V(g_amizade)$componente_str,
     vertex.size = 15,
     edge.arrow.width = .25,
     edge.arrow.size = .25, 
     #rescale=FALSE, 
     #layout=lo*1.2
     )

# conexidade e conectividade
min_cut(
  graph,
  source = , # um vertice de par especifico para remover
  target = , # outro vertice do par especifico a remover
  capacity = , #para redes ponderadas
  value.only = #retorna a quantidade (V) ou a conexão especifica que precisa remover (F)
)

## a rede de senadores é conexa?
is_connected(g_pls_sen)

is_connected(g_amizade, mode = "weak") #FALSE = tem 2 componentes
is_connected(g_amizade, mode = "strong") #FALSE = tem 9 componentes

## quem remover para separá-la?
min_cut(g_amizade) #zero porque ela já é separada/desconectada

min_cut(g_pls_sen) # quantos preciso tirar para passar de 1 a 2 componentes (desconectar a rede)
min_cut(g_pls_sen,
        value.only = F)

min_cut(g_pls_sen,
        source = "Romário",
        target = "Leila Barros",
        value.only = F)

## outras formas de testar conectividade
vertex.connectivity()
edge.connectivity()
