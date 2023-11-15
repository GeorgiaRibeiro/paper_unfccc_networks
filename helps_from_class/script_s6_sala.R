# script s6 sala
library(tidyverse)
library(igraph)
set.seed(123456)

# redes
load("datasets_s6.RData")
## les miserables (Undirected, Weighted, V=personagens, E=conhece)
## network science (Undirected, Weighted, V=pesquisador, E=publicou com)
## voosbr_du (Directed, Unwweighted, V=aeroportos, E=voos)
## powergr (Undirected, Unweighted, V=estacoes de energia, E=linhas de transmissao)

# DEGREE CENTRALITY
## calculando a centralidade de grau de cada vértice
degree(lesmis, normalized = F) # produz um vetor com valores para cada vertice

V(lesmis)$grau <- degree(lesmis,
                         normalized = F) # se eu quiser salvar os graus como atribudos dos vertices da rede

## quem é o personagem principal de les miserables?
which.max(V(lesmis)$grau)
V(lesmis)$label[12]
plot(lesmis,
     vertex.size=V(lesmis)$grau) # tamanho do vertice proporcional ao grau

## quem é o cientista de dados com mais colaborações acadêmicas?
netsci
V(netsci)$grau <- degree(netsci,
                         normalized = F)
which.max(V(netsci)$grau) # 4
V(netsci)$label[4]

# degree centrality com arestas direciondas
voosbr_du
V(voosbr_du)$indegree <- degree(
  voosbr_du,
  mode = "in", # mode determina se sera grau de entrada (indegree) ou saida (outdegree)
  normalized = F
)
V(voosbr_du)$outdegree <- degree(
  voosbr_du,
  mode = "out",
  normalized = F
)

## qual o aeroporto com mais in-degree?
which.max(V(voosbr_du)$indegree) #2
V(voosbr_du)$name[2]

## qual o aeroporto com mais out-degree?
which.max(V(voosbr_du)$outdegree) #2
V(voosbr_du)$name[2]

# STRENGTH: variacao do degree usando arestas ponderadas
E(netsci)$value
V(netsci)$strength <- strength(netsci,loops = F,weights = E(netsci)$value)
which.max(V(netsci)$grau)
which.max(V(netsci)$strength)

# CLOSENESS
V(lesmis)$clo <- closeness(lesmis, normalized = F)

## quem atinge mais rapidamente toda a rede?
plot(lesmis,
     vertex.size=V(lesmis)$clo*1000)

V(powergr)$clo <- closeness(powergr,
                            normalized = F)
hist(V(powergr)$clo)
hist(V(lesmis)$clo)

# BETWEENNESS
## quem é o cientista que fez a ponte entre comunidades acadêmicas?
V(netsci)$bet <- betweenness(netsci,
                             weights = NA,
                             normalized = F)
plot(netsci,
     vertex.size=V(netsci)$bet/1000,
     vertex.label=ifelse(
       V(netsci)$bet>15000,V(netsci)$label,""
     ),
     layout=layout_with_kk)

summary(V(netsci)$bet)
hist(V(netsci)$bet)

# TRANSITIVITY
V(lesmis)$transit <- transitivity(
  lesmis, type="local",
  weights = NA
)
V(lesmis)$transit[12]
V(netsci)$transit <- transitivity(
  netsci, type="local",
  weights = NA
)
V(netsci)$transit[4]
hist(V(netsci)$transit)
hist(V(lesmis)$transit)

# SALVANDO MEDIDAS EM UM DATAFRAME
lesmis_medidas <- data.frame(
  nome=V(lesmis)$label,
  grau=V(lesmis)$grau,
  strength=V(lesmis)$strength,
  closeness=V(lesmis)$clo,
  transitivity=V(lesmis)$transit
)
