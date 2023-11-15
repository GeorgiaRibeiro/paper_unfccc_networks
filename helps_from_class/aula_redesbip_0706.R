#-------------------------------#
# aula 07/06 - redes bipartidas #
#-------------------------------#

# importar pacotes
library(igraph)

# uma matriz retangular n x m vira u grafo bipartido de vertices tipo n e m:
## criar dataframe base para matriz
df = data.frame(
  pais = c(
    "AGO",
    "BRA",
    "CHL",
    "DEU",
    "EST",
    "FRA",
    "GEO",
    "IND"
  ),
  onu = c(1,1,1,1,1,1,1,1),
  fmi = c(1,0,0,0,1,1,0,0),
  bm = c(1,1,0,0,1,1,1,1)
)

## transformando dataframe em matriz
df_m = as.matrix(df[,2:4])
rownames(df_m) = df$pais

## transformando a matrix em rede
g1 = graph_from_incidence_matrix(df_m)


# plotando
## layout as bipartide
plot(g1,
     layout=layout_as_bipartite)

# metricas
# cmt: adaptacao das bipartidas p/ usar metricas habituais --> projetar como unipartida.
# tem custos como lidar com arestas ponderadas

## projetando em unipartida (de pais x organização para pais x pais)
df_m_proj_paises = df_m %*% t(df_m)
# cmt: perda de dado (qual organizações os paises se ligam)

## projetando em unipartida (de pais x organização para org x org)
df_m_proj_orgs =  t(df_m) %*% df_m
# cmt: utilidade (dados complexo demandam segurança das metricas)

## com o igrah (retorna as duas opcoes)
bipartite_projection(g1)

g1_proj_list = bipartite_projection(g1)
g1_proj_pais = g1_proj_list[[1]]
g1_proj_org = g1_proj_list[[2]]

plot(g1_proj_pais)
E(g1_proj_pais)$weight

# outras coisas com matrizes retagulares
## distancia euclidiana e dendograma
library(stats)

dist(df_m)
dist(t(df_m))

dist_df_m = dist(df_m)
plot(hclust(dist_df_m))
cutree(hclust(dist_df_m), h=1)

