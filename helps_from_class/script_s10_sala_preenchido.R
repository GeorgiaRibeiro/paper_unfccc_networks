library(igraph)
set.seed(1234)
load("datasets_s10.RData")

# redes
## g_s10: exemplo de sala (nao dirigida, binaria, pequena)
## lesmis: rede de personames les miserables (nao dirigida, ponderada, conexa, peq)
## lesmis_cut: sem o protagonista Jean Valjean (nao conexa)
## netsci: colaboracao entre cientistas em analise de rede (nao dirigida, ponderada, conexa, media)
## powergr: rede eletrica (nao dirigida, binaria, conexa, grande)
## voosbr_du: voos nacionais (dirigida, binaria, conexa, peq)
## voosbr_dw: voos nacionais (ponderada)


# aplicando louvain a g_s10
plot(g_s10)
cluster_louvain(g_s10)

# salvando objeto communities
com_gs10_louvain <- cluster_louvain(g_s10)
com_gs10_louvain

# recuperando
## tamanho dos grupos
sizes(com_gs10_louvain)

## membresia
membership(com_gs10_louvain)
V(g_s10)$com_louvain <- membership(com_gs10_louvain)
g_s10
plot(g_s10,
     vertex.color=V(g_s10)$com_louvain)

## modularidade
modularity(com_gs10_louvain)

# outros algoritmos (ex optimal)
com_gs10_optimal <- cluster_optimal(g_s10)
sizes(com_gs10_optimal)

# diferenças louvain x optimal?
membership(com_gs10_optimal)
membership(com_gs10_louvain)

# um algoritmo hierárquico: fast and greedy
com_gs10_fg <- cluster_fast_greedy(g_s10)
com_gs10_fg

## verificando se o output é hierárquico
is_hierarchical(com_gs10_fg)
is_hierarchical(com_gs10_louvain)

## visualizando a hierarquia
plot_dendrogram(com_gs10_fg)

## ajustando a altura do corte
com_gs10_fg
membership(com_gs10_fg)
cut_at(com_gs10_fg, steps = 4)
V(g_s10)$name

# aplicando edge betweenness ao voosbr
## sem direcao, sem peso
voosbr_du
com_voosbrdu_eb_sd <- cluster_edge_betweenness(voosbr_du,
                                            directed = F)
com_voosbrdu_eb_sd

## com direcao, sem peso
com_voosbrdu_eb_cd <- cluster_edge_betweenness(voosbr_du,
                                            directed = T)

## com direcao e peso
voosbr_dw
com_voosbrdw_eb_cdcp <- cluster_edge_betweenness(voosbr_dw,
                                                 directed = T,
                                                 weights = E(voosbr_dw)$minutos)
com_voosbrdw_eb_cdcp
## visualizando a hierarquia
plot_dendrogram(com_voosbrdw_eb_cdcp)

# aplique livremente, recupere métricas
cluster_edge_betweenness()
cluster_infomap()
cluster_label_prop()
cluster_fluid_communities()

# comparando partições
compare(com_voosbrdu_eb_sd, com_voosbrdu_eb_cd,
        method = "rand")

