library(igraph)
library(tidyverse)
library(grDevices)
# https://www.nceas.ucsb.edu/sites/default/files/2020-04/colorPaletteCheatsheet.pdf
install.packages ("ggraph")
library(ggraph)
# https://ggraph.data-imaginist.com/index.html

load("datasets_s11.RData")

# dados

## lesmis
plot(lesmis)
## lesmis_cut

## voosbr_dwll (igual a voosbr_dw, mas com latitude e longitude)

# plotando com o igraph apenas
plot(voosbr_dwll)


# editando fontes (vertex.label.)
png(file = "s11_1.png", res= 300, height = 2000, width = 2000)
plot(lesmis,
     vertex.label.color = "black",
     vertex.label.family= "sans",
     vertex.label.cex = 0.5,
     vertex.label.font = ifelse
     (V(lesmis)$name== "valjean",2,3
     ))
dev.off()
getwd()

# alterando tamanho do vértice
lesmis
plot(lesmis,
     vertex.size = V(lesmis)$grau,
     vertex.shape = "square")

# alterando espessura da aresta
png(file = "s11_1.png", res= 300, height = 2000, width = 2000)
plot(lesmis,
     edge.width = E(lesmis)$weight,
     edge.label = E(lesmis)$weight,
     layout = layout_in_circle)
dev.off()

png(file = "s11_2.png", res= 300, height = 2000, width = 2000)
plot(lesmis,
     edge.width = E(lesmis)$weight,
     edge.label = E(lesmis)$weight,
     edge.label.cex = 1.5,
     layout = layout_nicely)
dev.off()

#cex = tamanho da fonte

# mostrando rótulo nas arestas

# arestas curvas
plot(lesmis,
     edge.curved = T,
)

plot(voosbr_dwll,
     vertex.size = 3,
     vertex.label = "",
     edge.arrow.size = 0.4,
     edge.curved = T,
     layout= layout_with_fr,
     asp = 0.6)

# cores
## colorindo por comunidades - salvar um objeto a parte
com_lesmis_louvain <- cluster_louvain(lesmis,
                                      weights = E(lesmis)$weight)
com_lesmis_louvain
sizes(com_lesmis_louvain)
membership(com_lesmis_louvain)
# criar atributo
V(lesmis)$comunidade <- membership(com_lesmis_louvain)
lesmis

plot(lesmis,
     vertex.color= V(lesmis)$comunidade)

### usando regras
plot(lesmis,
     vertex.color= ifelse(
       V(lesmis)$grau>10, "blue", "yellow"
     ))

## plotando o objeto communities
plot(com_lesmis_louvain, lesmis)

## criando uma paleta customizada com grDevices - ele dá uma paleta pronta (conferir o site)
heat.colors(6) #aqui ele dá 300 opções de cores, mas estão mais próximas uma das outras
paleta_heatcolors <- heat.colors(6)

sizes(com_lesmis_louvain)
plot(lesmis,
     vertex.color = paleta_heatcolors [V(lesmis)$comunidade])

## paleta rainbow dá mais opções
pal_rainbow <- rainbow(6)
plot(lesmis,
     vertex.color = pal_rainbow [V(lesmis)$comunidade])

## cores baseadas no grau
summary(V(lesmis)$grau)
palet_heatcolor <- heat.colors(36)

plot(lesmis,
     vertex.color = palet_heatcolor[V(lesmis)$grau])

# layouts
## teste diferentes layouts para lesmis, lesmis_cut
lesmis
plot(lesmis,
     layout = layout_on_sphere)


lesmis_cut
plot(lesmis_cut,
     layout = layout_with_fr)


##coordenadas geograficas - sistema de coordenadas, não tem algortimo pronto
V(voosbr_dwll)$latitude

plot(voosbr_dwll,
     layout = cbind(
       V(voosbr_dwll)$longitude,
       V(voosbr_dwll)$latitude
     ))


# ggraph - lembra o ggplot, de acordo com prof. já n lembro de quase nada
#colocar quais as geometrias que quero que ele componha. Primeiro é das arestas
lesmis

ggraph(lesmis,
       layout = "auto") +
  geom_edge_link() +
  geom_node_point()

ggraph(lesmis,
       layout = "auto") +
  geom_edge_link() +
  geom_node_point(aes(size = grau,
                      color = as.factor(comunidade)))


ggraph(lesmis,
       layout = "auto") +
  geom_edge_link() +
  geom_node_text(aes(label=label))+
  theme_void()

ggraph(voosbr_dwll, layout = "fr") +
  geom_edge_fan(arrow = arrow (length = unit(4, "mm")),
                end_cap = circle(5, "mm"),
                start_cap = circle (2, "mm"))+
  geom_node_point(size = 5)



