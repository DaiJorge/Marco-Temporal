# SHAFILE DO NEOTRÓPICO ---------------------------------------------------
neotropics = ne_countries(continent = "South America", returnclass = "sf", scale = 50)

# SHAPEFILE DO BR
neo = neotropics[3, ]

# SHAPEFILE DOS ESTADOS ----------------------------------------
estados = read_state()
neotropics = st_transform(neotropics, crs = st_crs(estados))
neo = st_transform(neo, crs = st_crs(estados))
brasil = st_intersection( neo, estados)
# MAPA DO BRASIL COM TODAS AS RESERVAS -------------------------------------------------------------------
reservasp = ggplot() +
  geom_sf(data =neotropics,
          colour = alpha("black", 0.2), fill = "gray90") +
  geom_sf(data = brasil, fill = "white", color = alpha("black", 0.2)) +
  geom_sf( data = resultado, aes( fill = classe), color = NA) +
  theme_classic() +
  scale_x_continuous(breaks = seq(-73, -37, by = 5)) +
  scale_y_continuous( breaks = seq (-33, 4, by =  5)) +
  annotation_scale(location = "br", width_hint = 0.45) +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering) +
  annotate("text", x = -72, y = 3.5, label = "A", size = 10) +
  labs(fill = "Terras Indígenas") +
  #guides(fill = guide_legend(title.position = "top")) + para usar a legenda junto com o forest plot
  theme(panel.background = element_rect(fill = "aliceblue"),
        legend.background = element_blank(),
        axis.line = element_blank(), axis.ticks = element_blank(),
        axis.text = element_text(family = "serif", size = 16),
        legend.text.align = 0,
        legend.position = c(0.93 , 0.1), legend.justification = c(0.89, 0.2),
        panel.border = element_rect(colour = "black", fill = NA, size = 1),  
        legend.key = element_rect(color = "transparent"),legend.key.size = unit(0.7, "cm"),
        legend.text = element_text(family = "serif", size = 16), 
        legend.title = element_text(family = "serif", size = 16),
        axis.title = element_blank()) +
  
  scale_fill_manual(values = c( "#E69F00","#E31A1C",  "#1B7837"),
                    labels = c( "Não demarcada","Demarcada após\na constituição de 1988",
                                "Demarcada até\na constituição de 1988")) +
  coord_sf(xlim = c(-72.4, -36.3), ylim = c(-32.5, 3.8)) 

ggsave("f1_PT.jpg", reservasp, height = 10, width = 20, dpi = 300)

#legenda = get_legend(reservas)

# -------------------------------------------------------------------------

g = ggplotGrob(reservas)
t = ggplot() + theme_void() + xlim(0,10) + ylim(0,10) +
  annotation_custom(
    grob = g,
    xmin = 0,
    xmax = 6,
    ymin = 0,
    ymax = 10)

t1 =  t +  annotation_custom(
  grob = ggplotGrob(ind) ,
  xmin = 6,
  xmax = 10,
  ymin = 5,
  ymax = 10
  
) +
  annotation_custom(
    grob = ggplotGrob(sup) ,
    xmin = 6,
    xmax = 10,
    ymin = 0,
    ymax = 5
    
  )

ggsave("fig3.jpg", t1, dpi = 300, width = 22.82, height = 13.92)


# -------------------------------------------------------------------------
nome = paste("r", c(1:length(arquivos)), sep = "")
arquivos = list.files(path = "./dados_carbono",
                      pattern = "\\.tif$",
                      full.names = TRUE)

lista = list()

for (m in 1:length(arquivos)) {
  lista[[m]] = raster(arquivos[m])
}

ras = do.call(merge, lista)





# ESTOQUE DE CARBONO ------------------------------------------------------
for (w in 1:nrow(resultado)) {
  a = resultado[w,]
  a1 = crop(ras, a)
  a1 = mask(a1, a)
  plot(a1)
  resultado$C[w] = cellStats(a1, "sum")
  
}

ti = crop(ras,resultado)
t = mask(ti, resultado)
ti2 = as.data.frame(ti, xy = TRUE)

ext = extent(resultado) 
ti_df = raster::extract(ti, ext,
                        df = TRUE)

ggplot() +
  geom_raster(data = ti)




ggsave("t1.jpg", t1, height = 10, width = 20, dpi = 300)



# RIQUEZA DE ESPÉCIES -----------------------------------------------------
cores = c( "#E31A1C", "#E69F00", "#1B7837")


richness = ggplot() +
  geom_sf(data =neotropics,
          colour = alpha("black", 0.2), fill = "gray90") +
  geom_sf(data = brasil, fill = "white", colour = alpha("black", 0.2)) +
  geom_tile(data = riqueza, aes( x = x, y = y, fill = layer)) +
  
  theme_classic() +
  scale_fill_gradientn(colors  = cores ) + 
  scale_x_continuous(breaks = seq(-73, -37, by = 5)) +
  scale_y_continuous( breaks = seq (-33, 4, by =  5)) +
  annotation_scale(location = "br", width_hint = 0.45) +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering) +
  annotate("text", x = -72, y = 3.5, label = "A", size = 10) +
  labs(fill = "Richness", x = "\n\n\n\n\n\n") +
  theme(panel.background = element_rect(fill = "aliceblue"),
        legend.background = element_blank(),
        axis.line = element_blank(), axis.ticks = element_blank(),
        axis.text = element_text(family = "serif", size = 20),
        legend.text.align = 0,
        legend.position = c(0.97 , 0.1), legend.justification = c(0.89, 0.2),
        panel.border = element_rect(colour = "black", fill = NA, size = 1),  
        legend.key = element_rect(color = "transparent"),legend.key.size = unit(0.7, "cm"),
        legend.text = element_text(family = "serif", size = 20), 
        legend.title = element_text(family = "serif", size = 20),
        axis.title.y  = element_blank()) +
  coord_sf(xlim = c(-72.4, -36.3), ylim = c(-32.5, 3.8))

# IMAGEM ------------------------------------------------------------------



im = readPNG("./macaco.png")
img = rasterGrob(im)
ima = readPNG(source = "./noun-bird-3666024.png", native = TRUE)
ia = readPNG(source = "./noun-frog-1797599.png", native = TRUE)
ir = readPNG(source = "./noun-snake-2588886.png", native = TRUE)
ir = rasterGrob(ir)
# -------------------------------------------------------------------------

rebar = richness +  annotation_raster(
  ir ,
  xmin = -74,
  xmax = -64,
  ymin = -24,
  ymax = -15
  
) + 
  annotation_raster(
    raster = im,  
    xmin = -70.4,
    xmax = -67.7,
    ymin = -21,
    ymax = -17.6
  ) +
  
  annotation_custom(
    grob = ggplotGrob(ave) ,
    xmin = -67,
    xmax = -57,
    ymin = -24,
    ymax = -15
    
  ) + 
  annotation_raster(
    raster = ima,  
    xmin = -63.5,
    xmax = -60.5,
    ymin = -21,
    ymax = -18
    
  ) +
  annotation_custom(
    grob = ggplotGrob(anfi) ,
    xmin = -74,
    xmax = -64,
    ymin = -31,
    ymax = -22
    
  ) +
  annotation_raster(
    raster = ia, 
    xmin = -71,
    xmax = -67,
    ymin = -28,
    ymax = -25
  ) +
  annotation_custom(
    grob = ggplotGrob(reptil) ,
    xmin = -67,
    xmax = -57,
    ymin = -31,
    ymax = -22
    
  )+
  annotation_raster(
    raster = ir,  
    xmin = -64,
    xmax = -60,
    ymin = -28,
    ymax = -24.4
  )

fig2 = rebar + annotation_custom(legenda, xmin = -72,
                                 xmax = - 43 , ymin = -39, ymax = -36)



ggsave("fig2.jpg", fi, height = 10, width = 20, dpi = 300)


