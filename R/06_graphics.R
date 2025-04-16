# FUNÇÃ0 PARA MÉDIA -------------------------------------------------------

ic.m = function(x, conf = 0.95){
  n = length(x)
  media = mean(x)
  variancia = var(x)
  quantis = qt(c((1-conf)/2, 1 - (1-conf)/2), df = n-1)
  ic = media + quantis * sqrt(variancia/n)
  lo= ic[1]
  up=ic[2]
  med= media
  ic=cbind(med,lo,up)
  return(ic)
}

foresttr = ic.m(carbonor$value)


# MÉDIA PARA CADA CATEGORIA DAS RESERVAS ----------------------------------
mnreg = forestr[forestr$classe == "nreg", ]
mreg = forestr[forestr$classe == "reg", ]
mnh = forestr[forestr$classe == "NH", ]


# VALORES M?DIOS DE CARBONO -----------------------------------------------
forestc = read.csv("dadoscarbono.csv")
forestu = read.csv("dadosunidades.csv")
forestr = read.csv("dadosreservas.csv")
forestr = transform(forestr, n = as.numeric(n))
forestr$np = forestr$n/max(forestr[,1])
forestr = transform(forestr, X = as.numeric(X))
forestr$classe[forestr$classe == "N\xe3o Homologada"] = "NH"


# FOREST PLOT -------------------------------------------------------------


forest = ggplot(data = forestr, 
                aes(x = media, y = X, xmax= up, xmin= low, group = classe))+
  geom_errorbar(alpha=0.8, color="black") +
  geom_vline(xintercept = 17.82339, linetype = "twodash", colour = "#661100", size = 1) +
  geom_vline(xintercept = 47.62306, linetype = "twodash", colour = "#999933", size = 1) +
  geom_vline(xintercept = 49.91043, linetype = "solid", colour = "#117733", size = 1) +
  geom_point(aes(media, y = X, fill = classe, size = np), shape = 22) +
  
  facet_wrap(.~ classe, scales = "free_x") +
  scale_fill_manual(values = c("#E69F00", "#E31A1C", "#1B7837"),
                    breaks = c("NH","nreg","reg"),
                    labels = c("Not approved", 
                               "Approved after\nthe 1988 Constitution",
                               "Approved until\nthe 1988 Constitution")) +
  xlab(expression(paste("Carbon stock (MgC.ha"^-1, ")"))) +
  ylab("Indigenous lands")
guides (size = FALSE, fill = guide_legend(override.aes = list(size = 10))) +
  theme_bw() +
  theme(panel.grid= element_line(colour = "white"),
        axis.text = element_text(colour = "black",size = 20, family = "serif"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title = element_text(colour = "black", size = 20, family = "serif"),
        legend.position = "bottom",
        legend.text = element_text(family = "serif", size = 20),
        legend.title = element_blank(), strip.text = element_blank())

ggsave("forest.jpg", forest, dpi = 300, width = 14, height = 26,  units = "in")

# GRÁFICOS ACUMULADOS ---------------------------------------------------


#número de reservas
ind = ggplot(data = l1, aes(x = ano)) +
  geom_line(aes(y = acuanos, color = "All indigenous lands",
                group = "All indigenous lands" ), size = 1) +
  geom_line(data = cf1, aes(y = acuanos,color = "Approved after\nthe 1988 constitution",
                            group = "Approved after\nthe 1988 constitution"), size = 1) +
  geom_line(data = cf2, aes(y = acuanos,  color = "Approved until\nthe 1988 constitution",
                            group = "Approved until\nthe 1988 constitution"  ), size = 1) +
  geom_line(data = cf3, aes(y = acuanos, color = "Not approved",
                            group = "Not approved" ), size = 1) +
  annotate("text", x = 1928, y = 500, label = "B", size = 10) +
  geom_vline(xintercept = 1988, linetype = "dashed") +
  scale_color_manual(values = c("All indigenous lands" = "black", 
                                "Approved until\nthe 1988 constitution" = "#1B7837",
                                "Approved after\nthe 1988 constitution" = "#E31A1C", 
                                "Not approved" = "#E69F00"),
                     breaks = c("All indigenous lands","Not approved" ) ) +
  theme_classic() +
  scale_x_continuous(breaks = seq(1928, 2023, by = 10)) +
  labs( y = "Accumulated number of indigenous land") +
  theme(axis.text = element_text(family = "serif", size = 20),
        axis.title = element_text(family = "serif", size = 20),
        axis.title.x = element_blank(),
        legend.text = element_text(family = "serif", size = 18),
        legend.title = element_blank(), 
        legend.position = "bottom")




#número de superfície

sup = ggplot(data =l1, aes(x = ano)) +
  geom_line(aes(y = km2, color = "All indigenous lands",
                group = "All indigenous lands" ), size = 1) +
  geom_line(data = cf1, aes(y = km2,color = "Approved after\nthe 1988 constitution",
                            group = "Approved after\nthe 1988 constitution"), size = 1) +
  geom_line(data = cf2, aes(y = km2,  color = "Approved until\nthe 1988 constitution",
                            group = "Approved until\nthe 1988 constitution"  ), size = 1.5) +
  geom_line(data = cf3, aes(y = km2, color = "Not approved",
                            group = "Not approved" ), size = 1) +
  geom_vline(xintercept = 1988, linetype = "dashed") +
  scale_color_manual(values = c("All indigenous lands" = "black", 
                                "Approved until\nthe 1988 constitution" = "#1B7837",
                                "Approved after\nthe 1988 constitution" = "#E31A1C", 
                                "Not approved" = "#E69F00"),
                     breaks = c("Approved until\nthe 1988 constitution", "Approved after\nthe 1988 constitution")) +
  annotate("text", x = 1928, y = 9500, label = "C", size = 10) +
  theme_classic() +
  scale_x_continuous(breaks = seq(1928, 2023, by = 10)) +
  scale_y_continuous(breaks = seq(0, 10000, by = 2500)) +
  labs(x = "Years", y = "Accumulated area (km²)") +
  theme(axis.text = element_text(family = "serif", size = 20),
        axis.title = element_text(family = "serif", size = 20),
        legend.title = element_blank(), 
        legend.text = element_text(family = "serif", size = 18), 
        legend.position = "bottom") 


# GRÁFICO DE RIQUEZA ------------------------------------------------------
for (d in 1:nrow(dados)) {
  if (dados$dist_reg[d] == 0) {
    dados$reg[d] = 0
  } else {
    dados$reg[d] = 1
  }
  if (dados$dist_nreg[d] == 0) {
    dados$nreg[d] = 0
  } else {
    dados$nreg[d] = 1
  }
  if (dados$dist_nh[d] == 0) {
    dados$nh[d] = 0
  } else {
    dados$nh[d] = 1
  }
  
}
#gráfico

histnreg =sum(dados$nreg)
histreg =sum(dados$reg)
histnh = sum(dados$nh)

histgraf = ggplot() +
  geom_bar(stat = "identity", position = "fill",
           aes(x = factor(c(
             "Approved until\nthe 1988 constitution",
             "approved after\nthe 1988 constitution",
             "Not approved"
           ), levels = c(
             "Approved until\nthe 1988 constitution",
             "approved after\nthe 1988 constitution",
             "Not approved"
           )), y = c(histreg, histnreg, histnh), 
           fill = c(
             "Approved until\nthe 1988 constitution", 
             "approved after\nthe 1988 constitution", 
             "Not approved"
           )),
           width = 0.5) +
  labs(y = "Species richness") +
  scale_fill_manual(
    values = c("Approved until\nthe 1988 constitution" = "#1B7837", 
               "approved after\nthe 1988 constitution" = "#E31A1C",
               "Not approved" = "#E69F00"),  
    guide = FALSE) +
  theme_classic() +
  theme(
    axis.text = element_text(family = "serif", size = 15),
    axis.title.y  = element_text(family = "serif", size = 15),
    axis.title.x = element_blank()
  )
ggsave( "histgraf.png", histgraf, dpi = 300)


# -------------------------------------------------------------------------
gr = ggplotGrob(ind)
gr1 = ggplot() + theme_void() + xlim(0,10) + ylim(0,10) +
  annotation_custom(
    grob = gr,
    xmin = 0,
    xmax = 4.3,
    ymin = 0,
    ymax = 10)

gr2 =  gr1 +  annotation_custom(
  grob = ggplotGrob(sup) ,
  xmin = 4.4,
  xmax = 6.3,
  ymin = 4,
  ymax = 10
  
) +
  gr2 +  annotation_custom(
    grob = histgraf ,
    xmin = 4.5,
    xmax = 6,
    ymin = 2,
    ymax = 4
    
  ) 




