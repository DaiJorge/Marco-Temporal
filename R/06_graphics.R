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
