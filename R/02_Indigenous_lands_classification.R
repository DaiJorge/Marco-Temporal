# INFORMATION COLLECTED BY ISA HTML ----------------------------------
# site
terras = read_html("https://terrasindigenas.org.br/pt-br/brasil")

#url
url = html_nodes(terras, "table[class='table table-striped tablesorter']") %>% 
  html_nodes("a") %>% 
  html_attr("href")

# built a dataframe with the HTML information
ti = html_table(terras)
ti = as.data.frame(ti[[1]])


# automation to save information
n = length(url)

#choosing the HTML variables of interest
ti$Estagio = vector(mode = "character",length = n)
ti$Publicacao = vector(mode = "character",length = n)
ti$p_declarada_publ = vector(mode = "character",length = n)
ti$p_homologada_publ = vector(mode = "character",length = n)


# loop to collect all variables of interest
for (i in 1:n)  {
  print(i)
  
  #adding the URLs of indigenous territories
  ti = read_html(paste0('https://terrasindigenas.org.br',url[i]))
  
  #html information on indigenous territories
  rec =  html_nodes(ti, "table[class='table table-striped tablesorter tableexport']") %>% 
    html_table()
  
  #data from declared indigenous territories
  g = as.data.frame(rec[[2]])
  g$Publicação = strptime(as.character(g$Publicação), "%d/%m/%Y")
  
  if(nrow(g) == 1){
    h = g[nrow(g),c(2,5)]
  }
  if(nrow(g) != 1){
    if(sum(is.na(g[,5])) >= 1){
      g$Data = strptime(as.character(g$Data), "%d/%m/%Y")
      h = g[which.max(as.Date(g[, 4])),c(2,5)]
    } else h = g[which.max(as.Date(g[, 5])),c(2,5)]
  }
  
  h[,2] = as.character(h[,2])
  ti[i,6:7] = h
  
};rm(i)


ti = ti %>% mutate(classe = NA)