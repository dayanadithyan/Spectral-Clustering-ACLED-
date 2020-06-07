##  SETUP WORKINGDIR

workDir <- getwd()

output.folders <- c(
                    "1.Raw.Data","2.Clean.Data", "3.Analysis",
                    "4.Graphs", "5.Tables"
                    )

for(i in 1:length(output.folders)){
  if(!file.exists(output.folders[i])){
    print(paste(i, "does not exist"))
    dir.create(output.folders[i])
  }
  else  {
    print(paste(i,"does exist"))
  }
}

path.rd <- paste(workDir,"/",output.folders[1], "/", sep="") #rawdata
path.cd <- paste(workDir,"/",output.folders[2], "/", sep="") #cleandata
path.a <- paste(workDir,"/",output.folders[3], "/", sep="") #analysis
path.g <- paste(workDir,"/",output.folders[4], "/", sep="") #graphs
path.t <- paste(workDir,"/",output.folders[5], "/", sep="") #tables

# Install and Load External Dependencies

load.lib <- c(
                "ggpubr", "ggplot2","gridExtra", "jtools", "TSA", "tidyr", 
                "lubridate", "dplyr", "plyr", "tseries", "xts", "leaflet",
                "cowplot", "network", "sna", "igraph", "qgraph"
              )

install.lib <-load.lib[!load.lib %in% installed.packages()]

for(lib in install.lib) 
      install.packages(lib,dependencies=TRUE)
      sapply(load.lib,require,character=TRUE)


## READ DATA

main.raw <- read.csv("ACLEDRAWSL.csv", as.is = TRUE) #df for time-series

raw2 <- read.csv("ACLEDRAWSL.csv", as.is = TRUE, na.strings = "") #df for network-analyses

raw3 <- read.csv("ACLEDRAWSL.csv")


# remove whitespace
spaceless.table <- as.data.frame(lapply(main.raw,function(main.raw) 
{
  gsub(" ","",main.raw)
}
))

## CLEAN DATA 

raw3$date <- format(as.Date(raw3$event_date, format="%d %B %Y"),"%d-%B") #for ggplot graphics

raw3$event_date <- format(as.Date(raw3$event_date, format="%d %B %Y"),"%B")  #for ggplot graphics

## FOR TIME SERIES ANALYSIS ##

#  Define a vector containing all dates in the data, as a POSIXCT object
all.date.vector <- as.Date(spaceless.table$event_date,"%d%b%Y")


# Generate freq.table for dates
freq.all.dates <- as.data.frame(table(all.date.vector))

finaltimeseries <- unique(all.date.vector)

# empty vector filled with Freq values
date.freq <- freq.all.dates$Freq

riots <- 
  main.raw %>%
  dplyr::filter(actor1 == "Rioters (Sri Lanka)", actor2 == "Rioters (Sri Lanka)")

riots2 <- 
  main.raw %>%
  dplyr::filter(actor1 == "Rioters (Sri Lanka)", actor2 == "Rioters (Sri Lanka)")


double.edge.riot <- data.frame(riots2$assoc_actor_1, riots2$assoc_actor_2, stringsAsFactors = FALSE)

riot.3 <- 
  double.edge.riot %>%
  dplyr::filter(double.edge.riot$riots2.assoc_actor_1 != "", double.edge.riot$riots2.assoc_actor_2 != "")


double.edge.riot <- double.edge.riot[!apply(double.edge.riot == "", 1, all),]

double.edge.riot2 <- double.edge.riot[complete.cases(double.edge.riot),]

double.edge.riot2 <- double.edge.riot[!grepl(" ",double.edge.riot),]

require(igraph)

riot.mat <- as.matrix(riot.3)

plot(test.riot.graph)

test.riot.graph <- graph.edgelist(riot.mat, directed = FALSE)

V(test.riot.graph)

E(test.riot.graph)

gsize(test.riot.graph)

gorder(test.riot.graph)

require(igraph)
m1 <- layout_nicely(test.riot.graph)
plot(test.riot.graph, 
     vertex.label.color = "black",
     edge.color = 'black', 
     edge.width = igraph::E(test.riot.graph),
     layout = m1)

fuck.graph <- graph_from_adjacency_matrix(test.adj.riot, mode = "undirected", weighted = TRUE,
                            diag = TRUE)

fuck.graph.2 <- graph_from_adjacency_matrix(test.adj.riot, mode = "undirected", weighted = TRUE,
                                          diag = FALSE)


g23 <- delete_edges(fuck.graph, which(E(fuck.graph)$weight <= 1))


g24 <- delete_edges(fuck.graph, which(E(fuck.graph)$weight < 1))

w2 <- E(g24)$weight

new.adj.mat <- as_adjacency_matrix(g23,attr = "weight",sparse = T)

m2 <- layout_on_sphere(g24)

m3 <- layout_as_tree(g24)

deg <- degree(g24, mode="all")

lgl <- layout_with_lgl(g24)

plot(g24, 
     vertex.label= ifelse(degree(g24) >= 4, V(g24)$name, ""),
     vertex.color = 'white',
     vertex.label.cex = deg/6,
     vertex.size = deg*6,
     edge.color = 'black',
     edge.width = E(g24)$weight / 6,
     layout = m2)

plot(g24, 
     vertex.label= ifelse(degree(g24) >= 4, V(g24)$name, ""),
     vertex.color = 'white',
     vertex.shape = 'none',
     vertex.label.cex = deg/6,
     vertex.size = deg*6,
     edge.color = 'black',
     edge.width = E(g24)$weight / 6,
     layout = lgl)

wrap_strings <- function(vector_of_strings,width){
  as.character(sapply(vector_of_strings, FUN=function(x){
    paste(strwrap(x, width=width), collapse="\n")
  }))
}

V(g24)$label = wrap_strings(V(g24)$name, 12)


V(g24)$label.cex = 0.8

layout.by.attr <- function(graph, wc, cluster.strength=1,layout=layout.auto) {  
  g <- graph.edgelist(get.edgelist(graph)) # create a lightweight copy of graph w/o the attributes.
  E(g)$weight <- 1
  
  attr <- cbind(id=1:vcount(g), val=wc)
  g <- g + vertices(unique(attr[,2])) + igraph::edges(unlist(t(attr)), weight=cluster.strength)
  
  l <- layout(g, weights=E(g)$weight)[1:vcount(graph),]
  return(l)
}

set.seed(3)


mds <- layout_with_mds(g24)

plot(g24,
     vertex.size = deg*6,
     edge.color = 'black',
     edge.width = E(g24)$weight / 6,
     vertex.size=1, 
     vertex.label= ifelse(degree(g24) >= 4, V(g24)$label, ""),
     layout=lgl)

plot(g24,vertex.color = 'white',
     vertex.size = deg*7,
     edge.color = 'black',
     edge.width = E(g24)$weight / 6,
     vertex.size=1, 
     vertex.label= ifelse(degree(g24) >= 4, V(g24)$label, ""),
     layout=mds)
#############
require(igraph)
require(RColorBrewer)

pal <- brewer.pal(42, "Accent")

group <-factor(V(g24)$label)

vertex.col <- pal[group]



plot(g24,vertex.color = 'white',
     vertex.size = deg*7,
     edge.color = 'black',
     edge.width = E(g24)$weight / 6,
     vertex.size=1, 
     vertex.label= ifelse(degree(g24) >= 4, V(g24)$label, ""),
     layout=mds)

legend("topleft",bty = "n",
       legend=levels(group),
       border=NA) 




#LAPLACIAN
require(igraph)
lap.g24 <- as.matrix(graph.laplacian(g24))

lap.actor <- as.matrix(graph.laplacian(graph_from_adjacency_matrix(actor.adj.mat.1, weighted = TRUE, mode = "undirected")))

heatmap(actor.adj.mat.1)

require(pca3d)

pca3d(lap.actor)


lap.eig.act <- eigen(lap.actor)






wrap_strings <- function(vector_of_strings,width){
  as.character(sapply(vector_of_strings, FUN=function(x){
    paste(strwrap(x, width=width), collapse="\n")
  }))
}

V(g24)$label = wrap_strings(V(g24)$name, 12)


V(g24)$label.cex = 0.8

layout.by.attr <- function(graph, wc, cluster.strength=1,layout=layout.auto) {  
  g <- graph.edgelist(get.edgelist(graph)) # create a lightweight copy of graph w/o the attributes.
  E(g)$weight <- 1
  
  attr <- cbind(id=1:vcount(g), val=wc)
  g <- g + vertices(unique(attr[,2])) + igraph::edges(unlist(t(attr)), weight=cluster.strength)
  
  l <- layout(g, weights=E(g)$weight)[1:vcount(graph),]
  return(l)
}

set.seed(3)
plot(g24,vertex.color = 'white',
     vertex.size = deg*6,
     edge.color = 'black',
     edge.width = E(g24)$weight / 6,
     vertex.size=1, 
     vertex.label= ifelse(degree(g24) >= 4, V(g24)$label, ""),
     layout=wrap_strings)
####################### RETTRYING VISUALISATION FOR RIOTERS FROM SCRATCH 

graph.rioters.directed.test <- graph_from_adjacency_matrix(test.adj.riot, mode = "undirected", weighted = TRUE,
                                          diag = TRUE)

cut.off <- ((mean((E(graph.rioters.directed.test)$weight)))/2)

net.sp <- delete_edges(graph.rioters.directed.test, E(graph.rioters.directed.test)[weight<cut.off])

plot(net.sp, vertex.size = deg*6,
     edge.color = 'black',
     edge.width = E(net.sp)$weight / 6,
     vertex.size=1, 
     vertex.label= ifelse(degree(net.sp) >= 0.7, V(net.sp)$name, ""),
     layout=m2)


clp <- cluster_label_prop(net.sp)

plot(clp, net.sp,  vertex.size = deg*6,
     edge.color = 'black',
     edge.width = E(net.sp)$weight / 6,
     vertex.size=1, 
     vertex.label= ifelse(degree(g24) >= 4, V(g24)$label, ""),
     layout=m2)
##########
require(igraph)
clp <- cluster_optimal(net.sp)

V(net.sp)$community <- clp$membership
colrs <- adjustcolor( c("gray50", "tomato", "gold", "yellowgreen"), alpha=.6)
plot(net.sp, vertex.color=colrs[V(net.sp)$community],
     vertex.size = deg*6,
     edge.color = 'black',
     edge.width = E(net.sp)$weight / 6,
     vertex.size=1, 
     vertex.label= ifelse(degree(g24) >= 4, V(g24)$label, ""),
     layout=m2)

#################

oranges <- colorRampPalette(c("dark red", "gold"))
col <- oranges(max(dist.from.NYT)+1)
col <- col[dist.from.NYT+1]

############################################
distRioters <- dist(test.matrix) # check dist

resRioters <- hclust(distRioters)

hcd <- as.dendrogram(resRioters)

  plot(resRioters, hang = -1, cex = 0.6)
  
  plot(hcd, type = "rectangle", ylab = "Height")

fviz_dend(resRioters, repel =TRUE)

clusterRioters <- hcut(test.matrix, k = 6, stand = TRUE, repel =TRUE)

fviz_dend(clusterRioters, rect = TRUE, cex = 0.5,
          k_colors = c("#00AFBB","#2E9FDF", "#E7B800", "#FC4E07"))








all.riot.event <- 
  main.raw %>%
  dplyr::filter(event_type == 'Riots')

riot.dates <- all.riot.event$event_date

uniq.riot.dates <- unique(riot.dates)

clean.riot.dates <- as.Date(uniq.riot.dates, format ="%d %b %Y")

dummy.freq.riot <- data.frame(table(clean.riot.dates))

require(tidyr)
raw.riot.ts <-
  dummy.freq.riot %>%
  mutate(clean.riot.dates = as.Date(clean.riot.dates)) %>%
  tidyr::complete(clean.riot.dates = seq.Date(min(clean.riot.dates), #complete dates, gaps filled with NAs
                                     max(clean.riot.dates), by = "day")) %>%
  mutate(Freq = replace(Freq, is.na(Freq), 0)) # replace all NAs with 0



require(tidyr)
raw.riot.ts.2 <-
  fucked.riot.ts %>%
  mutate(Var1 = as.Date(Var1)) %>%
  tidyr::complete(Var1 = seq.Date(min(Var1), #complete dates, gaps filled with NAs
                                              max(Var1), by = "day")) %>%
  mutate(Freq = replace(Freq, is.na(Freq), 0)) # replace all NAs with 0

require(tseries)

riot.ts.2 <- ts(raw.riot.ts$Freq)

require(xts)
fucked.riot.ts <- data.frame(table(as.Date(riot.dates, format ="%d %b %Y")))

#periodicity
  
  new.riot.ts <- ts(fucked.riot.ts$Freq, start = 2010, end = 2020, frequency = 365)


periodicity(fucked.zoo.riot)
  
fucked.zoo.riot <- zoo(fucked.riot.ts, frequency = 365, order.by = raw.riot.ts$clean.riot.dates)

plot(raw.riot.ts$Freq)

adf.test(riot.ts.2)

adf.test(diff(riot.ts.2))

last.try <- ts(fucked.riot.ts, start = 2010, end = 2020, frequency = 365)

library(ggplot2)
ggplot(data.frame(last.try), aes(x = Var1, y = Freq)) +
  geom_line() +
  labs(title = "Riots in SL 2010-20",
       x = "Date",
       y = "No. of riots")

riot.ts.final.1 <- ts(raw.riot.ts$Freq, start = 2010, end = 2020, frequency = 365) #TIME SERIES BY DAY

riot.zoo.ts <- zoo(raw.riot.ts$Freq, raw.riot.ts$clean.riot.dates, frequency = 365, order.by = raw.riot.ts$clean.riot.dates)

plot(riot.ts.final.1)

spectrum(raw.riot.ts.2$Freq)

plot.ts(riot.zoo.ts)


spectral.plot.riot <- stats::spectrum(riot.ts.final.1)

# Periodogram / Power Spec Density / Fast Fourier Transform   

require(TSA)
pgram.riot <- periodogram(raw.riot.ts.2$Freq)

pgram.riot.sum <- data.frame(freq=pgram.riot$freq, 
                            spec=pgram.riot$spec)
order = pgram.riot.sum[order(
  -riot.sumy$spec),]
top50 = head(order, 50)
View(top50)


V(g24)$label <- letters[1:20]

which(V(g24)$weight == 0)

plot(g24, )
blablabla <- eigen_centrality(test.adj.riot)

get.adjacency(test.riot.graph, attr = 'weight', edg)

test.adj.riot <- as_adjacency_matrix(test.riot.graph, type = "undirected", sparse = T)

test.matrix <- as.matrix(test.adj.riot)

riot.pca <- princomp(test.matrix, cor = TRUE, scores = TRUE)


require(factoextra)
require(ggplot2)

fviz_eig(riot.pca)

fviz_pca_biplot(riot.pca, repel = TRUE)

fviz_pca_var(riot.pca, repel = TRUE)

pca3d::pca3d(riot.pca, group = levels(test.riot.graph))


clean.date.vec <- as.POSIXct(freq.all.dates$all.date.vector, format = "%Y-%m-%d")

clean.date.df <- data.frame(clean.date.vec, date.freq)

# Create a gapless timeseries (for day-by-day analysis)
raw.ts1 <-
  clean.date.df %>%
  mutate(clean.date.vec = as.Date(clean.date.vec)) %>%
  complete(clean.date.vec = seq.Date(min(clean.date.vec), #complete dates, gaps filled with NAs
                                     max(clean.date.vec), by = "day")) %>%
  mutate(date.freq = replace(date.freq, is.na(date.freq), 0)) # replace all NAs with 0
#
  colnames(raw.ts1) <- c("Date", "No.of Violent Events") #set collumn names

# ASSIGN a collum vector, where the vector shows what month the even took in
  num.month.vector <- as.character(raw.ts1$Date, format = "%m")
  
  num.year.vector <- as.character(raw.ts1$Date, format = "%Y")
  
  master.freq.ts <- data.frame(raw.ts1, num.month.vector, num.year.vector)
  
  raw.ts2 <- raw.ts1$`No.of Violent Events`
  
## DATA VISUALISATION ##

# SUMMARY STATS # # Basic descriptive plots # 
  
require(tidyr) 
  
no_of_levels <- raw3 %>% # check Levels
    dplyr::summarise_all(nlevels) %>%
    gather(variable, num_levels)

prop.event.type.by.year <- raw3 %>% #summary by proportion of event_type 
  group_by(year) %>%
  count(year, event_type) %>%
  add_count(year, wt = n, name = 'nn') %>%
  mutate(perc = n / nn) 


# Event Types 

ggplot(prop.event.type.by.year, aes(x = year, y = n, group = event_type)) + 
  geom_line(aes(color = event_type)) + 
  xlab("Year") + ylab("No. of Recorded Incidents") + ggtitle("Event Types by Frequency 2010-20") +
  theme_minimal() +
  scale_fill_brewer(palette = "Dark3")  ##check st


ggplot(media.sources[1:10,], aes (x = Var1, y = Freq))

# facet Plot over Time 
ggplot(prop.event.type.by.year, aes(x = year, y = n, group = event_type)) + 
  geom_line(color="steelblue") + 
  xlab("Year") + ylab("No. of Recorded Incidents") + ggtitle("Event Types by Frequency 2010-20") +
  facet_wrap(~ event_type) +
  theme_minimal()

# conflicts by location 

conflict.location <- raw3 %>% 
  group_by(location, event_type) %>%
  dplyr::summarise(freq = n()) %>% 
  arrange(desc(freq))

# top 10 locations by incidence
ggplot(conflict.location[1:10,], aes(reorder(location, -freq), freq)) + 
  geom_bar(stat = "identity", aes(fill = event_type, color = )) + 
  xlab("District") + ylab("No. of Recorded Incidents") + ggtitle("Locations with Highest Incidences of Conflict") +
  coord_flip() +
  theme_minimal()

# top 50 locations by incidence 
ggplot(conflict.location[1:50,], aes(reorder(location, -freq), freq)) + 
  geom_bar(stat = "identity", aes(fill = event_type)) + 
  xlab("Location") + ylab("No. of Recorded Incidents") + ggtitle("Locations with Highest Incidences of Conflict - Top 50") +
  coord_flip() +
  theme_minimal()

# Actors most frequently associated with incidents

act.3 <- raw3 %>% group_by(actor1) %>%  
  dplyr::summarise(total = n()) 
act.4 <- raw3 %>% group_by(actor2) %>%
  dplyr::summarise(total = n())

names(act.3) <- c("actors", "total")

names(act.4) <- c("actors", "total")

act.5 <- rbind(act.3, act.4)

act.5 <- arrange(act.5, desc(total))

ggplot(act.5[1:50,], aes(x = reorder(actors, total), y = total)) +
  geom_bar(stat='identity', aes(fill = actors)) +
  guides(fill=FALSE) +
  xlab("Actors") + ylab("Number of Conflicts") + ggtitle("Actors with Highest Incidences of Involvement")+
  coord_flip() + 
  theme_minimal()

ggplot(act.5[7:50,], aes(x = reorder(actors, total), y = total)) +
  geom_bar(stat='identity', aes(fill = actors)) +
  guides(fill=FALSE) +
  xlab("Actors") + ylab("Number of Conflicts") + ggtitle("Actors with Highest Incidences of Involvement(7-50)")+
  coord_flip() + 
  theme_minimal() 

# Actors responsible for fatalities 

act.1 <- raw3 %>% group_by(actor1) %>%
  dplyr::summarise(total = sum(fatalities)) 
act.2 <- raw3 %>% group_by(actor2) %>%
  dplyr::summarise(total = sum(fatalities))

names(act.1) <- c("actors", "total")
names(act.2) <- c("actors", "total")

act.6 <- rbind(af1,af2)
act.6 <- arrange(act.6, desc(total))

ggplot(act.6[1:20,], aes(x = reorder(actors, total), y = total)) +
  geom_bar(stat='identity', aes(fill = actors)) +
  guides(fill=FALSE) +
  xlab("Actors") + ylab("Number of fatalities") + ggtitle("Actors involved with most no. of fatalities")+
  coord_flip() + 
  theme_minimal()

##  FATALITIES GROUPED BY YEAR

fat.1 = aggregate(fatalities~year+event_date, raw3, sum)

ggplot(fat.1, aes(event_date,fatalities)) + 
  geom_bar(stat = "identity", aes(fill = event_date)) + facet_grid(year~.) +
  geom_text( aes(label = fatalities),fontface = "bold")+
  theme(axis.text.x = element_text(angle = 90, hjust = 6)) +
  guides(fill=FALSE) + 
  xlab("Month") + ylab("Fatalities") + ggtitle("Fatality rate over years") +
  theme_minimal()

# MAPPING AREAS
require(leaflet)
leaflet(raw3) %>% addTiles() %>%
  addCircles(lat = ~latitude, lng = ~longitude, 
             radius = ~fatalities, color = "#084081", opacity = 0.1) %>%
  setView(lng = median(raw3$longitude), lat = median(raw3$latitude), zoom = 10)


############ ANALYSIS ###################

## TIME SERIES ANALYSIS ##

# Descriptive Statistics for TS #

                                                  # INCOMPLETE !! ## 

#  Check Stationarity 

# Augmented Dickey-Fuller Test

require(tseries)

adf.test(raw.ts2)

adf.test(diff(raw.ts2))

adf.test(diff(diff(raw.ts2)))

# ARIMA Test

at.ts <- arima(raw.ts2)

# Main TSA #
ts1riot <- ts(raw.riot.ts.2$Freq, start = 2010, end = 2020, frequency = 365) #TIME SERIES BY DAY

ts.plot(ts1riot, xlab = "Year", ylab = "Number of Riots by Day", main = 
          "Riots in Sri Lanka 2010-20", type = "p")

lo <- loess(raw.riot.ts.2$Freq)

plot(x = seq(1:length(raw.riot.ts.2$Freq)), raw.riot.ts.2$Freq, type = "s")
lines(predict(lo), col='red', lwd=2)
# Spectral Density Plot

spectral.plot <- stats::spectrum(raw.ts2)

# Periodogram / Power Spec Density / Fast Fourier Transform   

pgram.ts1 <- periodogram(raw.ts2)

pgram.summary <- data.frame(freq=pgram.ts1$freq, 
                                    spec=pgram.ts1$spec)
order = pgram.summary[order(
   -pgram.summary$spec),]
      top50 = head(order, 50)
View(top50)

############################ NETWORK ANALYSIS ################################

# 8x8 Adjacency Matrix of Actors 

actor.adj.mat.1 <- matrix(c(
  0,6,78,4,138,141,74,0,6,0,0,0,0,0,7,0,78,0,0,0,243,
  1,0,0,4,0,0,0,0,0,14,1,138,0,243,0,0,21,250,2,141,0,2,
  0,21,0,2,0,74,7,243,14,250,2,0,2,0,0,0,1,2,0,2,0
),
nrow=8,
ncol=8
)



actor.adj.mat.1 <- matrix(c(
  0,6,78,4,138,141,74,0
  ,6,0,0,0,0,0,7,0
  ,78,0,0,0,243,1,0,0
  ,4,0,0,0,0,0,14,1,
  138, 0,243,0,0,21,250,2,
  141,0,2,21,0,2,0,
  74, 7,243,14,250,2,0,2,
  0,0,0,1,2,0,2,0
),
nrow=8,
ncol=8
)


# get eigen values/vectors

eigen.mat.1 <- eigen(actor.adj.mat.1)

eigen.print <- as.data.frame(eigen.mat.1)

# Principal Component Analysis

pca.mat.1 <- princomp(actor.adj.mat.1, cor = TRUE, scores = TRUE)

pca.2.mat.1 <- prcomp(actor.adj.mat.1)

summary(pca.mat.1)

require(factoextra)
require(ggplot2)

fviz_eig(pca.mat.1)

fviz_pca_biplot(pca.mat.1, repel = TRUE)

fviz_pca_var(pca.mat.1, repel = TRUE)

# variable contribution to principal axes 
  
fviz_contrib(pca.2.mat.1, choice = "var", axes =1, top = 10) 
fviz_contrib(pca.2.mat.1, choice = "var", axes =2, top = 10) # Contributions of variables to PC2
fviz_contrib(pca.2.mat.1, choice = "var", axes =3, top = 10) # 

fviz_pca_var(pca.2.mat.1, choice = "var", axes =3, top = 10) #

gr <- factor(pca.2.mat.1[,1])

pca <- prcomp(pca.mat.1[,-1], scale.=TRUE)

s3d<- plot3d(pca.mat.1$scores[,1:3])

pca3d(pca.mat.1, group = factor(actor.vec), show.labels = TRUE, ellipse.ci = o.95)

actor.vec <- c("State Forces", "Rebel Groups", "Political Militias", "Identity Militias", "Rioters", "Protestors", "Civillians", "External Forces")

##  cluster analyssi

distACT <- dist(actor.adj.mat.1) # check dist

resACT <- hclust(distACT)

plot(resACT)


plot(hclust(dist(actor.adj.mat.1)))

clusterActors <- hcut(actor.adj.mat.1, k = 5, stand = TRUE)

fviz_dend(clusterActors, rect = TRUE, cex = 0.5,
          k_colors = c("#00AFBB","#2E9FDF", "#E7B800", "#FC4E07"))

webGLOutput("myWebGL")
output$myWebGL <- renderWebGL({
pca3d(pca.mat.1, group = factor(actor.vec), show.labels = TRUE, ellipse.ci = o.95) 
})

pca3d(pca.mat.1, group = factor(actor.vec), show.labels = TRUE, ellipse.ci = o.95)

makeMoviePCA()

media.sources <- as.data.frame(sort(table(main.raw$source), decreasing = TRUE)) 

prop.media <- (media.sources$Freq / sum(media.sources$Freq) * 100)

media.sources$pec <- prop.media

require(tidyr)
require(dplyr)

    
    media.sources$lab.ypos <- cumsum(prop.media) - (0.5*prop.media)


require(ggplot2)
ggplot(media.sources[1:10,], aes(x="", y=Var, fill=Freq)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  geom_text(aes(y = lab.ypos, label = pec), color = "white") 
theme_void()



require(igraph)
page_rank(g24)

pie(media.sources$Freq)

pg.rioers <- page.rank(g24, directed =TRUE)$vector

sorted.pg.rioters <- sort(pg.rioers, decreasing = TRUE)

pg.8by8mat <- page.rank(actor.pagerank)

actors.sorted.pg <- sort(pg.8by8mat$vector, decreasing = TRUE)

try.pg.df <- page_rank(g24)
try.2 <- try.pg.df$vector

actor.pagerank <- graph_from_adjacency_matrix(actor.adj.mat.1, mode = "undirected", weighted = TRUE,
                                          diag = TRUE)




bp <- ggplot(media.sources, aes(x="", y=Freq, fill=))+
  geom_bar(width = 1, stat = "identity")
bp