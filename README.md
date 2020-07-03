# Network-Analysis---Amazon-Books



# NETWORK ANALYSIS - AMAZON BOOK DATA


# -- LOAD DATA
##-- Loading the data
setwd('/Users/gnels/Desktop/JupyterNB')

# Importing products.csv and copurchase.csv in R
product_raw<-read.csv(file.choose(), header = T)
copurchase_raw <-read.csv(file.choose(), header = T)


# -- LOAD LIBS
## Initializing the required Libraries
library(dplyr)
library(igraph)
library(sqldf)
library(ggplot2)
library(tidyr)
library(dlookr)
library(formattable)
library(rgl)


# -- PROCESS DATA
## Filtering the data to books
product <- filter(product_raw, group == "Book" &
                    product_raw$salesrank<=150000 &
                    product_raw$salesrank !=-1)
head(product, 2)

copurchase <- filter(copurchase_raw, copurchase_raw$Source %in% product$id & 
                       copurchase_raw$Target %in% product$id)
head(copurchase, 2)


#-----Plotting the network of each point 
grap2 <- graph.data.frame(copurchase, directed = T)
head(grap2[1])
str(grap2)

indeg <- degree(grap2, mode = 'in')
head(indeg)

outdeg <- degree(grap2, mode = 'out')
head(outdeg)

alldeg <- degree(grap2, mode = 'all')
head(alldeg)

#ADD
edge_density(grap2, loops=F)



# -- SUBCOMPONENT
## Finding the subcomponent by finding the node with the maximum degree 
max(alldeg)
maxoutdeg[maxoutdeg==53]

maxdeg = alldeg[alldeg == 53]

#--add
alldeg2 = alldeg
alldeg2_df <- data.frame(table(degree=factor(alldeg2, levels=seq_len(max(alldeg2)))))
alldeg2_df$degree <- as.numeric(as.character(alldeg2_df$degree))

# plotting 
ggplot(aes(x = degree, y=Freq ) , data = alldeg2_df) + 
  geom_histogram(stat = 'identity',
                 #binwidth=1500, 
                 colour="grey20", lwd=0.2) 

## The two points with a max degree of 53 are 4429 and 33.
## Let us consider the consider the subcomponent to be 33.
sub <- subcomponent(grap2, "4429", "all")

# Review items attached to subcomponent ids
filter(product, 
       id==33 | 
       id==4429)

filter(product, 
       id==829 | 
         id==62)

net_graph <- induced_subgraph(grap2, sub)
net_graph

V(net_graph)  
E(net_graph)
V(net_graph)$label <- V(net_graph)$name
V(net_graph)$degree <- degree(net_graph)

plot(net_graph,
     vertex.color='red',
     vertex.size= V(net_graph)$degree*0.01,
     edge.arrow.size=0.01,
     vertex.label.cex=0.01,
     layout=layout.kamada.kawai)


#### Calculating the diameter, which is the longest distance between two vertices.
# There are a total of 10 nodes with the the diameter 9, which is the longest distance.

diameter(net_graph, directed = T, weights = NA)

dia <- get_diameter(net_graph, weights = NULL)
dia


#### Plotting the above network graph, and highlighting the diameter nodes.

V(net_graph)$color<-"green"
V(net_graph)$color[dia]<-"red"

class(net_graph)
max(net_graph)

#Edge and Vertex color
ecol <- rep("gray80", ecount(net_graph))
ecol2 <- rep("darkorange", ecount(net_graph))
valcol = (V(net_graph)$degree + abs(min(V(net_graph)$degree)))/max(V(net_graph)$degree + abs(min(V(net_graph)$degree)))
vcol <- rep(rainbow(4, start = 0.7, end = 0.9), valcol)


# plot 1
plot(net_graph,
     vertex.color= rgb(0.5, 0.6 , valcol),
     vertex.size= V(net_graph)$degree*log(1.35),
     edge.arrow.size=0.01,
     edge.color = ecol,
     edge.width=1.5,
     vertex.label.color="red",
     vertex.label.dist=1.5,
     vertex.label = ifelse(degree(net_graph) > 50, V(net_graph)$label, NA),
     vertex.label.degree= -45,
     margin=-0.3,
     layout=layout.kamada.kawai)

# plot 2
plot(net_graph,
     vertex.color= rgb(0.5, 0.6 , valcol),
     vertex.size= V(net_graph)$degree*log(1.35),
     edge.arrow.size=0.01,
     edge.color = ecol,
     edge.width=1.5,
     vertex.label.color="black",
     vertex.label.dist=0,
     vertex.label = ifelse(degree(net_graph) > 50, V(net_graph)$label, NA),
     vertex.label.degree= 0,
     margin=-0.3,
     layout=layout_nicely(net_graph))

# plot 3
plot(net_graph,
     vertex.color= rgb(0.5, 0.6 , valcol),
     vertex.size= V(net_graph)$degree*log(1.35),
     edge.arrow.size=0.01,
     edge.color = ecol,
     edge.width=1.5,
     vertex.label.color="white",
     vertex.label.dist=0,
     vertex.label = ifelse(degree(net_graph) > 50, V(net_graph)$label, NA),
     vertex.label.degree= 0,
     margin=-0.3,
     layout=layout_with_fr(net_graph))


# plot 4
# Not working...
coords <- layout_with_fr(net_graph, dim=3)
rglplot(net_graph, layout=coords)

rgl.open()
rglplot(net_graph, r = 0.1,
        layout=coords)


##### Calculating the degree of the dsitribution
ddall <- degree_distribution(net_graph, cumulative=T, mode="all")
ggplot(as.data.frame(net_graph), aes(degree)) +
  boxplot()
boxplot(alldeg, main= "Degrees Plotted")
class(net_graph)
#ADD 
ggplot(net_graph, )

plot( x=0:max(alldeg), y=1 - ddall, pch=10, cex=0.5, col="blue", 
      xlab="Degree", ylab="Cumulative Frequency")



#### Calculating the required statistics
## Density
edge_density(net_graph, loops=F)
reciprocity(net_graph)

## Centrality
centr_degree(net_graph)

## Closeness
closeness <- closeness(net_graph, mode='all', weights=NA)
head(closeness)
max(closeness)

## betweenness
betweenness <- betweenness(net_graph, directed='T', weights=NA)

plot(net_graph,
     vertex.size= betweenness*0.03,
     main = 'Betweenness',
     vertex.color = rainbow(75),
     edge.arrow.size=0.1,
     vertex.label = ifelse(betweenness > (max(betweenness)*.98), V(net_graph)$label, NA),
     margin=-0.7,
     layout = layout.kamada.kawai)

closesize = (min(closeness)/(max(closeness)-min(closeness)))
plot(net_graph,
     vertex.size= closesize*5,
     main = 'Closeness',
     vertex.color = rainbow(75),
     edge.arrow.size=0.1,
     vertex.label = ifelse(closeness > (max(closeness*1000)*.9), V(net_graph)$label, NA),
     margin=-0.5,
     layout = layout.kamada.kawai)


## hub scores
hubscore <- hub.score(net_graph)$vector
head(hubscore[hubscore>0.98],5)
head(hubscore[hubscore<0.01],2)
max(hubscore)

plot(net_graph,
     vertex.size= hubscore*5,
     main = 'Hubs',
     vertex.color = rainbow(52),
     edge.arrow.size=0.1,
     vertex.label = ifelse(hubscore > .96, V(net_graph)$label, NA),
     margin=-0.7,
     layout = layout.kamada.kawai)

## Authority Score
authorityscore<-authority.score(net_graph)$vector
head(authorityscore, 5)

plot(net_graph,
     vertex.size= authorityscore*10,
     main = 'Authority',
     vertex.color = rainbow(52),
     edge.arrow.size=0.1,
     vertex.label = ifelse(authorityscore > .97, V(net_graph)$label, NA),
     margin=-0.6,
     layout = layout.kamada.kawai)



#### Calculating the mean variables
product$id<-as.vector(product$id)
sub_id<-as_ids(sub)
productsub<-product[product$id %in% sub_id,]
head(productsub)

mean <- copurchase %>%
  group_by(Target) %>%
  inner_join(productsub,by=c('Source'='id'))%>%
  summarise(nghb_mn_rating=mean(rating),
            nghb_mn_salesrank=mean(salesrank), 
            nghb_mn_review_cnt=mean(review_cnt))

# testing output from igraph
# not used
#neighbors = ego_size(net_graph,  order = 1,  mode = c("all"), mindist = 1)
#head(as.data.frame(neighbors))


# Create and sort mean neighbor datatables
sumprd = summary(rating_pred)
sc = as.data.frame(sumprd$coefficients)
round(mean[with(mean, order(nghb_mn_salesrank)), ], 2)

vals_1 = (mean[with(mean, order(-nghb_mn_salesrank)), ])
vals_2 = (mean[with(mean, order(nghb_mn_salesrank)), ])
formattable(head(vals_1), 3)
formattable(head(vals_2), 3)

vals_revcount1 = (mean[with(mean, order(nghb_mn_review_cnt)), ])
vals_revcount2 = (mean[with(mean, order(-nghb_mn_review_cnt)), ])
formattable(head(vals_revcount), 3)
formattable(head(vals_revcount2), 3)

# find median values
median(mean$nghb_mn_review_cnt)
median(mean$nghb_mn_rating)

hist(mean$nghb_mn_review_cnt,
     col='grey80',
     main='Histogram',
     ylab='Frequency')

hist(mean$nghb_mn_rating,
     col='grey80',
     main='Histogram',
     ylab='Frequency')


# -- SEPARATE DATA FOR MODELING
#### Seperating the data into different data frames
indegree <- as.data.frame(indeg)
indegree <- cbind(newColName = rownames(indegree), indegree)
rownames(indegree) <- 1:nrow(indegree)
colnames(indegree) <- c("Nodes", "in_degree")

outdegree <- as.data.frame(outdeg)
outdegree <- cbind(newColName = rownames(outdegree), outdegree)
rownames(outdegree) <- 1:nrow(outdegree)
colnames(outdegree) <- c("Nodes", "out_degree")

closeness_new <- as.data.frame(closeness)
closeness_new <- cbind(newColName = rownames(closeness_new), closeness_new)
rownames(closeness_new) <- 1:nrow(closeness_new)
colnames(closeness_new) <- c("Nodes", "closeness")

betweenness_new <- as.data.frame(betweenness)
betweenness_new <- cbind(newColName = rownames(betweenness_new), betweenness_new)
rownames(betweenness_new) <- 1:nrow(betweenness_new)
colnames(betweenness_new) <- c("Nodes", "betweenness")

hub_score <- as.data.frame(hubscore)
hub_score <- cbind(newColName = rownames(hub_score), hub_score)
rownames(hub_score) <- 1:nrow(hub_score)
colnames(hub_score) <- c("Nodes", "hub_score")

authority_score <- as.data.frame(authorityscore)
authority_score <- cbind(newColName = rownames(authority_score), authority_score)
rownames(authority_score) <- 1:nrow(authority_score)
colnames(authority_score) <- c("Nodes", "authority_score")


### creating an sql dataframe for poisson regression.
data <- sqldf("SELECT mean.Target, hub_score, betweenness, authority_score, 
closeness, in_degree, out_degree, nghb_mn_rating, nghb_mn_salesrank, nghb_mn_review_cnt,
product.review_cnt, product.downloads, product.rating, product.salesrank
                      FROM mean, product, hub_score, betweenness_new, authority_score, closeness_new, indegree, outdegree
                      WHERE mean.Target = betweenness_new.Nodes 
                      and mean.Target = authority_score.Nodes
                      and mean.Target = closeness_new.Nodes
                      and mean.Target = indegree.Nodes
                      and mean.Target = outdegree.Nodes
                      and mean.Target = hub_score.Nodes
                      and mean.Target = product.id")


# review quick eda on variables above
head(data)
names(data)
targeted1 = target_by(data, salesrank)
subset1_report = eda_report(targeted1, output_format = 'html')
normality(data)


# -- REGRESSION
### performing poisson regression
summary(rating_pred<- glm(salesrank ~ review_cnt + downloads + rating + hub_score + betweenness + 
                            authority_score + closeness + in_degree + out_degree + 
                            nghb_mn_rating + nghb_mn_salesrank + nghb_mn_review_cnt, family="poisson",
                          data=data))
sort(coef(rating_pred))
sumprd = summary(rating_pred)
sc = as.data.frame(sumprd$coefficients)
round(sc[with(sc, order(Estimate)), ], 3)

# Rev logs
summary(rating_pred2<- glm(salesrank ~ log(review_cnt) + log(downloads) + rating + hub_score + log(betweenness) + 
                            authority_score + closeness + in_degree + out_degree + 
                            nghb_mn_rating + nghb_mn_salesrank + log(nghb_mn_review_cnt), family="poisson",
                          data=data))

# -- ADDED 
# Additional evaluation on data
head(mean)
""
min(data$review_cnt)
median(data$review_cnt)
mean(data$review_cnt)

count(filter(data, 
             review_cnt ==0))


boxplot(data$rating, main= "Rating Count")
boxplot(data$review_cnt, main= "Review Count")
