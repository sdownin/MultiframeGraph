#' map
#' 
#' Map a vector (e.g., vertex sizes) proportionally or via log transform
#' to a given real interval.
#' 
#' @param x  an n-length vector of numeric values
#' @param minmax  a vector of two positive numeric values defining the interval
#' onto which x is mapped, e.g. c(1,20)
#' @param log  logical to do log transform; default FALSE
#' @return a vector same length as x argument
#' @details 
#' This function (f: Rd -> Rd) maps a vector proportionally or via log transform
#' to a given real interval. Output values of -Inf are coded as the minimum of
#' the minmax argument. Nonpositive (x_i < 0) elements of input vector are okay
#' since log transform is done after mapping to minmax interval, which must be
#' positive. Use log transform to even out the intervals between elements in x,
#' for example to make more clear sizes of graph vertices.
#' @author Stephen Downing
#' @examples
#' \dontrun{
#' ## simple example
#' x1 <- 90:100 
#' map(x = x1, minmax = c(1,20), log = F)
#' ## log transform with negative input values
#' x2 <- -5:5
#' map(x = x2, minmax = c(0,1), log = T)
#' ## log transform with negative decimal values
#' x3 <- seq(from = -1.5, to = 1.5, by = .1)
#' map(x = x3, minmax = c(1,2), log = T)
#' }
map <- function(x,   #vector of degrees
                minmax=c(1,20),  # vector of length 2: min and max
                log=F           # logicial for log transform
) { 
  if (any(minmax < 0)) stop ("Negative output range is not allowed.\nPlease assign minmax argument as vector of 2 non-negative values (x>=0) and rerun function.")
  n <- length(x)
  dist <- max(x) - min(x)  #scalar
  #output
  M <- max(minmax)
  m <- min(minmax)
  range <- M - m   # interval to be mapped to
  scale <- (x-min(x)) / dist 
  
  if(log) {
    #augment x proportions to > 1 yielding values suitable for log transform
    scalepos <- (scale+1)/min(scale+1)
    #account for log transform while still proportional to elements of x 
    # by normalizing with the log of the maximum
    scale <- log(scalepos) / log(max(scalepos))
  }
  y <- m + range*scale
  return(y)
}



# 
# #' multigraph
# #' 
# #' Plot a matrix of igraph objects (induced subgraphs by time period) 
# #' with nodes and edges colored and sized by attribute and overall plot 
# #' faceted by time period. 
# #' @param graphlist  a list of igraph objects (i.e., graphs)
# #' @param attrib  character name of igraph vector attribute for coloring, sizing
# #' @param savename  character name to save plot to working director
# #' @param pdbreak  integer vector of times to divide periods
# #' @param vertex.label.minmax numeric vector of length 2: minmax vertex label
# #' @param vertex.size.minmax numeric vector of length 2: minmax vertex size
# #' @param edge.width.minmax numeric vector of length 2: minmax edge width
# #' @param edge.label.minmax numeric vector of length 2: minmax edge label
# #' @return list of length 2: 1. gout: subgraphs by time period for each 
# #' igraph object in list; 2. namedf: df of attribute names and values for 
# #' the subgraph nodes.
# #' @details 
# #' This function creates a plot with a matrix of subgraphs from a list of 
# #' igraph objects. Each graph in the list is a row and each time period is 
# #' a column. Each igraph object in the list must have the same vertex 
# #' attribute names. Each igraph object in the list must have the time 
# #' period attribute called "period", a numeric or interger attribute. 
# #' Vertex coloring is automatically generated from rainbow pallette. 
# #' @author Stephen Downing
# 
# multigraph <- function(graphlist,
#                        attrib,
#                        savename,
#                        pdbreak,
#                        vertex.label.minmax,
#                        vertex.size.minmax,
#                        edge.width.minmax,
#                        edge.label.minmax,
#                        png.width,
#                        png.height,
#                        png.res
# ) {
#   n <- length(graphlist)
#   m <- length(pdbreak) + 1
#   gout <- list()
#   # get list of unique names from all graphs in list
#   name <- c()
#   pdlist <- c()
#   for (i in 1:n){
#     name <- c(name,get.vertex.attribute(graphlist[[i]],attrib))
#     pdlist <- c(pdlist,unique(V(graphlist[[i]])$period))
#   }
#   pdlist <- unique(pdlist)
#   name <- unique(name)
#   name <- name[order(name)]
#   namedf <- data.frame(name=name,number=1:length(name))
#   namedf[,1] <- as.character(namedf[,1])
#   
#   pdname <- c()
#   for (i in 1:length(pdlist)) {
#     pdname <- 
#   }
#   pdname <- rep(c("<1990","<2000","<2013"),n) # repeat for number of graphs
#   
#   for (i in 1:n)  {  #loop of territory 
#     g <- graphlist[[i]]
#     V(g)$ipcfnum <- NA
#     for (l in 1:dim(namedf)[1]) {
#       V(g)[which(get.vertex.attribute(g,attrib)==
#                    namedf[l,1])]$ipcfnum <- namedf[l,2]
#     }
#     territory <- unique(V(g)$territory)[1]
#     
#     #split by periods#####################
#     g3 <- g
#     g2 <- delete.vertices(graph = g3, v = V(g3)[V(g3)$period > pdbreak[1] |
#                                                   is.na(V(g3)$year)])
#     g1 <- delete.vertices(graph = g2, v = V(g2)[V(g2)$period > pdbreak[2]])
#     
#     V(g3)$indeg <- degree(g3,mode = 'in')
#     V(g3)$outdeg <- degree(g3,mode = 'out')
#     V(g3)$degdiff <- V(g3)$indeg - V(g3)$outdeg
#     #
#     V(g2)$indeg <- degree(g2,mode = 'in')
#     V(g2)$outdeg <- degree(g2,mode = 'out')
#     V(g2)$degdiff <- V(g2)$indeg - V(g2)$outdeg
#     #
#     V(g1)$indeg <- degree(g1,mode = 'in')
#     V(g1)$outdeg <- degree(g1,mode = 'out')
#     V(g1)$degdiff <- V(g1)$indeg - V(g1)$outdeg
#     
#     g3n <- unique(get.vertex.attribute(g3,attrib))
#     g3n <- g3n[order(g3n)]
#     g2n <- unique(get.vertex.attribute(g2,attrib))
#     g2n <- g2n[order(g2n)]
#     g1n <- unique(get.vertex.attribute(g1,attrib))
#     g1n <- g1n[order(g1n)]
#     
#     g3vl <- c(); g2vl <- c(); g1vl <- c()
#     for (k in 1:vcount(g3)) {
#       g3vl[k] <- which(get.vertex.attribute(g3,name = attrib,
#                                             index = k) == g3n) 
#     }
#     for (k in 1:vcount(g2)) {
#       g2vl[k] <- which(get.vertex.attribute(g2,name = attrib,
#                                             index = k) == g2n) 
#     }
#     for (k in 1:vcount(g1)) {
#       g1vl[k] <- which(get.vertex.attribute(g1,name = attrib,
#                                             index = k) == g1n) 
#     }
#     # contract graph by attribute name
#     # 3
#     gc3 <- contract.vertices(graph = g3,
#                              mapping = g3vl,
#                              vertex.attr.comb=list(territory='first',
#                                                    degree='sum',
#                                                    year='mean',
#                                                    degdiff='mean',
#                                                    indeg='sum',
#                                                    outdeg='sum',
#                                                    ipcfnum='mean'))
#     V(gc3)$name <- g3n
#     gc3 <- simplify(graph = gc3,remove.multiple = T,
#                     remove.loops = T,
#                     edge.attr.comb = list(weight='sum'))
#     #       for (l in 1:vcount(gc3)) {
#     #         V(gc3)[l]$ipcfnum <- namedf[which(V(gc3)$name[l]==namedf[l,1]),2]
#     #       }
#     # 2
#     gc2 <- contract.vertices(graph = g2,
#                              mapping = g2vl,
#                              vertex.attr.comb=list(territory='first',
#                                                    degree='sum',
#                                                    year='mean',
#                                                    degdiff='mean',
#                                                    indeg='sum',
#                                                    outdeg='sum',
#                                                    ipcfnum='mean'))
#     V(gc2)$name <- g2n
#     gc2 <- simplify(graph = gc2,remove.multiple = T,
#                     remove.loops = T,
#                     edge.attr.comb = list(weight='sum'))
#     # 1
#     gc1 <- contract.vertices(graph = g1,
#                              mapping = g1vl,
#                              vertex.attr.comb=list(territory='first',
#                                                    degree='sum',
#                                                    year='mean',
#                                                    degdiff='mean',
#                                                    indeg='sum',
#                                                    outdeg='sum',
#                                                    ipcfnum='mean'))
#     V(gc1)$name <- g1n
#     gc1 <- simplify(graph = gc1,remove.multiple = T,
#                     remove.loops = T,
#                     edge.attr.comb = list(weight='sum'))
#     # end contraction
#     
#     gout[[1+ length(gout)]] <- gc1
#     gout[[1+ length(gout)]] <- gc2
#     gout[[1+ length(gout)]] <- gc3
#     names(gout)[length(gout)-2] <- paste(territory,1,sep="")
#     names(gout)[length(gout)-1] <- paste(territory,2,sep="")
#     names(gout)[length(gout)] <- paste(territory,3,sep="")
#     # end split by periods ##
#   } # end territory loop: i territories
#   
#   nn <- 3*n
#   
#   #make universal size attributes #################
#   uvl <- c()
#   uvs <- c()
#   uw <-  c()
#   for (i in 1:nn) {
#     uvl <-   c(uvl,V(gout[[i]])$degree)
#     uvs <- c(uvs,V(gout[[i]])$degree)
#     uw <- c(uw,E(gout[[i]])$weight)
#   }
#   uvl <- unique(uvl)
#   uvldf <- data.frame(uvl=uvl,map=map(uvl,vertex.label.minmax,log = T))
#   uvs <- unique(uvs)
#   uvsdf <- data.frame(uvs=uvs,map=map(uvs,vertex.size.minmax,log = T))
#   uw <- unique(uw)
#   uwdf <- data.frame(uw=uw,map=map(uw,edge.width.minmax,log = T))
#   ueldf <- data.frame(uw=uw,map=map(uw,edge.label.minmax,log = T))
#   #
#   vertexsize <- c()
#   vertexlabelcex <- c()
#   edgewidth <- c()
#   edgelabelcex <- c()
#   for (i in 1:nn) {
#     if (vcount(gout[[i]]) > 0) {
#       for (j in 1:vcount(gout[[i]])) {
#         V(gout[[i]])$vertexsize[j] <- uvsdf[which(V(gout[[i]])$degree[j]==uvsdf[,1]),2] 
#         V(gout[[i]])$vertexlabelcex[j] <- uvldf[which(V(gout[[i]])$degree[j]==uvldf[,1]),2] 
#       }
#     }
#     
#     if (ecount(gout[[i]]) > 0) {
#       for (k in 1:ecount(gout[[i]])) {
#         E(gout[[i]])$edgewidth[k] <- uwdf[which(E(gout[[i]])$weight[k]==
#                                                   uwdf[,1]),2]
#         E(gout[[i]])$edgelabelcex[k] <- ueldf[which(E(gout[[i]])$weight[k]==
#                                                       ueldf[,1]),2] 
#       }
#     }
#   }# end universal size attributes #########################
#   
#   # begin graph attributes
#   png(paste(attrib,"_multi",savename,".png",sep=""),width = png.width,height = png.height,units = "in",res = png.res)
#   par(mar=c(.05,.05,2.2,.05))
#   par(mfrow=c(n,3))
#   
#   for (i in 1:nn) {
#     sub <- gout[[i]]
#     pdnamei <- pdname[i]
#     territory <- unique(V(sub)$territory)[1]
#     #
#     V(sub)$color <- "dark blue"
#     V(sub)[V(sub)$degdiff >= 0]$color <- "red"
#     
#     edgequant <- quantile(uw,probs = c(0,.25,.5,.75))
#     for (i in 1:length(edgequant)) {
#       E(sub)[E(sub)$weight > edgequant[i]]$color <- gray.colors(n=length(edgequant), start = 0.70, end = 0.2, gamma = 3.5,alpha = .4)[i]
#     }
#     
#     sub <- induced.subgraph(sub,V(sub)[!is.na(V(sub)$degdiff)
#                                        & !is.nan(V(sub)$degdiff)])
#     sub <- induced.subgraph(sub,V(sub)[!is.na(V(sub)$degree)
#                                        & !is.nan(V(sub)$degree)])
#     sub <- delete.vertices(sub,
#                            V(sub)[which( 
#                              !(V(sub)$name %in% get.edgelist(sub)) ) ]
#     )
#     
#     if (vcount(sub) > 0) {
#       
#       center <- which.max(degree(sub))
#       order <- order(V(sub)$name)
#       
#       plot.igraph(sub,
#                   layout=layout.star(graph = sub,
#                                      center = center,
#                                      order = order),
#                   vertex.size=V(sub)$vertexsize,
#                   vertex.shape='circle',
#                   vertex.label=V(sub)$ipcfnum,
#                   vertex.label.cex=V(sub)$vertexlabelcex,
#                   vertex.label.color='white',
#                   vertex.color=V(sub)$color,
#                   vertex.label.font=7,
#                   edge.arrow.size=map(E(sub)$weight,c(.01,.5)),
#                   edge.width=E(sub)$edgewidth,
#                   edge.label=E(sub)$weight,
#                   edge.label.cex=E(sub)$edgelabelcex,
#                   edge.label.color='black',
#                   edge.color=E(sub)$color,
#                   main=paste("Territory of ",territory,
#                              "\nPeriod: ",pdnamei,sep="")
#       )
#       
#     } else {  
#       #create a plot of nothing to use position and add only title
#       plot(x=0,xlab = NA,ylab = NA,axes = F,
#            col='white',
#            frame.plot = F,
#            main=paste("Territory of ",territory,
#                       "\nPeriod: ",pdnamei,sep=""))
#     }
#   } # end png save
#   dev.off()
#   return(list(graphs=gout,namedf=namedf) )
# } #end function