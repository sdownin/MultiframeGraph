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
    if(all(x>=1 | x==0)) {
      lx <- log(x)
      maxlx <- max(lx[lx<Inf & lx>-Inf])
      scale <- lx / maxlx
      y <- m + range*scale
      y[is.na(y)|is.nan(y)|y==-Inf|y==Inf] <- m
    } else {
    #augment x proportions to > 1 yielding values suitable for log transform
    scalepos <- (scale+1)/min(scale+1)
    #account for log transform while still proportional to elements of x 
    # by normalizing with the log of the maximum
    scale <- log(scalepos) / log(max(scalepos))
    y <- m + range*scale
    }
  } else {
  y <- m + range*scale
  }
  return(y)
}




#' multicontracted
#' 
#' Multiframe plot of igraph objects
#' 
#' @param graphlist  a list of igraph objects (i.e., graphs)
#' @param attrib  character name of igraph vector attribute for coloring, sizing
#' @param pdbreak a vector of the numeric values to divide periods
#' @param vertex.size.attrib  character {'degree','indeg','outdeg','degdiff'}
#' @param veretex.color.attrib  character {'degree','indeg','outdeg','degdiff',
#' <other attribute>}
#' @param color.attrib character specifying attribute for vertex color scale
#' @param vertex.label.minmax numeric vector of length 2: minmax vertex label
#' @param vertex.size.minmax numeric vector of length 2: minmax vertex size
#' @param edge.width.minmax numeric vector of length 2: minmax edge width
#' @param edge.label.minmax numeric vector of length 2: minmax edge label
#' @param savepng logical saveplot as png
#' @param savename  character name to save plot to working directory
#' @param png.width numeric width in inches of png output
#' @param png.height numeric height in inches of png output
#' @param png.res numeric resolution of png out
#' @return list of length two: 1. gout: subgraphs by time period for each 
#' igraph object in list; 2. namedf: df of attribute names and values for 
#' the subgraph nodes.
#' @details 
#' This function creates a multiframe of subgraphs from a list of 
#' igraph objects. Each graph in the list is a row and each time period is 
#' a column. Each igraph object in the list must have the same vertex 
#' attribute names. Each igraph object in the list must have the time 
#' period attribute called "period", a numeric or interger attribute. 
#' Vertex coloring is automatically generated from rainbow pallette. 
#' @author Stephen Downing


multicontracted <- function(graphlist,
                            attrib,
                            pdbreak,
                            vertex.size.attrib='degree', 
                            #{'degree','indeg','outdeg','degdiff'}
                            vertex.color.attrib='degdiff', 
                            #{'degree','indeg','outdeg','degdiff'
                            #<other attribute factors>}
                            color.attrib='degdiff',
                            vertex.label.minmax=c(.2,.5),
                            vertex.size.minmax=c(20,35),
                            edge.width.minmax=c(3,6),
                            edge.label.minmax=c(.01,.01),
                            savepng=F,
                            savename="mutiContractedGraph",
                            png.width=15,
                            png.height=12,
                            png.res=500
) {
  m <- length(graphlist)    #number of rows of multiplot
  n <- length(pdbreak) - 1  #number of columns of multiplot
  pdbreak <- pdbreak[order(pdbreak)] #start first breakpoint
  
  # vertex names
  name <- c()
  for (i in 1:m){
    name <- c(name,get.vertex.attribute(graphlist[[i]],attrib))
  }
  name <- unique(name)
  name <- name[order(name)]
  namedf <- data.frame(name=name,ID=seq_len(length(name)) )
  #numbered list of attributes to be vertex IDs
  namedf[,1] <- as.character(namedf[,1])
  
  # time period names
  pdname <- c()
  for (i in 1:(length(pdbreak)-1)) {
    pdname <- c(pdname,paste(pdbreak[1],"-",pdbreak[i+1],sep=""))
  }
  pdname <- rep(pdname,m) # repeat for number of graphs in list
  
  ############################################################################
  #loop of territory (rows of output multiframe plot) ########################
  gout <- list()
  for (i in seq_len(m))  { 
    territory <- names(graphlist)[i]
    g <- graphlist[[i]]
    V(g)$territory <- territory
    
    attriblen <- length(get.vertex.attribute(g,attrib))
    for (l in seq_len(nrow(namedf))) {
      V(g)[which(get.vertex.attribute(g,attrib)==
                   namedf[l,1])]$name <- namedf[l,2]
    }
    
    ######################################
    #subloop by periods#####################
    gpdlist <- list()
    for (j in seq_len(n)) {
      #subgraph of only vertices before (deleting >=) period breakpoint
      #which are not NA or NaN
      gpd <- delete.vertices(graph = g, v = V(g)[V(g)$period >= pdbreak[j+1] |
                                                   is.na(V(g)$period) |
                                                   is.nan(V(g)$period)])
      #assign degree values, loops default TRUE
      V(gpd)$degree <- degree(gpd, mode = 'total')
      V(gpd)$indeg <- degree(gpd,mode = 'in')
      V(gpd)$outdeg <- degree(gpd,mode = 'out')
      V(gpd)$degdiff <- V(gpd)$indeg - V(gpd)$outdeg
      #vector of vertex names included in this subgraph
      gpdname <- unique(get.vertex.attribute(gpd,attrib))
      gdpname <- gpdname[order(gpdname)]
      
      #index vector of vectices belonging to which attribute value
      gpdvl <- c()
      for (k in seq_len(vcount(gpd)) ) {
        gpdvl[k] <- which(get.vertex.attribute(gpd,name = attrib,
                                               index = k) == gpdname) 
      }
      
      #contract graph
      con <- contract.vertices(graph = gpd,
                               mapping = gpdvl,
                               vertex.attr.comb=list(territory='first',
                                                     period='mean',
                                                     degree='sum',
                                                     indeg='sum',
                                                     outdeg='sum',
                                                     degdiff='mean') )
      V(con)$name <- gpdname
      
      #remove unused vertex attributes
      z <- c('territory','degree','degdiff','indeg','outdeg','id','period','name')
      va <- list.vertex.attributes(con)
      va <- va[!(va %in% z)]
      for (w in seq_len(length(va))) {
        con <- remove.vertex.attribute(graph = con, name = va[w])
      }
      
      #simplify by removing duplicate edges and loops, summing into new
      #edge attribute, weight
      con <- simplify(graph = con, remove.multiple = T,
                      remove.loops = T,
                      edge.attr.comb = list(weight='sum'))
      
      #assign graph to next graph ouput list element
      gout[[((i-1)*n)+j]] <- con
      names(gout[[((i-1)*n)+j]]) <- paste(territory,"_",j,sep="")
      
    } # end period loop j
    
  } # end territory loop i
  ###################################################################
  
  #make universal size attributes ##################################
  mn <- m*n  # total plot frames
  uvl <- c() #unique vertex label
  uvs <- c() #unique vertex size
  uw <-  c() #unique weights of edges
  for (i in seq_len(mn)) {
    uvl <- c(uvl,get.vertex.attribute(graph = gout[[i]],
                                      name = vertex.size.attrib))
    uvs <- c(uvl,get.vertex.attribute(graph = gout[[i]],
                                      name = vertex.size.attrib))
    uw <- c(uw,E(gout[[i]])$weight)
  }
  uvl <- unique(uvl)
  uvldf <- data.frame(uvl=uvl,map=map(uvl,vertex.label.minmax,log = T))
  uvs <- unique(uvs)
  uvsdf <- data.frame(uvs=uvs,map=map(uvs,vertex.size.minmax,log = T))
  uw <- unique(uw)
  uwdf <- data.frame(uw=uw,map=map(uw,edge.width.minmax,log = T))
  ueldf <- data.frame(uw=uw,map=map(uw,edge.label.minmax,log = T))
  #
  vertexsize <- c()
  vertexlabelcex <- c()
  edgewidth <- c()
  edgelabelcex <- c()
  for (i in seq_len(mn)) {
    if (vcount(gout[[i]]) > 0) {
      for (j in seq_len(vcount(gout[[i]])) ) {
        V(gout[[i]])$vertexsize[j] <- uvsdf[which(get.vertex.attribute(gout[[i]],
                                                                      'degree')[j]
                                            == uvsdf[,1]),2] 
        V(gout[[i]])$vertexlabelcex[j] <- uvldf[which(get.vertex.attribute(gout[[i]],
                                                                           'degree')[j]
                                                      == uvldf[,1]),2] 
      }
    }
    
    if (ecount(gout[[i]]) > 0) {
      for (k in seq_len(ecount(gout[[i]])) ) {
        E(gout[[i]])$edgewidth[k] <- uwdf[which(E(gout[[i]])$weight[k]==
                                                  uwdf[,1]),2]
        E(gout[[i]])$edgelabelcex[k] <- ueldf[which(E(gout[[i]])$weight[k]==
                                                      ueldf[,1]),2] 
      }
    }
  }# end universal size attributes #########################
  
  # begin graph attributes  ########################
  center <- which.max(degree(sub))
  order <- order(V(sub)$name)
  layout <- layout.star(graph = sub,
                        center = center,
                        order = order)
  
  if (savepng) {
    png(paste(savename,"_",attrib,".png",sep=""),
        width = png.width, height = png.height, units = "in", res = png.res)
    par(mar=c(.05,.05,2.2,.05))
    par(mfrow=c(m,n))
  } else {
    par(mar=c(.05,.05,2.2,.05))
    par(mfrow=c(m,n))
  }
    for ( i in seq_len(mn)) {
      sub <- gout[[i]]
      pdnamei <- pdname[i]
      territoryi <- unique(V(sub)$territory)[1]
      
      # color
      if (color.attrib == 'degdiff') {
        V(sub)$color <- "dark blue"
        V(sub)[V(sub)$degdiff >= 0]$color <- "red"
      } else if (color.attrib == 'degree'|
                   color.attrib == 'indeg' |
                   color.attrib == 'outdeg') {
        cval <- get.vertex.attribute(graph = sub,name = color.attrib)
        V(sub)$color <- heat.colors(n = length(cval),alpha = .6)
      } else {
        cval <- get.vertex.attribute(graph = sub,name = color.attrib)
        V(sub)$color <- rainbow(n = length(cval),alpha = .6)
      }
      
      edgequant <- quantile(uw,probs = c(0,.25,.5,.75))
      for (i in 1:length(edgequant)) {
        E(sub)[E(sub)$weight > edgequant[i]]$color <- gray.colors(n=length(edgequant), start = 0.70, end = 0.2, gamma = 3.5,alpha = .4)[i]
      }
      

      sub <- induced.subgraph(sub,V(sub)[!is.na(V(sub)$degree) | 
                                                  !is.nan(V(sub)$degree)])
      sub <- delete.vertices(sub,
                             V(sub)[which( 
                               !(V(sub)$name %in% get.edgelist(sub)) ) ]
      )
      
      if (vcount(sub) > 0) {
        plot.igraph(sub,
                    layout=layout,
                    vertex.size=V(sub)$vertexsize,
                    vertex.shape='circle',
                    vertex.label=V(sub)$name,
                    vertex.label.cex=V(sub)$vertexlabelcex,
                    vertex.label.color='white',
                    vertex.color=V(sub)$color,
                    vertex.label.font=7,
                    edge.arrow.size=map(E(sub)$weight,c(.01,.05)),
                    edge.width=E(sub)$edgewidth,
                    edge.label=E(sub)$weight,
                    edge.label.cex=E(sub)$edgelabelcex,
                    edge.label.color='black',
                    edge.color=E(sub)$color,
                    main=paste("Territory of ",territoryi,
                               "\nPeriod: ",pdnamei,sep="")
        )
      } else {  
        #create a plot of nothing to use position and add only title
        plot(x=0,xlab = NA,ylab = NA,axes = F,
             col='white',
             frame.plot = F,
             main=paste("Territory of ",territoryi,
                        "\nPeriod: ",pdnamei,sep=""))
      }
    if (savepng) {dev.off()}
    }
 
  return(list(graphs=gout,namedf=namedf) )
} #end function