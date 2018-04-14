library("igraph")
 
#-- creating folders where matrices and plots will be stored ----
if (!file.exists("Plots")){dir.create("Plots")}
 
#-- generating a list of weighted graphs with weights from surveys (make sure to fix sec.ins and vertices based on which semester I am looking at) ----
reading.graph <- function(id = 1){
    if (id %in% c(1,3)) sec.ins = "A" 
    else if (id %in% c(2,4)) sec.ins = "B" 
    dat <- list()
    for (i in 1:5){
      dat[[i]] <- na.omit(read.csv(files.list[id+2*(i-1)], header = TRUE, 
                                   colClasses = "numeric", na.strings = c("NA", "", "#N/A")))
    }
    graph_GW <- lapply(dat, graph.data.frame, directed = TRUE, 
                       vertices = sna.data[sna.data$Sec.F == sec.ins,])
}
 
#-- removing disconnected "Others" ----
remove.disconnected <- function(x){
    gr <- delete.vertices(x, degree(x) == 0 & V(x)$Status %in% c("Other"))
    
}

#-- removing instructors---- 
remove.instr <- function(x){
    gr <- delete.vertices(x, V(x)$Status %in% c("LA", "TA", "INS"))
} 

#-- modified diameter ----
mod.diameter <- function(x){
  d <- diameter(x, weights = 1/E(x)$weight)
}

#-- converting a list of igraph objects to tnet-compatible format ----
graph.tnet <- function(x){
  gr <- lapply(x, simplify)
  gr <- lapply(gr, function(g){cbind(as.data.frame(get.edgelist(g, names = FALSE)), 
                                     weight=E(g)$weight)})
  gr <-lapply(gr, function(g) g[!duplicated(g[1:2]), ])

}
