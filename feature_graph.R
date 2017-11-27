library(visNetwork)
library(circlize)

feature_graph <- function(D, edges, features, labels, groups) {
  nodes <- data.frame(id=features, label=labels, color.border="black")
  legend <- NULL
  footer <- NULL
  group_levels <- levels(as.factor(groups))
  D <- D[,features,with=FALSE]

  if(length(group_levels) == 2) {
    means <- lapply(split(D, groups), colMeans)
    colors <- colorRamp2(c(-1,0,1), c("magenta","white","green"))(means[[1]] - means[[2]])
    nodes$color.background <- colors
    
    footer <- paste0("Node color is mean(", group_levels[1], ") - mean(", group_levels[2], ")")
    legend_nodes <- data.frame(id=1:3, label=c(-1, 0, 1), shape="ellipse", color.background=c("magenta", "white", "green"), color.border="black")
  }
  
  edges <- subset(edges, from %in% features & to %in% features)

  net <- visNetwork(nodes, edges, footer=footer) %>%
    visPhysics(solver = "forceAtlas2Based") %>%
    visNodes(shape = "ellipse") %>%
    visEdges(smooth = FALSE, color = list(color = "lightblue", highlight="black")) %>%
    visIgraphLayout()
  
  if(length(group_levels) == 2) {
    net <- net %>% visLegend(
      addNodes = legend_nodes,
      useGroups = FALSE,
      position = "right",
      stepY = 50,
      main=list(text="Mean difference", style="font-weight:bold;font-size:14px;text-align:center")
    )
  }
  
  return(net)
}