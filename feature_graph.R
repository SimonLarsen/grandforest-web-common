feature_graph <- function(D, edges, features, labels, groups) {
  nodes <- data.frame(id=features, label=labels)
  legend <- NULL
  footer <- NULL
  group_levels <- levels(as.factor(groups))
  D <- D[,features,with=FALSE]

  if(length(group_levels) == 2) {
    means <- lapply(split(D, groups), colMeans)
    col_ramp <- circlize::colorRamp2(c(-1,0,1), c("magenta","white","green"))
    colors <- col_ramp(means[[1]] - means[[2]])
    nodes$color.background <- colors
    nodes$color.border <- "black"
    nodes$color.highlight.background <- colors
    nodes$color.highlight.border <- "black"
    
    footer <- paste0("Node color is mean(", group_levels[1], ") - mean(", group_levels[2], ")")
    legend_values <- c(-1, -0.5, 0, 0.5, 1.0)
    legend_labels <- c("-1.0 ", "-0.5 ", " 0.0 ", " 0.5 ", " 1.0 ")
    legend_nodes <- data.frame(id=1:5, label=legend_labels, color.background=col_ramp(legend_values), color.border="black", shape="box")
  }
  
  edges <- subset(edges, from %in% features & to %in% features)

  library(visNetwork)
  net <- visNetwork(nodes, edges, footer=footer) %>%
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
