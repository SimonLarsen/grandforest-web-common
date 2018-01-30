library(DOSE)
library(clusterProfiler)
library(ReactomePA)

gene_set_enrichment <- function(genes, universe, type, pvalueCutoff, qvalueCutoff) {
  if(type == "gobp") {
    out <- enrichGO(genes, universe=universe, OrgDb="org.Hs.eg.db", ont="BP", pvalueCutoff=pvalueCutoff, qvalueCutoff=qvalueCutoff)
  } else if(type == "gomf") {
    out <- enrichGO(genes, universe=universe, OrgDb="org.Hs.eg.db", ont="MF", pvalueCutoff=pvalueCutoff, qvalueCutoff=qvalueCutoff)
  } else if(type == "gocc") {
    out <- enrichGO(genes, universe=universe, OrgDb="org.Hs.eg.db", ont="CC", pvalueCutoff=pvalueCutoff, qvalueCutoff=qvalueCutoff)
  } else if(type == "reactome") {
    out <- enrichPathway(genes, universe=universe, organism="human", pvalueCutoff=pvalueCutoff, qvalueCutoff=qvalueCutoff)
  } else if(type == "do") {
    out <- enrichDO(genes, universe=universe, pvalueCutoff=pvalueCutoff, qvalueCutoff=qvalueCutoff)
  } else if(type == "disgenet") {
    out <- enrichDGN(genes, universe=universe, pvalueCutoff=pvalueCutoff, qvalueCutoff=qvalueCutoff)
  }
  return(out)
}

gene_set_enrichment_types <- function() {
  list(
    "GO Biological process" = "gobp",
    "GO Molecular function" = "gomf",
    "GO Cellular component" = "gocc",
    "Reactome pathways" = "reactome",
    "Disease Ontology" = "do",
    "DisGeNET" = "disgenet"
  )
}

get_gene_set_enrichment_links <- function(D, type) {
  make_links <- function(ids, type) {
    link <- switch(type,
      amigo = "http://amigo.geneontology.org/amigo/term/%s",
      reactome = "https://reactome.org/content/detail/%s",
      do = "http://disease-ontology.org/term/%s"
    )
    fmt <- sprintf("<a href=\"%s\" target=_blank>%%s</a>", link)
    sapply(ids, function(x) if(is.na(x)) NA else sprintf(fmt, x, x))
  }
  
  if(type == "gobp" || type == "gomf" || type == "gocc") {
    D[[1]] <- make_links(D[[1]], "amigo")
  } else if(type == "reactome") {
    D[[1]] <- make_links(D[[1]], "reactome")
  } else if(type == "do") {
    D[[1]] <- make_links(D[[1]], "do")
  }
  return(D)
}
