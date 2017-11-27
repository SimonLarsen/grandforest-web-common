get_network_file <- function(id) {
  path <- switch(id, 
    biogrid    = paste0("grandforest-web-common/data/biogrid.human.entrez.tsv"),
    htri       = paste0("grandforest-web-common/data/htri.entrez.tsv"),
    iidall     = paste0("grandforest-web-common/data/iid.human.all.entrez.tsv"),
    iidexp     = paste0("grandforest-web-common/data/iid.human.exp.entrez.tsv"),
    regnetwork = paste0("grandforest-web-common/data/regnetwork.entrez.tsv")
  )
}

