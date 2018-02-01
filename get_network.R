get_network_file <- function(id) {
  path <- switch(id, 
    biogrid    = paste0("grandforest-web-common/data/biogrid.human.entrez.rds"),
    htri       = paste0("grandforest-web-common/data/htri.entrez.rds"),
    iidall     = paste0("grandforest-web-common/data/iid.human.all.entrez.rds"),
    iidexp     = paste0("grandforest-web-common/data/iid.human.exp.entrez.rds"),
    regnetwork = paste0("grandforest-web-common/data/regnetwork.entrez.rds")
  )
}

