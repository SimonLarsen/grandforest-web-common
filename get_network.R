get_network_file <- function(id) {
  prefix <- "grandforest-web-common/data/networks/"
  
  path <- switch(id, 
    human.iid.exp    = paste0(prefix, "human.iid.exp.rds"),
    human.iid.all    = paste0(prefix, "human.iid.all.rds"),
    human.biogrid    = paste0(prefix, "human.biogrid.rds"),
    human.htri       = paste0(prefix, "human.htri.rds"),
    human.regnetwork = paste0(prefix, "human.regnetwork.rds"),
    
    mouse.iid.exp    = paste0(prefix, "mouse.iid.exp.rds"),
    mouse.iid.all    = paste0(prefix, "mouse.iid.all.rds"),
    mouse.biogrid    = paste0(prefix, "mouse.biogrid.rds"),
    mouse.regnetwork = paste0(prefix, "mouse.regnetwork.rds")
  )
}

get_network_options <- function(species) {
  if(species == "human") {
    list(
      "IID, experimental only (2018-05)" = "human.iid.exp",
      "IID, incl. predictions (2018-05)" = "human.iid.all",
      "BioGRID (3.4.160)" = "human.biogrid",
      "HTRIdb (2018-05)" = "human.htri",
      "RegNetwork (2018-05)" = "human.regnetwork"
    )
  } else if(species == "mouse") {
    list(
      "IID, experimental only (2018-05)" = "mouse.iid.exp",
      "IID, incl. predictions (2018-05)" = "mouse.iid.all",
      "BioGRID (3.4.160)" = "mouse.biogrid",
      "RegNetwork (2018-05)" = "mouse.regnetwork"
    )
  }
}
