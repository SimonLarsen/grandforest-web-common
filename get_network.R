get_network_file <- function(id) {
  prefix <- "grandforest-web-common/data/networks/"
  
  path <- switch(id, 
    human.biogrid    = paste0(prefix, "human.biogrid.rds"),
    human.htri       = paste0(prefix, "human.htri.rds"),
    human.iid.all    = paste0(prefix, "human.iid.all.rds"),
    human.iid.exp    = paste0(prefix, "human.iid.exp.rds"),
    human.regnetwork = paste0(prefix, "human.regnetwork.rds"),
    
    mouse.iid.all = paste0(prefix, "mouse.iid.all.rds"),
    mouse.iid.exp = paste0(prefix, "mouse.iid.exp.rds")
  )
}

get_network_options <- function(species) {
  if(species == "human") {
    list(
      "IID, experimental only" = "human.iid.exp",
      "IID, incl. predictions" = "human.iid.all",
      "RegNetwork" = "human.regnetwork",
      "BioGRID" = "human.biogrid",
      "HTRIdb" = "human.htri"
    )
  } else if(species == "mouse") {
    list(
      "IID, experimental only" = "mouse.iid.exp",
      "IID, incl. predictions" = "mouse.iid.all"
    )
  }
}
