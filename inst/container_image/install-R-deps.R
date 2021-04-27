RPackages <- Sys.getenv("RPackages")
if(RPackages!=""){
    pkgs <- strsplit(RPackages,",")[[1]]
    pkgs <- sapply(pkgs,function(i) URLdecode(i))
    message("Installing the package: ", paste0(pkgs, sep = ", "))
    if (length(pkgs)!=0){
        AnVIL::install(pkgs, ask = FALSE)
        todo <- sapply(pkgs, function(x)!requireNamespace(x, quietly = TRUE))
        BiocManager::install(pkgs[todo], update = FALSE, upgrade = "never")
    }
}