combineList <- function(x, newX){
    for(i in seq_along(newX)){
        x[[names(newX)[i]]] <- newX[[i]]
    }
    x
}

packPackages <- function(x){
    encodedX <- vapply(x, function(i) URLencode(i, reserved = TRUE), character(1))
    paste0(encodedX, collapse = ",")
}

getSysPackages <- function(cluster){
    workerContainer <- .getWorkerContainer(cluster)
    workerContainer$sysPackages
}
setSysPackages <- function(cluster, packages){
    workerContainer <- .getWorkerContainer(cluster)
    workerContainer$sysPackages <- unique(packages)
    workerContainer$sysPackages
}
addSysPackages  <- function(cluster, packages){
    workerContainer <- .getWorkerContainer(cluster)
    workerContainer$sysPackages <- unique(c(packages,workerContainer$sysPackages))
    workerContainer$sysPackages
}

getRPackages <- function(cluster){
    workerContainer <- .getWorkerContainer(cluster)
    workerContainer$RPackages
}
setRPackages <- function(cluster, packages){
    workerContainer <- .getWorkerContainer(cluster)
    workerContainer$RPackages <- unique(packages)
    workerContainer$RPackages
}
addRPackages  <- function(cluster, packages){
    workerContainer <- .getWorkerContainer(cluster)
    workerContainer$RPackages <- unique(c(packages,workerContainer$RPackages))
    workerContainer$RPackages
}
