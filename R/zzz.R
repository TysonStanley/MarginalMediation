.onAttach <- function(libname = find.package("MarginalMediation"), pkgname = "MarginalMediation") {
  packageStartupMessage("MarginalMediation 0.5.1: Please report any bugs (t.barrett@aggiemail.usu.edu).")
}

.onLoad <- function(libname = find.package("MarginalMediation"), pkgname = "MarginalMediation"){
  if(getRversion() >= "2.15.1") {
    utils::globalVariables(".")
  }
  invisible()
}