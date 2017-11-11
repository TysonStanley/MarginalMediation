.onAttach <- function(libname = find.package("MarginalMediation"), pkgname = "MarginalMediation") {
  packageStartupMessage("MarginalMediation 0.4.2: This is beta software.\nPlease report any bugs (t.barrett@aggiemail.usu.edu).")
}

.onLoad <- function(libname = find.package("MarginalMediation"), pkgname = "MarginalMediation"){
  if(getRversion() >= "2.15.1") {
    utils::globalVariables(".")
  }
  invisible()
}