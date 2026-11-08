.onLoad <- function(libname, pkgname) {
  S7::methods_register()
  registerS3method("print", "nd_plan", print_nd_plan, envir = baseenv())
  registerS3method("print", "nd_plan_dataset", print_nd_plan_dataset,
                   envir = baseenv())
}
