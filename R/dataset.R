#' Dataset: a collection of aligned Variables on shared dimensions
#'
#' A Dataset holds multiple data variables that share a common set of
#' dimensions and coordinates. The alignment constraint: any two variables
#' that share a dimension name must agree on its size.
#'
#' @param data_vars Named list of Variable objects.
#' @param coords Named list of coordinate objects (ImplicitCoord or ExplicitCoord).
#' @param attrs Named list of global metadata.
#'
#' @examples
#' lat <- ImplicitCoord(dimension = "lat", n = 180L, offset = -89.5, step = 1.0)
#' lon <- ImplicitCoord(dimension = "lon", n = 360L, offset = 0.5, step = 1.0)
#'
#' ds <- Dataset(
#'   data_vars = list(
#'     temperature = Variable(
#'       dims = c("lat", "lon"),
#'       data = matrix(rnorm(180*360), 180, 360),
#'       attrs = list(units = "K")
#'     ),
#'     pressure = Variable(
#'       dims = c("lat", "lon"),
#'       data = matrix(rnorm(180*360), 180, 360),
#'       attrs = list(units = "Pa")
#'     )
#'   ),
#'   coords = list(lat = lat, lon = lon),
#'   attrs = list(title = "Example dataset")
#' )
#' ds
#'
#' @export
Dataset <- new_class("Dataset",
  properties = list(
    data_vars = new_property(class_list, default = list()),
    coords    = new_property(class_list, default = list()),
    attrs     = new_property(class_list, default = list())
  ),
  validator = function(self) {
    # build a registry of dim -> size from all variables
    dim_sizes <- list()
    for (nm in names(self@data_vars)) {
      v <- self@data_vars[[nm]]
      if (!S7_inherits(v, Variable))
        return(sprintf("data_vars[['%s']] must be a Variable", nm))
      s <- shape(v)
      for (d in names(s)) {
        if (d %in% names(dim_sizes)) {
          if (dim_sizes[[d]] != s[d]) {
            return(sprintf(
              "dimension '%s' has size %d in variable '%s' but %d elsewhere",
              d, s[d], nm, dim_sizes[[d]]
            ))
          }
        } else {
          dim_sizes[[d]] <- s[d]
        }
      }
    }
    # check coords match dim sizes
    for (nm in names(self@coords)) {
      coord <- self@coords[[nm]]
      cdim <- coord_dim(coord)
      clen <- coord_length(coord)
      if (cdim %in% names(dim_sizes) && dim_sizes[[cdim]] != clen) {
        return(sprintf(
          "coordinate '%s' has length %d but dim '%s' has size %d in data",
          nm, clen, cdim, dim_sizes[[cdim]]
        ))
      }
    }
    NULL
  }
)


#' Extract a DataArray from a Dataset
#'
#' Use `ds$temperature` or `ds[["temperature"]]` to extract a variable
#' as a DataArray with its relevant coordinates attached.
#'
#' @param ds A Dataset
#' @param var_name Character name of the variable
#' @return A DataArray
#' @export
`$.ndr::Dataset` <- function(ds, var_name) {
  var_name <- as.character(substitute(var_name))
  extract_dataarray(ds, var_name)
}

#' @export
`[[.ndr::Dataset` <- function(ds, var_name) {
  extract_dataarray(ds, var_name)
}

extract_dataarray <- function(ds, var_name) {
  if (!var_name %in% names(ds@data_vars))
    stop(sprintf("no variable '%s' in dataset (available: %s)",
                 var_name, paste(names(ds@data_vars), collapse = ", ")))

  v <- ds@data_vars[[var_name]]
  vdims <- v@dims

  # select only coords whose dim is in this variable
  relevant_coords <- Filter(function(c) coord_dim(c) %in% vdims, ds@coords)

  DataArray(variable = v, coords = relevant_coords, name = var_name)
}


#' List dimension names and sizes across a Dataset
#' @param ds A Dataset
#' @return Named integer vector of all dimensions
#' @export
ds_dims <- function(ds) {
  dim_sizes <- integer()
  for (v in ds@data_vars) {
    s <- shape(v)
    for (d in names(s)) {
      if (!d %in% names(dim_sizes)) {
        dim_sizes[d] <- s[d]
      }
    }
  }
  dim_sizes
}
