#' Print methods for ndr objects
#'
#' Displays dimension names, shapes, coordinate info, and attributes
#' without loading or touching the underlying data.
#'
#' @name print-methods
NULL


#' @export
method(print, Variable) <- function(x, ...) {
  s <- shape(x)
  dtype <- typeof(x@data)

  if (length(s) == 0L) {
    cat(sprintf("<Variable> scalar %s\n", dtype))
    cat(sprintf("  value: %s\n", format(x@data[1])))
  } else {
    shape_str <- paste(sprintf("%s: %d", names(s), s), collapse = ", ")
    cat(sprintf("<Variable> (%s) %s\n", shape_str, dtype))
  }

  if (length(x@attrs) > 0L) {
    cat("  Attributes:\n")
    for (nm in names(x@attrs)) {
      val <- x@attrs[[nm]]
      cat(sprintf("    %s: %s\n", nm, format_attr_value(val)))
    }
  }

  invisible(x)
}


#' @export
method(print, ImplicitCoord) <- function(x, ...) {
  end_val <- x@offset + (x@n - 1L) * x@step
  cat(sprintf(
    "<ImplicitCoord> '%s' [%g, %g, ..., %g] (n=%d, step=%g)\n",
    x@dimension, x@offset, x@offset + x@step, end_val, x@n, x@step
  ))
  invisible(x)
}


#' @export
method(print, ExplicitCoord) <- function(x, ...) {
  n <- length(x@values)
  cls <- class(x@values)[1]
  if (n <= 4L) {
    vals <- paste(format(x@values), collapse = ", ")
  } else {
    vals <- paste(
      c(format(x@values[1:2]), "...", format(x@values[(n-1):n])),
      collapse = ", "
    )
  }
  cat(sprintf("<ExplicitCoord> '%s' %s[%d]: %s\n", x@dimension, cls, n, vals))
  invisible(x)
}


#' @export
method(print, DataArray) <- function(x, ...) {
  s <- shape(x@variable)
  dtype <- typeof(x@variable@data)
  nm <- if (length(x@name) > 0L) x@name else "(unnamed)"

  cat(sprintf("<DataArray> '%s'\n", nm))

  # dimensions
  if (length(s) > 0L) {
    shape_str <- paste(sprintf("%s: %d", names(s), s), collapse = ", ")
    cat(sprintf("  Dimensions:  (%s)\n", shape_str))
  }

  # coordinates
  if (length(x@coords) > 0L) {
    cat("  Coordinates:\n")
    for (cnm in names(x@coords)) {
      coord <- x@coords[[cnm]]
      cat(sprintf("    * %s  %s\n", cnm, format_coord_summary(coord)))
    }
  }

  # data type + size
  nbytes <- as.numeric(length(x@variable)) * switch(dtype,
    double = 8, integer = 4, logical = 4, complex = 16, 8
  )
  cat(sprintf("  dtype: %s  (%s)\n", dtype, format_bytes(nbytes)))

  # attrs
  if (length(x@variable@attrs) > 0L) {
    cat("  Attributes:\n")
    for (anm in names(x@variable@attrs)) {
      cat(sprintf("    %s: %s\n", anm, format_attr_value(x@variable@attrs[[anm]])))
    }
  }

  invisible(x)
}


#' @export
method(print, LazyDataArray) <- function(x, ...) {
  nm <- if (length(x@name) > 0L) x@name else "(unnamed)"

  cat(sprintf("<LazyDataArray> '%s'  [not loaded]\n", nm))

  # Dimensions with selection info
  if (length(x@dims) > 0L) {
    cat("  Dimensions:\n")
    for (i in seq_along(x@dims)) {
      dn <- x@dims[i]
      full_size <- x@dim_sizes[i]
      sel <- x@.selection[[dn]]
      if (is.null(sel)) {
        cat(sprintf("    * %s: %d\n", dn, full_size))
      } else if (length(sel) == 1L) {
        cat(sprintf("    * %s: %d -> [%d] (scalar, will drop)\n",
                    dn, full_size, sel))
      } else {
        cat(sprintf("    * %s: %d -> [%d:%d] (%d selected)\n",
                    dn, full_size, min(sel), max(sel), length(sel)))
      }
    }
  }

  # Coordinates
  if (length(x@coords) > 0L) {
    cat("  Coordinates:\n")
    for (cnm in names(x@coords)) {
      coord <- x@coords[[cnm]]
      cat(sprintf("    * %s  %s\n", cnm, format_coord_summary(coord)))
    }
  }

  # Estimated size
  selected_sizes <- vapply(seq_along(x@dims), function(i) {
    sel <- x@.selection[[x@dims[i]]]
    if (is.null(sel)) x@dim_sizes[i] else length(sel)
  }, integer(1L))
  nbytes <- prod(as.numeric(selected_sizes)) * 8
  cat(sprintf("  Estimated size: %s\n", format_bytes(nbytes)))

  # Backend info
  cat(sprintf("  Backend: %s\n", x@.backend$dsn))

  # Attrs
  if (length(x@attrs) > 0L) {
    cat("  Attributes:\n")
    for (anm in names(x@attrs)) {
      cat(sprintf("    %s: %s\n", anm, format_attr_value(x@attrs[[anm]])))
    }
  }

  cat("  Use collect() to materialise data.\n")
  invisible(x)
}


#' @export
method(print, Dataset) <- function(x, ...) {
  cat("<Dataset>\n")

  # all dims
  dims <- ds_dims(x)
  if (length(dims) > 0L) {
    dim_str <- paste(sprintf("%s: %d", names(dims), dims), collapse = ", ")
    cat(sprintf("  Dimensions:  (%s)\n", dim_str))
  }

  # coords
  if (length(x@coords) > 0L) {
    cat("  Coordinates:\n")
    for (cnm in names(x@coords)) {
      coord <- x@coords[[cnm]]
      cat(sprintf("    * %s  %s\n", cnm, format_coord_summary(coord)))
    }
  }

  # data variables (loaded)
  has_loaded <- length(x@data_vars) > 0L
  be <- x@.backend
  has_lazy <- !is.null(be) && length(be$schemas) > 0L

  if (has_loaded || has_lazy) {
    cat("  Data variables:\n")
    # loaded vars
    for (vnm in names(x@data_vars)) {
      v <- x@data_vars[[vnm]]
      s <- shape(v)
      dtype <- typeof(v@data)
      dim_str <- paste(names(s), collapse = ", ")
      cat(sprintf("    %-20s (%s) %s\n", vnm, dim_str, dtype))
    }
    # lazy vars
    if (has_lazy) {
      for (vnm in names(be$schemas)) {
        if (vnm %in% names(x@data_vars)) next  # already shown
        sc <- be$schemas[[vnm]]
        dim_str <- paste(sc$dim_names, collapse = ", ")
        size_str <- format_bytes(prod(as.numeric(sc$dim_sizes)) * 8)
        cat(sprintf("    %-20s (%s) [not loaded] ~%s\n", vnm, dim_str, size_str))
      }
    }
  }

  # attrs
  if (length(x@attrs) > 0L) {
    cat("  Attributes:\n")
    for (anm in names(x@attrs)) {
      cat(sprintf("    %s: %s\n", anm, format_attr_value(x@attrs[[anm]])))
    }
  }

  invisible(x)
}


# --- helpers ---

format_coord_summary <- function(coord) {
  if (S7_inherits(coord, ImplicitCoord)) {
    end_val <- coord@offset + (coord@n - 1L) * coord@step
    sprintf("(%s) %g to %g", coord@dimension, coord@offset, end_val)
  } else {
    n <- coord_length(coord)
    vals <- coord@values
    cls <- class(vals)[1]
    if (n <= 3L) {
      sprintf("(%s) %s [%s]", coord@dimension, cls, paste(format(vals), collapse=", "))
    } else {
      sprintf("(%s) %s %s ... %s", coord@dimension, cls, format(vals[1]), format(vals[n]))
    }
  }
}

format_attr_value <- function(val) {
  if (is.character(val) && length(val) == 1L) return(val)
  if (length(val) == 1L) return(format(val))
  if (length(val) <= 5L) return(paste(format(val), collapse = ", "))
  paste0(paste(format(val[1:3]), collapse = ", "), ", ... (", length(val), " values)")
}

format_bytes <- function(nbytes) {
  if (nbytes < 1024) return(sprintf("%d B", nbytes))
  if (nbytes < 1024^2) return(sprintf("%.1f kB", nbytes / 1024))
  if (nbytes < 1024^3) return(sprintf("%.1f MB", nbytes / 1024^2))
  sprintf("%.1f GB", nbytes / 1024^3)
}
