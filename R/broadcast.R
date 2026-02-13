#' Dimension-aware broadcasting
#'
#' The core engine that makes `temperature * land_mask` work when the two
#' variables have different (but compatible) dimensions. Broadcasting aligns
#' by dimension name, inserts size-1 dimensions where needed, and expands
#' arrays to a common shape.
#'
#' @name broadcasting
NULL

#' Find the common (broadcast) shape for two Variables
#'
#' Takes two Variables and returns the output dimension names and shape.
#' Rules:
#' 1. Output dims = union of input dims (preserving order: left then new from right)
#' 2. For shared dims, sizes must match
#' 3. For dims present in only one input, the other is treated as size-1
#'
#' @param a,b Variable objects
#' @return A named integer vector: the broadcast shape
#' @keywords internal
broadcast_shape <- function(a, b) {
  sa <- shape(a)
  sb <- shape(b)
  da <- names(sa)
  db <- names(sb)

  # output dims: left's order, then any new dims from right
  out_dims <- union(da, db)
  out_shape <- stats::setNames(integer(length(out_dims)), out_dims)

  for (d in out_dims) {
    in_a <- d %in% da
    in_b <- d %in% db
    if (in_a && in_b) {
      if (sa[d] != sb[d]) {
        stop(sprintf(
          "dimension '%s' has size %d in left operand and %d in right — cannot broadcast",
          d, sa[d], sb[d]
        ))
      }
      out_shape[d] <- sa[d]
    } else if (in_a) {
      out_shape[d] <- sa[d]
    } else {
      out_shape[d] <- sb[d]
    }
  }
  out_shape
}


#' Align a Variable's data to a target set of named dimensions
#'
#' Permutes existing dims to match target order and inserts size-1 dims
#' where the Variable is missing a dimension. Returns a raw R array with
#' dim matching the target (with 1s for missing dims).
#'
#' @param v A Variable
#' @param target_dims Character vector of dimension names (the output order)
#' @return An R array with dim attribute set
#' @keywords internal
align_data <- function(v, target_dims) {
  sv <- shape(v)
  dv <- names(sv)
  arr <- as.array(v)

  if (length(dv) == 0L) {
    # scalar — return with all-1 dims
    out <- array(as.vector(arr), dim = rep(1L, length(target_dims)))
    return(out)
  }

  # figure out the permutation for existing dims
  # target_dims is the output order; dv is the current order
  # only dims that exist in v need permuting; missing dims get size 1
  existing_in_target <- target_dims[target_dims %in% dv]
  perm <- match(existing_in_target, dv)

  if (!identical(perm, seq_along(perm))) {
    arr <- aperm(arr, perm)
  }

  # now arr has dims in the order they appear in target_dims (for existing dims)
  # we need to insert size-1 dims for missing ones
  out_dim <- integer(length(target_dims))
  j <- 1L
  for (i in seq_along(target_dims)) {
    if (target_dims[i] %in% dv) {
      out_dim[i] <- dim(arr)[j]
      j <- j + 1L
    } else {
      out_dim[i] <- 1L
    }
  }

  dim(arr) <- out_dim
  arr
}


#' Broadcast an array from one shape to another
#'
#' Expands size-1 dimensions by repeating values. Both shapes must have the
#' same length and names (use align_data first).
#'
#' @param data An R array
#' @param from_shape Named integer vector (current shape, may contain 1s)
#' @param to_shape Named integer vector (target shape)
#' @return An R array with dim = to_shape
#' @keywords internal
broadcast_array <- function(data, from_shape, to_shape) {
  if (identical(unname(from_shape), unname(to_shape))) return(data)

  idx <- lapply(seq_along(to_shape), function(i) {
    if (from_shape[i] == 1L && to_shape[i] > 1L) {
      rep(1L, to_shape[i])
    } else {
      seq_len(to_shape[i])
    }
  })
  do.call(`[`, c(list(data), idx, list(drop = FALSE)))
}


#' Broadcast two Variables to a common shape and apply an operation
#'
#' This is the workhorse behind arithmetic on Variables. Handles dimension
#' alignment, shape checking, broadcasting, and returns a new Variable.
#'
#' @param a,b Variable objects (or scalars)
#' @param op A binary function (e.g. `+`, `*`)
#' @return A new Variable with broadcast dimensions
#' @keywords internal
broadcast_op <- function(a, b, op) {
  # wrap bare numerics as scalar Variables
  if (!S7_inherits(a, Variable)) a <- Variable(dims = character(), data = array(a))
  if (!S7_inherits(b, Variable)) b <- Variable(dims = character(), data = array(b))

  out_shape <- broadcast_shape(a, b)
  out_dims <- names(out_shape)

  a_aligned <- align_data(a, out_dims)
  b_aligned <- align_data(b, out_dims)

  a_broad <- broadcast_array(a_aligned, dim(a_aligned), unname(out_shape))
  b_broad <- broadcast_array(b_aligned, dim(b_aligned), unname(out_shape))

  result <- op(a_broad, b_broad)
  dim(result) <- unname(out_shape)

  Variable(dims = out_dims, data = result)
}
