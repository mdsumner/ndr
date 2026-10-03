# Lazy data in a Variable

A Variable's `data` can be a lazy chunked array from the altarr package:
an ordinary double, integer or logical vector with a `dim` attribute
that reads its values chunk by chunk only when they are asked for.
Variables read by
[`open_dataset()`](https://mdsumner.github.io/ndr/reference/open_dataset.md)
hold such arrays, and any altarr array can be used directly:

## Details

    v <- Variable(dims = c("lon", "lat", "time"), data = altarr::altarr(...))

Every Variable, DataArray and Dataset method accepts lazy data:

- [`dim()`](https://rdrr.io/r/base/dim.html),
  [`shape()`](https://mdsumner.github.io/ndr/reference/ndim.md),
  printing and coordinate work read nothing.

- [`isel()`](https://mdsumner.github.io/ndr/reference/indexing.md) and
  [`sel()`](https://mdsumner.github.io/ndr/reference/indexing.md) read
  nothing: they return a Variable whose data is a lazy view of the
  selection, still backed by the same chunks.

- Reductions
  ([`nd_mean()`](https://mdsumner.github.io/ndr/reference/reductions.md)
  and friends) stream the array in blocks aligned to its chunks, one
  planned read per block, and never hold more than one block plus the
  result in memory.

- Arithmetic, [`as.array()`](https://rdrr.io/r/base/array.html),
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) and
  [`collect()`](https://mdsumner.github.io/ndr/reference/collect.md)
  read the (selected) values into memory with one planned read.

The block size for reductions is `getOption("ndr.block_values")` values
(default `2^22`); the chunk cache and fetch batching are altarr's
options (see
[`?altarr::altarr_contract`](https://rdrr.io/pkg/altarr/man/altarr_contract.html)).
