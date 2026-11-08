ds <- open_dataset(cmems_dsn <- dsn <- 'ZARR:"/vsicurl/https://s3.waw3-1.cloudferro.com/mdl-arco-time-045/arco/SEALEVEL_GLO_PHY_L4_MY_008_047/cmems_obs-sl_glo_phy-ssh_my_allsat-l4-duacs-0.125deg_P1D_202411/timeChunked.zarr"')

ds              # <Dataset> with lazy vars, coords, attrs — no data read
names(ds)       # "adt" "sla" "err_sla" "ugosa" ...
#ds$             # tab-completes ↑

  ds$adt          # <LazyDataArray> 'adt' [not loaded]
#   longitude: 2880, latitude: 1440, time: 11688
#   Estimated size: ~370 GB
#   Use collect() to materialise data.

ds$adt |> sel(time = as.Date("2020-06-15"),
              latitude = c(-60, -30))
# still LazyDataArray — selection accumulated, no read

da <- ds$adt |> sel(time = as.Date("2020-06-15")) |> collect()
ximage::ximage(da@variable@data)

library(raadtools)
r <- as.array(read_adt_daily("2020-06-16", lon180 = T))
ximage::ximage(r[,,1])
ximage::ximage(t(da@variable@data[,ncol(da@variable@data):1]))

# → DataArray, one GDAL hyperslab read, just the slice

# Or auto-collect via arithmetic:
ds$adt |> isel(time = 1L) + 0    # triggers collect, returns DataArray

# Or auto-collect via reduction:
ds$adt |> isel(latitude = 500:600, longitude = 1:100, time = 1:30) |>
  nd_mean("time", na.rm = TRUE)  # collects then reduces
