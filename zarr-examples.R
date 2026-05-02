library(ndr)

## 1. GPCP Daily Precipitation (Pangeo Forge)
open_dataset('ZARR:"/vsicurl/https://ncsa.osn.xsede.org/Pangeo/pangeo-forge/gpcp-feedstock/gpcp.zarr"')

## 2a. Copernicus Marine Sea Level (ARCO timeChunked)
open_dataset('ZARR:"/vsicurl/https://s3.waw3-1.cloudferro.com/mdl-arco-time-045/arco/SEALEVEL_GLO_PHY_L4_MY_008_047/cmems_obs-sl_glo_phy-ssh_my_allsat-l4-duacs-0.125deg_P1D_202411/timeChunked.zarr"')

## 2b. Copernicus Marine Sea Level (ARCO geoChunked)
open_dataset('ZARR:"/vsicurl/https://s3.waw3-1.cloudferro.com/mdl-arco-geo-045/arco/SEALEVEL_GLO_PHY_L4_MY_008_047/cmems_obs-sl_glo_phy-ssh_my_allsat-l4-duacs-0.125deg_P1D_202411/geoChunked.zarr"')

## 3. CMIP6 Sea Surface Height (GFDL-ESM4, ssp585)
open_dataset('ZARR:"/vsicurl/https://storage.googleapis.com/cmip6/CMIP6/ScenarioMIP/NOAA-GFDL/GFDL-ESM4/ssp585/r1i1p1f1/Omon/zos/gn/v20180701"')

## 4. CMIP6 HighResMIP Sea Level Pressure (CMCC-CM2-HR4)
open_dataset('ZARR:"/vsicurl/https://storage.googleapis.com/cmip6/CMIP6/HighResMIP/CMCC/CMCC-CM2-HR4/highresSST-present/r1i1p1f1/6hrPlev/psl/gn/v20170706"')

## 5. ARCO-ERA5 Single-Level Reanalysis (~8s to open)
open_dataset('ZARR:"/vsicurl/https://storage.googleapis.com/gcp-public-data-arco-era5/ar/1959-2022-full_37-1h-0p25deg-chunk-1.zarr-v2"')

## 6. ARCO-ERA5 Full 37-level v3 (~90s to open)
open_dataset('ZARR:"/vsicurl/https://storage.googleapis.com/gcp-public-data-arco-era5/ar/full_37-1h-0p25deg-chunk-1.zarr-v3"')

## 7. WeatherBench2 ERA5 (6-hourly, 1.5 deg)
open_dataset('ZARR:"/vsicurl/https://storage.googleapis.com/weatherbench2/datasets/era5/1959-2023_01_10-6h-64x32_equiangular_conservative.zarr"')

## 8. CMIP6 Latent Heat Flux (TaiESM1, 1pctCO2)
open_dataset('ZARR:"/vsicurl/https://cmip6-pds.s3.amazonaws.com/CMIP6/CMIP/AS-RCEC/TaiESM1/1pctCO2/r1i1p1f1/Amon/hfls/gn/v20200225"')

## 9. MUR SST L4 Global (JPL, NASA)
open_dataset('ZARR:"/vsicurl/https://mur-sst.s3.us-west-2.amazonaws.com/zarr-v1"')

## 10. HRRR Weather Model (surface analysis, subgroup structure)
open_dataset('ZARR:"/vsicurl/https://hrrrzarr.s3.amazonaws.com/sfc/20200801/20200801_00z_anl.zarr"')

## 11. ITS_LIVE Ice Velocity Datacube
open_dataset('ZARR:"/vsicurl/https://its-live-data.s3.us-west-2.amazonaws.com/datacubes/v2/N00E020/ITS_LIVE_vel_EPSG32735_G0120_X750000_Y10050000.zarr"')

## 12. NASA POWER Daily Meteorology (MERRA-2)
open_dataset('ZARR:"/vsicurl/https://nasa-power.s3.amazonaws.com/merra2/spatial/power_merra2_daily_spatial_utc.zarr"')

## 13. National Water Model Reanalysis v2.1
open_dataset('ZARR:"/vsicurl/https://noaa-nwm-retro-v2-zarr-pds.s3.amazonaws.com"')

## 14. NOAA OISST CDR (Kerchunk JSON, needs GDAL >= 3.11)
open_dataset('ZARR:"/vsicurl/https://ncsa.osn.xsede.org/Pangeo/pangeo-forge/pangeo-forge/aws-noaa-oisst-feedstock/aws-noaa-oisst-avhrr-only.zarr/reference.json"')

## 15. Sentinel-1 Global Coherence (Kerchunk JSON, needs GDAL >= 3.11)
open_dataset('ZARR:"/vsicurl/https://sentinel-1-global-coherence-earthbigdata.s3.us-west-2.amazonaws.com/data/wrappers/zarr-all.json"')

## 16. Daymet V4 Daily (Planetary Computer, needs Azure SAS token)
# open_dataset('ZARR:"/vsicurl/https://daymeteuwest.blob.core.windows.net/daymet-zarr/daily/na.zarr"')

## 17. ERA5 (Planetary Computer) — RETIRED

## 18. NEX-GDDP-CMIP6 (remote NetCDF on S3, not Zarr)
open_dataset('/vsicurl/https://nex-gddp-cmip6.s3.us-west-2.amazonaws.com/NEX-GDDP-CMIP6/ACCESS-CM2/historical/r1i1p1f1/tasmax/tasmax_day_ACCESS-CM2_historical_r1i1p1f1_gn_1950.nc')
