library(ncdf4)
library(foreach)
library(doParallel)

data_dir <- "/Users/levkolinski/Desktop/MERRA DATA"

# test file
# nc_file<-nc_open("/Users/levkolinski/Downloads/MERRA DATA/MERRA2_401.statD_2d_slv_Nx.20200904.nc4")
# names(nc_file$var)

lon_index <- 426
lat_index <- 202

extracted_data <- list()

extract_data <- function(file_name) {
  # Open NetCDF file
  nc <- nc_open(file_name)
  
  # Extract maximum temperature at specified latitude and longitude indices
  max_t <- ncvar_get(nc, "T2MMAX")
  max_temp <- max_t[lon_index, lat_index]
  
  # extract rainfall data at specified latitutde and longitude indices
  rain <- ncvar_get(nc, "TPRECMAX")  # precipitation rate
  rainfall <- rain[lon_index, lat_index]
  
  # Extract start date
  start_date <- ncatt_get(nc, 0, "RangeBeginningDate")$value
  
  # Close the NetCDF file
  nc_close(nc)
  
  # Store extracted temperature along with its corresponding start date in the list, on list for each day
  return(list(temperature = max_temp, date = start_date, rainfall = rainfall))
}


registerDoParallel(cores = 4) # leverage more computer cores to do this faster 


# Iterate over each .nc4 file in the directory
foreach(file_name = list.files(data_dir, pattern = "\\.nc4$", full.names = TRUE),
        .combine = 'c') %do% {
          extracted_data <- append(extracted_data, list(extract_data(file_name)))
        }

# Stop parallel processing (if used)
stopImplicitCluster()
# Convert the list to a data frame if needed
extracted_df <- do.call(rbind, lapply(extracted_data, unlist))
rownames(extracted_df) <- NULL

write.csv(extracted_df, "/Users/levkolinski/Desktop/taboga_estradiol/Data/MERRA2_dataset.csv")


