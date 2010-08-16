# TODO: Add comment
# 
# Author: jonathan
###############################################################################


# J Abatz
vector_spatialpoints=Rstarspan_vector_to_spatialpoints(
vector_file="/home/CFC/jonathan.greenberg/shares/projects/water_deficit/data/vector/CAHCNCOOP.asc",
vector_proj4string="+proj=latlon +ellps=WGS84",
xcolname="lon",
ycolname="lat",
header=TRUE,
sep=",")


raster_stack=Rstarspan_create_raster_stack(
		"/home/CFC/jonathan.greenberg/shares/projects/us_climate/data/raster/radiation/us_radiation_glob_015.envi",
		raster_proj4string="+proj=latlon +ellps=WGS84")

raster_stack=Rstarspan_create_raster_stack(
		"/home/CFC/jonathan.greenberg/shares/projects/us_climate/data/raster/wind/uwnd.10m.mon.mean.nc",
		raster_proj4string="+proj=lcc +lat_1=50 +lat_2=50 +lat_0=50 +lon_0=-107 +x_0=5632642 +y_0=4612546")

single_raster_extraction_raw=Rstarspan_single_raster_extraction(vector_spatialpoints,raster_stack)


#
raster_files=c(
		'/home/CFC/jonathan.greenberg/shares/RAID5/projects/us_climate/data/raster/radiation/.envi$',
		'/home/CFC/jonathan.greenberg/shares/RAID5/projects/us_climate/data/raster/wind/^uwnd.10m.mon.mean.nc$',
		'/home/CFC/jonathan.greenberg/shares/RAID5/projects/us_climate/data/raster/wind/^vwnd.10m.mon.mean.nc$'
)

raster_proj4strings=c("+proj=latlon +ellps=WGS84",
		"+proj=lcc +lat_1=50 +lat_2=50 +lat_0=50 +lon_0=-107 +x_0=5632642 +y_0=4612546",
		"+proj=lcc +lat_1=50 +lat_2=50 +lat_0=50 +lon_0=-107 +x_0=5632642 +y_0=4612546")

vector_files=c("/home/CFC/jonathan.greenberg/shares/projects/water_deficit/data/vector/CAHCNCOOP.asc")
vector_proj4strings=c("+proj=latlon +ellps=WGS84")
xcolnames=c("lon")
ycolnames=c("lat")
headers=c(TRUE)
seps=c(",")
output_raster_dates=TRUE
raster_file_types=c("rsun_jg","narr_nc","narr_nc")

raster_dates=raster_dates_extractor(raster_file,raster_file_type)



raster_file="/home/CFC/jonathan.greenberg/shares/projects/us_climate/data/raster/wind/uwnd.10m.mon.mean.nc"


abag_data=Rstarspan(
		vector_files=c("/home/CFC/jonathan.greenberg/shares/projects/water_deficit/data/vector/CAHCNCOOP.asc"),
		raster_files=c(
				'/home/CFC/jonathan.greenberg/shares/RAID5/projects/us_climate/data/raster/radiation/.envi$',
				'/home/CFC/jonathan.greenberg/shares/RAID5/projects/us_climate/data/raster/wind/^uwnd.10m.mon.mean.nc$',
				'/home/CFC/jonathan.greenberg/shares/RAID5/projects/us_climate/data/raster/wind/^vwnd.10m.mon.mean.nc$'
		),
		output_name="/home/CFC/jonathan.greenberg/shares/projects/water_deficit/data/vector/rad_wind.csv",
		outformat="csv",
		outorder="db",
		method="simple",
		vector_proj4strings=c("+proj=latlon +ellps=WGS84"),
		xcolnames=c("lon"),
		ycolnames=c("lat"),
		headers=c(TRUE),
		seps=c(","),
		raster_proj4strings=c("+proj=latlon +ellps=WGS84",
				"+proj=lcc +lat_1=50 +lat_2=50 +lat_0=50 +lon_0=-107 +x_0=5632642 +y_0=4612546",
				"+proj=lcc +lat_1=50 +lat_2=50 +lat_0=50 +lon_0=-107 +x_0=5632642 +y_0=4612546"),
		output_dataframe=TRUE,
		output_raster_dates=TRUE,
		raster_file_types=c("rsun_jg","narr_nc","narr_nc"),
		output_raster_fname=TRUE,
		output_vector_fname=TRUE,
		output_vector_data=TRUE,
		out_raster_fullpath=FALSE,
		output_raster_file_types=TRUE)	