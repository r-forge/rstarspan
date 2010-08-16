# TODO: Add comment
# 
# Author: jonathan
###############################################################################


raster_dates_extractor=function(raster_file,raster_file_type)
{
	require("raster")
	require("chron")
	days_in_months=c(31,28,31,30,31,30,31,31,30,31,30,31)
	# http://www.esrl.noaa.gov/psd/data/gridded/data.narr.monolevel.html
	if (raster_file_type=="narr_nc")
	{
		require("RNetCDF")
		con_narr=open.nc(raster_file)
		date_vector=as.POSIXct((var.get.nc(con_narr,"time")*3600),origin="1800-01-01")
		close.nc(con_narr)
		dates_N=length(date_vector)
		start_dates=format(date_vector,format="%Y-%m-%d")
		date_chron=chron(start_dates,format=c(dates="y-m-d"))
		end_dates=vector("character",length=dates_N)
		for (j in (1:dates_N))
		{
			if (leap.year(date_chron[j]) & months(date_chron[j])=="Feb")
			{
				end_day=29
			} else
			{
				end_day=days_in_months[months(date_chron[j])]
			}
			end_dates[j]=paste(years(date_chron[j]),sprintf("%02d",unclass(months(date_chron[j]))),end_day,sep="-")
		}
		raster_dates=data.frame(start_dates=start_dates,end_dates=end_dates)
	}
	
	if (raster_file_type=="rsun_jg")
	{
		# This is a custom one used for my rsun runs.
		require("clim.pact")
		
		temp_date_vector=strsplit(strsplit(basename(raster_file),"[.]")[[1]][1],"_")[[1]]
		temp_date_doy=as.numeric(temp_date_vector[length(temp_date_vector)])
		temp_date=caldat(temp_date_doy-1)
		start_dates=paste(1970,temp_date$month,temp_date$day,sep="-")
		end_dates=start_dates
	
		raster_dates=data.frame(start_dates=start_dates,end_dates=end_dates)
	}
	
	
	return(raster_dates)
}
