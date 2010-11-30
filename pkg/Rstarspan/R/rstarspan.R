# TODO: Add comment
# 
# Author: jonathan
###############################################################################

#require(rgdal)
#require(raster)
#vector_files=c("/home/CFC/jonathan.greenberg/shares/projects/water_deficit/data/vector/CAHCNCOOP.asc")
#raster_files=c("/Users/jonathan/Documents/code/R/rstarspan/test_data/starspan_raster_test_01.img")
# source("rstarspan_v01.R")
# TODO: check vector and raster projects, reproject vector if neccessary, or allow for ignore.

Rstarspan_file_search=function(search_files)
{
	search_files_N=length(search_files)
	for(i in 1:search_files_N)
	{
		temp_search_file_search=dir(dirname(search_files[i]),
				pattern=basename(search_files[i]),
				full.names=TRUE)
		
		temp_search_file_search_N=length(temp_search_file_search)
		
		if(length(temp_search_file_search_N)==0)
		{
			print(paste("Search pattern",search_files[i],"returned no results, please check and rerun...",sep=" "))
			return(NULL)
		}
		
		temp_search_file_search_id=rep(i,temp_search_file_search_N)
		temp_search_file_search_dataframe=data.frame(
				search_fname=temp_search_file_search,
				search_id=temp_search_file_search_id,
				stringsAsFactors=FALSE)
		
		if(i==1)
		{
			search_file_search_dataframe=temp_search_file_search_dataframe
		} else
		{
			search_file_search_dataframe=rbind(search_file_search_dataframe,temp_search_file_search_dataframe)
		}
	}
	return(search_file_search_dataframe)
}

Rstarspan_create_raster_stack=function(raster_file,bands=NA,raster_proj4string=NA)
{
	require(raster)
	if(is.na(bands))
	{
		raster_stack=stack(raster_file)
	} else
	{
		raster_stack=stack(raster_file,bands=bands)
	}
	
	if(!(is.na(raster_proj4string)))
	{
		projection(raster_stack) <- raster_proj4string
	}
	return(raster_stack)
	
}

Rstarspan_vector_to_spatialpoints = function(
		vector_file,
		vector_proj4string,
		xcolname="x",
		ycolname="y",
		header=TRUE,
		sep=",")
{
	require(sp)
	require(rgdal)
		
	### VECTOR INITIALIZATION
	
	# We need to check if the vector file is in a usable format.
	# ...
	
	# If CSV, we need to read it in.  Note that the xcolname, ycolname, and proj MUST be set.
		if (missing(vector_proj4string))
		{
			print("ASCII files require vector_proj4string to be assigned...")
			print("Please refer to http://www.remotesensing.org/geotiff/proj_list/")
			return(NULL)
		}
		
		vector_data_frame=read.table(vector_file,header=header,sep=sep)
		if (is.character(xcolname) || is.character(ycolname))
		{
			if (header==TRUE)
			{
				vector_data_frame_colnames=names(vector_data_frame)
				vector_data_frame_col_N=length(vector_data_frame_colnames)
				if (is.character(xcolname))
				{
					xcolnum=seq(1:vector_data_frame_col_N)[vector_data_frame_colnames==xcolname]
					if(length(xcolnum)==0)
					{
						print(paste("xcol",xcolname,"not found in the ASCII file, please fix...",sep=""))
						return(NULL)
					}
				}
				if (is.character(ycolname))
				{
					ycolnum=seq(1:vector_data_frame_col_N)[vector_data_frame_colnames==ycolname]
					if(length(ycolnum)==0)
					{
						print(paste("ycol",ycolname,"not found in the ASCII file, please fix...",sep=""))
						return(NULL)
					}
				}
			}
		} else # Header isn't there, so the xcolname and ycolname must be column numbers.
		{
			# Do some error checking here.
			xcolnum=xcolname
			ycolnum=ycolname
		}
	
		if ((length(vector_data_frame))==2)
		{
			vector_spatialpoints=SpatialPoints(c(vector_data_frame[xcolnum],vector_data_frame[ycolnum]),
					proj4string=CRS(vector_proj4string))
		} else
		{
			vector_data_frame_datacols=seq(1:length(vector_data_frame))[
					!(seq(1:length(vector_data_frame)) %in% c(xcolnum,ycolnum))]
			vector_spatialpoints=SpatialPointsDataFrame(c(vector_data_frame[xcolnum],vector_data_frame[ycolnum]),
					vector_data_frame[vector_data_frame_datacols],
					proj4string=CRS(vector_proj4string))
		}
		return(vector_spatialpoints)
}



Rstarspan_single_raster_extraction=function(
		vector,
		raster,
		bands=NA,
		outformat="data.frame",
		outorder="db",
		method='simple',
		rasterid=NA)
{
	require(sp)
	require(rgdal)
	require(raster)
	
	if(missing(vector))
	{
		print("Missing vector_spatialpoints...")
		return(NULL)
	}
	
	if(missing(raster))
	{
		print("Missing raster_stack...")
		return(NULL)
	}
	
	# Reproject vector_spatialpoints to raster_stack projection.
	# print(class(vector_spatialpoints))
	vector_reproject=spTransform(vector,CRS=CRS(projection(raster)))
#	print(vector_spatialpoints)
#	print(vector_spatialpoints_reproject)
#	print(projection(raster_stack))
	
	# writeOGR(vector_spatialpoints_reproject, "/home/CFC/jonathan.greenberg/shares/projects/water_deficit/data/vector", "COOP", driver="ESRI Shapefile")
	
	# print(length(vector_spatialpoints_reproject))
	
	# Do the extraction...
	if(is.na(bands))
	{
		if(class(vector)=="SpatialPoints" | class(vector)=="SpatialPointsDataFrame")
		{
			single_raster_extraction_raw=xyValues(raster,vector_reproject,method='bilinear')
		} 
		if(class(vector)=="SpatialPolygons" | class(vector)=="SpatialPolygonsDataFrame")
		{
			# TODO: add in weights!
			single_raster_extraction_raw=polygonValues(vector_reproject,raster)
		}
		# TODO: Lines!
		
		bandsid=paste("B",sprintf("%05d",seq(1:nlayers(raster))),sep="")
	} else
	{
		single_raster_extraction_raw=xyValues(stack(raster@layers[bands]),vector_reproject,method='bilinear')
		bandsid=paste("B",sprintf("%05d",bands),sep="")
	}
	
	single_raster_extraction_raw=as.matrix(single_raster_extraction_raw)
	
#	print(class(single_raster_extraction_raw))	
	if(!(is.na(rasterid)))
	{
		colnames(single_raster_extraction_raw)=paste(rasterid,bandsid,sep=".")
	}
	print(dim(vector_reproject))
	if(length(vector_reproject)>1)
	{
		rownames(single_raster_extraction_raw)=seq(1:dim(vector_reproject)[1])
	} else
	{
		rownames(single_raster_extraction_raw)=1
	}
	return(single_raster_extraction_raw)
}

Rstarspan_raster_string_to_rasters_list=function(raster)
{
	require("raster")
	if(class(raster)=="character")
	{
		file_search=dir(dirname(raster),
				pattern=basename(raster),
				full.names=TRUE)
		
		file_search_N=length(file_search)
		
		if(length(file_search_N)==0)
		{
			print(paste("Search pattern",raster,"returned no results, please check and rerun...",sep=" "))
			return(NULL)
		} else
		{
			raster_search_list=as.list(file_search)
		}
		
		rasters_list=sapply(raster_search_list,brick,simplify=FALSE)
		names(rasters_list)=sapply(rasters_list,function(x) basename(filename(x)),simplify=FALSE)
		
	} else
	{
		# Need some more error checking for now...
		rasters_list=raster
		names(rasters_list)=basename(filename(raster))
	}
	return(rasters_list)
}

Rstarspan=function(
#		vector_files,
#		raster_files,
		rasters,
		vectors,
		output_name,
		outformat="none",
		outorder="db",
		method="simple",
		vector_proj4strings,
		xcolnames,
		ycolnames,
		headers,
		seps,
		raster_proj4strings,
		output_dataframe=TRUE,
		output_raster_dates=FALSE,
		raster_file_types,
		output_raster_fname=TRUE,
		output_vector_fname=TRUE,
		output_vector_data=TRUE,
		out_raster_fullpath=FALSE,
		output_raster_file_types=TRUE,
		index_match=FALSE,
		use_zvalue=TRUE,
		vector_names=NA)	
{
	
	# raster_files: a vector of raster files or patterns to match, following the pattern requirements of dir()
	#	The full path should be included in the search pattern.  
	# rasters: a list of pre-defined raster/brick/stack objects (see package "raster").
	
	# 	Some examples: ???
	
	# raster_proj4strings: a vector of properly formatted PROJ4 strings to apply to raster files, if the projection
	#	either does not exist or does not conform.  The list must be equal to the number of raster_files.
	#	Note that this implies that each search search string should be assumed to have the same projections.  
	
	# vector_files: a vector of vector files or patterns to match. 
	# vectors: a list of sp objects or spZoo objects.
	
	if(missing(vectors))
	{
		print("You must assign either vector_files or vectors (or both), exiting..")
		return(NULL)
	}
	
	if(missing(rasters))
	{
		print("You must assign either raster_files or rasters (or both), exiting..")
		return(NULL)
	}
	
	if(missing(output_name))
	{
		return(NULL)
	}
	
	if(missing(raster_file_types))
	{
		output_raster_file_types=FALSE
	}
	

	# Prepare vectors
	if(class(vectors)=="SpatialPolygonsDataFrame" || class(vectors) == "SpatialPointsDataFrame")
	{
		# Only one vector was provided.
		vectors_list_N=1
		vectors=list(vectors)
		if(is.na(vector_names))
		{
			names(vectors)="vector0001"
		}
		else
		{
			if(length(vector_names)>1)
			{
				print("length(vector_names) must be equal to length(vectors)...")
				return(NULL)
			} else
			{
				names(vectors)=vector_names
			}
			
		}
	} else
	{
		# If a list, FINISH.  This needs to read in files and vector objects.
		require("RSAGA")
		vectors_fnames=vectors
		vectors_extensions=mapply(get.file.extension,vectors_fnames)
		vector_list_N=length(vectors_fnames)
		for(v in 1:vector_list_N)
		{
			if(vectors_extensions[[v]]==".shp")
			{
				# Should check to make sure file exists...
				dsn=dirname(vectors_fnames[[v]])
				layer=strsplit(vectors_fnames[[v]],".shp")[[1]]
				temp_vector=readOGR(dsn,layer)
				vectors[[v]]=temp_vector
			}
			
		}
		
	
	}
	
	if(class(rasters)=="list")
	{
	#	rasters_types=sapply(rasters,class,simplify=FALSE)
		rasters_list=unlist(sapply(rasters,Rstarspan_raster_string_to_rasters_list,simplify=FALSE))
		rasters_list_N=length(rasters_list)
	} else
	{
		rasters_list_N=1
		
	}

	# Determine indices if index_match=TRUE
	if(index_match)
	{
		
		
	}
	
	# Begin raster extractions.
	for (v in 1:vector_list_N)
	{
#		print(names(vector_list)[v])
		for (r in 1:rasters_list_N)
		{
			
			rasterid=paste("R",sprintf("%05d",r),sep="")
			single_raster_extraction_raw=Rstarspan_single_raster_extraction(vector_list[[v]],rasters_list[[r]],rasterid=rasterid)
	
			vectorpointid=rownames(single_raster_extraction_raw)
			rasterbandids=colnames(single_raster_extraction_raw)
		
			if(outorder=="db")
			{
				single_raster_extraction_raw_db=cbind(expand.grid(vectorpointid,rasterbandids),mat=as.vector(single_raster_extraction_raw))
				names(single_raster_extraction_raw_db)=c("vectorpointid","rasterbandids","value")
				
				# Merge in the rest of the data
				if(output_raster_fname)
				{
				#	raster_fnames=data.frame(raster_fnames=
				#					rep(raster_file_search_dataframe$search_fname[r],length(vectorpointid)*length(rasterbandids)))
					raster_fnames=data.frame(raster_fnames=
								rep(names(rasters_list)[r],length(vectorpointid)*length(rasterbandids)))
					single_raster_extraction_raw_db=cbind(single_raster_extraction_raw_db,raster_fnames)
				}
				
				if(output_vector_fname)
				{
				#	vector_fnames=data.frame(vector_fnames=
				#					rep(basename(vector_file_search_dataframe$search_fname[v]),length(vectorpointid)*length(rasterbandids)))
					vector_fnames=data.frame(vector_fnames=
						rep(names(vector_list)[v],length(vectorpointid)*length(rasterbandids)))
				
				single_raster_extraction_raw_db=cbind(single_raster_extraction_raw_db,vector_fnames)
				}
				
				if (class(vector_list[[v]])=="SpatialPointsDataFrame" && output_vector_data)
				{
					vector_data=as.data.frame(vector_list[[v]])
					vector_data=cbind(rownames(vector_data),vector_data)
					names(vector_data)[1]="vectorpointid"
					single_raster_extraction_raw_db=merge(single_raster_extraction_raw_db,vector_data,by="vectorpointid")
				}
				
				if(output_raster_dates)
				{
					raster_dates=cbind(rasterbandids,raster_dates_extractor(raster_file_search_dataframe$search_fname[r],
						raster_file_types[raster_file_search_dataframe$search_id[r]]))
					single_raster_extraction_raw_db=merge(single_raster_extraction_raw_db,raster_dates,by="rasterbandids")
				}
				
				if(output_raster_file_types)
				{
					raster_file_types_col=data.frame(raster_file_types=
						rep(raster_file_types[raster_file_search_dataframe$search_id[r]],
						length(vectorpointid)*length(rasterbandids)))
					single_raster_extraction_raw_db=cbind(single_raster_extraction_raw_db,raster_file_types_col)
				}
				
				if (r==1 && v==1)
				{
					if (output_dataframe)
					{
						single_vector_extraction_raw=single_raster_extraction_raw_db
					}
					if(outformat=="csv")
					{
						write.table(single_raster_extraction_raw_db,file=output_name,row.names = FALSE,sep=",",append=FALSE)
					}
				} else
				{
					if (output_dataframe)
					{
						single_vector_extraction_raw=rbind(single_vector_extraction_raw,single_raster_extraction_raw_db)
					}
					if(outformat=="csv")
					{
						write.table(single_raster_extraction_raw_db,file=output_name,row.names = FALSE,col.names=FALSE,sep=",",append=TRUE)
					}
				}
				
				
			}
			if(outorder=="flat")
			{	
				# NOTHING YET.
			}
		}
	}
	if (output_dataframe)
	{
		return(single_vector_extraction_raw)
	} else
	{
		return(NULL)
	}
}
