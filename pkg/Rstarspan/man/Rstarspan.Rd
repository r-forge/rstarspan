\name{Rstarspan}
\alias{Rstarspan}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
TBD
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Rstarspan(vector_files, raster_files, output_name, outformat = "csv", outorder = "db", method = "simple", vector_proj4strings, xcolnames, ycolnames, headers, seps, raster_proj4strings, output_dataframe = TRUE, output_raster_dates = FALSE, raster_file_types, output_raster_fname = TRUE, output_vector_fname = TRUE, output_vector_data = TRUE, out_raster_fullpath = FALSE, output_raster_file_types = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{vector_files}{
%%     ~~Describe \code{vector_files} here~~
}
  \item{raster_files}{
%%     ~~Describe \code{raster_files} here~~
}
  \item{output_name}{
%%     ~~Describe \code{output_name} here~~
}
  \item{outformat}{
%%     ~~Describe \code{outformat} here~~
}
  \item{outorder}{
%%     ~~Describe \code{outorder} here~~
}
  \item{method}{
%%     ~~Describe \code{method} here~~
}
  \item{vector_proj4strings}{
%%     ~~Describe \code{vector_proj4strings} here~~
}
  \item{xcolnames}{
%%     ~~Describe \code{xcolnames} here~~
}
  \item{ycolnames}{
%%     ~~Describe \code{ycolnames} here~~
}
  \item{headers}{
%%     ~~Describe \code{headers} here~~
}
  \item{seps}{
%%     ~~Describe \code{seps} here~~
}
  \item{raster_proj4strings}{
%%     ~~Describe \code{raster_proj4strings} here~~
}
  \item{output_dataframe}{
%%     ~~Describe \code{output_dataframe} here~~
}
  \item{output_raster_dates}{
%%     ~~Describe \code{output_raster_dates} here~~
}
  \item{raster_file_types}{
%%     ~~Describe \code{raster_file_types} here~~
}
  \item{output_raster_fname}{
%%     ~~Describe \code{output_raster_fname} here~~
}
  \item{output_vector_fname}{
%%     ~~Describe \code{output_vector_fname} here~~
}
  \item{output_vector_data}{
%%     ~~Describe \code{output_vector_data} here~~
}
  \item{out_raster_fullpath}{
%%     ~~Describe \code{out_raster_fullpath} here~~
}
  \item{output_raster_file_types}{
%%     ~~Describe \code{output_raster_file_types} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (vector_files, raster_files, output_name, outformat = "csv", 
    outorder = "db", method = "simple", vector_proj4strings, 
    xcolnames, ycolnames, headers, seps, raster_proj4strings, 
    output_dataframe = TRUE, output_raster_dates = FALSE, raster_file_types, 
    output_raster_fname = TRUE, output_vector_fname = TRUE, output_vector_data = TRUE, 
    out_raster_fullpath = FALSE, output_raster_file_types = TRUE) 
{
    if (missing(vector_files)) {
        return(NULL)
    }
    if (missing(raster_files)) {
        return(NULL)
    }
    if (missing(output_name)) {
        return(NULL)
    }
    if (missing(raster_file_types)) {
        output_raster_file_types = FALSE
    }
    raster_file_search_dataframe = Rstarspan_file_search(raster_files)
    raster_file_search_N = dim(raster_file_search_dataframe)[1]
    raster_list = vector(mode = "list", length = raster_file_search_N)
    if (missing(raster_proj4strings)) {
        for (i in 1:raster_file_search_N) {
            raster_list[[i]] = Rstarspan_create_raster_stack(raster_file_search_dataframe$search_fname[i])
        }
    }
    else {
        for (i in 1:raster_file_search_N) {
            raster_list[[i]] = Rstarspan_create_raster_stack(raster_file_search_dataframe$search_fname[i], 
                raster_proj4string = raster_proj4strings[raster_file_search_dataframe$search_id[i]])
        }
    }
    vector_file_search_dataframe = Rstarspan_file_search(vector_files)
    vector_file_search_N = dim(vector_file_search_dataframe)[1]
    vector_list = vector(mode = "list", length = vector_file_search_N)
    if (missing(vector_proj4strings)) {
        for (i in 1:vector_file_search_N) {
            vector_list[[i]] = Rstarspan_vector_to_spatialpoints(vector_file_search_dataframe$search_fname[i])
        }
    }
    else {
        for (i in 1:vector_file_search_N) {
            vector_list[[i]] = Rstarspan_vector_to_spatialpoints(vector_file = vector_file_search_dataframe$search_fname[i], 
                vector_proj4string = vector_proj4strings[vector_file_search_dataframe$search_id[i]], 
                xcolname = xcolnames[vector_file_search_dataframe$search_id[i]], 
                ycolname = ycolnames[vector_file_search_dataframe$search_id[i]], 
                header = headers[vector_file_search_dataframe$search_id[i]], 
                sep = seps[vector_file_search_dataframe$search_id[i]])
        }
    }
    for (v in 1:vector_file_search_N) {
        for (r in 1:raster_file_search_N) {
            rasterid = paste("R", sprintf("\%05d", r), sep = "")
            single_raster_extraction_raw = Rstarspan_single_raster_extraction(vector_list[[v]], 
                raster_list[[r]], rasterid = rasterid)
            vectorpointid = rownames(single_raster_extraction_raw)
            rasterbandids = colnames(single_raster_extraction_raw)
            if (outorder == "db") {
                single_raster_extraction_raw_db = cbind(expand.grid(vectorpointid, 
                  rasterbandids), mat = as.vector(single_raster_extraction_raw))
                names(single_raster_extraction_raw_db) = c("vectorpointid", 
                  "rasterbandids", "value")
                if (output_raster_fname) {
                  raster_fnames = data.frame(raster_fnames = rep(raster_file_search_dataframe$search_fname[r], 
                    length(vectorpointid) * length(rasterbandids)))
                  single_raster_extraction_raw_db = cbind(single_raster_extraction_raw_db, 
                    raster_fnames)
                }
                if (output_vector_fname) {
                  vector_fnames = data.frame(vector_fnames = rep(basename(vector_file_search_dataframe$search_fname[v]), 
                    length(vectorpointid) * length(rasterbandids)))
                  single_raster_extraction_raw_db = cbind(single_raster_extraction_raw_db, 
                    vector_fnames)
                }
                if (class(vector_list[[v]]) == "SpatialPointsDataFrame" && 
                  output_vector_data) {
                  vector_data = as.data.frame(vector_list[[v]])
                  vector_data = cbind(rownames(vector_data), 
                    vector_data)
                  names(vector_data)[1] = "vectorpointid"
                  single_raster_extraction_raw_db = merge(single_raster_extraction_raw_db, 
                    vector_data, by = "vectorpointid")
                }
                if (output_raster_dates) {
                  raster_dates = cbind(rasterbandids, raster_dates_extractor(raster_file_search_dataframe$search_fname[r], 
                    raster_file_types[raster_file_search_dataframe$search_id[r]]))
                  single_raster_extraction_raw_db = merge(single_raster_extraction_raw_db, 
                    raster_dates, by = "rasterbandids")
                }
                if (output_raster_file_types) {
                  raster_file_types_col = data.frame(raster_file_types = rep(raster_file_types[raster_file_search_dataframe$search_id[r]], 
                    length(vectorpointid) * length(rasterbandids)))
                  single_raster_extraction_raw_db = cbind(single_raster_extraction_raw_db, 
                    raster_file_types_col)
                }
                if (r == 1 && v == 1) {
                  if (output_dataframe) {
                    single_vector_extraction_raw = single_raster_extraction_raw_db
                  }
                  if (outformat == "csv") {
                    write.table(single_raster_extraction_raw_db, 
                      file = output_name, row.names = FALSE, 
                      sep = ",", append = FALSE)
                  }
                }
                else {
                  if (output_dataframe) {
                    single_vector_extraction_raw = rbind(single_vector_extraction_raw, 
                      single_raster_extraction_raw_db)
                  }
                  write.table(single_raster_extraction_raw_db, 
                    file = output_name, row.names = FALSE, col.names = FALSE, 
                    sep = ",", append = TRUE)
                }
            }
            if (outorder == "flat") {
            }
        }
    }
    if (output_dataframe) {
        return(single_vector_extraction_raw)
    }
    else {
        return(NULL)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
