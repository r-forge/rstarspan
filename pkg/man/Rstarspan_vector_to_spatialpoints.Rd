\name{Rstarspan_vector_to_spatialpoints}
\alias{Rstarspan_vector_to_spatialpoints}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Rstarspan_vector_to_spatialpoints(vector_file, vector_proj4string, xcolname = "x", ycolname = "y", header = TRUE, sep = ",")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{vector_file}{
%%     ~~Describe \code{vector_file} here~~
}
  \item{vector_proj4string}{
%%     ~~Describe \code{vector_proj4string} here~~
}
  \item{xcolname}{
%%     ~~Describe \code{xcolname} here~~
}
  \item{ycolname}{
%%     ~~Describe \code{ycolname} here~~
}
  \item{header}{
%%     ~~Describe \code{header} here~~
}
  \item{sep}{
%%     ~~Describe \code{sep} here~~
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
function (vector_file, vector_proj4string, xcolname = "x", ycolname = "y", 
    header = TRUE, sep = ",") 
{
    require(sp)
    require(rgdal)
    if (missing(vector_proj4string)) {
        print("ASCII files require vector_proj4string to be assigned...")
        print("Please refer to http://www.remotesensing.org/geotiff/proj_list/")
        return(NULL)
    }
    vector_data_frame = read.table(vector_file, header = header, 
        sep = sep)
    if (is.character(xcolname) || is.character(ycolname)) {
        if (header == TRUE) {
            vector_data_frame_colnames = names(vector_data_frame)
            vector_data_frame_col_N = length(vector_data_frame_colnames)
            if (is.character(xcolname)) {
                xcolnum = seq(1:vector_data_frame_col_N)[vector_data_frame_colnames == 
                  xcolname]
                if (length(xcolnum) == 0) {
                  print(paste("xcol", xcolname, "not found in the ASCII file, please fix...", 
                    sep = ""))
                  return(NULL)
                }
            }
            if (is.character(ycolname)) {
                ycolnum = seq(1:vector_data_frame_col_N)[vector_data_frame_colnames == 
                  ycolname]
                if (length(ycolnum) == 0) {
                  print(paste("ycol", ycolname, "not found in the ASCII file, please fix...", 
                    sep = ""))
                  return(NULL)
                }
            }
        }
    }
    else {
        xcolnum = xcolname
        ycolnum = ycolname
    }
    if ((length(vector_data_frame)) == 2) {
        vector_spatialpoints = SpatialPoints(c(vector_data_frame[xcolnum], 
            vector_data_frame[ycolnum]), proj4string = CRS(vector_proj4string))
    }
    else {
        vector_data_frame_datacols = seq(1:length(vector_data_frame))[!(seq(1:length(vector_data_frame)) \%in\% 
            c(xcolnum, ycolnum))]
        vector_spatialpoints = SpatialPointsDataFrame(c(vector_data_frame[xcolnum], 
            vector_data_frame[ycolnum]), vector_data_frame[vector_data_frame_datacols], 
            proj4string = CRS(vector_proj4string))
    }
    return(vector_spatialpoints)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
