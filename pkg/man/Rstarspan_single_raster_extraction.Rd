\name{Rstarspan_single_raster_extraction}
\alias{Rstarspan_single_raster_extraction}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Rstarspan_single_raster_extraction(vector_spatialpoints, raster_stack, bands = NA, outformat = "data.frame", outorder = "db", method = "simple", rasterid = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{vector_spatialpoints}{
%%     ~~Describe \code{vector_spatialpoints} here~~
}
  \item{raster_stack}{
%%     ~~Describe \code{raster_stack} here~~
}
  \item{bands}{
%%     ~~Describe \code{bands} here~~
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
  \item{rasterid}{
%%     ~~Describe \code{rasterid} here~~
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
function (vector_spatialpoints, raster_stack, bands = NA, outformat = "data.frame", 
    outorder = "db", method = "simple", rasterid = NA) 
{
    require(sp)
    require(rgdal)
    require(raster)
    if (missing(vector_spatialpoints)) {
        print("Missing vector_spatialpoints...")
        return(NULL)
    }
    if (missing(raster_stack)) {
        print("Missing raster_stack...")
        return(NULL)
    }
    vector_spatialpoints_reproject = spTransform(vector_spatialpoints, 
        CRS(projection(raster_stack)))
    if (is.na(bands)) {
        single_raster_extraction_raw = xyValues(raster_stack, 
            vector_spatialpoints_reproject)
        bandsid = paste("B", sprintf("\%05d", seq(1:nlayers(raster_stack))), 
            sep = "")
    }
    else {
        single_raster_extraction_raw = xyValues(stack(raster_stack@layers[bands]), 
            vector_spatialpoints_reproject)
        bandsid = paste("B", sprintf("\%05d", bands), sep = "")
    }
    if (!(is.na(rasterid))) {
        colnames(single_raster_extraction_raw) = paste(rasterid, 
            bandsid, sep = ".")
    }
    rownames(single_raster_extraction_raw) = seq(1:dim(vector_spatialpoints_reproject)[1])
    return(single_raster_extraction_raw)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
