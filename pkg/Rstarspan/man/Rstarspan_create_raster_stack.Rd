\name{Rstarspan_create_raster_stack}
\alias{Rstarspan_create_raster_stack}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
TBD
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Rstarspan_create_raster_stack(raster_file, bands = NA, raster_proj4string = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{raster_file}{
%%     ~~Describe \code{raster_file} here~~
}
  \item{bands}{
%%     ~~Describe \code{bands} here~~
}
  \item{raster_proj4string}{
%%     ~~Describe \code{raster_proj4string} here~~
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
function (raster_file, bands = NA, raster_proj4string = NA) 
{
    require(raster)
    if (is.na(bands)) {
        raster_stack = stack(raster_file)
    }
    else {
        raster_stack = stack(raster_file, bands = bands)
    }
    if (!(is.na(raster_proj4string))) {
        projection(raster_stack) <- raster_proj4string
    }
    return(raster_stack)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
