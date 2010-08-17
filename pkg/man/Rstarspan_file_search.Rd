\name{Rstarspan_file_search}
\alias{Rstarspan_file_search}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Rstarspan_file_search(search_files)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{search_files}{
%%     ~~Describe \code{search_files} here~~
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
function (search_files) 
{
    search_files_N = length(search_files)
    for (i in 1:search_files_N) {
        temp_search_file_search = dir(dirname(search_files[i]), 
            pattern = basename(search_files[i]), full.names = TRUE)
        temp_search_file_search_N = length(temp_search_file_search)
        if (length(temp_search_file_search_N) == 0) {
            print(paste("Search pattern", search_files[i], "returned no results, please check and rerun...", 
                sep = " "))
            return(NULL)
        }
        temp_search_file_search_id = rep(i, temp_search_file_search_N)
        temp_search_file_search_dataframe = data.frame(search_fname = temp_search_file_search, 
            search_id = temp_search_file_search_id, stringsAsFactors = FALSE)
        if (i == 1) {
            search_file_search_dataframe = temp_search_file_search_dataframe
        }
        else {
            search_file_search_dataframe = rbind(search_file_search_dataframe, 
                temp_search_file_search_dataframe)
        }
    }
    return(search_file_search_dataframe)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
