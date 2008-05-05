\name{hmakeTag}

\alias{hmakeTag}

\title{HTML/XML tag formatter}

\description{
  Formats an HTML/XML tag, using a low-level syntax.
}

\usage{
hmakeTag(tag, data=NULL, newline=FALSE, ...)
}

\arguments{
  \item{tag}{a character vector containing the name of the HTML/XML tag.}

  \item{data}{a character vector containing the body of the tag.}

  \item{newline}{a logical. Appends a newline \code{\\n} character at the end of the tag.}	

  \item{...}{optional arguments that will be appended in the tag.}

}

\value{
  A string containing the output HTML/XML tag.
}

\details{
 This low-level function is used by \code{hwrite} to build HTML tags,
 which is doing all the HTML formatting job. This function is useful to
 build non-standard or rare HTML tags.
}

\seealso{
  \code{\link{hwrite}}.
}

\examples{
## simple call
hmakeTag('a','Centipede',href='http://en.wikipedia.org/wiki/Centipede')

## vectorized calls
hmakeTag('tag',1:10,color='red')
hmakeTag(colors()[1:10],1:10,semantic='color')

## nested calls
hmakeTag('html',hmakeTag('body','Text'),'xml:lang'='en')
}

\author{
  Gregoire Pau, \email{gpau@ebi.ac.uk}, 2008
}

\keyword{interface}

