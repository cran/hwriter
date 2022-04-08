\name{hwriter-package}

\alias{hwriter}
\docType{package}

\title{HTML writer}

\description{
  \code{hwriter} is an easy-to-use package able to format and
  output R objects in HTML format. It supports advanced formatting,
  tables, CSS styling, Javascript, images and provides a convenient mapping between
  R tables and HTML tables. \code{hwriter} generates XHTML 1.0 transitional
  HTML code.

  See Examples and \code{hwrite} for more details.
 
  %% The project page is \url{http://www.ebi.ac.uk/~gpau/hwriter}.
}

\section{Package content}{
  \code{hwriter} provides the following functions (but most of the job is
  carried out by \code{hwrite}):
  \itemize{
    \item \code{hwrite} outputs an R object in HTML format.
    \item \code{hwriteImage} writes an image.
    \item \code{openPage}, \code{closePage} handles HTML page/document 
    creation.
    \item \code{hmakeTag} is a low-level HTML tag formatting function.
  }
}

\seealso{
  \code{\link{hwrite}}, \code{\link{hwriteImage}}, \code{\link{openPage}}, \code{\link{closePage}}, \code{\link{hmakeTag}}.
}

\examples{
 hwriter:::showExample()
}

\author{
  Gregoire Pau, \email{gpau@ebi.ac.uk}, 2008
}

\keyword{interface}
