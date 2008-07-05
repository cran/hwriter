\name{openPage}

\alias{openPage}
\alias{closePage}

\title{HTML document/page management}

\description{
 Opens and closes a HTML page/document, allowing a sequential building
 of an HTML page.
}

\usage{
openPage(filename, dirname=NULL, title=filename, head=NULL)

closePage(page)
}

\arguments{
  \item{filename}{a character string containing the filename or the path
    of the HTML file to be created.}

  \item{dirname}{an optional character string containing the path of the
    directory where the file should be written.}

  \item{title}{an optional character string containing the title of the
    HTML page.}

  \item{head}{an optional character string containing an HTML fragment
    to be added in the <head> section of the page.}

  \item{page}{a page handle returned by a previous \code{openPage} call.}

}

\value{
  A connection which is a handle to the current HTML page.
}

\details{
 \code{openPage} opens a new file for writing and returns a page handle
 which is used by \code{hwrite} to append HTML elements in a current
 page. A previous existing file will be overwritten.

 The argument \code{head} is useful to add CSS links or HTML script tags in the
 page header. Use \code{hmakeTag} to easily build HTML tags.

 \code{closePage} ends the HTML page formatting, flushes the pending
writing operations and closes the file.
}

\seealso{
  \code{\link{hwrite}}, \code{\link{hmakeTag}}.
}

\examples{
## Creates a new web page 'test.html' in the R temporary directory
tmpdir <- tempdir()
p <- openPage('test.html', dirname=tmpdir)
hwrite('Iris example', p, center=TRUE, heading=1)
hwrite(iris, p, row.bgcolor='#ffa')
closePage(p)

## Opens a web browser on the web page
if (interactive()) try(browseURL(file.path(tmpdir, 'test.html')))
}

\author{
  Gregoire Pau, \email{gpau@ebi.ac.uk}, 2008
}

\keyword{interface}
