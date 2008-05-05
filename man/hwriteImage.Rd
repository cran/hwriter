\name{hwriteImage}

\alias{hwriteImage}

\title{Insert an HTML image}

\description{
  Inserts an image in an HTML document. The image can be either an
  external one specified by an URL or can be captured from the current
  graphic device.
}

\usage{
hwriteImage(image.url, page=NULL, image.border=0, width=NULL,
height=NULL, capture=FALSE, ...)
}

\arguments{
  \item{image.url}{a character string containing the URL of an image,
    which can be also a path to a local filename.}
  
  \item{page}{an optional connection, a character string naming the file to write
    to or a page object returned by \code{openPage}.}

  \item{image.border}{an optional numeric value specifiying the width of the image border. 
Default is 0.}

  \item{width, height}{an optional HTML length unit (in pixels) specifiying the width (resp. height) at which the image
    should be rendered. If missing, the default image width
    (resp. height) will be used.}

  \item{capture}{a logical. If \code{TRUE} the image from the current
    graphic device is captured and written as a PNG file to the filename specified by
    \code{image.url}. Capture resolution is controlled by \code{width}
    and \code{height}, which have a default value of 400 pixels. Default is \code{FALSE}.}
  
  \item{...}{optional arguments that will be dispatched to the
    underlying \code{hwrite} call.}
}

\value{
  A character string containing the output HTML code.
}

\details{
 \code{hwriteImage} constructs an HTML <img> tag to insert a image. This function can be seamlessly in conjuction with \code{hwrite}
to position an image. The \code{capture} argument enables to capture
easily a current plot and to insert it in a web page.
}

\seealso{
  \code{\link{hwrite}}.
}

\examples{
## Creates a new web page 'test.html'
p <- openPage('test.html')

## Insert an external image
img=hwriteImage('http://www.ebi.ac.uk/~gpau/hwriter/iris1.jpg', center=TRUE)
hwrite(c(img,caption='Iris'), p, dim=c(2,1),
row.style=list(caption='text-align:center;background-color:#fac'),
row.names=FALSE, br=TRUE)

## Closes the web page
closePage(p)

## Opens a web browser to see the result
browseURL('test.html')
}

\author{
  Gregoire Pau, \email{gpau@ebi.ac.uk}, 2008
}

\keyword{interface}