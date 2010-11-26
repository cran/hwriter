\name{hwrite}

\alias{hwrite}

\title{HTML writer}

\description{
  Outputs an R object in HTML format.
}

\usage{
  hwrite(x, page=NULL, ...)
}

\arguments{
  \item{x}{an R object to output in HTML format. Objects of type \code{character}, \code{vector}, \code{matrix}
    and \code{data.frame} are currently supported. }

  \item{page}{an optional connection, a character string naming the file to write
    to or a page object returned by \code{openPage}.}
  
  \item{...}{optional arguments. See Details.}
}

\value{
  A character vector containing the output HTML code.
}

\details{
  If \code{x} is a vector of only one element, it will be written by default
  as an HTML text element unless \code{table} is \code{TRUE}: in that case,
  it will be written as an HTML table containing an unique element.

  If \code{x} is a vector of more than one element, a matrix or a data.frame, it will be
  written by default as an HTML table unless \code{table} is \code{FALSE}: in that case,
  it will be written as a vector or a matrix of HTML text elements.

  Many optional arguments can be used to render an HTML object. See below for
  additional information. Many comprehensive examples can be found in the
  Examples section by typing \code{example(hwrite)}.
}

\section{General arguments}{
  The following optional arguments can always be used:
  \describe{
  \item{br}{a logical specifying if a breakline (carriage return) should
      be appended at the end of \code{x}. Default is \code{FALSE}.}
    
  \item{table}{a logical controlling if the object \code{x} should be written as an HTML table. Default is \code{TRUE} for matrices and vectors containing more than one element, and \code{FALSE} otherwise. If set to \code{FALSE}, the object is written as a vector (or a matrix) of HTML text elements.}

  \item{link}{a character vector containing the URLs the HTML element
      will point to. This argument is the equivalent of the attribute \code{href} of the HTML tag <a>.}
    
  \item{name}{a character string naming the HTML element for further reference. This is the equivalent of the attribute \code{name} of the HTML tag <a>.}
    
  \item{div}{a logical. If \code{TRUE}, places the HTML element into a HTML section, using the <div> HTML tag. This is helpful for styling a section. Default is \code{FALSE}.}
    
  \item{center}{a logical indicating if \code{x} should be centered. Default is \code{FALSE}. This element may interfere with the current CSS style. Please consider the use the CSS style attribute "text-align" instead.}
   
  \item{...}{Additional arguments are added to the HTML element as HTML attributes. For HTML tables, attributes are distributed on table cells using R recycling rules. For text elements, a <span> HTML tag (or <div> if \code{div} is \code{TRUE}) is used to accommodate the attributes.} 
  }
}

\section{Additional arguments for text elements}{
  If \code{x} is rendered as an HTML text element, the following optional arguments can be used:
  
  \describe{ 
  \item{heading}{a numeric containing the heading level style. Valid values spans from 1 to 5. See Examples.}
  }
}

\section{Additional arguments for vectors}{
  If \code{x} is a vector with more than one element, the following optional
  arguments can be used:
  
  \describe{ 
  \item{dim}{a couple of optional numeric values indicating the desired number of rows
      and columns in the table. This is useful to orient a vector.}
    
  \item{byrow}{logical. If \code{TRUE}, the table is filled by
      rows first, otherwise the table is filled by columns first. Default is \code{FALSE}.}
    
  \item{names}{a logical indicating if the names of the elements should
      be written if the vector is named. Default is \code{TRUE}.}
  }
}

\section{Additional arguments for tables}{
  If \code{x} is rendered as an HTML table element, the following optional arguments can be used:
  
  \describe{ 
  \item{border}{a numeric. Specifies the table border width. A value of 0 implies that no borders will be drawn.
      This argument may interfere with the "border" CSS style attribute.}
    
  \item{row.names, col.names}{a logical value indicating whether the row (resp. column) names of
      \code{x} are to be written. Default is \code{TRUE}.}
    
  \item{cellspacing, cellpadding}{a numeric. Defines the spacing and padding space in pixels between cells. These arguments may interfere with the "border" and "padding" CSS style attributes.}
    
  \item{width}{a character string. Specifies the global table width in HTML units (pixels or \%).}
    
  \item{col.width}{a named character vector. Specifies the columns width
      in HTML units (pixels or \%) where names of \code{col.width} are used to
      point column names of \code{x}. NAs may be used to let several column
      widths unspecified.}
      
  \item{row.*, col.*}{a list of character vectors or a character vector. Distributes the attribute '*' on the HTML table cells, according to rows (resp. columns).
      Named lists (or vectors) point the corresponding rows/columns, according to their names. Unnamed lists (or vectors) point the rows/columns in the numeric order and NAs can be used to omit rows/columns. If pointed rows/columns sizes don't match, vector values are recycled using R rules.}
    
  \item{table.*}{a character string. Uses the global table attribute '*' to render the HTML table. The attribute is added to the main <table> tag. Some uses include
      setting of the "border" and "margin" CSS attributes that cannot be distributed on cells.}
    
  \item{*}{a character string, vector or matrix. Distributes the attribute '*' on the HTML table cells, using R recycling rules.
      Any valid HTML attributes can be used. The value may contain NAs to omit cells. Matrices may contain one extra row and/or column to target heading cells.}
  }
  
  See Examples for many illustrated examples of all arguments.
}

\seealso{
  \code{\link{openPage}}, \code{\link{closePage}}, \code{\link{hwriteImage}}, \code{\link{hmakeTag}}.
}

\examples{
 hwriter:::showExample()
}

\author{
  Gregoire Pau, \email{gpau@ebi.ac.uk}, 2008
}

\keyword{interface}
