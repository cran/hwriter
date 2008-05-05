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

  \item{...}{optional arguments depending on the type of
    \code{x}. See Details.}

}

\value{
  A character string containing the output HTML code.
}

\details{
  Depending on its type and its length, \code{x} is written as a
  character string, a character vector or as an HTML table.

  Many optional arguments can be used to render an R object. Arguments are listed below and rendering
examples are shown in Examples.

  Following optional arguments can be always used, independently of the
  type of \code{x}:

  \describe{
    %\item{class}{a character string containing a CSS style class to be used.
    %  The style class should exist in the current stylesheet, see
    %  \code{openPage} for details on managing stylesheets.  Default
    %  available style classes include \code{'verbatim'}, \code{'cleantable'}, see Examples.
    %  This option is the HTML equivalent of <div>.}
    
    %\item{style}{a character string containing a CSS inline style to be
    %  used. This argument is the HTML equivalent of <div>.}
    
    \item{br}{a logical specifying if a breakline (carriage return) should
      be appended at the end of \code{x}. Default is \code{FALSE}.}
    
    %\item{wiki}{a logical indicating is the resulting HTML code is meant to be
    %  inserted in a Wikimedia page, switching to the special Wikimedia HTML
    %  code conventions. Default is \code{FALSE}.}
    
    \item{center}{a logical indicating if \code{x} should be centered. 
Default is \code{FALSE}.}
  }
}

\section{Additional arguments for text elements}{
  If \code{x} is a vector of only one element, it will be written as a
  character string. Following optional arguments can be used:
  
  \describe{ 
  \item{heading}{a numeric containing the heading level style. Valid values spans from 1 to 5. See Examples. }
    
  \item{link}{a character string containing an URL the HTML output text
      will point to. This argument is the HTML equivalent of <a>.}
    
  \item{style}{a character string containing the CSS inline style to be used to render the object. This
      versatile argument allows to change the font properties (typeface, bold, italic, size, color), margins,
      alignement, spacing, padding. See Examples. }
  }
}

  
\section{Additional arguments for vectors}{
  If \code{x} is a vector with more than one element, it will be written 
as an
  HTML table by default unless \code{table} is set to \code{FALSE}, for which
  it will be written as a vector of character strings. Optional 
specific
  arguments to either character strings or HTML tables can be used,
  together with the following ones:
  
  \describe{ 
  \item{table}{If \code{TRUE}, the vector is written as an HTML table
      with one row and a number of columns equals to the number of elements
      in \code{x}. If \code{FALSE}, the vector is written as a character vector. Default is \code{TRUE}.}
 
  \item{dim}{a couple of numeric values indicating the desired number of rows
      and columns in the table. Can be used only if \code{table} is
      \code{TRUE}.}
    
  \item{byrow}{logical. If \code{FALSE} the table is filled by
      columns, otherwise the table is filled by rows. Can be used only if \code{table} is
      \code{TRUE}. Default is \code{FALSE}.}
   
  \item{names}{a logical indicating if the names of the elements should
      be written if the vector is named. Default is \code{TRUE}.}

  }
}

\section{Additional arguments for tables}{
  If \code{x} is a matrix or a data.frame, \code{x} is written as an 
HTML table. Following optional arguments can be used:
  
  \describe{ 
  \item{border}{a numeric. Specifies the table border width. A value of 0 implies that no borders will be drawn.
      Default border styles are not really pretty. Use 'style' instead to have a finer control on borders.}

  \item{row.names, col.names}{a logical value indicating whether the row (resp. columns) names of
      \code{x} are to be written. Default is \code{TRUE}.}
    
  \item{bgcolor}{a character string or a character matrix. Specifies the cell background colors, in HTML color format (hexadecimal or color names). 
      If \code{bgcolor} is a character string, the whole table will be painted with this color. If \code{bgcolor} is a character matrix of the same
      size as \code{x} which contains the cell colors, cells will be painted accordingly to the matrix. A matrix can contain NAs when cells don't have 
      a background color. The matrix may contain one extra row and/or column to target the heading (row.names, col.names) table cells. See Examples.}
      
  \item{row.bgcolor, col.bgcolor}{a list of named character vectors. This argument may contain a list of named vectors of colors pointing the corresponding rows (resp. columns) names of \code{x}. If sizes don't match, values are recycled using R style. Unnamed vector of colors will point the heading rows (resp. columns). See Examples.}
    
  \item{style, row.style, col.style}{These arguments follows the same 
rules as \code{bgcolor}, \code{row.bgcolor} and \code{col.bgcolor} but define
 CSS styles that should be used to render table cells. CSS styles are very versatile and allow to change border style, typeface, font properties, align, boldness, font color, italic... See Examples.}
    
  \item{link, row.link, col.link}{These arguments follows the same rules 
as \code{bgcolor}, \code{row.bgcolor} and \code{col.bgcolor} but 
define here URLs that the cells will point to.  See Examples.}

  \item{cellspacing, cellpadding}{a numeric. Defines the HTML cell 
spacing and HTML cell pading settings of the table. See Examples.}
    
  \item{width}{a character string. Specifies the global table width in HTML units (pixels or \%).}

  \item{col.width}{a named character vector. Specifies the columns width 
in HTML units (pixels or \%) where names of \code{col.width} are used to 
point column names of \code{x}. NAs may be used to let some column 
widths unspecified.}
  }

  See Examples for many illustrated examples of all arguments.
}

\seealso{
  \code{\link{openPage}}, \code{\link{closePage}}, \code{\link{hwriteImage}}, \code{\link{hmakeTag}}.
}

\examples{
 hwriter:::writeExample()
}

\author{
  Gregoire Pau, \email{gpau@ebi.ac.uk}, 2008
}

\keyword{interface}
