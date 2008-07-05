## hwriter
## HTML writing functions
## gregoire.pau@ebi.ac.uk

## version 0.92
## - pstrick removed from Rnw
## - fixed width table
## - openPage head
## - inst/images + system.file to load images
## - hwriteImage with vector url.image

## TODO:
## - lists
## - split.table, split.table.args
## - th headings
## - missing
## - CSS class
## - clever global style, row.style handling
## - div sections (not span)
## - quick row.bgcolor, row.style, 1.bgcolor
## - unearth writeCells
## - table for one element
## - FIX: col.width doesn't work if nothing on the first row
## - FIX: split.max.x require a matrix for link even if data is a 1-row vector

hwrite=function(x,page=NULL,...)
  UseMethod('hwrite')

hwrite.character=function(x,...)
  hwrite.vector(x,...)

hwrite.numeric=function(x,...)
  hwrite.vector(x,...)

hwrite.array=function(x,...)
  hwrite.table(x,...)

hwrite.matrix=function(x,...)
  hwrite.table(x,...)

hwrite.data.frame=function(x,...)
  hwrite.table(as.matrix(x),...)

## public, flow
## - switch between hwriteString and hwrite.matrix
## - redimension 'dim' and 'byrow' matrix orientation
## consumes: names, table, byrow, dim
hwrite.vector=function(data,page=NULL,names=TRUE,table=TRUE,byrow=FALSE,dim=NULL,...) {
  if (length(data)<=1||!table) hwriteString(data,page=page,...)
  else {
    if (is.null(dim)) dim=c(1,length(data))
    datanames=names(data)
    data=matrix(data,nrow=dim[1],ncol=dim[2],byrow=byrow)
    mode(data)='character'
    ## preserve names, if possible
    if (names) {
      if (dim[1]==length(datanames)) rownames(data)=datanames
      if (dim[2]==length(datanames)) colnames(data)=datanames
    }
    hwrite.matrix(data,page=page,...)
  }
}

## private, flow
## consumes: center, heading, link, br, wiki, style
hwriteString=function(txt,page=NULL,link=NULL,heading=NULL,center=FALSE,br=FALSE,wiki=FALSE,style=NULL) {
  if (br) txt=paste(txt,'<br/>\n',sep='')
  if (!is.null(link)) {
    if (wiki) txt=paste('[',link,' ',txt,']',sep='')
    else txt=hmakeTag('a',txt,href=link)
  }
  if (!is.null(heading)) txt=hmakeTag(paste('h',heading,sep=''),txt)
  if (center) txt=hmakeTag('center',txt)
  if (!is.null(style)) txt=hmakeTag('span',txt,style=style)
  
  if (is.null(page)) txt
  else if (is.character(page)) {
    p=openPage(page)
    cat(txt,file=p)
    closePage(p)
    invisible(txt)
  } else invisible(cat(txt,file=page)) 
}

## public
## consume: image.border, width, height
hwriteImage=function(image.url,page=NULL,image.border=0,width=NULL,height=NULL,capture=FALSE,...) {
  ## take a snapshot of the current device ?
  if (capture) {
    if (is.null(width)) width=400
    if (is.null(height)) height=400
    dev.print(png,width=width,height=height,image.url)
  }
  str=hmakeTag('img',border=image.border,src=image.url,alt=image.url,width=width,height=height)

  ## final
  hwrite(str,page,...)
}

## public
hwriteCells=function(url.image,caption,page=NULL,ncol=4,link.image=NULL,...) {
  n=nrow(caption)
  if (length(url.image)!=n) stop('\'url.image\' must be as long as nrow(caption)')

  ## format images
  imgf=hwriteImage(url.image,table=FALSE,link=link.image)
  
  ## combine table, rotate tables and write the resulting table
  data=t(cbind(image=imgf,caption))
  
  hwrite(data,page,split.maxncol=ncol,...)
}

hwriter.resync=function() {
  try(detach('package:hwriter'),silent=TRUE)
  hwrite=NULL
  source('R/hwriter.R')
  source('R/page.R')
  source('R/table.R')
  source('R/example.R')
  library(hwriter)
}

## public
hmakeTag=function(tag,data=NULL,newline=FALSE,...) {
  attrs=list(...)

  if (is.null(data)) data=''
  na=length(attrs)

  ## attributes grid
  xattrs=NULL
  if (na>0) {
    namax=max(sapply(attrs,length))
    n=max(c(length(tag),length(data),namax))
    xattrs=matrix('',nr=n,nc=na)
    nattrs=names(attrs)
    for (i in 1:na) {
      if (!is.null(attrs[[i]])) {
        fna=!is.na(attrs[[i]])
        xattrs[fna,i]=paste(' ',nattrs[i],'=\"',attrs[[i]][fna],'\"',sep='')
      }
    }
    xattrs=apply(xattrs,1,paste,collapse='')
  }
  
  if (newline) nl='\n' else nl=NULL
  paste('<',tag,xattrs,'>',nl,data,'</',tag,'>',nl,sep='')
}
