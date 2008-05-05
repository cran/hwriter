## private
## - process .row or .col argument on its corresponding rcdata matrix
hprocessRowCol=function(rowcol,rcdata,data,horientation=T) {
  dim=dim(data)
  if (is.null(rcdata)) rcdata=matrix(NA,nr=dim[1],nc=dim[2])
  if (length(rowcol)==1 & is.null(names(rowcol))) {
    rowcol=as.list(rowcol)
    names(rowcol)=''
  }
  if (!is.list(rowcol)) stop(paste('\'',deparse(substitute(rowcol), 500),'\' must be a list',sep=''))
  
  nrowcol=names(rowcol)
  if (horientation) {
    if (is.null(rownames(data))) z=1
    else z=match(nrowcol,rownames(data))
    dmax=dim[2]
  }
  else {
    if (is.null(colnames(data))) z=1
    else z=match(nrowcol,colnames(data))
    dmax=dim[1]
  }

  if (length(z)>0) {
    for (i in 1:length(z)) {
      if (!is.na(z[i])) {
        rc=rowcol[[i]]
        if (length(rc)==1) dmin=1
        else dmin=(dmax-length(rc)+1)
        if (horientation) rcdata[z[i],dmin:dmax]=rc
        else rcdata[dmin:dmax,z[i]]=rc
      }
    }
  }
  
  rcdata
}

## private
## reindex row or col named vector into a numeric fashion
hmapRowCol=function(x,rowcolnames) {
  if (is.null(x)) y=NULL
  else {
    if (is.null(names(x))) y=x
    else {
      if (is.null(rowcolnames)) y=NULL
      else {
        z=match(names(x),rowcolnames)
        y=rep(NA,length(rowcolnames))
        y[na.omit(z)]=x[which(!is.na(z))]
      }
    }
  }
  
  y
}

## public, flow
## - column and row names handling
## consumes: row.names, col.names
## consumes: col.link, row.link, col.bgcolor, row.bgcolor
## consumes: @split.maxncol, @split.maxnrow
## uses: bgcolor, link, col.width
hwrite.table=function(data,page=NULL,row.names=T,col.names=T,col.link=NULL,row.link=NULL,link=NULL,col.bgcolor=NULL,row.bgcolor=NULL,bgcolor=NULL,split.maxncol=NULL,split.maxnrow=NULL,width=NULL,col.width=NULL,row.style=NULL,col.style=NULL,style=NULL,...) { 
  ## process row and column names (add a col and a row, resp.)
  acol=row.names & !is.null(rownames(data))
  arow=col.names & !is.null(colnames(data))
  if (acol) data=cbind(rownames(data),data)
  if (arow) data=rbind(colnames(data),data)
  dim=dim(data)

  ## expand if needed
  if (!is.null(link)) link=hexpand(link,dim)
  if (!is.null(bgcolor)) bgcolor=hexpand(bgcolor,dim)
  if (!is.null(style) & is.matrix(style)) style=hexpand(style,dim)

  ## process .row and .field arguments
  if (!is.null(col.link)) link=hprocessRowCol(col.link,link,data,F)
  if (!is.null(row.link)) link=hprocessRowCol(row.link,link,data,T)
  if (!is.null(col.bgcolor)) bgcolor=hprocessRowCol(col.bgcolor,bgcolor,data,F)
  if (!is.null(row.bgcolor)) bgcolor=hprocessRowCol(row.bgcolor,bgcolor,data,T)
  if (!is.null(col.style)) {
    if (!is.null(style) & !is.matrix(style)) style=hexpand(style,dim)
    style=hprocessRowCol(col.style,style,data,F)
  }
  if (!is.null(row.style)) {
    if (!is.null(style) & !is.matrix(style)) style=hexpand(style,dim)
    style=hprocessRowCol(row.style,style,data,T)
  }

  ## process col.width argument
  icol.width=hmapRowCol(col.width,colnames(data))

  ## process split.maxncol and split.maxnrow
  if (!is.null(split.maxncol) | !is.null(split.maxnrow)) {
    if (!is.null(split.maxncol) & !is.null(split.maxnrow)) stop('either \'split.maxnrow\' or \'split.maxncol\' must be NULL')
    
    ## split ancillary tables
    data=hsplitArray(data,maxnrow=split.maxnrow,maxncol=split.maxncol,preserve.size=T,output.list=F,arow=arow,acol=acol)
    link=hsplitArray(link,maxnrow=split.maxnrow,maxncol=split.maxncol,preserve.size=T,output.list=F,arow=arow,acol=acol)
    bgcolor=hsplitArray(bgcolor,maxnrow=split.maxnrow,maxncol=split.maxncol,preserve.size=T,output.list=F,arow=arow,acol=acol)
    style=hsplitArray(style,maxnrow=split.maxnrow,maxncol=split.maxncol,preserve.size=T,output.list=F,arow=arow,acol=acol)
  }
  
  hwriteRawTable(data,page=page,link=link,bgcolor=bgcolor,style=style,icol.width=icol.width,...)
}

## private
## expands a to be of size db, by adding top/left NA rows/columns if needed
hexpand=function(a,db) {
  da=dim(a)
  if (length(a)==1) a=matrix(a[1],nr=db[1],nc=db[2])
  else if (any(da!=db)) {
    if (da[1]==db[1]-1) a=rbind(NA,a)
    if (da[2]==db[2]-1) a=cbind(NA,a)
  }
  a
}

## private, flow
## consumes: border, cellspacing, cellpadding
## consumes: bgcolor, link, br, center, style
## consumes: width, icol.width
## consumes: @wiki
hwriteRawTable=function(data,page=NULL,border=1,cellspacing=NA,cellpadding=NA,link=NULL,bgcolor=NULL,wiki=F,br=F,style=NULL,center=F,width=NULL,icol.width=NULL) {
  if (!is.matrix(data)) stop('\'data\' must be a matrix')
  dim=dim(data)
  data=as.vector(data)
  data[is.na(data)]='&nbsp;'

  ## global versus local
  if (is.matrix(style)) {
    mstyle=style
    gstyle=NULL
  }
  else {
    gstyle=style
    mstyle=NULL
  }
  
  ## process cells td
  if (!is.null(link)) {
    z=!is.na(link)
    if (wiki) data[z]=paste('[',link[z],' ',data[z],']',sep='')
    else data[z]=hmakeTag('a',data[z],href=link[z])
  }
  ## first td
  tdwidth=matrix(NA,nr=dim[1],nc=dim[2])
  if (!is.null(icol.width)) tdwidth[1,1:length(icol.width)]=icol.width
  data=hmakeTag('td',data,bgcolor=bgcolor,width=tdwidth,style=mstyle)
  
  ## process rows tr
  dim(data)=dim
  data=apply(data,1,function(z) paste(z,collapse=''))
  data=hmakeTag('tr',data,newline=T)

  ## process table
  data=paste(data,collapse='')
  str=hmakeTag('table',data,border=border,cellspacing=cellspacing,
    cellpadding=cellpadding,newline=T,style=gstyle,width=width)
  
  ## final
  hwrite(str,page,br=br,center=center)
}

## private
## - split an array into a list of subarrays
## - rownames and colnames are ignored
## - arow and acol indicate that the first row (resp col) should be treated as a rowname (resp colname)
hsplitArray=function(data,maxnrow=0,maxncol=0,preserve.size=T,output.list=T,arow=F,acol=F) {
  if (is.null(data)) return(NULL)
  if (!is.matrix(data)) stop('\'data\' must be a matrix')
  if (is.null(maxnrow)) maxnrow=0
  if (is.null(maxncol)) maxncol=0
  if (maxnrow==0 && maxncol==0) stop('splitting must be done in one direction: either \'maxnrow\' or \'maxncol\' must be non null')
  if (maxnrow*maxncol!=0) stop('splitting cannot be done on both directions: either \'maxnrow\' or \'maxncol\' must be null')
  if (!output.list && !preserve.size) stop('outputting matrix is possible only if \'preserve.size\' is TRUE')

  ## preserve first row (resp col)
  if (arow) {
    zrow=data[1,]
    data=data[-1,]
  }
  if (acol) {
    zcol=data[,1]
    data=data[,-1]
  }
  
  nr=nrow(data)
  nc=ncol(data)
  if (output.list) out=list()
  else out=NULL
  
  ## maxncol splitting (horizontal splitting)
  if (maxncol>0) { 
    nc2=maxncol  
    z1=1
    repeat {
      if (z1>nc) break
      z2=z1+nc2-1
      if (z2>nc) z2=nc
      if (preserve.size) {
        napz=nc2-z2+z1-1
        zdata=matrix(c(data[,z1:z2],rep(NA,nr*napz)),nrow=nr,ncol=nc2)
        nap=rep(NA,napz)
      } else {
        zdata=matrix(data[,z1:z2],nrow=nr,ncol=1+z2-z1)
        nap=NULL
      }
      
      if (acol&arow) {
        zdata=cbind(zcol,zdata)
        zdata=rbind(c(zrow[c(1,1+(z1:z2))],nap),zdata)
      }
      else if (acol) zdata=cbind(zcol,zdata)
      else if (arow) zdata=rbind(c(zrow[z1:z2],nap),zdata)
     
      if (output.list) out=c(out,list(zdata))
      else out=rbind(out,zdata)
      z1=z2+1
    }
  }
  
  ## maxnrow splitting (vertical splitting)
  if (maxnrow>0) { 
    nr2=maxnrow
    z1=1
    repeat {
      if (z1>nr) break
      z2=z1+nr2-1
      if (z2>nr) z2=nr
      if (preserve.size) {
        napz=nr2-z2+z1-1
        zdata=matrix(c(t(data[z1:z2,]),rep(NA,nc*napz)),nrow=nr2,ncol=nc,byrow=T)
        nap=rep(NA,napz)
      } else {
        zdata=matrix(data[z1:z2,],nrow=1+z2-z1,ncol=nc)
        nap=NULL
      }
      if (acol) zdata=cbind(c(zcol[z1:z2],nap),zdata)
      if (arow) zdata=rbind(zrow,zdata)
      
      if (output.list) out=c(out,list(zdata))
      else out=cbind(out,zdata)
      z1=z2+1
    }
  }
  
  out
}
