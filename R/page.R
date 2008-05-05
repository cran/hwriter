## public
openPage=function(filename,dirname=NULL,title=filename) {
  if (!is.null(dirname)) {
    if (!file.exists(dirname)) dir.create(dirname,rec=T,showWar=F)
    filename=file.path(dirname,filename)
  }
  page=file(filename,'wt')
  doctype='<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">\n'
  head=hmakeTag('meta',NULL,'http-equiv'='Content-Type',content="text/html; charset=utf-8",newline=F)
  head=paste(head,hmakeTag('title',title),collapse='')
  head=hmakeTag('head',head,newline=T)
  hwrite(paste(doctype,"<html xmlns='http://www.w3.org/1999/xhtml' xml:lang='en' lang='en'>",head,'<body>'),page)
  page
}

getHwriterVersion=function() {
  (sessionInfo()$otherPkgs)[['hwriter']]$Version
}

## public
closePage=function(page) {
  hwriterlink=hwrite('hwriter',link='http://www.ebi.ac.uk/~gpau/hwriter/index.html')
  hwrite(paste('\n<br/><br/><font size=\"-2\">(Page generated on ',date(),' by ',hwriterlink,' ',getHwriterVersion(),')</font>',sep=''),page,br=T)
  hwrite('</body></html>',page,br=F)
  close(page)
}
