## public
openPage=function(filename, dirname=NULL, title=filename, link.javascript=NULL, link.css=NULL, css=NULL, head=NULL, charset="utf-8", lang="en") {
  if (!is.null(dirname)) {
    if (!file.exists(dirname)) dir.create(dirname, rec=TRUE, showWar=FALSE)
    filename = file.path(dirname, filename)
  }
  page = file(filename,'wt')
  doctype = '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">\n'
  meta = hmakeTag('meta',NULL,'http-equiv'='Content-Type',content=paste("text/html; charset=", charset, sep=''), newline=FALSE)
  
  if (!is.null(link.javascript)) link.javascript = paste(hmakeTag('script', language='JavaScript', src=link.javascript), collapse='\n')
  if (!is.null(link.css)) link.css = paste(hmakeTag('link', rel='stylesheet', type='text/css', href=link.css), collapse='\n')
  if (!is.null(css)) css = paste(hmakeTag('style', css), collapse='\n')
  
  head = paste(meta, hmakeTag('title',title), head, link.javascript, link.css, css, sep='\n')
  head = hmakeTag('head', head, newline=TRUE)
  hwrite(paste(doctype, "<html xmlns='http://www.w3.org/1999/xhtml' xml:lang='", lang, "' lang='", lang, "'>", head, '<body>', sep=''), page)
  page
}

getHwriterVersion=function() {
  (sessionInfo()$otherPkgs)[['hwriter']]$Version
}

## public
closePage=function(page, splash=TRUE) {
  hwriterlink = hwrite('hwriter', link='http://www.ebi.ac.uk/~gpau/hwriter/index.html')
  if (splash) hwrite(paste('\n<br/><br/><font size=\"-2\">(Page generated on ', date(), ' by ', hwriterlink, ' ', getHwriterVersion(), ')</font>', sep=''), page, br=TRUE)
  else hwrite('\n<br/><br/>', page, br=TRUE)
  hwrite('</body></html>', page, br=FALSE)
  close(page)
}
