#load multiple packages
load_library = function(...) {
  
  obj = eval(substitute(alist(...)))
  
  return(invisible(lapply(obj, function(x)library(toString(x), character.only=TRUE))))
}