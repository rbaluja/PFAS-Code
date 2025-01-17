#load multiple packages
load_library = function(...) {
  
  obj = eval(substitute(alist(...)))
  
  return(invisible(lapply(obj, function(x)library(toString(x), character.only=TRUE))))
}

modify_path = function(path) {
  if(code_check) {
    return(gsub("Data_Verify", "Data_Verify_Konan", path))
  } else {
    return(path)
  }
}

modify_path2 = function(path) {
  if(code_check) {
    return(gsub("Tables", "Tables_Konan", path))
  } else {
    return(path)
  }
}

modify_path3 = function(path) {
  if(code_check) {
    return(gsub("Figures", "Figures_Konan", path))
  } else {
    return(path)
  }
}

modify_path4 = function(path) {
  if(code_check) {
    return(gsub("New York", "New York Konan", path))
  } else {
    return(path)
  }
}
