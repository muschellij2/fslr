
makefunc = function(funcname, type="numeric", ex_text = NULL, 
                    numeric = FALSE,
                    write=FALSE, remove=FALSE){
  x = readLines('fslr_Generic_Function.R')
  f_no_dot = gsub("[.]", "_", funcname)
  x = gsub("%ff%", f_no_dot, x)
  x = gsub("%%", funcname, x)
  x = gsub("%type", type, x)
  if (!is.null(ex_text)){
    #     print("doing stuff")
    ex_text[1] = paste0("@examples ", ex_text[1])
    ex_text = paste0("#' ", ex_text)
    ex_text = paste0(ex_text, collapse= "\n")
    x = gsub("%example%", ex_text, x, fixed=TRUE)
  } else {
    x = gsub("%example%", "#'", x)
  }
  if (numeric) {
    x = gsub("%numeric%", "as.numeric", x)
  } else {
    x = gsub("%numeric%", "", x)
    
  }
  
  if (write) writeLines(text=x, con = paste0("R/", f_no_dot, ".R"))
  if (remove) file.remove(paste0("R/", f_no_dot, ".R"))
  return(TRUE)
}

remove = FALSE
# makefunc("slope", write=TRUE, remove=remove)
makefunc("data_type", write=TRUE, remove=remove, numeric = TRUE)
makefunc("sizeof_hdr", write=TRUE, remove=remove, numeric = TRUE)
makefunc("data_type", write=TRUE, remove=remove, numeric = FALSE)

makefunc("intent_p1", write=TRUE, remove=remove, numeric = TRUE)
makefunc("intent_p2", write=TRUE, remove=remove, numeric = TRUE)
makefunc("intent_p3", write=TRUE, remove=remove, numeric = TRUE)
makefunc("intent_code", write=TRUE, remove=remove, numeric = TRUE)
makefunc("datatype", write=TRUE, remove=remove, numeric = TRUE)
makefunc("bitpix", write=TRUE, remove=remove, numeric = TRUE)
makefunc("slice_start", write=TRUE, remove=remove, numeric = TRUE)


makefunc("vox_offset", write=TRUE, remove=remove, numeric = TRUE)
makefunc("scl_slope", write=TRUE, remove=remove, numeric = TRUE)
makefunc("scl_inter", write=TRUE, remove=remove, numeric = TRUE)
makefunc("slice_end", write=TRUE, remove=remove, numeric = TRUE)
makefunc("slice_code", write=TRUE, remove=remove, numeric = TRUE)
# makefunc("xyzt_units", write=TRUE, remove=remove)

makefunc("cal.max", write=TRUE, remove=remove, numeric = TRUE)
makefunc("cal.min", write=TRUE, remove=remove, numeric = TRUE)
makefunc("slice_duration", write=TRUE, remove=remove, numeric = TRUE)
makefunc("descrip", write=TRUE, remove=remove, numeric = FALSE)
makefunc("aux.file", write=TRUE, remove=remove, numeric = FALSE)
makefunc("qform_code", write=TRUE, remove=remove, numeric = TRUE)
makefunc("sform_code", write=TRUE, remove=remove, numeric = TRUE)
# makefunc("quatern_b", write=TRUE, remove=remove)
# makefunc("quatern_c", write=TRUE, remove=remove)
# makefunc("quatern_d", write=TRUE, remove=remove)
# makefunc("qoffset_x", write=TRUE, remove=remove)
# makefunc("qoffset_y", write=TRUE, remove=remove)
# makefunc("qoffset_z", write=TRUE, remove=remove)
# makefunc("srow_x", write=TRUE, remove=remove)
# makefunc("srow_y", write=TRUE, remove=remove)
# makefunc("srow_z", write=TRUE, remove=remove)
makefunc("intent_name", write=TRUE, remove=remove, numeric = FALSE)
makefunc("magic", write=TRUE, remove=remove, numeric = FALSE)
# makefunc("extender", write=TRUE, remove=remove)
# makefunc("reoriented", write=TRUE, remove=remove)
