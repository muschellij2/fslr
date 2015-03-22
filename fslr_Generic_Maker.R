
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
makefunc("data_type", write=TRUE, remove=remove)
makefunc("sizeof_hdr", write=TRUE, remove=remove)
makefunc("data_type", write=TRUE, remove=remove)
makefunc("db_name", write=TRUE, remove=remove)
makefunc("extents", write=TRUE, remove=remove)
makefunc("session_error", write=TRUE, remove=remove)
makefunc("regular", write=TRUE, remove=remove)
makefunc("dim_info", write=TRUE, remove=remove)
makefunc("dim_", write=TRUE, remove=remove)

makefunc("intent_p1", write=TRUE, remove=remove)
makefunc("intent_p2", write=TRUE, remove=remove)
makefunc("intent_p3", write=TRUE, remove=remove)
makefunc("intent_code", write=TRUE, remove=remove)
makefunc("datatype", write=TRUE, remove=remove)
makefunc("bitpix", write=TRUE, remove=remove)
makefunc("slice_start", write=TRUE, remove=remove)
makefunc("pixdim", write=TRUE, 
         remove=remove, 
         ex_text=c('\\dontrun{', 
                   'url <- "http://nifti.nimh.nih.gov/nifti-1/data/avg152T1_LR_nifti.nii.gz"',
                   'urlfile <- file.path(system.file("nifti", package="oro.nifti"),', 
                   '"mniLR.nii.gz")',
                   'download.file(url, urlfile, quiet=TRUE)',
                   '}',
                   'urlfile <- file.path(system.file("nifti", package="oro.nifti"),',
                   '                     "mniLR.nii.gz")', 
                   'mniLR <- readNIfTI(urlfile)', 
                   'pixdim(mniLR)'))

makefunc("vox_offset", write=TRUE, remove=remove)
makefunc("scl_slope", write=TRUE, remove=remove)
makefunc("scl_inter", write=TRUE, remove=remove)
makefunc("slice_end", write=TRUE, remove=remove)
makefunc("slice_code", write=TRUE, remove=remove)
makefunc("xyzt_units", write=TRUE, remove=remove)

makefunc("cal.max", write=TRUE, 
         remove=remove, 
         ex_text = c('\\dontrun{',
                     'url <- "http://nifti.nimh.nih.gov/nifti-1/data/avg152T1_LR_nifti.nii.gz"',
                     'urlfile <- file.path(system.file("nifti", package="oro.nifti"),',
                     '                     "mniLR.nii.gz")',
                     'download.file(url, urlfile, quiet=TRUE)',
                     '}',
                     'urlfile <- file.path(system.file("nifti", package="oro.nifti"),',
                     '                     "mniLR.nii.gz")',
                     'mniLR <- readNIfTI(urlfile)',
                     'cal.max(mniLR)'))
makefunc("cal.min", write=TRUE, 
         remove=remove,
         ex_text = c('\\dontrun{',
                     'url <- "http://nifti.nimh.nih.gov/nifti-1/data/avg152T1_LR_nifti.nii.gz"',
                     'urlfile <- file.path(system.file("nifti", package="oro.nifti"),',
                     '                     "mniLR.nii.gz")',
                     'download.file(url, urlfile, quiet=TRUE)',
                     '}',
                     'urlfile <- file.path(system.file("nifti", package="oro.nifti"),',
                     '                     "mniLR.nii.gz")',
                     'mniLR <- readNIfTI(urlfile)',
                     'cal.min(mniLR)'))
makefunc("slice_duration", write=TRUE, remove=remove)
makefunc("toffset", write=TRUE, remove=remove)
makefunc("glmax", write=TRUE, remove=remove)
makefunc("glmin", write=TRUE, remove=remove)
makefunc("descrip", write=TRUE, 
         remove=remove, 
         ex_text = c('\\dontrun{',
                     'url <- "http://nifti.nimh.nih.gov/nifti-1/data/avg152T1_LR_nifti.nii.gz"',
                     'urlfile <- file.path(system.file("nifti", package="oro.nifti"),',
                     '                     "mniLR.nii.gz")',
                     'download.file(url, urlfile, quiet=TRUE)',
                     '}',
                     'urlfile <- file.path(system.file("nifti", package="oro.nifti"),',
                     '                     "mniLR.nii.gz")',
                     'mniLR <- readNIfTI(urlfile)',
                     'descrip(mniLR)',
                     '\\dontrun{',
                     'descrip(mniLR) <- paste(descrip(mniLR), version$version.string, sep="; ")',
                     'descrip(mniLR)',
                     '}'))
makefunc("aux.file", 
         write=TRUE, 
         remove=remove,
         ex_text = c('\\dontrun{', 
                     'url <- "http://nifti.nimh.nih.gov/nifti-1/data/avg152T1_RL_nifti.nii.gz"', 
                     'urlfile <- file.path(system.file("nifti", package="oro.nifti"),', 
                     '                    "mniRL.nii.gz")', 
                     'download.file(url, urlfile, quiet=TRUE)', 
                     '}', 
                     'options("niftiAuditTrail"=FALSE)', 
                     '', 
                     'urlfile <- file.path(system.file("nifti", package="oro.nifti"),', 
                     '                     "mniRL.nii.gz")', 
                     'mniRL <- readNIfTI(urlfile)', 
                     'aux.file(mniRL)', 
                     'aux.file(mniRL) <- "avg152T1_RL_nifti"', 
                     'aux.file(mniRL)'))
makefunc("qform_code", write=TRUE, remove=remove)
makefunc("sform_code", write=TRUE, remove=remove)
makefunc("quatern_b", write=TRUE, remove=remove)
makefunc("quatern_c", write=TRUE, remove=remove)
makefunc("quatern_d", write=TRUE, remove=remove)
makefunc("qoffset_x", write=TRUE, remove=remove)
makefunc("qoffset_y", write=TRUE, remove=remove)
makefunc("qoffset_z", write=TRUE, remove=remove)
makefunc("srow_x", write=TRUE, remove=remove)
makefunc("srow_y", write=TRUE, remove=remove)
makefunc("srow_z", write=TRUE, remove=remove)
makefunc("intent_name", write=TRUE, remove=remove)
makefunc("magic", write=TRUE, remove=remove)
makefunc("extender", write=TRUE, remove=remove)
makefunc("reoriented", write=TRUE, remove=remove)
