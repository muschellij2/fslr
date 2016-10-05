library(fslr)
x = readLines("NAMESPACE")
x = grep("^export\\(", x, value = TRUE)
x = gsub("export\\((.*)\\)", "\\1", x)
x = grep("^fsl", x, value = TRUE)
funcs = grep("[.]help$", x, value = TRUE, invert = TRUE)
u_funcs = grep("fsl_", funcs, value = TRUE, invert = FALSE)
funcs = grep("fsl_", funcs, value = TRUE, invert = TRUE)
help_funcs = paste0(funcs, ".help")
df = data.frame(f = funcs,
                has_help = help_funcs %in% x,
                stringsAsFactors = FALSE)

stopifnot(all(funcs %in% 
            c("fsl_anat", "fsl_applywarp", "fsl_atlas_dir", "fsl_biascorrect", 
              "fsl_data_dir", "fsl_dir", "fsl_std_dir", "fsl_version", "fslabs", 
              "fslacos", "fsladd", "fslasin", "fslatan", "fslbet", "fslbin", 
              "fslbinv", "fslchfiletype", "fslcmd", "fslcog", "fslcos", "fslcpgeom", 
              "fsldilate", "fsldir", "fsldiv", "fsledge", "fslentropy", "fslerode", 
              "fslexp", "fslfill", "fslfill2", "fslgetorient", "fslgetqform", 
              "fslgetqformcode", "fslgetsform", "fslgetsformcode", "fslhd", 
              "fslhd.parse", "fslhelp", "fslindex", "fsllog", "fslmask", "fslmaths", 
              "fslmean", "fslmerge", "fslmul", "fslnan", "fslnanm", "fslorient", 
              "fslrand", "fslrandn", "fslrange", "fslrecip", "fslrem", "fslreorient2std", 
              "fslroi", "fslsd", "fslsin", "fslslicetimer", "fslsmooth", "fslsplit", 
              "fslsqr", "fslsqrt", "fslstats", "fslsub", "fslsub2", "fslsum", 
              "fslswapdim", "fsltan", "fslthresh", "fslval", "fslversion", 
              "fslview", "fslvol", "fslvolume"))
)

nochange = c("fslhd.parse", "fslcmd", "fsldir", "fslhelp",
             "fslchfiletype", "fslcog", "fslcpgeom", 
             "fslentropy", "fslgetorient", "fslgetqform", "fslgetqformcode", 
             "fslgetsform", "fslgetsformcode", 
             "fslhd", "fslmean", "fslorient", "fslrange",  
             "fslreorient2std", "fslfill2",
             "fslroi", "fslsd", "fslsin", 
             "fslsplit", "fslstats", "fslsum", "fslval", 
             "fslversion", "fslview", "fslvol", "fslvolume")

df = df[ !(df$f %in% nochange),]
#############################################
# Function maker that involves 1 file
#############################################
makefunc = function(func_name, remove = FALSE){
  
  stopifnot(grepl("^fsl", func_name))
  
  no_underscore = func_name
  underscore = gsub("^fsl", "fsl_", no_underscore)
  
  x = readLines('fslr_underscore_functions.R')
  x = gsub("%underscore%", underscore, x)
  x = gsub("%nounderscore%", no_underscore, x)

  fname = paste0("R/", underscore, ".R")
  writeLines(text = x, con = fname)
  if (remove) {
    file.remove(fname)
  }
    
  return(TRUE)
}

sources = sapply(df$f, function(x){
  as.character(head(get(x), 1000))
})
any_fsl_stats = sapply(sources, function(x) {
  any(grepl("fslstats", x))
  })
stopifnot(all(!any_fsl_stats))

underscores = df$f
underscore = gsub("^fsl", "fsl_", underscores)
fnames = paste0("R/", underscore, ".R")
# file.remove(fnames)
stopifnot(all(!file.exists(fnames)))


################################################################
# Make Functions that take in 1 file
################################################################
# makefunc("r", FUNC = "Robust Range", func = "rrange",
#          remove = TRUE)
sapply(df$f, makefunc)




