## ----setup, include=FALSE------------------------------------------------
library(knitr)

## ------------------------------------------------------------------------
tdir = tempdir()
tfile = file.path(tdir, "example_dwi.zip")
download.file("http://cmic.cs.ucl.ac.uk/camino//uploads/Tutorials/example_dwi.zip",destfile = tfile)
out = unzip(zipfile = tfile, exdir = tdir, overwrite = TRUE)

## ----bvecs---------------------------------------------------------------
library(fslr)
b_data_file = grep("[.]txt$", out, value = TRUE)
b_vecs = read.delim2(b_data_file, header = FALSE)
b_vecs = as.matrix(b_vecs)
class(b_vecs) = "numeric"

## ----b_values------------------------------------------------------------
b_vals = rep(1000, nrow(b_vecs))
all_zero = apply(b_vecs  == 0, 1, all)
b_vals[all_zero] = 0

## ----check_img-----------------------------------------------------------
img = grep("4Ddwi_b1000", out, value = TRUE)
nim = readnii(img)
n_timepoints = dim(nim)[4]
stopifnot(nrow(b_vecs) == n_timepoints)

## ------------------------------------------------------------------------
if (have.fsl()) {
  outfile = tempfile(fileext = ".nii.gz")
  ret = eddy_correct(infile = img, outfile = outfile, 
                     retimg = TRUE, reference_no = 0)
}

## ----dtifit--------------------------------------------------------------
if (have.fsl()) {
  mask_fname = grep("mask", out, value = TRUE)
  res = dtifit(infile = ret, bvecs = b_vecs, 
               bvals = b_vals, mask = mask_fname)
}

## ----read_res------------------------------------------------------------
if (have.fsl()) {
  res_imgs = lapply(res, readnii)
}

## ------------------------------------------------------------------------
if (have.fsl()) {
  ortho2(res_imgs$FA)
}

## ------------------------------------------------------------------------
if (have.fsl()) {
  ortho2(res_imgs$MD)
}

## ------------------------------------------------------------------------
if (have.fsl()) {
  double_ortho(res_imgs$FA, res_imgs$MD)
}

## ------------------------------------------------------------------------
if (have.fsl()) {
  mask = readnii(mask_fname)
  df = data.frame(FA = res_imgs$FA[ mask == 1], MD = res_imgs$MD[ mask == 1] )
  plot(df)
}

## ---- eval = FALSE-------------------------------------------------------
#  xfibres(infile = outfile,
#          bvecs = b_vecs,
#          bvals = b_vals,
#          mask = mask_fname)

