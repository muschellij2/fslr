library(fslr)
library(extrantsr)

base_url = "https://github.com/poldracklab/pydeface/raw/master/pydeface/data"
mask_file = tempfile(fileext = ".nii.gz")
download.file(file.path(base_url, "facemask.nii.gz"), destfile = mask_file)
infile = tempfile(fileext = ".nii.gz")
download.file(file.path(base_url, "mean_reg2mean.nii.gz"), destfile = infile)

mm = "2"
outfile = file.path("inst", "extdata",
                    paste0("MNI152_T1_", mm, "mm_facemask.nii.gz"))
if (!file.exists(outfile)) {
  temp = mni_fname(mm = mm)
  tfile = tempfile(fileext = ".nii.gz")
  mask = readnii(mask_file) + 1
  res = registration(filename = infile, 
                     template.file = temp, other.files = mask,
                     typeofTransform = "Rigid",
                     other.outfiles = tfile, 
                     other_interpolator = "NearestNeighbor")
  # tfile = ants_apply_transforms(fixed = temp, moving = mask, 
  # transformlist = res$fwdtransforms, interpolator = "NearestNeighbor")
  # img = readnii(temp)
  m = readnii(tfile)
  m = m != 1
  writenii(m, outfile)
}
