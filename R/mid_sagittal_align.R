# rm(list= ls())
# library(extrantsr)
# library(fslr)
# library(R.matlab)
# fname = '/Users/johnmuschelli/Dropbox/CTR/DHanley/CT_Registration/Seg_Release_data/seg_release_paper/seg_release_paper/TCGA-GBM/Head_NC_Only/ss/Tcga140789_TCGA140789_0_2213570360326183_19971121_Routine_Head_221636.nii.gz'
# img = readnii(fname)
# img[ img < 0 ] = 0


mid_sagittal_align = function(img) {
  
  interpolator = "Linear"
  rp = rpi_orient_file(img)
  
  img = rp$img
  img = check_nifti(img)
  dd = dropEmptyImageDimensions(
    img > 0,
    other.imgs = img)
  img = dd$other.imgs
  
  flip_lr = function(x){
    fsl_swapdim(file = x, a = "-x")
  }
  
  flipped = flip_lr(img)
  
  # reg = registration(filename = img,
  #                    skull_strip = FALSE,
  #                    correct = FALSE,
  #                    template.file = flipped,
  #                    typeofTransform = "Rigid",
  #                    interpolator = interpolator)
    
  # amat = readMat(reg$fwdtransforms)
  # amat = amat$AffineTransform.float.3.3
  # amat = matrix(amat, ncol = 4, byrow = FALSE)
  # 
  omat = tempfile(fileext = ".mat")
  outfile = tempfile(fileext = ".nii.gz")
  ff = flirt(infile = img, 
             reffile = flipped,
             omat = omat, dof = 6,
             retimg = FALSE, outfile = outfile)  
  
  
  
  mat = readLines(omat)
  mat = trimws(mat)
  mat = strsplit(mat, " ")
  mat = lapply(mat, function(x) x[ !x %in% ""])
  mat = lapply(mat, as.numeric)
  mat = do.call("rbind", mat)
  xmat = mat
  
  # library(expm)
  # A = xmat[1:3, 1:3]
  # AtA = t(A) %*% A
  # AtA[ abs(AtA) < 1e-8] = 0
  # S = solve(A %*% t(A))
  # sqrtAtA = sqrtm(AtA)
  # R = solve(sqrtAtA) %*% A
  # A2 = S%*% R*0.5
  # newmat = xmat
  # newmat[1:3,4] = xmat[1:3,4] * 0.5
  # newmat[1:3, 1:3] = A2
  
  
  scaled = fsl_avscale(file = omat)
  parsed = parse_avscale(scaled)
  
  mat = parsed$fwd_half_transform
  # mat = mat * 0.5
  
  new_omat = tempfile(fileext = ".mat")
  mat = apply(mat, 1, paste, collapse = " ")
  mat = paste0(mat, " ")
  writeLines(mat, new_omat)
  
  centered = flirt_apply(infile = img, reffile = flipped, 
                         initmat = new_omat)
  
  
}