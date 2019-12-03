#' @title Probabilistic diffusion tractography with multiple fibre orientations
#' @description This function wraps \code{probtrackx2} from FSL 
#' 
#' @param samples Basename for samples files - e.g. 'merged'
#' @param mask Bet binary mask file in diffusion space
#' @param seed Seed volume or list (ascii text file) of volumes and/or surfaces
#' @param verbose Verbose level, [0-2]
#' @param out Output file (default='fdt_paths')
#' @param dir Directory to put the final volumes in - code makes this directory - default='logdir'
#' @param forcedir Use the actual directory name given - i.e. don't add + to make a new directory
#' @param simple Track from a list of voxels (seed must be a ASCII list of coordinates)
#' @param network Activate network mode - only keep paths going through at least one of the other seed masks
#' @param opd Output path distribution
#' @param pd Correct path distribution for the length of the pathways
#' @param fopd Other mask for binning tract distribution
#' @param os2t Output seeds to targets
#' @param s2tastext Output seed-to-target counts as a text file (default in simple mode)
#' @param targetmasks File containing a list of target masks - for `seeds_to_targets` classification
#' @param waypoints Waypoint mask or ascii list of waypoint masks - only keep paths going through ALL the masks
#' @param waycond Waypoint condition. Either 'AND' (default) or 'OR'
#' @param wayorder Reject streamlines that do not hit waypoints in given order. Only valid if waycond=AND
#' @param onewaycondition Apply waypoint conditions to each half tract separately
#' @param avoid Reject pathways passing through locations given by this mask
#' @param stop Stop tracking at locations given by this mask file
#' @param omatrix1 Output matrix1 - SeedToSeed Connectivity
#' @param distthresh1 Discards samples (in matrix1) shorter than this threshold (in mm - default=0)
#' @param omatrix2 Output matrix2 - SeedToLowResMask
#' @param target2 Low resolution binary brain mask for storing connectivity distribution in matrix2 mode
#' @param omatrix3 Output matrix3 (NxN connectivity matrix)
#' @param target3 Mask used for NxN connectivity matrix (or Nxn if lrtarget3 is set)
#' @param lrtarget3 Column-space mask used for Nxn connectivity matrix
#' @param omatrix4 Output matrix4 - DtiMaskToSeed (special Oxford Sparse Format)
#' @param colmask4 Mask for columns of matrix4 (default=seed mask)
#' @param target4 Brain mask in DTI space
#' @param distthresh3 Discards samples (in matrix3) shorter than this threshold (in mm - default=0)
#' @param xfm Transform taking seed space to DTI space (either FLIRT matrix or FNIRT warpfield) - default is identity
#' @param invxfm Transform taking DTI space to seed space (compulsory when using a warpfield for seeds_to_dti)
#' @param seedref Reference vol to define seed space in simple mode - diffusion space assumed if absent
#' @param meshspace Mesh reference space - either 'caret' (default) or 'freesurfer' or 'first' or 'vox'
#' @param nsamples Number of samples - default=5000
#' @param nsteps Number of steps per sample - default=2000
#' @param step_length Steplength in mm - default=0.5
#' @param distthresh Discards samples shorter than this threshold (in mm - default=0)
#' @param curvature_threshold Curvature threshold - default=0.2
#' @param fibthresh Volume fraction before subsidiary fibre orientations are considered - default=0.01
#' @param loopcheck Perform loopchecks on paths - slower, but allows lower curvature threshold
#' @param usef Use anisotropy to constrain tracking
#' @param modeuler Use modified euler streamlining
#' @param sampvox Sample random points within x mm sphere seed voxels (e.g. --sampvox=5). Default=0
#' @param randfib Default 0. Set to 1 to randomly sample initial fibres (with f > fibthresh).  Set to 2 to sample in proportion fibres (with f>fibthresh) to f.  Set to 3 to sample ALL populations at random (even if f<fibthresh)
#' @param fibst Force a starting fibre for tracking - default=1, i.e. first fibre orientation. Only works if randfib==0
#' @param rseed Random seed
#' @param mesh Freesurfer-type surface descriptor (in ascii format)
#' @param mask2 Second mask in `twomask_symm` mode.
#' @param ompl Output mean path length from seed

#' @param opts Additional options or way to specify things instead of command
#' line arguments
#' @param ... Additional arguments
probtrackx2 = function(
  samples = "merged",
  mask,
  seed,
  verbose = TRUE,
  outfile = NULL,
  mode = NULL,
  mask2 = NULL,
  dir = NULL,
  forcedir = FALSE,
  simple = NULL,
  network = FALSE,
  opd = NULL,
  pd = FALSE,
  ompl = FALSE,
  fopd = NULL,
  os2t = FALSE,
  s2tastext = NULL,
  targetmasks = NULL,
  waypoints = NULL,
  waycond = c("AND", "OR"),
  wayorder = NULL,
  onewaycondition = FALSE,
  avoid = NULL,
  stop = NULL,
  omatrix1 = NULL,
  distthresh1 = 0,
  omatrix2 = NULL,
  target2 = NULL,
  omatrix3 = NULL,
  target3 = NULL,
  lrtarget3 = NULL,
  distthresh3 = 0,
  omatrix4 = NULL,
  colmask4 = NULL,  
  target4 = NULL,  
  xfm = NULL,
  mesh = NULL,
  invxfm = NULL,
  seedref = NULL,
  meshspace = c("caret", "freesurfer", "first", "vox"),
  n_samples = 5000,
  n_steps = 2000,
  step_length = 0.5,
  distthresh = 0,
  curvature_threshold = 0.2,
  fibthresh = 0.01,
  loopcheck = FALSE,
  usef = FALSE,
  modeuler = FALSE,
  sampvox = 0,
  randfib = 0, # max 3
  fibst = 1,
  rseed = NULL,
  opts = "",
  ...
){
  if (!all(is.character(seed))) {
    seed = checkimg(seed)
  }
  if (!is.character(mask)) {
    mask = checkimg(mask)
  }
  if (!is.null(mask2)) {
    mask2 = checkimg(mask2)
  }
  if (!is.null(waypoints) && !is.character(waypoints)) {
    waypoints = checkimg(waypoints)
  }
  if (is.null(outfile)) {
    outfile = tempfile(fileext = ".nii.gz")
  }
  parse_args = function(x){
    x = paste0(names(x), '=', x)
    x = paste(x, collapse = " ")
    x
  }
  meshspace = match.arg(meshspace)
  
  
  args = list(
    samples = samples, 
    mask = mask, 
    seed = seed, 
    verbose = as.numeric(verbose), 
    mode = mode, 
    targetmasks = targetmasks, 
    mask2 = mask2, 
    waypoints = waypoints, 
    network = network, 
    mesh = mesh, 
    seedref = seedref, 
    dir = dir, 
    forcedir = forcedir, 
    opd = opd, 
    pd = pd, 
    os2t = os2t, 
    out = outfile, 
    avoid = avoid, 
    stop = stop, 
    xfm = xfm, 
    invxfm = invxfm, 
    nsamples = n_samples, 
    nsteps = n_steps, 
    distthresh = distthresh, 
    cthr = curvature_threshold, 
    fibthresh = fibthresh, 
    sampvox = sampvox, 
    steplength = step_length, 
    loopcheck = loopcheck, 
    usef = usef, 
    randfib = randfib, 
    fibst = fibst, 
    modeuler = modeuler, 
    ompl = ompl,
    rseed = rseed)
  null_args = sapply(args, is.null)
  args = args[!null_args]
  for (iarg in c("target2", "target3", "lrtarget3", 
                 "target4", "colmask4")) {
    arg  = args[[iarg]]
    if (!is.null(arg) && !is.character(arg)) {
      arg = checkimg(arg)
    }
    args[[iarg]] = arg
  }
  
  opts = paste(opts, collapse = " ")
  args = args[ names(args) != ""]
  
  
  nums = c("distthresh3", "nsamples", "nsteps", "steplength", "distthresh",
           "distthresh1",
           "cthr", "fibthresh", "sampvox", "randfib", "fibst")
  chars = c("samples",
            "mask",
            "seed",
            "out", "dir","opd", "fopd", "s2tastext", "targetmasks",
            "waypoints", "waycond", "wayorder", "avoid", "stop", "omatrix1",
            "distthresh1", "omatrix2", "target2", "omatrix3", "target3",
            "lrtarget3", "xfm", "invxfm", "seedref", "meshspace", "rseed"
  )
  logs = c("verbose", "forcedir", "simple", "network", "pd", "os2t", "onewaycondition",
           "loopcheck", "usef",   "modeuler")
  
  vec = c("--data" = infile,
          "--mask" = mask,
          "--bvecs" = bvecs,
          "--bvals" = bvals)
  vec = shQuote(vec)
  vec = parse_args(vec)
  
  ####need to unname
  num_vec = c(
    "--nfibres" = nfibres,
    "--njumps" = njumps,
    "--burnin" = burnin,
    "--burnin_noard" = burnin_noard,
    "--sampleevery" = sampleevery,
    "--updateproposalevery" = updateproposalevery)
  num_vec = parse_num_args(num_vec)
  vec = paste(vec, num_vec)
  
  
  verbose = as.numeric(verbose)
  make_args
  # 
  
  
}
# samples = NULL, 
# mask = NULL, 
# seed = NULL, 
# verbose = NULL, 
# help = NULL, 
# mode = NULL, 
# targetmasks = NULL, 
# mask2 = NULL, 
# waypoints = NULL, 
# network = NULL, 
# mesh = NULL, 
# seedref = NULL, 
# dir = NULL, 
# forcedir = NULL, 
# opd = NULL, 
# pd = NULL, 
# os2t = NULL, 
# out = NULL, 
# avoid = NULL, 
# stop = NULL, 
# xfm = NULL, 
# invxfm = NULL, 
# nsamples = NULL, 
# nsteps = NULL, 
# distthresh = NULL, 
# cthr = NULL, 
# fibthresh = NULL, 
# sampvox = NULL, 
# steplength = NULL, 
# loopcheck = NULL, 
# usef = NULL, 
# randfib = NULL, 
# fibst = NULL, 
# modeuler = NULL, 
# rseed = NULL, 
# s2tastext = NULL,                       