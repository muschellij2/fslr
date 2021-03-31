#' @title Probabilistic diffusion tractography with multiple fibre orientations
#' @description This function wraps \code{probtrackx} from FSL 
#' 
#' @param samples (nifti/character) Basename for samples files
#' @param mask (nifti/character) Bet binary mask file in diffusion space
#' @param seed (nifti/character) Seed volume, or voxel, or ascii file with multiple volumes, or freesurfer label file
#' @param verbose (logical/numeric) Verbose level, [0-2]
#' @param mode (character) Use --mode=simple for single seed voxel
#' @param targetmasks (character) File containing a list of target masks - required for seeds_to_targets classification
#' @param mask2 (nifti/character) Second mask in twomask_symm mode.
#' @param waypoints (nifti/character) Waypoint mask or ascii list of waypoint masks - only keep paths going through ALL the masks
#' @param network (logical) Activate network mode - only keep paths going through at least one seed mask (required if multiple seed masks)
#' @param mesh (character) Freesurfer-type surface descriptor (in ascii format)
#' @param seedref (nifti/character) Reference vol to define seed space in simple mode - diffusion space assumed if absent
#' @param dir (logical) Directory to put the final volumes in - code makes this directory - default='logdir'
#' @param forcedir (logical) Use the actual directory name given - i.e. don't add + to make a new directory
#' @param opd (logical) Output path distribution
#' @param pd (logical) Correct path distribution for the length of the pathways
#' @param os2t (logical) Output seeds to targets
#' @param outdir (character) Output file (default='fdt_paths')
#' @param avoid (nifti/character) Reject pathways passing through locations given by this mask
#' @param stop (nifti/character) Stop tracking at locations given by this mask file
#' @param xfm (character) Transform taking seed space to DTI space (either FLIRT matrix or FNIRT warpfield) - default is identity
#' @param invxfm (character) Transform taking DTI space to seed space (compulsory when using a warpfield for seeds_to_dti)
#' @param nsamples (numeric) Number of samples - default=5000
#' @param nsteps (numeric) Number of steps per sample - default=2000
#' @param distthresh (numeric) Discards samples shorter than this threshold (in mm - default=0)
#' @param cthr (numeric) Curvature threshold - default=0.2
#' @param fibthresh (numeric) Volume fraction before subsidiary fibre orientations are considered - default=0.01
#' @param sampvox (logical) Sample random points within seed voxels
#' @param steplength (numeric) Steplength in mm - default=0.5
#' @param loopcheck (logical) Perform loopchecks on paths - slower, but allows lower curvature threshold
#' @param usef (logical) Use anisotropy to constrain tracking
#' @param randfib (numeric) Default 0. Set to 1 to randomly sample initial fibres (with f > fibthresh). Set to 2 to sample in proportion fibres (with f>fibthresh) to f. Set to 3 to sample ALL populations at random (even if f<fibthresh)
#' @param fibst (numeric) Force a starting fibre for tracking - default=1, i.e. first fibre orientation. Only works if randfib==0
#' @param modeuler (logical) Use modified euler streamlining
#' @param rseed (numeric) Random seed
#' @param s2tastext (logical) Output seed-to-target counts as a text file (useful when seeding from a mesh)
#' @param opts Additional options or way to specify things instead of command
#' line arguments
#' 
#' @return A filename of the output file
#' @export
probtrackx = function(
  samples = "merged",
  mask,
  seed,
  outdir = "fdt_paths", 
  verbose = TRUE,
  mode = NULL, 
  targetmasks = NULL, 
  mask2 = NULL, 
  waypoints = NULL, 
  network = FALSE, 
  mesh = NULL, 
  seedref = NULL, 
  dir = FALSE, 
  forcedir = FALSE, 
  opd = FALSE, 
  pd = FALSE, 
  os2t = FALSE, 
  avoid = NULL, 
  stop = NULL, 
  xfm = NULL, 
  invxfm = NULL, 
  nsamples = 5000, 
  nsteps = 2000, 
  distthresh = 0, 
  cthr = 0.2, 
  fibthresh = 0.01, 
  sampvox = FALSE, 
  steplength = 0.5, 
  loopcheck = FALSE, 
  usef = FALSE, 
  randfib = c(0, 1, 2, 3), 
  fibst = 1, 
  modeuler = FALSE, 
  rseed = NULL, 
  s2tastext = FALSE,
  opts = ""
  ) {
  
  warning(paste0("probtrackx has not been tested thoroughly, ",
                 "use at your own risk!"))
  verbose = as.numeric(verbose)
  randfib = as.character(randfib)
  randfib = match.arg(randfib, choices = c("0", "1", "2", "3"))
  args = list(
    samples = samples, 
    mask = mask, 
    seed = seed, 
    verbose = verbose, 
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
    outdir = outdir, 
    avoid = avoid, 
    stop = stop, 
    xfm = xfm, 
    invxfm = invxfm, 
    nsamples = nsamples, 
    nsteps = nsteps, 
    distthresh = distthresh, 
    cthr = cthr, 
    fibthresh = fibthresh, 
    sampvox = sampvox, 
    steplength = steplength, 
    loopcheck = loopcheck, 
    usef = usef, 
    randfib = randfib, 
    fibst = fibst, 
    modeuler = modeuler, 
    rseed = rseed, 
    s2tastext = s2tastext)
  types = list(
    samples = "character", 
    mask = "nifti/character", 
    seed = "nifti/character", 
    verbose = "numeric", 
    mode = "character", 
    targetmasks = "character", 
    mask2 = "nifti/character", 
    waypoints = "nifti/character", 
    network = "logical", 
    mesh = "character", 
    seedref = "nifti/character", 
    dir = "logical", 
    forcedir = "logical", 
    opd = "logical", 
    pd = "logical", 
    os2t = "logical", 
    outdir = "character", 
    avoid = "nifti/character", 
    stop = "nifti/character", 
    xfm = "character", 
    invxfm = "character", 
    nsamples = "numeric", 
    nsteps = "numeric", 
    distthresh = "numeric", 
    cthr = "numeric", 
    fibthresh = "numeric", 
    sampvox = "logical", 
    steplength = "numeric", 
    loopcheck = "logical", 
    usef = "logical", 
    randfib = "numeric", 
    fibst = "numeric", 
    modeuler = "logical", 
    rseed = "numeric", 
    s2tastext = "logical")
  
  if (!all(is.character(seed))) {
    seed = checkimg(seed)
  }
  if (!is.character(mask)) {
    mask = checkimg(mask)
  }
  if (!is.null(mask2)) {
    mask2 = checkimg(mask2)
  }
  if (is.null(outdir)) {
    outdir = tempfile()
  }
  if (!is.null(waypoints) && !is.character(waypoints)) {
    waypoints = checkimg(waypoints)
  }
  null_args = sapply(args, is.null)
  args = args[!null_args]
  
  for (arg_name in seq(names(args))) {
    arg = args[[arg_name]]
    arg_type = types[[arg_name]]
    if (arg_type == "nifti/character") {
      arg = checkimg(arg)
    }
    if (grepl("character", arg_type)) {
      arg = path.expand(arg)
    }
    if (!is.null(arg)) {
      arg_length = length(arg)
      if (arg_length > 1) {
        stop(paste0(arg_name, " is multiple values!"))
      }
      #make it logical
      if (arg_type == "logical") {
        arg = as.logical(arg)
      }
      # steps = 0.5 becomes --steps=0.5
      if (arg_type != "logical") {
        arg = paste0("--", arg_name, "=", arg)
      } else {
        # just --sampvox or NULL (goes away)
        if (arg) {
          arg = paste0("--", arg_name)
        } else {
          arg = NULL
        }
      }
    }
    args[[arg_name]] = arg
  }
  null_args = sapply(args, is.null)
  args = args[!null_args]
  
  args = paste(args, collapse = " ")
  opts = paste(opts, collapse = " ")
  args = paste(args, opts)
  outdir = args$outdir
  
  cmd = get.fsl()
  cmd = paste(cmd, args)
  if (verbose > 0) {
    message(cmd, "\n")
  }
  res = system(cmd, intern = FALSE)
  attr(outdir, "result") = res
  outdir 
}