# fslr 2.25.2

- Pushing back to CRAN after `neurobase` is back on.

# fslr 2.25.1

- Moved `orient_rpi*` functionality to `neurobase` rather than `fslr` due to dependencies.
- Added `fsl_std_file`.

# fslr 2.25.0

- Added `rpi*` functionality that uses `RNifti` and is likely what we'd want to use.

# fslr 2.24.0

- Fixes bug with `getForms` (and therefore a lot of stuff) with FSL version 6.0 and greater.

# fslr 2.24.0

- Added `quickshear_deface_image` to remove faces based on the QuickShear method and `nipy/quickshear`.
- Added `mridefacer` to add to the anonymization pipelines.
- Added `invert_xfm` and other `convert_xfm` functionality.
- Added `fslreorient2std_mat` to get the matrix output from `fslreorient2std`.


# fslr 2.23.0

- Added `face_removal_mask` for removal of faces, based on `pydeface`.
- Fixed `fslsmooth` to return an output file if `retimg = FALSE`.

# fslr 2.22.0

- fixed bug in `reverse_rpi_orient_file` that was causing `ERROR: Could not open file /usr/local/fsl/bin/fslswapdim: 107: [: =: unexpected operator` error.
- added `topup` to the exports.
- added `fsl_resample`.
- Added `fsl_cluster`.   Removed `cluster` from exports due to conflicts.
- added `read_xfm` for reading transformations.
- Fixes `mid_sagittal_align` for flipping.
- Removed `matrixStats` as unnecessary dependency.
 

# fslr 2.17.3

- Added functionality of `topup` and other DTI-based tools.
- Tried to get `susan` to work properly.
- Need to incorporate `fsleyes` in the future release.

# fslr v1.6.0 (Release date: 2015-12-01)

## Major changes

* Changed default reading/writing to `readnii`/`writenii`

# fslr v1.5.1 (Release date: 2015-10-20)

## Major changes

* Added `readnii` and `writenii`

* Added `ortho_diff`

# fslr v1.4-4 (Release date: 2015-05-20)

## Major Changes

* Moved functions such as cal_img to oro.nifti package.  They are all legacies, such as cal_img still works, but calibrateImage and other camelCase functions have been made default.

* Fixed fsl_biascorrect.  