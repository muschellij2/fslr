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