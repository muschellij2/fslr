#' @title Plot differences for Prediction and Gold Standard
#'
#' @description Uses \code{\link{ortho2}} to plot differences between a predicted binary
#' image and the assumed ground truth (\code{roi}).
#' @param img image to be underlaid
#' @param pred binary segmentation (prediction)
#' @param roi binary manual segmentation (ground truth)
#' @param cols colors for false negatives/positives
#' @param levels labels for false negatives/positives
#' @param addlegend add legend, passed to \code{\link{ortho2}}
#' @param center run \code{\link{xyz}} on \code{roi}
#' @param leg.cex multiplier for legend size
#' @param ... arguments to be passed to \code{\link{ortho2}}
#' @export
#' @seealso \code{\link{ortho2}}
#' @return NULL
ortho_diff <- function(img, 
                       pred, # binary segmentation (prediction)
                      roi, # binary manual segmentation (ground truth)
                      cols = c("#56B4E9", "#D55E00", "#009E73"), # colors for false negatives, positives
                      levels = c("False Negative", "False Positive", "True Positive"), # labels for false negatives, positives
                      addlegend = TRUE, # add legend
                      center = TRUE, # run \code{\link{xyz}} on \code{roi} 
                      leg.cex = 1.5,  # multiplier for legend size
                      ...
){
  
  ###########################
  ### Drop empty image dimensions
  ###########################
  if (center) {
    xyz = xyz(roi)
  } else {
    xyz = NULL
  }
  
  pred = pred > 0
  roi = roi > 0
  
  diff = niftiarr(pred, NA)
  # false negative
  diff[ roi %in% 1 & pred %in% 0] = 1
  # false positive
  diff[ roi %in% 0 & pred %in% 1] = 2
  # true positive
  diff[ roi %in% 1 & pred %in% 1] = 3
  diff = cal_img(diff)
  
  ortho2(x = img, 
         y = diff, 
         # don't do alpha blending
         col.y = cols,
         xyz = xyz, 
         addlegend = addlegend,
         legend = levels, 
         leg.col = cols, 
         leg.cex = leg.cex,
         ybreaks = c(0, 1.1, 2.1, 3.1), 
         ...
  )
  return(invisible(NULL))
}
