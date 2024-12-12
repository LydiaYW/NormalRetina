#' Visualize Retina Sensitivity Data
#' @param pred_sens A data frame containing predicted sensitivity data from interpolation or prediction.
#' @param exam Exam type.
#' @import ggplot2
#' @export
VisualRetina <- function(pred_sens, exam) {
  # Define color mappings for the exam types
  color_mapping <- list(
    "Mesopic" = scale_fill_gradient2(limits = c(10, 28), low = "orange", mid = "yellow", high = "green", midpoint = 22),
    "Cyan" = scale_fill_gradient2(limits = c(0, 25), low = "black", mid = "blue", high = "cyan", midpoint = 10),
    "Red" = scale_fill_gradient2(limits = c(10, 25), low = "white", high = "red", midpoint = 15),
    "CRdiff" = scale_fill_gradient2(limits = c(-20, 5), low = "red", mid = "purple", high = "cyan", midpoint = -10)
  )

  # Ensure the exam type is valid
  if (!exam %in% c('Mesopic', 'Cyan', 'Red', 'CRdiff')) {
    stop("Invalid exam type. Choose from 'Mesopic', 'Cyan', 'Red', or 'CRdiff'.")
  }

  # Extract the color mapping for the specified exam type
  fill_colors <- color_mapping[[exam]]

  # Generate the plot
  plot <- ggplot(pred_sens, aes(x = X_corr, y = Y_corr, z = mean)) +
    geom_tile(aes(fill = mean), alpha = 0.7) +
    stat_contour(binwidth = 1, size = 0.5, colour = "black") +
    fill_colors +
    xlab('Temporal - nasal [deg]') +
    ylab('Inferior - superior [deg]') +
    xlim(-10,10) +
    ylim(-10,10) +
    ggtitle(paste("Examtype: ", exam))

  return(plot)
}
