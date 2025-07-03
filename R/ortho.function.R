#function for orthopho import from Rstoolbox
.toRaster <- function(x) {
  if (inherits(x, "SpatRaster")) {
    return(stack(x))
  } else {
    return(x)
  }
}

.numBand <- function(raster, ...){
  bands <- list(...)
  lapply(bands, function(band) if(is.character(band)) which(names(raster) == band) else band ) 
}
ggRGB<-function(img, r = 3, g = 2, b = 1, scale, maxpixels = 5e+05, 
                stretch = "none", ext = NULL, limits = NULL, clipValues = "limits", 
                quantiles = c(0.02, 0.98), ggObj = TRUE, ggLayer = FALSE, 
                alpha = 1, coord_equal = TRUE, geom_raster = FALSE, nullValue = 0) 
{
  img <- .toRaster(img)
  verbose <- getOption("RStoolbox.verbose")
  annotation <- !geom_raster
  rgb <- unlist(.numBand(raster = img, r, g, b))
  nComps <- length(rgb)
  if (inherits(img, "RasterLayer")) 
    img <- brick(img)
  rr <- sampleRegular(img[[rgb]], maxpixels, ext = ext, asRaster = TRUE)
  RGB <- getValues(rr)
  if (!is.matrix(RGB)) 
    RGB <- as.matrix(RGB)
  if (!is.null(limits)) {
    if (!is.matrix(limits)) {
      limits <- matrix(limits, ncol = 2, nrow = nComps, 
                       byrow = TRUE)
    }
    if (!is.matrix(clipValues)) {
      if (!anyNA(clipValues) && clipValues[1] == "limits") {
        clipValues <- limits
      }
      else {
        clipValues <- matrix(clipValues, ncol = 2, nrow = nComps, 
                             byrow = TRUE)
      }
    }
    for (i in 1:nComps) {
      if (verbose) {
        message("Number of pixels clipped in ", 
                c("red", "green", "blue")[i], 
                " band:\n", "below limit: ", sum(RGB[, 
                                                     i] < limits[i, 1], na.rm = TRUE), " | above limit: ", 
                sum(RGB[, i] > limits[i, 2], na.rm = TRUE))
      }
      RGB[RGB[, i] < limits[i, 1], i] <- clipValues[i, 
                                                    1]
      RGB[RGB[, i] > limits[i, 2], i] <- clipValues[i, 
                                                    2]
    }
  }
  rangeRGB <- range(RGB, na.rm = TRUE)
  if (missing("scale")) {
    scale <- rangeRGB[2]
  }
  if (rangeRGB[1] < 0) {
    RGB <- RGB - rangeRGB[1]
    scale <- scale - rangeRGB[1]
    rangeRGB <- rangeRGB - rangeRGB[1]
  }
  if (scale < rangeRGB[2]) {
    warning("Scale < max value. Resetting scale to max.", 
            call. = FALSE)
    scale <- rangeRGB[2]
  }
  RGB <- na.omit(RGB)
  if (stretch != "none") {
    stretch <- tolower(stretch)
    for (i in seq_along(rgb)) {
      RGB[, i] <- .stretch(RGB[, i], method = stretch, 
                           quantiles = quantiles, band = i)
    }
    scale <- 1
  }
  naind <- as.vector(attr(RGB, "na.action"))
  nullbands <- sapply(list(r, g, b), is.null)
  if (any(nullbands)) {
    RGBm <- matrix(nullValue, ncol = 3, nrow = NROW(RGB))
    RGBm[, !nullbands] <- RGB
    RGB <- RGBm
  }
  if (!is.null(naind)) {
    z <- rep(NA, times = ncell(rr))
    z[-naind] <- rgb(RGB[, 1], RGB[, 2], RGB[, 3], max = scale, 
                     alpha = alpha * scale)
  }
  else {
    z <- rgb(RGB[, 1], RGB[, 2], RGB[, 3], max = scale, alpha = alpha * 
               scale)
  }
  df_raster <- data.frame(coordinates(rr), fill = z, stringsAsFactors = FALSE)
  x <- y <- fill <- NULL
  if (ggObj) {
    exe <- as.vector(extent(rr))
    df <- data.frame(x = exe[1:2], y = exe[3:4])
    if (annotation) {
      dz <- matrix(z, nrow = nrow(rr), ncol = ncol(rr), 
                   byrow = TRUE)
      p <- ggplot2::annotation_raster(raster = dz, xmin = exe[1], 
                                      xmax = exe[2], ymin = exe[3], ymax = exe[4], 
                                      interpolate = FALSE)
      if (!ggLayer) {
        p <- ggplot2::ggplot() + p + ggplot2::geom_blank(data = df, aes(x = x, 
                                                                        y = y))
      }
    }
    else {
      p <- ggplot2::geom_raster(data = df_raster, aes(x = x, y = y, 
                                                      fill = fill), alpha = alpha)
      if (!ggLayer) {
        p <- ggplot2::ggplot() + p + ggplot2::scale_fill_identity()
      }
    }
    if (coord_equal & !ggLayer) 
      p <- p + ggplot2::coord_equal()
    return(p)
  }
  else {
    return(df_raster)
  }
}