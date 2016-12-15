#' SVD
#'
#' @author Adithya Murali
#' @param matrix is the dataset containing the observations.
#' @param quantile is the parameter used for selecting columns that achieves the degree of explanation
#' @return Returns a SVD barchar or list with vector with the column numbers that were found to be the key variables and the svd calculation
#' Subsets given data using column names or number and performs k-means clustering on the subset data with error handling.
#' @examples print(SVD(mtcars))
#' @seealso \code{kmeans}
#' @export

SVD <- function(matrix, quantile = 0.8, plot = TRUE, print = TRUE) {
  output <- getSvd(matrix, quantile, 0.9, 0.05)
  if (plot & print) {
    plot(output)
  }

  return(output)
}


#' A modified function to fit scalabale applications
#' @author max
getSvd <- function(mtrx, quantile, similarity_threshold,
                   plot_threshold = 0.05,
                   varnames	= NULL, plot = TRUE) {

  if(any(is.na(mtrx)))
    stop("Missing values are not allowed in this function. Impute prior to calling this function (try with mice or similar package).")

  if (quantile < 0 || quantile > 1)
    stop("The quantile mus be between 0-1")

  if (similarity_threshold < 0 || similarity_threshold > 1)
    stop("The similarity_threshold mus be between 0-1")

  svd_out <- svd(scale(mtrx))
  perc_explained <- svd_out$d^2/sum(svd_out$d^2)
  # Select the columns that we want to look at
  cols_expl <- which(cumsum(perc_explained) <= quantile)

  # Select the variables of interest
  getMostInfluentialVars <- function(){
    vars <- list()
    require("Hmisc")
    for (i in 1:length(perc_explained)){
      v_abs <- abs(svd_out$v[,i])
      maxContributor <- which.max(v_abs)
      similarSizedContributors <- which(v_abs >= v_abs[maxContributor]*similarity_threshold)
      if (any(similarSizedContributors %nin% maxContributor)){
        maxContributor <- similarSizedContributors[order(v_abs[similarSizedContributors], decreasing=TRUE)]
      }
      vars[[length(vars) + 1]] <- maxContributor
    }
    return(vars)
  }
  vars <- getMostInfluentialVars()

  plotSvdSelection <- function(){
    require(lattice)

    if (plot_threshold < 0 || plot_threshold > 1)
      stop("The plot_threshold mus be between 0-1")

    if (plot_threshold > similarity_threshold)
      stop(paste0("You can't plot less that you've chosen - it makes no sense",
                  " - the plot (", plot_threshold, ")",
                  " >",
                  " similarity (", similarity_threshold, ")"))

    # Show all the bars that are at least at the threshold
    # level and group the rest into a "other category"
    bar_count <- length(perc_explained[perc_explained >= plot_threshold])+1
    if (bar_count > length(perc_explained)){
      bar_count <- length(perc_explained)
      plot_percent <- perc_explained
    }else{
      plot_percent <- rep(NA, times=bar_count)
      plot_percent <- perc_explained[perc_explained >= plot_threshold]
      plot_percent[bar_count] <- sum(perc_explained[perc_explained < plot_threshold])
    }

    # Create transition colors
    selected_colors <- colorRampPalette(c("darkgreen", "#FFFFFF"))(bar_count+2)[cols_expl]
    nonselected_colors <- colorRampPalette(c("darkgrey", "#FFFFFF"))(bar_count+2)[(max(cols_expl)+1):bar_count]

    max_no_print <- 4
    names <- unlist(lapply(vars[1:bar_count], FUN=function(x){
      if (is.null(varnames)){
        varnames <- colnames(mtrx)
      }
      if (length(x) > max_no_print)
        ret <- paste(c(varnames[1:(max_no_print-1)],
                       sprintf("+ %d other", length(x) + 1 - max_no_print)), collapse="\n")
      else
        ret <- paste(varnames[x], collapse="\n")

      return(ret)
    }))
    rotation <- 45 + (max(unlist(lapply(vars[1:bar_count],
                                        function(x) {
                                          min(length(x), max_no_print)
                                        })))-1)*(45/max_no_print)

    if (bar_count < length(perc_explained)){
      names[bar_count] <- "Other"
      nonselected_colors[length(nonselected_colors)] <- grey(.5)
    }

    las <- 2
    m <- par(mar=c(8.1, 4.1, 4.1, 2.1))
    on.exit(par(mar=m))

    plot <- barchart(plot_percent * 100 ~ 1:bar_count,
                   horiz=FALSE,
                   ylab="Percentage explained (%D)",
                   main="SVD - the maximum contributors defined by V column",
                   xlab="Pattern contributing variables",
                   col=c(selected_colors, nonselected_colors),
                   key=list(text=list(c("Selected", "Not selected")),
                            rectangles=list(col=c("darkgreen", "#777777"))),
                   scales=list(x=list(rot=rotation, labels=names)))
    return (plot);
  }

  if (plot)
    return (plotSvdSelection())
  else {
    ret <- list(most_influential = unique(unlist(vars[cols_expl])),
                svd = svd_out)
    return(ret)
  }
}

