#' One-Way Anova
#'
#' @details One-Way Anova function ...
#'
#' @param dataset Dataset
#' @param vars The treatments variables
#' @param data_filter Expression entered in, e.g., Data > View to filter the dataset in Radiant. The expression should be a string (e.g., "price > 10000")
#' @param envir Environment to extract data from
#' @param sum_check Optional output. "tukey" to show the Tukey Multiple Pairwise Comparisons. "posthoc" to show simultaneous tests for General Linear Hypotheses.
#'
#' @return A list of all variables used in the one-way anova function as an object of class avar
#'
#' @seealso \code{\link{summary.avar}} to summarize results
#' @seealso \code{\link{plot.avar}} to plot results
#'
#' @import dplyr
#' @import multcomp
#'
#' @export
avar <- function(dataset, vars, sum_check="", data_filter="", envir=parent.frame()){
  labels = "none"
  df_name <- if (is_string(dataset)) dataset else deparse(substitute(dataset))
  dataset <- get_data(dataset, if (labels == "none") vars else c(labels, vars), filt = data_filter, envir = envir) %>%
    as.data.frame() %>%
    mutate_if(is.Date, as.numeric)
  rm(envir)
  anyCategorical <- sapply(dataset, function(x) is.numeric(x)) == FALSE
  ## in case : is used
  if (length(vars) < ncol(dataset)) vars <- colnames(dataset)
  newdf<-tidyr::gather(dataset, columnNames, values)
  newdf$columnNames<-as.factor(newdf$columnNames)
  one_way_anova<-aov(values~columnNames,data=newdf)
  tukey_ow_anova<-TukeyHSD(one_way_anova)
  df_tot<-nrow(dataset)
  if ("tukey" %in% sum_check) {
    tukey_ow_anova<-TukeyHSD(one_way_anova)
  } else {
    tukey_ow_anova<-"To run multiple comparisons of means, click on 'Tukeys Confidence Intervals' for Tukey Procedure's Confidence Intervals."
  }
  if ("posthoc" %in% sum_check) {
    glh_ow_anova<-summary(glht(one_way_anova,linfct=mcp(columnNames="Tukey")))
  } else {
    glh_ow_anova<-"To run multiple comparisons tests of means, click on 'Post-Hoc' for simultaneous tets of general linear hypothesis."
  }
  as.list(environment()) %>% add_class(c("avar"))

}

#' Summary method for the avar function
#'
#' @details One-Way Anova function ...
#'
#' @param object Return value from \code{\link{avar}}
#' @param ... further arguments passed to or from other methods
#'
#' @import dplyr
#'
#' @export
summary.avar <- function(object,sum_check, ...){
  if (is.character(object)) return(object)
  cat("----------------------------------------------------\n")
  cat("One-Way Analysis of Variance (ANOVA) \n")
  cat("----------------------------------------------------\n\n")
  cat("Model: I or II\n")
  cat("Variables   :", paste0(object$vars, collapse = ", "), "\n")
  cat("\n\n")
  ow_sum <-summary(object$one_way_anova)
  sstr<-ow_sum[[1]][1,2]
  sse<-ow_sum[[1]][2,2]
  mstr<-ow_sum[[1]][1,3]
  mse<-ow_sum[[1]][2,3]
  df_sstr<-ow_sum[[1]][1,1]
  df_sse<-ow_sum[[1]][2,1]
  ssto<-sstr+sse
  na<-"-"
  df_ssto<-object$df_tot
  owa_tab <- data.frame(matrix(nrow = 3, ncol = 3), stringsAsFactors = FALSE)
  colnames(owa_tab) <- c("SS", "df", "MS")
  rownames(owa_tab) <- c("Between Treatments", "Within Treatments", "Total")
  owa_tab$SS <- c(sstr, sse, ssto)
  owa_tab$df <- c(df_sstr, df_sse, df_ssto)
  owa_tab$MS <- c(mstr,mse,na)
  cat("ANOVA Table \n")
  format(owa_tab, scientific = FALSE) %>% print()
  # sum_display<-object$summary_ow_anova
  # sum_display %<>% print()
  cat("\n\n")
  cat("F-Test:\n")
  owa_ftest<-ow_sum[[1]][1,4:5]
  rownames(owa_ftest)<-c("")
  owa_ftest %<>% print()
  cat("\n\n")
  tuk_display<-object$tukey_ow_anova
  tuk_display %<>% print()
  cat("\n")
  glh_display<-object$glh_ow_anova
  glh_display %<>% print()
  cat("\n")
}
