#' Analysis of Variance (ANOVA)
#'
#' @details Anova function ...
#'
#' @param dataset Dataset
#' @param treat The treatment variables
#' @param treats The treatment variables 2
#' @param response The response variable
#' @param way One Way or Two Way ANOVA
#' @param model Type of ANOVA Model
#' @param interaction With Interaction
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
avar <- function(dataset, treat,response, treattwo, way="one", model="one", interaction="FALSE", sum_check="", data_filter="", envir=parent.frame()){
  ## Data Manipulation
  ## One Way ANOVA
  if(way=="one"){
    # labels = "none"
    # df_name <- if (is_string(dataset)) dataset else deparse(substitute(dataset))
    # dataset <- get_data(dataset, if (labels == "none") vars else c(labels, vars), filt = data_filter, envir = envir) %>%
    #   as.data.frame() %>%
    #   mutate_if(is.Date, as.numeric)
    # rm(envir)
    # anyCategorical <- sapply(dataset, function(x) is.numeric(x)) == FALSE
    # if (length(vars) < ncol(dataset)) vars <- colnames(dataset)
    df_name <- if (is_string(dataset)) dataset else deparse(substitute(dataset))
    newdf <- get_data(dataset, c(treat, response), filt = data_filter, envir = envir, na.rm = FALSE)
    colnames(newdf)<-c("columnNames","values")
    newdf$columnNames<-as.factor(newdf$columnNames)
    one_way_anova<-aov(values~columnNames,data=newdf)
    df_tot<-nrow(newdf)
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
  }
  ## Two Way ANOVA
  if (way=="two"){
    df_name <- if (is_string(dataset)) dataset else deparse(substitute(dataset))
    newdf <- get_data(dataset, c(treat, treattwo, response), filt = data_filter, envir = envir, na.rm = FALSE)
    colnames(newdf)<-c("Treatment_1","Treatment_2","Response")
    newdf$Treatment_1<-as.factor(newdf$Treatment_1)
    newdf$Treatment_2<-as.factor(newdf$Treatment_2)
    if (model=="one"){
      if(interaction=="FALSE"){
        two_way_anova<-aov(Response~Treatment_1+Treatment_2, data=newdf)
        df_tot<-nrow(newdf)
      }
      if(interaction=="TRUE"){
        two_way_anova<-aov(Response~Treatment_1*Treatment_2, data=newdf)
        df_tot<-nrow(newdf)
      }
      m1sum<-summary(two_way_anova)
      if (interaction=="FALSE"){
        factorA<-(m1sum[[1]][1,3])/(m1sum[[1]][3,3])
        factorB<-(m1sum[[1]][2,3])/(m1sum[[1]][3,3])
        fApv<-1-pf(factorA,m1sum[[1]][1,1],m1sum[[1]][3,1])
        fBpv<-1-pf(factorB,m1sum[[1]][2,1],m1sum[[1]][3,1])
        if(fApv==0)
          fApv <- "<2e-16"
        if(fBpv==0)
          fBpv <- "<2e-16"
      } else{
        factorA<-(m1sum[[1]][1,3])/(m1sum[[1]][4,3])
        factorB<-(m1sum[[1]][2,3])/(m1sum[[1]][4,3])
        factorAB<-(m1sum[[1]][3,3])/(m1sum[[1]][4,3])
        fApv<-1-pf(factorA,m1sum[[1]][1,1],m1sum[[1]][3,1])
        fBpv<-1-pf(factorB,m1sum[[1]][2,1],m1sum[[1]][3,1])
        fABpv<-1-pf(factorAB,m1sum[[1]][3,1],m1sum[[1]][3,1])
        if(fApv==0)
          fApv <- "<2e-16"
        if(fBpv==0)
          fBpv <- "<2e-16"
        if(fABpv==0)
          fABpv <- "<2e-16"
      }
    }
    if (model=="two"){
      two_way_anova<-aov(Response~Treatment_1*Treatment_2, data=newdf)
      df_tot<-nrow(newdf)
      m1sum<-summary(two_way_anova)
      factorA<-(m1sum[[1]][1,3])/(m1sum[[1]][3,3])
      factorB<-(m1sum[[1]][2,3])/(m1sum[[1]][3,3])
      factorAB<-(m1sum[[1]][3,3])/(m1sum[[1]][4,3])
      fApv<-1-pf(factorA,m1sum[[1]][1,1],m1sum[[1]][3,1])
      fBpv<-1-pf(factorB,m1sum[[1]][2,1],m1sum[[1]][3,1])
      fABpv<-1-pf(factorAB,m1sum[[1]][3,1],m1sum[[1]][4,1])
      if(fApv==0)
        fApv <- "<2e-16"
      if(fBpv==0)
        fBpv <- "<2e-16"
      if(fABpv==0)
        fABpv <- "<2e-16"
    }
    if (model=="three"){
      two_way_anova<-aov(Response~Treatment_1*Treatment_2, data=newdf)
      df_tot<-nrow(newdf)
      m1sum<-summary(two_way_anova)
      factorA<-(m1sum[[1]][1,3])/(m1sum[[1]][3,3])
      factorB<-(m1sum[[1]][2,3])/(m1sum[[1]][4,3])
      factorAB<-(m1sum[[1]][3,3])/(m1sum[[1]][4,3])
      fApv<-1-pf(factorA,m1sum[[1]][1,1],m1sum[[1]][3,1])
      fBpv<-1-pf(factorB,m1sum[[1]][2,1],m1sum[[1]][4,1])
      fABpv<-1-pf(factorAB,m1sum[[1]][3,1],m1sum[[1]][4,1])
      if(fApv==0)
        fApv <- "<2e-16"
      if(fBpv==0)
        fBpv <- "<2e-16"
      if(fABpv==0)
        fABpv <- "<2e-16"
    }
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
summary.avar <- function(object,...){
  if (is.character(object)) return(object)
  if (object$way=="one"){
    cat("----------------------------------------------------\n")
    cat("One-Way Analysis of Variance (ANOVA) \n")
    cat("----------------------------------------------------\n\n")
    cat("Model: I or II\n")
    cat("Treatment   :", object$treat, "\n")
    cat("Response   :", object$response, "\n")
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
  if (object$way=="two"){
    cat("----------------------------------------------------\n")
    cat("Two-Way Analysis of Variance (ANOVA) \n")
    cat("----------------------------------------------------\n\n")
    cat("Model:", object$model,"\n")
    if (object$model=="one"){
      cat("Treatment I (fixed):", object$treat, "\n")
      cat("Treatment II (fixed):", object$treattwo, "\n")
    } else if (object$model=="two") {
      cat("Treatment I (random):", object$treat, "\n")
      cat("Treatment II (random):", object$treattwo, "\n")
    } else {
      cat("Treatment I (fixed):", object$treat, "\n")
      cat("Treatment II (random):", object$treattwo, "\n")
    }
    cat("Response   :", object$response, "\n")
    cat("\n\n")

    if (object$model == "one"){
      if (object$interaction=="FALSE"){
        tw_sum <- summary(object$two_way_anova)
        cat("ANOVA Table \n")
        tw_sum_display<-tw_sum[[1]][1:3,1:3]
        tw_sum_display %<>% print()
      } else{
        tw_sum <- summary(object$two_way_anova)
        cat("ANOVA Table \n")
        tw_sum_display<-tw_sum[[1]][1:4,1:3]
        tw_sum_display %<>% print()
      }
      cat("\n\n")
      tw_ftest <- data.frame(matrix(nrow = 2, ncol = 2), stringsAsFactors = FALSE)
      colnames(tw_ftest) <- c("F-Value"="fvalue", "Pr(>F)"="pval")
      rownames(tw_ftest) <- c("Treatment I", "Treatment II")
      tw_ftest$fvalue<-c(object$factorA,object$factorB)
      tw_ftest$pval<-c(object$fApv, object$fBpv)
      cat("F Test: Testing the presence of effects of ... \n")
      format(tw_ftest, scientific = FALSE) %>% print()
    } else{
      tw_sum <- summary(object$two_way_anova)
      cat("ANOVA Table \n")
      tw_sum_display<-tw_sum[[1]][1:4,1:3]
      tw_sum_display %<>% print()
      tw_ftest <- data.frame(matrix(nrow = 3, ncol = 2), stringsAsFactors = FALSE)
      colnames(tw_ftest) <- c("F-Value"="fvalue", "Pr(>F)"="pval")
      rownames(tw_ftest) <- c("Treatment I", "Treatment II", "Interaction")
      tw_ftest$fvalue<-c(object$factorA,object$factorB, object$factorAB)
      tw_ftest$pval<-c(object$fApv, object$fBpv, object$fABpv)
      cat("\n\n")
      cat("F Test: Testing the presence of effects of ... \n")
      format(tw_ftest, scientific = FALSE) %>% print()
    }
    }
  }

