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
#' @import lmtest
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
    # Shapiro-Wilk's Test (Test for Normality)
    shap_wilks <-shapiro.test(one_way_anova$residuals)
    # Breusch-Pagan Test (Test for Homosekdasticity)
    bp<-lmtest::bptest(one_way_anova)
    # Ljung–Box Test (Test for Independence)
    lj_box<-Box.test(one_way_anova$residuals, type="Ljung")
    # Outliers
    rs<-rstandard(one_way_anova)[rstandard(one_way_anova) < -3 | rstandard(one_way_anova) > 3]
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
    if ("tukey" %in% sum_check) {
      tukey_twa1<-TukeyHSD(two_way_anova, which = "Treatment_1")
      tukey_twa2<-TukeyHSD(two_way_anova, which = "Treatment_2")
    } else {
      tukey_twa1<-"To run multiple comparisons of means, click on 'Tukeys Confidence Intervals' for Tukey Procedure's Confidence Intervals."
      tukey_twa2<-" "
    }
    if ("posthoc" %in% sum_check) {
      glh_twa1<-summary(glht(two_way_anova,linfct=mcp(Treatment_1="Tukey")))
      glh_twa2<-summary(glht(two_way_anova,linfct=mcp(Treatment_2="Tukey")))
    } else {
      glh_twa1<-"To run multiple comparisons tests of means, click on 'Post-Hoc' for simultaneous tets of general linear hypothesis."
      glh_twa2<-" "
    }
    # Shapiro-Wilk's Test (Test for Normality)
    shap_wilks <-shapiro.test(two_way_anova$residuals)
    # Breusch-Pagan Test (Test for Homosekdasticity)
    bp<-lmtest::bptest(two_way_anova)
    # Ljung–Box Test (Test for Independence)
    lj_box<-Box.test(two_way_anova$residuals, type="Ljung")
    # Outliers
    rs<-rstandard(two_way_anova)[rstandard(two_way_anova) < -3 | rstandard(two_way_anova) > 3]
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
    cat("\n\n")
    tuk_display1<-object$tukey_twa1
    tuk_display2<-object$tukey_twa2
    tuk_display1 %<>% print()
    tuk_display2 %<>% print()
    cat("\n")
    glh_display1<-object$glh_twa1
    glh_display2<-object$glh_twa2
    glh_display1 %<>% print()
    glh_display2 %<>% print()
    cat("\n")
  }
  if ("diag" %in% object$sum_check) {
    cat("----------------------------------------------------\n")
    cat("Diagnostic Testing \n")
    cat("----------------------------------------------------\n\n")
    cat("TEST FOR NORMALITY: Shapiro-Wilks Test \n")
    cat("Null hyp.: The dataset is normally distributed \n")
    cat("Alt. hyp.: The dataset is not normally distributed \n")
    sw_display<-object$shap_wilk
    sw_display %<>% print()
    cat("\n\n")
    cat("TEST FOR HOMOSKEDASTICITY: Breusch-Pagan Test \n")
    cat("Null hyp.: The residuals are homoscedastic\n")
    cat("Alt. hyp.: The residuals are heteroscedastic\n")
    bp_display <-object$bp
    bp_display %<>% print()
    cat("\n\n")
    cat("TEST FOR INDEPENDENCE: Ljung–Box Test \n")
    cat("Null hyp.: The errors are uncorrelated\n")
    cat("Alt. hyp.: The errors are correlated\n")
    lb_display<-object$lj_box
    lb_display %<>% print()
    cat("\n\n")
    cat("POSSIBLE OUTLIERS: Studentized Residuals \n")
    cat("Absolute value of internally studentized residuals larger than 3 printed below\n")
    rs_display<-object$rs
    rs_display %<>% print()
    cat("\n\n")
  }
}

#' Plot method for the ANOVA function
#'
#' @details Plotting interaction plot ...
#'
#' @param x Return value from \code{\link{pca}}
#' @param plots Plot to return.
#' @param shiny Did the function call originate inside a shiny app
#' @param custom Logical (TRUE, FALSE) to indicate if ggplot object (or list of ggplot objects) should be returned. This option can be used to customize plots (e.g., add a title, change x and y labels, etc.). See examples and \url{https://ggplot2.tidyverse.org/} for options.
#' @param ... further arguments passed to or from other methods
#'
#' @import ggplot2
#' @remote vqv/ggbiplot
#'
#' @export
plot.avar<-function(x, plots="TRUE", shiny = FALSE, custom = FALSE, ...){
  if (radiant.data::is_empty(plots)) return(invisible())
  if (is.character(x)) return(invisible())
  plot_list = list()
  plotdf <- x$newdf %>% group_by(Treatment_1, Treatment_2) %>% summarise(mean=mean(Response))
  if (plots=="TRUE"){
    interaction_plot <- plotdf %>%
      ggplot(aes(Treatment_1,mean))+
      geom_line(size=1.2, aes(group=Treatment_2, color = Treatment_2))+
      geom_point(size=2.6, aes(col=Treatment_2), shape=15) +
      labs(
        title= "Interaction between Treatment 1 and Treatment 2",
        x = "Treatment 1",
        y= "Treatment 2"
      )
  }
  plot_list[["interaction"]]<-interaction_plot
  if (length(plot_list) > 0) {
    if (custom) {
      if (length(plot_list) == 1) plot_list[[1]] else plot_list
    } else {
      patchwork::wrap_plots(plot_list, ncol = min(length(plot_list), 2)) %>%
        {if (shiny) . else print(.)}
    }
  }
}

