### Created on: 26.05.27 ###
### Last edited: 26.05.27 ###

# NOTE: The purpose of this file is to create a joined regtable for the different H1 regressions that
# That are calculated in step4, step4.1, step4.5 and step5

# Setup
library(dplyr)
library(readr)
library(ggplot2)
library(forcats)
library(purrr)
library(stringr)
library(haven)
library(labelled)
library(tidyr)
library(dotwhisker)
library(ggeffects)
library(ggthemes)
library(modelsummary)

# LOADS ALL H1 HYPOTHESES FOR A JOINED TABLE #
robust_learning_reg_sf <-
  readRDS("robust_learning_reg_sf.rds")

robust_learning_reg_source <-
  readRDS("robust_learning_reg_source.rds")

h1_bivariate <-
  readRDS("h1_bivariate.rds")

h1_pre_learning_reg <-
  readRDS("h1_pre_learning_reg.rds")

iv_model <-
  readRDS("iv_model.rds")

engagement_pre_learning_reg <-
  readRDS("engagement_pre_learning_reg.rds")

exclude_robustness <-
  readRDS("exclude_robustness.rds")

h1_attention_reg <-
  readRDS("h1_attention_reg.rds")


# MAIN H1 TABLE #
modelsummary(
  list(
    "ITT (bivariat)" = h1_bivariate,
    "ITT (kontrol)" = h1_pre_learning_reg,
    "LATE (IV)" = iv_model
  ),
  stars = TRUE,
  fmt = 3,
  estimate = "{estimate}{stars}",
  statistic = "({std.error})",
  coef_map = c(
    "(Intercept)" = "Konstant",
    "treatmentchat bot" = "Chatbot",
    "engaged_chatbot_dummy" = "Engageret chatbot",
    "pre_afstand_total" = "Præ-treatment afstand"
  ),
  gof_map = c(
    "nobs",
    "r.squared",
    "adj.r.squared"
  ),
  title = "Hypotese 1: Effekten af chatbot-treatment på læring",
  output = "regressions/h1_hovedtabel.tex"
)


# ROBUSTNESS H1 TABLE #
modelsummary(
  list(
    "Baseline" = h1_pre_learning_reg,
    "SF ekskl." = robust_learning_reg_sf,
    "Prompt-kontrol" = robust_learning_reg_source,
    "Engagerede" = engagement_pre_learning_reg,
    "Post-fix" = exclude_robustness,
    "Opm.tjek" = h1_attention_reg
  ),
  stars = TRUE,
  fmt = 3,
  estimate = "{estimate}{stars}",
  statistic = "({std.error})",
  coef_map = c(
    "(Intercept)" = "Konstant",
    "treatmentchat bot" = "Chatbot",
    "pre_afstand_total" = "Præ-treatment afstand",
    "source_promptEfter fix af prompt - personlige opslag" =
      "Efter fix (personlige opslag)",
    "source_promptEfter fix - Facebook grupper" =
      "Efter fix (Facebook-grupper)",
    "attention_check_dummy" = "Opmærksomhedstjek"
  ),
  gof_map = c(
    "nobs",
    "r.squared",
    "adj.r.squared"
  ),
  title = "Robusthedsanalyser for Hypotese 1",
  output = "regressions/h1_robusthed.tex"
)
