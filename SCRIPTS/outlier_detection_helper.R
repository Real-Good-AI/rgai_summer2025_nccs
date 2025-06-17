library(mvtnorm)
library(fields)
library(ggplot2)
library(tidyr)

get_likelihood <- function(row, dist_mat, rho, all_years, data){
      # Compute Matérn Covariance Matrix
      mat_cov <- Matern(d = dist_mat, smoothness = row[1], range = rho, phi = row[3]) + diag(row[2], dim(dist_mat)[1])
      
      # Compute likelihood
      likelihood <- dmvnorm(data, 
                            mean = rep(0, length(data)), 
                            sigma = mat_cov[all_years, all_years],
                            log = TRUE)
      
      return(likelihood)
}

plot_true_vs_pred <- function(df, ein, predictions, standard_errors, all_years, candidate_outliers, combo){
      df_long <- df |> filter(EIN2==ein) |>
            mutate(OUTLIER = case_when(TAX_YEAR %in% unlist(candidate_outliers) ~ "Candidate Outlier", .default = "Non-outlier"))
      df_long$PRED <- unlist(predictions)
      df_long$SE <- unlist(standard_errors)
      
      df_long <- df_long |>
            pivot_longer(cols = c(LOG_REV, PRED), names_to = "Variable", values_to = "Value")
      
      # Add a column to flag outliers (TRUE if OUTLIER is not NA), add confidence intervals
      df_long <- df_long |>
            mutate(CI.LOWER = case_when(
                  Variable == "LOG_REV" ~ 0,
                  Variable == "PRED" ~ qt(0.95, df = length(all_years)-1) * SE)) |>
            mutate(CI.UPPER = case_when(
                  Variable == "LOG_REV" ~ 0,
                  Variable == "PRED" ~ qt(0.95, df = length(all_years)-1) * SE))
      
      # Base plot of true versus predicted with error bars on predicted
      p <- ggplot(df_long, aes(x = TAX_YEAR, y = Value, color = Variable, shape = OUTLIER)) +
            geom_point(size=3) +
            geom_line(aes(group = Variable), linewidth=1) +
            geom_ribbon(data = (df_long |> filter(Variable == "PRED")),
                        aes(ymin = Value - CI.LOWER, ymax = Value + CI.UPPER, group = Variable),
                        fill = "#55B748",
                        alpha = 0.25) +
            labs(x = "Tax Year", y = "Log Revenue") +
            scale_color_manual(values = c("LOG_REV" = "#1796D2", "PRED" = "#55B748"), 
                               labels = c("LOG_REV" = "True", "PRED" = "Predicted, 95% CI"), 
                               name = "Legend") +
            scale_shape_manual(values = c("Candidate Outlier" = 8, "Non-outlier" = 16)) +
            ggtitle(
                  bquote("Observed vs Predicted Log Rev:" ~ .(ein) ~ "," ~ nu == .(combo$nu) ~ "," ~tau^2 == .(combo$nugget) ~ "," ~ sigma^2 == .(combo$sigma.squared))
            )
      
      return(p)
}