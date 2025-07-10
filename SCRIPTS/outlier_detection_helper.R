library(mvtnorm)
library(fields)
library(ggplot2)
library(tidyr)
library(cowplot)
library(plotly)


get_likelihood_bayesopt <- function(distance.matrix, data, years, nu, nugget, sigma.squared, rho = 1){
      # Compute Matérn Covariance Matrix
      mat_cov <- Matern(d = distance.matrix, 
                        smoothness = nu, 
                        range = rho, 
                        phi = sigma.squared) + diag(nugget, dim(distance.matrix)[1])
      
      # Compute likelihood
      likelihood <- dmvnorm(data, 
                            mean = rep(0, length(data)), 
                            sigma = mat_cov[years, years],
                            log = TRUE)
      
      return(likelihood)
}

get_likelihood <- function(row, dist_mat, rho, all_years, data, compute.sigma = FALSE){
      if (compute.sigma){
            # Compute Matérn Covariance Matrix
            mat_cov <- Matern(d = dist_mat, smoothness = row[1], range = rho, phi = 1) + diag(row[2], dim(dist_mat)[1])
            
            #Compute sigma^2
            sigma.squared <- (1/length(data)) * (data %*% solve(mat_cov[all_years, all_years]) %*% data)[1,1] #chol2inv(chol(mat_cov[all_years, all_years])) solve(mat_cov[all_years, all_years])
            
            # Compute likelihood
            likelihood <- dmvnorm(data, 
                                  mean = rep(0, length(data)), 
                                  sigma = sigma.squared * mat_cov[all_years, all_years],
                                  log = TRUE)
      } else {
            # Compute Matérn Covariance Matrix
            mat_cov <- Matern(d = dist_mat, smoothness = row[1], range = rho, phi = row[3]) + diag(row[2], dim(dist_mat)[1])
            
            # Compute likelihood
            likelihood <- dmvnorm(data, 
                                  mean = rep(0, length(data)), 
                                  sigma = mat_cov[all_years, all_years],
                                  log = TRUE)
      }
      
      return(likelihood)
}

get_likelihood_sigma <- function(row, dist_mat, rho, all_years, data){
      # Compute Matérn Covariance Matrix
      mat_cov <- Matern(d = dist_mat, smoothness = row[1], range = rho, phi = 1) + diag(row[2], dim(dist_mat)[1])
      
      #Compute sigma^2
      sigma.squared <- (1/length(data)) * (data %*% chol2inv(chol(mat_cov[all_years, all_years])) %*% data)[1,1] #chol2inv(chol(mat_cov[all_years, all_years])) solve(mat_cov[all_years, all_years])
      
      # Compute likelihood
      likelihood <- dmvnorm(data, 
                            mean = rep(0, length(data)), 
                            sigma = sigma.squared * mat_cov[all_years, all_years],
                            log = TRUE)
      return(likelihood)
}

plot_true_vs_pred <- function(df, ein, predictions, standard_errors, all_years, candidate_outliers, combo, y_lim, label = "", round_to = 2){
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
            scale_shape_manual(values = c("Candidate Outlier" = 8, "Non-outlier" = 16)) 
      if (hasArg(y_lim)){
            p <- p + ylim(y_lim[1], y_lim[2])
      }
      if (hasArg(combo)){
            round_nu <- round(combo$nu, round_to)
            round_nugget <- round(combo$nugget, round_to)
            round_sigma.squared <- round(combo$sigma.squared, round_to)
            p <- p + ggtitle(bquote(.(label) ~ nu == .(round_nu) ~ "," ~tau^2 == .(round_nugget) ~ "," ~ sigma^2 == .(round_sigma.squared)))
      } else{p <- p + ggtitle(label)}
      
      return(p)
}

plot_heatmap <- function(ein, combos, all_likelihoods, sigma_fixed, nu_fixed, nugget_fixed){
      p <- list()
      if (hasArg(sigma_fixed)){
            subset_data <- subset(combos |> mutate(value = all_likelihoods[[ein]]), 
                                  sigma.squared == sigma_fixed)
            
            p[["sigma.squared"]] <- ggplot(subset_data, aes(x = nu, y = nugget, fill = value)) +
                  geom_tile() +
                  labs(title = paste(ein, "Heatmap at sigma^2 =", sigma_fixed)) +
                  scale_fill_viridis_c() +
                  theme_minimal()
      }
      
      if (hasArg(nu_fixed)){
            subset_data <- subset(combos |> mutate(value = all_likelihoods[[ein]]), 
                                  nu == nu_fixed)
            
            p[["nu"]] <- ggplot(subset_data, aes(x = nugget, y = sigma.squared, fill = value)) +
                  geom_tile() +
                  labs(title = paste(ein, "Heatmap at nu =", nu_fixed)) +
                  scale_fill_viridis_c() +
                  theme_minimal()
      }
      
      if (hasArg(nugget_fixed)){
            subset_data <- subset(combos |> mutate(value = all_likelihoods[[ein]]), 
                                  nugget == nugget_fixed)
            
            p[['nugget']] <- ggplot(subset_data, aes(x = nu, y = sigma.squared, fill = value)) +
                  geom_tile() +
                  labs(title = paste(ein, "Heatmap at nugget =", nugget_fixed)) +
                  scale_fill_viridis_c() +
                  theme_minimal()
      }
      return(plot_grid(plotlist = p, labels = "AUTO", ncol = 1))
}

plot_surface <- function(ein, combos, all_likelihoods, fixed, fixed_val){
      if (fixed == "nu"){
            subset_data <- combos |> mutate(value = all_likelihoods[[ein]]) |>
                  filter(nu == fixed_val)
            
            # Create sorted unique vectors
            nugget_vals <- sort(unique(subset_data$nugget))
            sigma_vals <- sort(unique(subset_data$sigma.squared))
            
            # Reshape value into matrix: rows = nugget, columns = sigma.squared
            z_matrix <- matrix(
                  subset_data$value,
                  nrow = length(nugget_vals),
                  ncol = length(sigma_vals),
                  byrow = FALSE)
            
            # Now plot
            p <- plot_ly(
                  x = nugget_vals,
                  y = sigma_vals,
                  z = z_matrix,
                  type = "surface")
            
            x_label <- list(title = 'nugget')
            y_label <- list(title = 'sigma^2')
            
      } else if (fixed == "nugget"){
            subset_data <- combos |> mutate(value = all_likelihoods[[ein]]) |>
                  filter(nugget == fixed_val)
            
            # Create sorted unique vectors
            nu_vals <- sort(unique(subset_data$nu))
            sigma_vals <- sort(unique(subset_data$sigma.squared))
            
            # Reshape value into matrix: rows = nu, columns = sigma.squared
            z_matrix <- matrix(
                  subset_data$value,
                  nrow = length(nu_vals),
                  ncol = length(sigma_vals),
                  byrow = FALSE)
            
            # Now plot
            p <- plot_ly(
                  x = nu_vals,
                  y = sigma_vals,
                  z = z_matrix,
                  type = "surface")
            
            x_label <- list(title = 'nu')
            y_label <- list(title = 'sigma^2')
            
      } else {
            subset_data <- combos |> mutate(value = all_likelihoods[[ein]]) |>
                  filter(sigma.squared == fixed_val)
            
            # Create sorted unique vectors
            nu_vals <- sort(unique(subset_data$nu))
            nugget_vals <- sort(unique(subset_data$nugget))
            
            # Reshape value into matrix: rows = nu, columns = nugget
            z_matrix <- matrix(
                  subset_data$value,
                  nrow = length(nu_vals),
                  ncol = length(nugget_vals),
                  byrow = FALSE)
            
            # Now plot
            p <- plot_ly(
                  x = nu_vals,
                  y = nugget_vals,
                  z = z_matrix,
                  type = "surface")
            
            x_label <- list(title = 'nu')
            y_label <- list(title = 'nugget')
            
      }
      p <- p |> layout(title = paste(ein, "likelihood surface plot,", fixed, "=", fixed_val), 
                  plot_bgcolor = "#e5ecf6",
                  xaxis = x_label,
                  yaxis = y_label)
      return(p)
}