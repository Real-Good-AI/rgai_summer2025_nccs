plot_data_distributions <- function(df, column, tot = NULL, xlab = NULL, title = NULL) {
      # Capture the column name correctly
      col_enquo <- enquo(column)
      
      # Default x-axis label if not provided
      if (is.null(xlab)) {
            xlab <- as_label(col_enquo)
      }
      
      # Default total to take proportion out of if not provided
      if (is.null(n)) {
            tot <- nrow(df)
      }
      
      # Prepare the plot
      p <- df |> 
            group_by(!!col_enquo) |> 
            summarize(data.prop = n() / tot, .groups = "drop") |> 
            ggplot(aes(x = !!col_enquo, y = data.prop)) +
            geom_bar(stat = "identity", color = "black") + 
            labs(x = xlab, y = "Proportion", title = title)
      
      return(p)
}