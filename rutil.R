library(viridis)
library(ggplot2)
library(ggbiplot)
library(corrplot)
library(progress)

empty_directories <- function(directories) {
  for (directory in directories) {
    unlink(directory, recursive = TRUE)
    dir.create(directory, showWarnings = FALSE)
  }
}

not_empty <- function(file) {
  return(file != "")
}

print_section <- function(string) {
  separator <- paste(rep("*", nchar(string)), collapse = "")
  print(separator)
  print(string)
  print(separator)
}

full_summary <- function(data, save_to = "") {
  summary <- summary(data)
  if (not_empty(save_to)) sink(save_to)
  print(summary)
  if (not_empty(save_to)) sink()
  return(summary)
}

numerical_summary <- function(data, numerical_variables, save_to = "") {
  summary <- do.call(
    cbind,
    lapply(data[complete.cases(data), numerical_variables], summary)
  )
  print(summary)
  if (not_empty(save_to)) write.csv(summary, save_to)
  return(summary)
}

plot_percentage <- function(data, variable, save_to = "") {
  if (not_empty(save_to)) png(save_to)
  barplot(
    prop.table(table(data[, variable])),
    ylab = "Frequency",
    col = "white"
  )
  if (not_empty(save_to)) dev.off()
}


matrix_scatterplots <- function(data, numerical_variables, save_to = "") {
  if (not_empty(save_to)) png(save_to, 2000, 2000)
  pairs(as.matrix(data[, numerical_variables]))
  if (not_empty(save_to)) dev.off()
}

all_scatterplots <- function(data, numerical_variables, save_to = "") {
  create_graphs_iteratively(data, numerical_variables, plot_scatterplot, save_to)
}

create_graphs_iteratively <- function(data,
                                      numerical_variables,
                                      plot_function,
                                      save_to = "") {
  numerical_variables[["Proportion"]] <- FALSE
  variables <- names(numerical_variables[numerical_variables == TRUE])
  
  n_variables <- (length(variables) - 1)
  progress_bar <- progress_bar$new(
    format = "Progress [:bar] :percent ETA: :eta",
    total = n_variables
  )
  for (i in 1:n_variables) {
    progress_bar$tick()
    for (j in (i + 1):length(variables)) {
      image_name <- paste(
        save_to,
        variables[i], "_",
        variables[j], ".png",
        sep = ""
      )
      plot_function(
        data,
        var_x = variables[i],
        var_y = variables[j],
        save_to = image_name,
        regression = TRUE
      )
    }
  }
}


plot_scatterplot <- function(data,
                             var_x,
                             var_y,
                             var_color = "Proportion",
                             regression = FALSE,
                             save_to = "") {
  if (var_color != "") {
    plot <- ggplot(data, aes_string(x = var_x, y = var_y, color = var_color))
  } else {
    plot <- ggplot(data, aes_string(x = var_x, y = var_y))
  }
  # TODO: Fix this: discrete value provided to continuous scale
  # plot <- plot + scale_color_viridis()
  plot <- plot + geom_point()
  if (regression) {
    plot <- plot + stat_smooth(method = "lm", col = "grey", se = FALSE)
  }
  if (not_empty(save_to)) png(save_to)
  print(plot)
  if (not_empty(save_to)) dev.off()
}

correlations_plot <- function(data, numerical_variables, save_to = "") {
  if (not_empty(save_to)) png(save_to, 1280, 1280)
  corrplot(cor(data[, numerical_variables]), tl.col = "black", tl.cex = 0.6)
  if (not_empty(save_to)) dev.off()
}

extend <- function(c1, c2){
  paste(c1,c2,sep = "")
}

principal_components <- function(data, numerical_variables, save_to = "") {
  numerical_variables[["Proportion"]] <- FALSE
  
  pca <- prcomp(data[, numerical_variables], center = TRUE, scale. = TRUE)
  biplot <- ggbiplot(pca, groups = data$Vote)
  biplot <- biplot + scale_color_discrete(name = "")
  biplot <- biplot + theme(legend.position = "top",
                           legend.direction = "horizontal")
  
  if (not_empty(save_to)) sink(extend(save_to, "_results.txt"))
  print(pca)
  if (not_empty(save_to)) sink()
  
  if (not_empty(save_to)) sink(extend(save_to, "_summary.txt"))
  print(summary(pca))
  if (not_empty(save_to)) sink()
  
  if (not_empty(save_to)) png(extend(save_to, ".png"), 500, 500)
  plot(pca, type = "l", main = "Principal Components' Variances" )
  if (not_empty(save_to)) dev.off()
  
  if (not_empty(save_to)) png(extend(save_to, "_biplot.png"), 800, 800)
  print(biplot)
  if (not_empty(save_to)) dev.off()
  
  return(pca)
}

variable_histogram <- function(data, variable, save_to = "") {
  save_png(data, variable, save_to, histogram)
}

variable_qqplot <- function(data, variable, save_to = "") {
  save_png(data, variable, save_to, quantile_quantile)
}

save_png <- function(data, variable, save_to, function_to_create_image) {
  if (not_empty(save_to)) png(save_to)
  function_to_create_image(data, variable)
  if (not_empty(save_to)) dev.off()
}

histogram <- function(data, variable) {
  hist(data[, variable], main = "Histogram", xlab = "Proportion")
}

quantile_quantile <- function(data, variable) {
  qqnorm(data[, variable], main = "Normal QQ-Plot for Proportion")
  qqline(data[, variable])
}

generate_combinations_unvectorized <- function(variables,
                                               min_percentage,
                                               max_percentage) {
  variables[["Proportion"]] <- FALSE
  variables                 <- names(variables[variables == TRUE])
  n                         <- length(variables)
  n_min                     <- floor(n * min_percentage)
  n_max                     <- ceiling(n * max_percentage)
  all_combinations          <- NULL
  
  progress_bar <- progress_bar$new(
    format = "Progress [:bar] :percent ETA: :eta",
    total = length(n_min:n_max)
  )
  
  for (k in n_min:n_max) {
    progress_bar$tick()
    combinations <- combn(variables, k)
    for (column in 1:ncol(combinations)) {
      new_list <- list(combinations[, column])
      all_combinations <- c(all_combinations, list(new_list))
    }
  }
  return(unlist(all_combinations, recursive = FALSE))
}

generate_combinations_vectorized <- function(variables,
                                             min_percentage,
                                             max_percentage) {
  variables[["Proportion"]] <- FALSE
  variables <- names(variables[variables == TRUE])
  
  n       <- length(variables)
  n_min   <- floor(n * min_percentage)
  n_max   <- ceiling(n * max_percentage)
  
  progress_bar <- progress_bar$new(
    format = "Progress [:bar] :percent ETA: :eta",
    total = length(n_min:n_max)
  )
  
  all_combinations <- unlist(
    lapply(lapply(n_min:n_max, function(k) {
      progress_bar$tick()
      return(combn(variables, k))
    }),
    function(y) unlist(apply(y, 2, list), recursive = FALSE)),
    recursive = FALSE
  )
  return(all_combinations)
}
