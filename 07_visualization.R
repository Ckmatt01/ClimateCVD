###############################################################################################
# TABLE 1 to FOREST PLOT GENERATOR ----
#
# Reformats Table 1 into a forest plot (like figure 2)
# Accepts input tables (need 1 for CHD and 1 for stroke) that contains:
# - column "Variable" with variable names (set clean_var_names variable names to false if it does not include Anomaly)
# - column ci_col that contains string following format "(#,#)" (e.g. a confidence interval)
###############################################################################################

setwd(working_directory_output)

####
#' Create results table for a given model
#'
#' @param table_1 Table 1 object (e.x. model_9a_CHD_table1)
#' @param table_2 Table 1 object (e.x. model_9a_STROKE_table1)
#' @param value_col Column containing values of interest (e.x. model_9a_CHD_table1$Pop_Weight_Effect_Size_Anomaly)
#' @param ci_col Column containing confidence interval of values of interest (e.x. model_9a_CHD_table1$Pop_Weight_Effect_Size_Anomaly_CI)
#' @param x_label Time period label for output files (e.x. 70_79_13_22)
#' @param outcome_labels Input "CHD" or "Stroke"
#' @param reference_lines Defaults to auto generating reference lines. A list can manually be entered here instead
#' @return Formatted results table
####

reformat_forest_plot <- function(
    table_1, 
    table_2, 
    value_col = "Value",           
    ci_col = "CI",                 
    x_label = "Value",             
    outcome_labels = c("CHD", "Stroke"), 
    reference_lines = NULL,        
    clean_var_names = TRUE         
) {
  
  # Extract and combine data for df 1 (CHD)
  df_1 <- data.frame(
    Variable = table_1$Variable,
    Value = as.numeric(table_1[[value_col]]),
    Lower_CI = as.numeric(gsub("[\\(\\),]", "", str_split_fixed(table_1[[ci_col]], ", ", 2)[,1])),
    Upper_CI = as.numeric(gsub("[\\(\\),]", "", str_split_fixed(table_1[[ci_col]], ", ", 2)[,2])),
    Outcome = outcome_labels[1]
  )
  # Extract and combine data for df 2 (Stroke)
  df_2 <- data.frame(
    Variable = table_2$Variable,
    Value = as.numeric(table_2[[value_col]]),
    Lower_CI = as.numeric(gsub("[\\(\\),]", "", str_split_fixed(table_2[[ci_col]], ", ", 2)[,1])),
    Upper_CI = as.numeric(gsub("[\\(\\),]", "", str_split_fixed(table_2[[ci_col]], ", ", 2)[,2])),
    Outcome = outcome_labels[2]
  )
  
  combined_df <- rbind(df_1, df_2)
  
  if (clean_var_names) {
    combined_df$Variable <- gsub(" Anomaly", "", combined_df$Variable)
    combined_df$Variable <- gsub("\\(\\*.*\\)", "", combined_df$Variable)
  }
  
  # Prepare data for plotting
  variable_means <- combined_df %>%
    group_by(Variable) %>%
    summarise(Mean_Value = mean(Value, na.rm = TRUE)) %>%
    arrange(Mean_Value)
  
  combined_df$Variable_Group <- factor(combined_df$Variable, levels = variable_means$Variable)
  combined_df <- combined_df %>% arrange(desc(Variable_Group), Outcome == outcome_labels[1])
  
  combined_df <- combined_df %>%
    mutate(Variable_Outcome = paste(Variable, Outcome, sep = " - "))
  
  combined_df$Variable_Outcome <- factor(
    combined_df$Variable_Outcome,
    levels = rev(unique(combined_df$Variable_Outcome))
  )
  
  # Get the min and max CI values
  min_ci <- min(combined_df$Lower_CI, na.rm = TRUE)
  max_ci <- max(combined_df$Upper_CI, na.rm = TRUE)
  
  # Calculate range for automatic reference lines if not provided
  if (is.null(reference_lines)) {
    # Find nearest 0.5 marks (without additional 0.5 buffer)
    get_reference_boundary <- function(val, direction) {
      if (direction == "lower") {
        # Round down to nearest 0.5 below the value
        return(floor(val/0.5)*0.5)
      } else {
        # Round up to nearest 0.5 above the value
        return(ceiling(val/0.5)*0.5)
      }
    }
    
    lowest_ref <- get_reference_boundary(min_ci, "lower")
    highest_ref <- get_reference_boundary(max_ci, "upper")
    
    # Generate sequence of reference lines at 0.5 intervals
    reference_lines <- seq(lowest_ref, highest_ref, by = 0.5)
  }
  
  # Ensure 0 is always included in reference lines
  if (!0 %in% reference_lines) {
    reference_lines <- sort(c(0, reference_lines))
  }
  
  # Create plot
  fig <- ggplot(combined_df, aes(
    x = Value,
    y = Variable_Outcome,
    color = Outcome,
    shape = Outcome
  )) +
    lapply(reference_lines, function(x) {
      geom_vline(
        xintercept = x,
        linetype = ifelse(x == 0, "solid", "dashed"),
        color = ifelse(x == 0, "black", "gray"),
        linewidth = ifelse(x == 0, 0.8, 0.5)
      )
    }) +
    geom_point(size = 2.5) +
    geom_errorbarh(aes(xmin = Lower_CI, xmax = Upper_CI),
                   height = 0.4,
    ) +
    scale_x_continuous(limits = c(
      min(min_ci, min(reference_lines)),
      max(max_ci, max(reference_lines))
    )) +
    labs(x = x_label) +
    scale_color_manual(values = c("black", "red"), labels = outcome_labels) +
    scale_shape_manual(values = c(16, 17), labels = outcome_labels) +
    theme_minimal() +
    theme(
      text = element_text(family = "Arial", size = 12),
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      legend.position = "bottom",
      panel.grid = element_blank(),
      axis.text.y = element_blank(),
      axis.title.y = element_blank(),
      axis.title.x = element_text(face = "bold")
    )
  
  return(list(combined_data = combined_df, plot = fig))
}

#### Recreate figure 2 to verify correctly functioning script ####
# Generate population weighted figure 2 (verifies that results are the same as paper figure 2)

fig_PWAES_data <- reformat_forest_plot(
  table_1 = model_old_CHD_table1,
  table_2 = model_old_STROKE_table1,
  value_col = "Pop_Weight_Effect_Size_Anomaly",
  ci_col = "Pop_Weight_Effect_Size_Anomaly_CI",
  x_label = "Prevalence"
)

print(fig_PWAES_data$plot)

#### Generate migration figure 2's ####

fig_MIG_bot_in_data <- reformat_forest_plot(
  table_1 = migration_model_results[[1]][[1]][[1]],
  table_2 = migration_model_results[[2]][[1]][[1]],
  value_col = "Effect_Anomaly_Size",
  ci_col = "Combined_CI",
  x_label = "Prevalence",
  reference_lines = c(-3, -2.5, -2, -1.5, -1, -0.5, 0.5, 1, 1.5, 2, 2.5, 3, 3.5)
)

fig_MIG_bot_out_data <- reformat_forest_plot(
  table_1 = migration_model_results[[1]][[1]][[2]],
  table_2 = migration_model_results[[2]][[1]][[2]],
  value_col = "Effect_Anomaly_Size",
  ci_col = "Combined_CI",
  x_label = "Prevalence",
  reference_lines = c(-3, -2.5, -2, -1.5, -1, -0.5, 0.5, 1, 1.5, 2, 2.5, 3, 3.5)
)

fig_MIG_bot_both_data <- reformat_forest_plot(
  table_1 = migration_model_results[[1]][[1]][[3]],
  table_2 = migration_model_results[[2]][[1]][[3]],
  value_col = "Effect_Anomaly_Size",
  ci_col = "Combined_CI",
  x_label = "Prevalence",
  reference_lines = c(-3, -2.5, -2, -1.5, -1, -0.5, 0.5, 1, 1.5, 2, 2.5, 3, 3.5)
)

fig_MIG_top_in_data <- reformat_forest_plot(
  table_1 = migration_model_results[[1]][[2]][[1]],
  table_2 = migration_model_results[[2]][[2]][[1]],
  value_col = "Effect_Anomaly_Size",
  ci_col = "Combined_CI",
  x_label = "Prevalence",
  reference_lines = c(-3, -2.5, -2, -1.5, -1, -0.5, 0.5, 1, 1.5, 2, 2.5, 3, 3.5)
)

fig_MIG_top_out_data <- reformat_forest_plot(
  table_1 = migration_model_results[[1]][[2]][[2]],
  table_2 = migration_model_results[[2]][[2]][[2]],
  value_col = "Effect_Anomaly_Size",
  ci_col = "Combined_CI",
  x_label = "Prevalence",
  reference_lines = c(-3, -2.5, -2, -1.5, -1, -0.5, 0.5, 1, 1.5, 2, 2.5, 3, 3.5)
)

fig_MIG_top_both_data <- reformat_forest_plot(
  table_1 = migration_model_results[[1]][[2]][[3]],
  table_2 = migration_model_results[[2]][[2]][[3]],
  value_col = "Effect_Anomaly_Size",
  ci_col = "Combined_CI",
  x_label = "Prevalence",
  reference_lines = c(-3, -2.5, -2, -1.5, -1, -0.5, 0.5, 1, 1.5, 2, 2.5, 3, 3.5)
)

#### Create tables for migration data ####
Figure_S5_CHD_Top <- cbind(migration_model_results$CHD$top_50$`in`$Variable, migration_model_results$CHD$top_50$`in`$Effect_Anomaly_Size, migration_model_results$CHD$top_50$`in`$Combined_CI, migration_model_results$CHD$top_50$`out`$Effect_Anomaly_Size, migration_model_results$CHD$top_50$`out`$Combined_CI, migration_model_results$CHD$top_50$`both`$Effect_Anomaly_Size, migration_model_results$CHD$top_50$`both`$Combined_CI)
Figure_S5_CHD_Bot <- cbind(migration_model_results$CHD$bottom_50$`in`$Variable, migration_model_results$CHD$bottom_50$`in`$Effect_Anomaly_Size, migration_model_results$CHD$bottom_50$`in`$Combined_CI, migration_model_results$CHD$bottom_50$`out`$Effect_Anomaly_Size, migration_model_results$CHD$bottom_50$`out`$Combined_CI, migration_model_results$CHD$bottom_50$`both`$Effect_Anomaly_Size, migration_model_results$CHD$bottom_50$`both`$Combined_CI)

Figure_S5_Stroke_Top <- cbind(migration_model_results$Stroke$top_50$`in`$Variable, migration_model_results$Stroke$top_50$`in`$Effect_Anomaly_Size, migration_model_results$Stroke$top_50$`in`$Combined_CI, migration_model_results$Stroke$top_50$`out`$Effect_Anomaly_Size, migration_model_results$Stroke$top_50$`out`$Combined_CI, migration_model_results$Stroke$top_50$`both`$Effect_Anomaly_Size, migration_model_results$Stroke$top_50$`both`$Combined_CI)
Figure_S5_Stroke_Bot <- cbind(migration_model_results$Stroke$bottom_50$`in`$Variable, migration_model_results$Stroke$bottom_50$`in`$Effect_Anomaly_Size, migration_model_results$Stroke$bottom_50$`in`$Combined_CI, migration_model_results$Stroke$bottom_50$`out`$Effect_Anomaly_Size, migration_model_results$Stroke$bottom_50$`out`$Combined_CI, migration_model_results$Stroke$bottom_50$`both`$Effect_Anomaly_Size, migration_model_results$Stroke$bottom_50$`both`$Combined_CI)

Figure_S5_Top <- cbind(Figure_S5_CHD_Top, Figure_S5_Stroke_Top[,2:7])
Figure_S5_Bot <- cbind(Figure_S5_CHD_Bot, Figure_S5_Stroke_Bot[,2:7])


#### Write migration data outputs ####
ggsave("TableS6_mig_bot_in_plot.png", fig_MIG_bot_in_data$plot, width = 2, height = 6, dpi = 300)
ggsave("TableS6_mig_bot_out_plot.png", fig_MIG_bot_out_data$plot, width = 2, height = 6, dpi = 300)
ggsave("TableS6_mig_bot_both_plot.png", fig_MIG_bot_both_data$plot, width = 2, height = 6, dpi = 300)

ggsave("TableS7_mig_top_in_plot.png", fig_MIG_top_in_data$plot, width = 2, height = 6, dpi = 300)
ggsave("TableS7_mig_top_out_plot.png", fig_MIG_top_out_data$plot, width = 2, height = 6, dpi = 300)
ggsave("TableS7_mig_top_both_plot.png", fig_MIG_top_both_data$plot, width = 2, height = 6, dpi = 300)

ggsave("TableS6_S7_figure2_verify.png", fig_PWAES_data$plot, width = 2, height = 6, dpi = 300)


write.csv(Figure_S5_Top, "TableS7_top50_migration.csv", row.names = FALSE)
write.csv(Figure_S5_Bot, "TableS6_bot50_migration.csv", row.names = FALSE)

write.csv(fig_MIG_bot_in_data$combined_data, "TableS6_Figure2_mig_bot_in_df.csv", row.names = FALSE)
write.csv(fig_MIG_bot_out_data$combined_data, "TableS6_Figure2_mig_bot_out_df.csv", row.names = FALSE)
write.csv(fig_MIG_bot_both_data$combined_data, "TableS6_Figure2_mig_bot_both_df.csv", row.names = FALSE)

write.csv(fig_MIG_top_in_data$combined_data, "TableS7_Figure2_mig_top_in_df.csv", row.names = FALSE)
write.csv(fig_MIG_top_out_data$combined_data, "TableS7_Figure2_mig_top_out_df.csv", row.names = FALSE)
write.csv(fig_MIG_top_both_data$combined_data, "TableS7_Figure2_mig_top_both_df.csv", row.names = FALSE)