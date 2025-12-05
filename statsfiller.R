Bodyfat_csv <- read_excel("Bodyfat.csv.xlsx")

#filter
bodyfat_subset <- subset(Bodyfat_csv,select = c(1,2,3,4,11))

bodyfat_subset$EDUCATION <- as.factor(bodyfat_subset$EDUCATION)

bodyfat_subset$EDUCATION_GROUPED <- fct_collapse(bodyfat_subset$EDUCATION,
                                     "Post-Secondary" = c("3", "4", "5","6"), 
                                     "Not Post-Secondary" = c("1","2")
)
bodyfat_subset$group_binary <- ifelse(bodyfat_subset$EDUCATION_GROUPED == "Post-Secondary", 1, 0)

bodyfat_subset$aboveorbelow25 <- cut(x = bodyfat_subset$BODYFAT,
                    breaks = c(-Inf, 25, Inf),
                    labels = c("<25%", ">= 25%"),
                    right = FALSE) 
contingency_table <- table(bodyfat_subset$EDUCATION_GROUPED, bodyfat_subset$aboveorbelow25)

contingency_table
#calculate relative risk ratio
# Extracting values from the contingency table
a <- contingency_table["Post-Secondary", ">= 25%"]      # Events in exposed
b <- contingency_table["Post-Secondary", "<25%"]   # No events in exposed
c <- contingency_table["Not Post-Secondary", ">= 25%"]    # Events in unexposed
d <- contingency_table["Not Post-Secondary", "<25%"] # No events in unexposed

# Calculating risks
risk_exposed <- a / (a + b)
risk_unexposed <- c / (c + d)

# Calculating relative risk
relative_risk <- risk_exposed / risk_unexposed
print(paste("Relative Risk:", relative_risk))
#getting stats for the datagroups
get_mode <- function(v) {
  uniqv <- unique(v[!is.na(v)])
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

bodyfat_statsinfo <-aggregate(BODYFAT ~ EDUCATION_GROUPED, data = bodyfat_subset, FUN = function(x) c(
  mean = mean(x),
  median = median(x),
  var = var(x),
  sd = sd(x),
  mode = get_mode(x) #custom mode function
))

welch_result <- t.test(BODYFAT ~ EDUCATION_GROUPED, data = bodyfat_subset)

#correlation test
bodyfat_subset$EDUCATION <- as.numeric(bodyfat_subset$EDUCATION)
correlation_results <- bodyfat_subset %>%
  summarize(
    spearman_rho = cor(BODYFAT, EDUCATION, method = "spearman"),
    p_value = cor.test(BODYFAT, EDUCATION, method = "spearman", exact = FALSE)$p.value
  )
correlation_results

#linear regression
model <- lm(group_binary ~ BODYFAT, data = bodyfat_subset)
summary(model)

ggplot(bodyfat_subset, aes(x = group_binary, y = BODYFAT)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") + 
  labs(title = "Education by bodyfat", x = ("Education"), y = "Bodyfat(in Percentage)")
#graphs
ggplot(bodyfat_subset, aes(x = EDUCATION_GROUPED,  y = BODYFAT)) +
  geom_boxplot() +
  labs(
    title = "Non-Post-Secondary Vs Post-Secondary Body Fat Ratios",
    subtitle = "By Percentage",
    x = "Post Secondary Education levels",
    y = "Bodyfat")
 + theme_minimal()
#histrogram
bodyfat_subset |>
  ggplot(aes(x = BODYFAT)) +
  geom_histogram(color = "red", fill = "yellow") +
  labs(x = "Bodyfat",
       y = "Frequency",
       title = "Body fat Percentage Frequency",
       ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(hjust = 0.5, face = "bold", size = 7),
    axis.text.y = element_text()
  )
