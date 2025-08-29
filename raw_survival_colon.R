# Packages
#-------------------------------------------------------------------------------
library(tidyverse)
library(ggrepel)
library(survival)
library(ggsurvfit)
library(patchwork)
library(paletteer)
library(skimr)

# Load the data
#-------------------------------------------------------------------------------
df <- survival::colon %>% as_tibble()

# Preview
#-------------------------------------------------------------------------------
df %>% head()
df %>% skim()

# Pre-processing
#-------------------------------------------------------------------------------
df %>% select(nodes,node4) %>% table()

df <- df %>% 
  mutate(node4 = ifelse(nodes > 4,1,0),
         node4 = ifelse(nodes <= 4,0,1))

df %>% select(nodes,node4) %>% table()


df <- df %>% 
  mutate(
    sex = case_match(sex,
                     0 ~ "Female",
                     1 ~ "Male"),
    obstruct = case_match(obstruct,
                          0 ~ "No",
                          1 ~ "Yes"),
    perfor = case_match(perfor,
                        0 ~ "No",
                        1 ~ "Yes"),
    adhere = case_match(adhere,
                        0 ~ "No",
                        1 ~ "Yes"),
    differ = case_match(differ,
                        1 ~ "Well",
                        2 ~ "Moderate",
                        3 ~ "Poor"),
    extent = case_match(extent,
                        1 ~ "Submucosa",
                        2 ~ "Muscle",
                        3 ~ "Serosa",
                        4 ~ "Contiguous Structures"),
    surg = case_match(surg,
                      0 ~ "Short",
                      1 ~ "Long"),
    node4 = case_match(node4,
                       0 ~ "No",
                       1 ~ "Yes"),
    etype = case_match(etype,
                       1 ~ "Recurrence",
                       2 ~ "Death"),
    sex = factor(sex,levels = c("Female","Male")),
    obstruct = factor(obstruct,levels = c("No","Yes")),
    perfor = factor(perfor,levels = c("No","Yes")),
    adhere = factor(adhere,levels = c("No","Yes")),
    differ = factor(differ,levels = c("Well","Moderate","Poor")),
    extent = factor(extent,levels = c("Submucosa","Muscle","Serosa","Contiguous Structures")),
    surg = factor(surg,levels = c("Short","Long")),
    node4 = factor(node4,levels = c("No","Yes"))
  )


df %>% head()

# EDA
#-------------------------------------------------------------------------------

# Age versus Time: Status and Treatment

df %>% 
  ggplot(aes(x = age,y = time,color = as.factor(status))) + 
  geom_point(alpha = 0.3) + 
  facet_wrap(etype~rx) + 
  theme_minimal() + 
  labs(x = "Age (Years)",y = "Time (Days)",color = "",
       title = "Patients' Time-To-Event: Treatment") + 
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold")) + 
  scale_color_manual(values = c("#00BFC4","#F8766D"),
                     labels = c("0" = "Censored","1" = "Event"))


df <- df %>% 
  pivot_wider(names_from = etype,
              names_glue = "{etype}_{.value}",
              values_from = c(time,status),
              names_vary = "slowest") %>% 
  rename_with(~ str_replace_all(.,"Death_","d") %>% 
                str_replace_all("Recurrence_","r"))

df %>% head()


# Distribution of factors among patients

df %>% 
  select(where(is.factor)) %>% 
  pivot_longer(everything(),names_to = "category") %>% 
  count(category,value) %>% 
  mutate(
    value = case_when(
      is.na(value) ~ "NA",
      TRUE ~ value)
  ) %>% 
  group_by(category) %>% 
  mutate("proportion" = round(n*100/sum(n)),
         category = factor(category,levels = c("rx","sex","differ",
                                               "obstruct","perfor","adhere",
                                               "extent","surg","node4"))) %>% 
  ggplot(aes(x = proportion,y = reorder(value,proportion),fill = value)) + 
  geom_col(color = "black") + 
  geom_text(aes(label = paste0(proportion,"%"),hjust = -0.1),size = 3,fontface = "bold") + 
  facet_wrap(~category,scales = "free_y",dir = "v") + 
  theme_minimal() + 
  labs(x = "Percentage of Patients",y = "",
       title = "Patient Characteristics: Distributions") + 
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"),
        legend.position = "none") + 
  scale_x_continuous(limits = c(0,110),breaks = c(0,25,50,75,100)) + 
  scale_fill_paletteer_d("ggsci::default_ucscgb")


# Survival Analysis
#-------------------------------------------------------------------------------

# Kaplan-Meier Curves

survfit2(Surv(dtime,dstatus) ~ 1,data = df) %>% 
  ggsurvfit() + 
  add_risktable() + 
  scale_ggsurvfit() + 
  labs(
    y = "Survival Percentage",
    title = "Post-Treatment: Time-to-Death"
  )


factor_vars <- df %>% select(where(is.factor)) %>% names()

print(factor_vars)

kmCurve <- function(fct_var){
  formula <- as.formula(paste("Surv(dtime,dstatus) ~",fct_var))
  fit <- survfit2(formula,data = df)
  
  ggsurvfit(fit) + 
    add_risktable() + 
    scale_ggsurvfit() + 
    labs(
      y = "Survival Percentage",
      title = fct_var
    ) + 
    theme(legend.position = "inside",
          legend.position.inside = c(0.05,0.05),
          legend.justification = c("left","bottom"),
          legend.box.margin = margin(5,5,5,5),
          plot.title = element_text(hjust = 0.5))
}

map(factor_vars,kmCurve) %>% 
  wrap_plots(nrow = 3)


# Log-rank Tests

lrTest <- function(fct_var){
  formula <- as.formula(paste("Surv(dtime,dstatus) ~",fct_var))
  fit <- survdiff(formula,data = df)
}

map(factor_vars,lrTest)

a <- map(factor_vars,lrTest)



# Cox Proportional Hazards Model
#-------------------------------------------------------------------------------

# Proportional Hazards Assumption

coxphCheck <- function(fct_var){
  formula <- as.formula(paste("Surv(dtime,dstatus) ~",fct_var))
  fit <- survfit2(formula,data = df)
  
  ggsurvfit(fit,type = "cloglog") + 
    labs(
      y = "log(-log(Survival))",
      title = fct_var
    ) + 
    theme(legend.position = "inside",
          legend.position.inside = c(0.95,0.05),
          legend.justification = c("right","bottom"),
          legend.box.margin = margin(5,5,5,5),
          plot.title = element_text(hjust = 0.5))
}

map(factor_vars,coxphCheck) %>% 
  wrap_plots(nrow = 3)

df <- df %>% 
  mutate(
    rx_type = case_when(
      rx == "Obs" ~ "Control",
      rx == "Lev" ~ "Treatment",
      rx == "Lev+5FU" ~ "Treatment"
    ),
    differ_grade = case_when(
      differ == "Well" ~ "Low Grade",
      differ == "Moderate" ~ "Low Grade",
      differ == "Poor" ~ "High Grade"
    ),
    extent_invas = case_when(
      extent == "Submucosa" ~ "Non-invasive",
      extent == "Muscle" ~ "Non-invasive",
      extent == "Serosa" ~ "Invasive",
      extent == "Contiguous Structures" ~ "Invasive"
    ),
    rx_type = factor(rx_type,levels = c("Control","Treatment")),
    differ_grade = factor(differ_grade,levels = c("Low Grade","High Grade")),
    extent_invas = factor(extent_invas,levels = c("Non-invasive","Invasive"))
  )

map(c("rx_type","differ_grade","extent_invas"),coxphCheck) %>% 
  wrap_plots(nrow = 1)


# Building the model
covariates = c("obstruct","surg","node4","differ_grade","extent_invas")

# All combinations of factors
covar_combinations <- sapply(1:length(covariates),
                             function(x) combn(covariates,x,simplify = FALSE)) %>% 
  list_flatten() %>% 
  map(~ paste(.x, collapse = " + "))

# Now we get the log likelihood of each model
loglik_rec <- sapply(1:length(covar_combinations),function(x){
  formula <- as.formula(paste("Surv(dtime,dstatus) ~ ",covar_combinations[[x]]))
  coxph(formula,data = df)[["loglik"]][2]
})

# Log likelihood: Null model
null_loglik <- coxph(Surv(dtime,dstatus) ~ 1,data = df)[["loglik"]]

# Insert into vectors for dataframe
covar_combinations <- append(covar_combinations,".",after = 0)
loglik_rec <- append(loglik_rec,null_loglik,after = 0)


loglik_df <- data.frame(
  covar_combn = unlist(covar_combinations),
  dummy_covar_combn = unlist(covar_combinations), # dummy copy for coded column
  minus2logL = -2*loglik_rec,
  dummy_y = rep(0,length(covar_combinations))
)

loglik_df <- loglik_df %>% 
  mutate(
    dummy_covar_combn = str_replace_all(dummy_covar_combn,"\\.","Null"),
    dummy_covar_combn = str_replace_all(dummy_covar_combn,"obstruct","O"),
    dummy_covar_combn = str_replace_all(dummy_covar_combn,"surg","S"),
    dummy_covar_combn = str_replace_all(dummy_covar_combn,"node4","N"),
    dummy_covar_combn = str_replace_all(dummy_covar_combn,"differ_grade","D"),
    dummy_covar_combn = str_replace_all(dummy_covar_combn,"extent_invas","E"),
    covariateCount = gsub(pattern = " \\+ |Null",replacement = "",x = dummy_covar_combn) %>% nchar()
  )

# We create this second one for display
loglik_df_display <- loglik_df %>% 
  select(dummy_covar_combn,covar_combn,minus2logL,covariateCount,dummy_y) %>% 
  rename("Covariate Combination" = covar_combn,"Coded" = dummy_covar_combn, "Number of Covariates" = covariateCount) %>% 
  arrange(desc(minus2logL))

remove(loglik_df)


# Likelihood Ratio Test
#-------------------------------------------------------------------------------

# First variable

# This is for displaying
loglik_df_display %>% 
  filter(`Number of Covariates` == 0 | `Number of Covariates` == 1) %>% 
  select(-`Number of Covariates`,-Coded,-dummy_y) %>% 
  mutate(G = (-2*null_loglik) - minus2logL)

# This is for storing new values for next step
var1 <- loglik_df_display %>% 
  filter(`Number of Covariates` == 0 | `Number of Covariates` == 1) %>% 
  mutate(G = (-2*null_loglik) - minus2logL)

newNull <- var1 %>% filter(G == max(G)) %>% pull(minus2logL)
firstVar <- var1 %>% filter(G == max(G)) %>% pull(Coded)


# Second variable

loglik_df_display %>% 
  filter(`Number of Covariates` == 2 & str_detect(Coded,firstVar)) %>% 
  select(-`Number of Covariates`,-Coded,-dummy_y) %>% 
  mutate(G = newNull - minus2logL)

var2 <- loglik_df_display %>% 
  filter(`Number of Covariates` == 2 & str_detect(Coded,firstVar)) %>% 
  mutate(G = newNull - minus2logL)

newNull <- var2 %>% filter(G == max(G)) %>% pull(minus2logL)
secondVar <- var2 %>% filter(G == max(G)) %>% pull(Coded)

# Third variable

loglik_df_display %>% 
  filter(`Number of Covariates` == 3 & str_detect(Coded,
                                                  str_replace_all(secondVar,"\\+", "\\\\+"))) %>% 
  select(-`Number of Covariates`,-Coded,-dummy_y) %>% 
  mutate(G = newNull - minus2logL)

var3 <- loglik_df_display %>% 
  filter(`Number of Covariates` == 3 & str_detect(Coded,
                                                  str_replace_all(secondVar,"\\+", "\\\\+"))) %>% 
  mutate(G = newNull - minus2logL)

newNull <- var3 %>% filter(G == max(G)) %>% pull(minus2logL)
thirdVar <- var3 %>% filter(G == max(G)) %>% pull(Coded)


# Fourth variable

loglik_df_display %>% 
  filter(`Number of Covariates` == 4 & str_detect(Coded,
                                                  str_replace_all(thirdVar,"\\+", "\\\\+"))) %>% 
  select(-`Number of Covariates`,-Coded,-dummy_y) %>% 
  mutate(G = newNull - minus2logL)

var4 <- loglik_df_display %>% 
  filter(`Number of Covariates` == 4 & str_detect(Coded,
                                                  str_replace_all(thirdVar,"\\+", "\\\\+"))) %>% 
  mutate(G = newNull - minus2logL)

newNull <- var4 %>% filter(G == max(G)) %>% pull(minus2logL)
fourthVar <- var4 %>% filter(G == max(G)) %>% pull(Coded)


# Fifth variable

loglik_df_display %>% 
  filter(`Number of Covariates` == 5 & str_detect(Coded,
                                                  str_replace_all(thirdVar,"\\+", "\\\\+"))) %>% 
  select(-`Number of Covariates`,-Coded,-dummy_y) %>% 
  mutate(G = newNull - minus2logL)

var5 <- loglik_df_display %>% 
  filter(`Number of Covariates` == 5 & str_detect(Coded,
                                                  str_replace_all(thirdVar,"\\+", "\\\\+"))) %>% 
  mutate(G = newNull - minus2logL)

newNull <- var5 %>% filter(G == max(G)) %>% pull(minus2logL)
fifthVar <- var5 %>% filter(G == max(G)) %>% pull(Coded)

print(loglik_df_display)

ggplot(loglik_df_display,aes(x = minus2logL,y = dummy_y,label = Coded,color = as.factor(`Number of Covariates`))) + 
  geom_point() + 
  geom_label_repel(max.overlaps = Inf,min.segment.length = 0) + 
  theme_minimal() + 
  labs(x = "-2logL",color = "Number of Covariates") + 
  scale_y_continuous(limits = c(-1,1),
                     breaks = c(-1,1,1)) + 
  guides(color = guide_legend(override.aes = aes(label = ""),nrow = 1)) + 
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "bottom")


# Stratification without interaction
reducedmodel <- coxph(Surv(dtime,dstatus) ~ node4 + differ_grade + extent_invas + strata(rx), data = df)

# Stratification with interaction
fullmodel <- coxph(Surv(dtime,dstatus) ~ node4 + differ_grade + extent_invas + node4*rx + differ_grade*rx + extent_invas*rx + strata(rx), data = df)

# Likelihood Ratio Test
print(-2*reducedmodel[["loglik"]][2] - -2*fullmodel[["loglik"]][2])

# Final Model
coxph(Surv(dtime,dstatus) ~ node4 + differ_grade + extent_invas + strata(rx), data = df)

