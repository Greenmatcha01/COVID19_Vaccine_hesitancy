#1. Load the data -------------------------------------
library(foreign)
library(dplyr)
library(tidyverse)
library(naniar)
library(haven)
library(forcats)
library(mice)
library(nnet)

path = "/Users/min/Documents/Edin_Dissertation/MD3318"
data <- read.spss(file.path(path, "3318.sav"), to.data.frame = TRUE)

#2. Data Cleaning -----------------------------------

data <- data %>% 
  # Select only the survey questions of interest
  select(SEXO, EDAD, P1, P2, PRUEBAENTRE, PRUEBACONVI, CORONAENTRE, CORONACONVI,
                EVOLUCENTRE, EVOLUCCONVI, P4CORONA_1, P4CORONA_2, P4CORONA_3,
                P4MUERTOS_1, P4MUERTOS_2, P4MUERTOS_3, P5, P7, P8, P9, P11, P15,
                SERSANIENTRE, SERVIOTROSCONVI, NUMPERSONAS, ESTUDIOS, SITLAB) %>%
  # Recode the variables categories from Spanish to English
  mutate(
        # Occupation
        SITLAB = fct_recode(SITLAB,
                             "Work" = "Trabaja",
                             "Retired" = "Jubilado/a o pensionista (anteriormente ha trabajado) ",
                             "Retired" = "Pensionista (anteriormente no ha trabajado) ",
                             "Unemployed" = "En paro y ha trabajado antes ",
                             "Unemployed" = "En paro y busca su primer empleo ",
                             "Student" = "Estudiante ",
                             "Unpaid Domestic work" = "Trabajo doméstico no remunerado ",
                             "Other Situations" = "Otra situación",
                             "Don't know/No Answer" = "N.C."),
         # Gender
         SEXO = fct_recode(SEXO,
                           "Male" = "Hombre",
                           "Female" = "Mujer"),
         
         # Measures the respondents' levels of concerns towards the impacts of coronavirus
         P1 = fct_recode(P1,
                         "High" = "Mucho",
                         "High" = "Bastante",
                         "Low" = "Poco",
                         "Medium" = "(NO LEER) Regular",
                         "Nothing" = "Nada",
                         "Don't know/No Answer" = "N.S.",
                         "Don't know/No Answer" = "N.C."), 
         
         # Asks the respondents what worries them more from the effects of pandemic (on health, economy, and/or employment)
         P2 = fct_recode(P2,
                         "Health" = "Los efectos sobre la salud",
                         "Economy & Employment" = "Los efectos sobre la economía y el empleo",
                         "Both equally" = "(NO LEER) Ambos por igual",
                         "Neither" = "(NO LEER) Ni unos ni otros",
                         "Don't know/No Answer" = "N.S.",
                         "Don't know/No Answer" = "N.C."),
         
         # Asks the respondents if they have had to contact the health services (either when they think they had symptoms related to the coronavirus or for other causes related to the coronavirus)
         SERSANIENTRE = fct_recode(SERSANIENTRE,
                                   "Yes, successful" = "Sí",
                                   "Yes, not successful" = "(NO LEER) Sí,  pero no consiguió contactar con los servicios",
                                   "Don't know/No Answer" = "N.C."),
         
         # Asks the respondents if they have tested for coronavirus
         PRUEBAENTRE = fct_recode(PRUEBAENTRE,
                                  "Yes" = "Sí",
                                  "Don't know/No Answer" = "N.C.",
                                  "Don't know/No Answer" = "N.S."),
         
         # Asks the respondents whether they have been finally diagnosed with the coronavirus
         CORONAENTRE = fct_recode(CORONAENTRE,
                                  "Yes" = "Sí",
                                  "Don't know/No Answer" = "N.C."),
         
         # Asks the respondents if any person they're living with have tested for coronavirus
         PRUEBACONVI = fct_recode(PRUEBACONVI,
                                  "Yes" = "Sí",
                                  "Don't know/No Answer" = "N.S./N.R.",
                                  "Don't know/No Answer" = "N.C."),
         
         # Asks the respondents if that person was finally diagnosed with coronavirus
         CORONACONVI = fct_recode(CORONACONVI,
                                  "Yes" = "Sí",
                                  "No" = "No",
                                  "Don't know/No Answer" = "N.S./N.R.",
                                  "Don't know/No Answer" = "N.C."),
         
         # Asks the respondents how did that person's illness evolve
         EVOLUCCONVI = fct_recode(EVOLUCCONVI,
                                  "Mild, home" = "Tuvo síntomas leves y la pasó en casa",
                                  "Significant, home" = "Tuvo síntomas importantes, pero la pasó en casa",
                                  "Had to go to hospital" = "Tuvo que ingresar en el hospital",
                                  "No symptoms" = "No tuvo ningún síntoma",
                                  "Don't know/No Answer" = "N.S./N.R.",
                                  "Don't know/No Answer" = "N.C."),
         
         #a Asks the respondents if any family member of different households had the coronavirus
         P4CORONA_1 = fct_recode(P4CORONA_1,
                                 "Yes" = "Sí",
                                 "Don't know/No Answer" = "N.C.",
                                 "Don't know/No Answer" = "N.S."),
         
         # Asks the respondents if any of their friends had the coronavirus
         P4CORONA_2 = fct_recode(P4CORONA_2,
                                 "Yes" = "Sí",
                                 "Don't know/No Answer" = "N.S.",
                                 "Don't know/No Answer" = "N.C."),
         
         # Asks the respondents if any of their acquaintances had the coronovirus
         P4CORONA_3 = fct_recode(P4CORONA_3,
                                 "Yes" = "Sí",
                                 "Don't know/No Answer" = "N.S.",
                                 "Don't know/No Answer" = "N.C."),
         
         # Asks the respondents if any FAMILY member of different households haven't been able to overcome/have died from the coronavirus
         P4MUERTOS_1 = fct_recode(P4MUERTOS_1,
                                  "Yes" = "Sí",
                                  "Don't know/No Answer" = "N.S.",
                                  "Don't know/No Answer" = "N.C."),
         
         # Asks the respondents if any of their FRIENDS haven't been able to overcome/have died from the coronavirus
         P4MUERTOS_2 = fct_recode(P4MUERTOS_2,
                                  "Yes" = "Sí",
                                  "Don't know/No Answer" = "N.S.",
                                  "Don't know/No Answer" = "N.C."),
         
         #a Asks the respondents if any of their ACQUAINTANCES haven't been able to overcome/have died from the coronavirus
         P4MUERTOS_3 = fct_recode(P4MUERTOS_3,
                                  "Yes" = "Sí",
                                  "Don't know/No Answer" = "N.S.",
                                  "Don't know/No Answer" = "N.C."),
         
         # Asks the respondents how their illnesses had evolved
         EVOLUCENTRE = fct_recode(EVOLUCENTRE,
                                  "Mild, home" = "Tuve síntomas leves y la pasé en casa",
                                  "Significant, home" = "Tuve síntomas importantes, pero la pasé en casa",
                                  "Had to go to hospital" = "Tuve que ingresar en el hospital",
                                  "No symptoms" = "No tuve ningún síntoma",
                                  "Don't know/No Answer" = "N.C."),
         
         # Ask the respondents if any member of their households had contacted the health services for having symptoms/other causes related to the coronavirus
         SERVIOTROSCONVI = fct_recode(SERVIOTROSCONVI,
                                      "Yes" = "Sí",
                                      "Lives alone" = "Vive sola la persona entrevistada",
                                      "Yes, not successful" = "(NO LEER) Intentó contactar pero no lo consiguió",
                                      "Don't know/No Answer" = "N.C."),
         
         # Asks the respondents how many people they live with have contacted the health services
         NUMPERSONAS = fct_recode(NUMPERSONAS,
                                  "1" = "1 persona",
                                  "2" = "2 personas",
                                  "3+" = "3 personas",
                                  "3+" = "4 personas",
                                  "3+" = "5 personas",
                                  "3+" = "6",
                                  "Don't know/No Answer" = "N.C."),
         
         # Asks the respondents if they are willing to get vaccinated against the covid-19 when their turns come
         P5 = fct_recode(P5,
                         "Yes" = "Sí",
                         "Conditional Yes" = "Sí, si tiene garantías, si está probada, si es fiable",
                         "Conditional Yes" = "Sí, según el origen de la vacuna",
                         "Conditional Yes" = "Sí, si hay información suficiente",
                         "Conditional Yes" = "Sí, por consejo de autoridades, científicos/as, o sanitarios",
                         "Yes" = "Ya le/la han vacunado/a",
                         "Other responses" = "Otras respuestas",
                         "Don't know/No Answer" = "(NO LEER) No sabe, duda",
                         "Don't know/No Answer" = "N.C."),
         
         # Asks the respondents if they think the majority of Spaniards are setting an example of civility and solidarity in following the measures against the covid-19
         P7 = fct_recode(P7,
                         "Yes" = "Cree que la mayoría está reaccionando con civismo y solidari",
                         "No" = "Cree que la mayoría está siendo poco cívica e indisciplinada",
                         "Don't know, doubt" = "(NO LEER) No lo sabe, duda",
                         "Don't know/No Answer"= "N.C."),
         
         # Asks the respondents the levels of impacts of the pandemic on their personal lives
         P8 = fct_recode(P8,
                         "A lot" = "Me está afectando mucho",
                         "A lot" = "Me está afectando bastante",
                         "Regular" = "(NO LEER) Regular",
                         "Something" = "Me está afectando algo",
                         "(Almost) Nothing" = "No me está afectando nada o casi nada",
                         "Don't know/No Answer" = "N.C.",
                         "Don't know/No Answer" = "N.S."),
         
         # Asks the respondents the levels of impacts of the pandemic on their social and relationship lives
         P9 = fct_recode(P9,
                         "A lot" = "Me está afectando mucho",
                         "A lot" = "Me está afectando bastante",
                         "Regular" = "(NO LEER) Regular",
                         "Something" = "Me está afectando algo",
                         "(Almost) Nothing" = "No me está afectando nada o casi nada",
                         "Don't know/No Answer" = "N.C.",
                         "Don't know/No Answer" = "N.S."),
         
         # Asks the respondents what own control measures they've taken apart from the official ones
         P11 = fct_recode(P11,
                          "None, normal" = "Ninguna, hace vida normal",
                          "Cautious, normal" = "Tiene cuidado con las cosas que toca o por dónde va, pero en",
                          "Isolation, essential" = "Permanece prácticamente en aislamiento, saliendo de casa sól",
                          "Strict isolation" = "No sale de casa para nada que no resulte imprescindible y le",
                          "Work, essential" = "Acudir al trabajo y otras necesidades esenciales",
                          "Reduce contact" = "Reducir el contacto social y familiar (salir menos, evitar a",
                          "Follow ALL sanitary measures" = "Seguir todas las medidas sanitarias requeridas",
                          "Follow SOME sanitary measures" = "Seguir algunas de las medidas sanitarias recomendadas (uso d",
                          "Other" = "Otras respuestas",
                          "Don't know/No Answer" = "N.S., duda",
                          "Don't know/No Answer"= "N.C."),
         
         # Asks the respondents to rate their current personal financial situations
         P15 = fct_recode(P15,
                          "Very good" = "Muy buena",
                          "Good" = "Buena",
                          "Regular" = "(NO LEER) Regular",
                          "Very bad" = "Muy mala",
                          "Bad" = "Mala",
                          "Don't know/No Answer"= "N.S.",
                          "Don't know/No Answer"= "N.C."),
        
         # Educational level
         ESTUDIOS = fct_recode(ESTUDIOS,
                               "Without studies" = "Sin estudios",
                               "Primary" = "Primaria",
                               "Secondary" = "Secundaria 1ª etapa",
                               "Secondary" = "Secundaria 2ª etapa",
                               "Vocational" = "F.P.",
                               "University" = "Superiores",
                               "Other" = "Otros",
                               "Don't know/No Answer" = "N.C.")) %>% 
  
  # Re-categorize the Age variable
  mutate(EDAD = cut(
    as.numeric(as.character(EDAD)), 
    breaks = c(-Inf, 25, 35, 45, 55, 65, Inf), 
    right = TRUE, 
    labels = c("18-24", "25-34", "35-44", "45-54", "55-64", "65+")
  )) %>%
  # Set the answer Yes as the baseline
  mutate(
    P5 = relevel(P5, ref = 2),
    SERSANIENTRE = relevel(SERSANIENTRE, ref = 2),
    P2 = relevel(P2, ref = 4)
  ) %>%
  # Reorder the levels
  mutate(
    P1 = fct_relevel(P1, "Nothing", "Low", "Medium", "High"),
    P8 = fct_relevel(P8, "(Almost) Nothing", "Something", "Regular", "A lot"),
    P9 = fct_relevel(P9, "(Almost) Nothing", "Something", "Regular", "A lot"),
    P15 = fct_relevel(P15, "Very bad", "Bad", "Regular", "Good", "Very good"),
    P5 = fct_relevel(P5, "No", "Conditional Yes", "Yes")
  )

# 3. Data Exploration ----------------------------------------------------------

# Get a frequency table
# Loop through variables and get their frequency tables
for (var in colnames(data)) {
    data %>% 
    group_by(.data[[var]]) %>% 
    summarise(Frequency = n()) %>% 
    print()
}


# 4. Missing values ------------------------------------------------------------
# Check if any columns contain missing values
any(is.na(data))

# Check the number of missing values in each column
colSums(is.na(data))

# Generate a missing value plot
gg_miss_var(df)

# Transform data into a tidy format
data_tidy <- data %>%
  mutate(across(everything(), as.character)) %>%
  gather(key = "variable", value = "value") %>%
  mutate(status = ifelse(is.na(value), "Missing", "Non-Missing"))

# Plot heatmap
ggplot(data = data_tidy, aes(x = variable, fill = status)) +
  geom_tile(aes(y = 1), width = 0.5) +
  scale_fill_manual(values = c("Missing" = "red", "Non-Missing" = "grey50")) +
  theme_minimal() +
  labs(title = "Missing Data Heatmap",
       x = "Variables",
       y = "",
       fill = "Status") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Replace the missing values
# Specify the method
init = mice(data, maxit=0) 
meth = init$method
meth[] = "polyreg" # Polytomous regression for the factor variables

# Perform the imputation
imputed_data <- mice(data, method=meth, m=5) # m=5 specifies the number of multiple imputations
completed_data <- complete(imputed_data, 1) # Choose the first imputed dataset

#5. Modelling --------------------------------------------------------------------
#Intercept only model
OIM <- multinom(P5 ~ 1, data = completed_data)
summary(OIM)

# Multinomial model
# Function to fit multinomial logistic regression
fit_multinom <- function(data) {
  fit <- multinom(P5 ~ ., data = data)
  return(fit)
}

# Apply the function to each imputed dataset
fits <- with(imputed_data, exp=fit_multinom(.))

# Pool the results
pooled_results <- pool(fits)

# Examine the pooled results
summary(pooled_results)

pooled_results$tab 

p_value <- 2 * (1 - pnorm(abs(Z_score)))


