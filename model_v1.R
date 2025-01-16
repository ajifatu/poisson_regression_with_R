#Charger les données
library(foreign)
medData <- read.spss("med.poissonregression.equaltimes.sav",to.data.frame=TRUE)


#Description du dataset
str(medData)
head(medData)
summary(medData)


# Vérifier s'il y a des lignes dupliquées
print("Lignes dupliquées: ", sum(duplicated(medData)))

# Nombre total de valeurs manquantes
print("Valeurs manquantes: ",sum(is.na(medData)))


"""
ANALYSES UNIVARIEES
"""

hist(medData$esteem)

# Compter le nombre d'observations pour chaque groupe
alcohol_counts <- table(medData$alcohol)
# Calculer les pourcentages
alcohol_percent <- prop.table(alcohol_counts) * 100
# Afficher les pourcentages
print(alcohol_percent)

# Diagramme camembert alcohol
pie(alcohol_counts, 
    labels = paste0(names(alcohol_counts), " (", round(alcohol_percent, 1), "%)"),
    main = "Répartition des patients selon la consommation d'alcool",
    col = c("skyblue", "orange"))


# Calculer les effectifs par type de traitement
treatment_counts <- table(medData$treatment)
# Calculer les pourcentages
treatment_percent <- prop.table(treatment_counts) * 100
# Afficher les effectifs et les pourcentages
print(treatment_counts)
print(round(treatment_percent, 1)) # Arrondi à 1 décimale

# Calculer les effectifs par type de traitement
treatment_counts <- table(medData$treatment)
# Calculer les pourcentages
treatment_percent <- prop.table(treatment_counts) * 100
# Afficher les effectifs et les pourcentages
print(treatment_counts)
print(round(treatment_percent, 1)) # Arrondi à 1 décimale

# Barplot pour la répartition des traitements
barplot(treatment_counts, 
        names.arg = c("Haute dose", "Faible dose", "Placebo"),
        col = c("skyblue", "orange", "lightgreen"),
        main = "Répartition des patients selon le traitement",
        ylab = "Effectifs",
        ylim = c(0, max(treatment_counts) + 5)) 


# Statistiques descriptives pour 'events'
summary(medData$events)

events_values <- medData$events
print(events_values)


# Calcul manuel de la moyenne et de l'écart-type
mean_events <- mean(adjusted_events)
sd_events <- sd(adjusted_events)
var_events <- var(adjusted_events)

# Diagramme de fréquence pour 'events'
event_counts <- table(medData$events)
barplot(event_counts,
        main = "Fréquence des nombres de crises d'épilepsie",
        xlab = "Nombre de crises d'épilepsie",
        ylab = "Effectifs",
        col = "orange")

# Boxplot pour 'events'
boxplot(medData$events, 
        main = "Boxplot des crises d'épilepsie",
        ylab = "Nombre de crises d'épilepsie",
        col = "lightgreen")

# Ajustement à une distribution de Poisson
lambda_est <- mean(adjusted_events)
poisson_fit <- dpois(0:max(adjusted_events), lambda = lambda_est)


# Superposition de la distribution théorique sur l'histogramme
hist(adjusted_events, probability = TRUE, 
     main = "Comparaison avec la loi de Poisson",
     xlab = "Nombre de crises d'épilepsie",
     col = "lightgreen")
lines(0:max(adjusted_events), poisson_fit, type = "b", col = "red")



# Ajustement à une distribution de Poisson
lambda_est <- mean(adjusted_events)
poisson_fit <- dpois(0:max(adjusted_events), lambda = lambda_est)


# Superposition de la distribution théorique sur l'histogramme
hist(adjusted_events, probability = TRUE, 
     main = "Comparaison avec la loi de Poisson",
     xlab = "Nombre de crises d'épilepsie",
     col = "lightblue")
lines(0:max(adjusted_events), poisson_fit, type = "b", col = "red")



"""
Data prep
"""
#Recodage treatment - high
T1 <- rep(0,nrow(D))
T1[medData$treatment==1] <- 1
print(sum(T1))

#Recodage treatment - low
T2 <- rep(0,nrow(D))
T2[medData$treatment==2] <- 1
print(sum(T2))

"""
MODELE
"""
# Modèle de régression de Poisson
poisson_model <- glm(events ~ T1 + T2 + alcohol + esteem, 
                     family = poisson(link = "log"), 
                     data = medData)
summary(poisson_model)


# Ajustement du modèle de régression de Poisson: sans alcohol
new_poisson_model <- glm(events ~ T1 + T2 + esteem, 
                         family = poisson(link = "log"), 
                         data = medData)
summary(new_poisson_model)


# Tableau comparatif de nos deux models
models_comparison <- data.frame(
  Model = c("Avec Alcohol", "Sans Alcohol"),
  AIC = c(AIC(poisson_model), AIC(new_poisson_model)),
  Deviance = c(deviance(poisson_model), deviance(new_poisson_model)),
  df = c(df.residual(poisson_model), df.residual(new_poisson_model))
)
print(models_comparison)

#Test du rapport de vraisemblance :
anova(new_poisson_model, poisson_model, test="Chisq")



library(ggplot2)

# Extraire les coefficients et leurs erreurs standards
coef_old <- data.frame(
  coef = coef(poisson_model),
  se = sqrt(diag(vcov(poisson_model))),
  variable = names(coef(poisson_model)),
  model = "Avec Alcohol"
)

coef_new <- data.frame(
  coef = coef(new_poisson_model),
  se = sqrt(diag(vcov(new_poisson_model))),
  variable = names(coef(new_poisson_model)),
  model = "Sans Alcohol"
)

coef_comparison <- rbind(coef_old, coef_new)

# Créer le graphique
ggplot(coef_comparison, aes(x = variable, y = coef, color = model)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = coef - 1.96*se, ymax = coef + 1.96*se),
                position = position_dodge(width = 0.5), width = 0.2) +
  coord_flip() +
  theme_minimal() +
  labs(title = "Comparaison des coefficients entre les modèles",
       y = "Estimation des coefficients",
       x = "Variables")


# Calculer les prédictions pour les deux modèles
predictions_old <- predict(poisson_model, type = "response")
predictions_new <- predict(new_poisson_model, type = "response")

# Créer un graphique de comparaison
plot_data <- data.frame(
  Observed = medData$events,
  Old_Model = predictions_old,
  New_Model = predictions_new
)

par(mfrow=c(1,2))
plot(plot_data$Observed, plot_data$Old_Model, 
     main="Modèle avec Alcohol",
     xlab="Observé", ylab="Prédit")
abline(0,1, col="red")

plot(plot_data$Observed, plot_data$New_Model, 
     main="Modèle sans Alcohol",
     xlab="Observé", ylab="Prédit")
abline(0,1, col="red")


"""
ANALYSE DES RESIDUS
"""
# Diagnostic plots de base
par(mfrow=c(2,2))
plot(new_poisson_model)

# Résidus de déviance standardisés
residus <- rstandard(new_poisson_model, type="deviance")
plot(predict(new_poisson_model), residus,
     xlab="Valeurs prédites",
     ylab="Résidus standardisés",
     main="Résidus vs Valeurs prédites")
abline(h=0, col="red")


"""
MESURE DES PERFOMANCES
"""
# RMSE (Root Mean Square Error)
rmse <- sqrt(mean((medData$events - predict(new_poisson_model, type="response"))^2))
rmse
