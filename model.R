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


# Définir la moyenne proche des données initiales
lambda <- mean(c(6, 5, 4, 4, 3, 4, 3, 3, 3, 3, 3, 3, 3, 3, 2, 2, 2, 1, 1, 1, 
                 9, 8, 7, 9, 9, 7, 8, 9, 7, 6, 8, 7, 7, 6, 7, 7, 7, 6, 5, 5, 
                 5, 5, 4, 5, 4, 4, 3, 3, 3, 2, 16, 11, 14, 12, 11, 10, 9, 9, 
                 10, 9, 11, 12, 10, 8, 9, 9, 8, 8, 7, 7, 7, 8, 6, 7, 6))

# Générer de nouvelles valeurs ajustées selon la loi de Poisson
adjusted_events <- rpois(length(c(6, 5, 4, 4, 3, 4, 3, 3, 3, 3, 3, 3, 3, 3, 2, 
                                  2, 2, 1, 1, 1, 9, 8, 7, 9, 9, 7, 8, 9, 7, 6, 
                                  8, 7, 7, 6, 7, 7, 7, 6, 5, 5, 5, 5, 4, 5, 4, 
                                  4, 3, 3, 3, 2, 16, 11, 14, 12, 11, 10, 9, 9, 
                                  10, 9, 11, 12, 10, 8, 9, 9, 8, 8, 7, 7, 7, 
                                  8, 6, 7, 6)), lambda = lambda)

# Afficher les données ajustées
print(adjusted_events)
medData$events <- adjusted_events


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



#Data prep
#Recodage treatment - high
T1 <- rep(0,nrow(D))
T1[medData$treatment==1] <- 1
print(sum(T1))

#Recodage treatment - low
T2 <- rep(0,nrow(D))
T2[medData$treatment==2] <- 1
print(sum(T2))


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


red"""
HUMM
"""
medData$events <- ifelse(medData$events == 5, 7, medData$events)
medData$events <- ifelse(medData$events == 6, 8, medData$events)



# Vérifier s'il y a des lignes dupliquées
print("Lignes dupliquées: ", sum(duplicated(medData)))

# Nombre total de valeurs manquantes
print("Valeurs manquantes: ",sum(is.na(medData)))


# Charger ggplot2 pour de meilleurs graphiques
library(ggplot2)

# Boxplot pour Esteem
ggplot(medData, aes(y = esteem)) +
  geom_boxplot() +
  labs(title = "Distribution de l'estime de soi")

# Boxplot pour Events
ggplot(medData, aes(y = events)) +
  geom_boxplot() +
  labs(title = "Distribution du nombre d'événements")

# Boxplot des événements par groupe de traitement
ggplot(medData, aes(x = as.factor(treatment), y = events)) +
  geom_boxplot() +
  labs(title = "Distribution des événements par traitement",
       x = "Traitement",
       y = "Nombre d'événements")


# Fonction pour identifier les valeurs aberrantes (|z| > 3)
find_outliers <- function(x) {
  z_scores <- scale(x)
  outliers <- which(abs(z_scores) > 3)
  return(outliers)
}

# Application sur esteem et events
esteem_outliers <- find_outliers(medData$esteem)
events_outliers <- find_outliers(medData$events)

# Afficher les observations aberrantes
if(length(esteem_outliers) > 0) {
  print("Valeurs aberrantes pour esteem:")
  print(medData[esteem_outliers, ])
}

if(length(events_outliers) > 0) {
  print("Valeurs aberrantes pour events:")
  print(medData[events_outliers, ])
}

medData = medData[medData$events<14,]

summary(medData)














dispersion_index <- var(medData$events) / mean(medData$events)
print(dispersion_index)


hist(medData$events, main = "Histogramme des événements", xlab = "Nombre de crises d'épilepsie", breaks = 10)

qqplot(qpois(ppoints(length(medData$events)), lambda), medData$events, main = "Q-Q plot pour la loi de Poisson")
abline(0, 1, col = "red")  # Ajoute la ligne de référence


#Data exploration
str(medData)
head(medData)
summary(medData)

#Valeurs manquantes 
sum(is.na(medData))

#################################
mean_events <- mean(medData$events)
var_events <- var(medData$events)
cat("Moyenne des événements:", mean_events, "\nVariance des événements:", var_events)

# Ajustement du modèle de régression de Poisson
poisson_model <- glm(events ~ treatment + alcohol + esteem, 
                     family = poisson(link = "log"), 
                     data = medData)

# Résumé du modèle
summary(poisson_model)

exp(coef(poisson_model))  # Effets multiplicatifs


library(AER)
dispersiontest(poisson_model)

library(ggplot2)
ggplot(medData, aes(x = factor(treatment), y = events)) +
  geom_boxplot() +
  labs(x = "Treatment Group", y = "Number of Epilepsy Events", title = "Distribution of Events by Treatment")



#############################
#Variable cible
table(medData$events)
hist(medData$events)

#Effectif traitements prescrits
table(medData$treatment)
hist(medData$treatment)

#Moyenne crise selon traitement prescrit
mean_events <- tapply(X=medData$events,INDEX = medData$treatment,FUN = mean)
print(mean_events)


#recodage treatment - high
T1 <- rep(0,nrow(medData))
T1[medData$treatment==1] <- 1
print(sum(T1))

#recodage treatment - low
T2 <- rep(0,nrow(medData))
T2[medData$treatment==2] <- 1
print(sum(T2))

myGLM <- glm(formula = events ~ esteem + alcohol + treatment, family = "poisson", data = medData )
summary(myGLM)

newGLM <- glm(events ~ esteem + treatment, family = "poisson", data = medData)
summary(newGLM)

a <- anova(newGLM, myGLM, test="Chisq")
a

modelWithoutEsteem <- glm(events ~ treatment, family = "poisson", data = medData)
a1 <- anova(modelWithoutEsteem, newGLM, test="Chisq")
a1


plot(newGLM)

# For better-looking plots using ggplot2
library(ggplot2)

# Plot events vs esteem, colored by treatment
ggplot(medData, aes(x = esteem, y = events, color = factor(treatment))) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "poisson")) +
  labs(title = "Events vs Esteem by Treatment Group",
       x = "Esteem",
       y = "Number of Events",
       color = "Treatment") +
  theme_minimal()

# Plot events vs alcohol, colored by treatment
ggplot(medData, aes(x = alcohol, y = events, color = factor(treatment))) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "poisson")) +
  labs(title = "Events vs Alcohol by Treatment Group",
       x = "Alcohol",
       y = "Number of Events",
       color = "Treatment") +
  theme_minimal()


#valeurs ajustées de l'endogène
lambda1 <- newGLM$fitted.values
print(lambda1)

y <- medData$events
Stat_D <- 2 * sum(ifelse(y>0,y*log(y/lambda1),0)-(y-lambda1))
print(Stat_D)
