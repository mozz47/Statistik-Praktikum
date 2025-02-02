#1a)
data = read.table("oecdM.csv", header = TRUE, sep = ",", dec=".")
head(data)

#1b)
listOfMean = lapply(data, mean, na.rm = TRUE)
listOfVar = lapply(data, var, na.rm = TRUE)

listOfMean
listOfVar

#1c)

"Niederlande" %in% data[[1]]
"China" %in% data[[1]]

#1d)

indexAlkohol = which.max(data$Alkohol)
data[indexAlkohol,]

#1e)

indexDeath = which.min(data$Säuglsterblichkeit)
data[indexDeath,]


#1f)

averageMovement = mean(data$Bewegung, na.rm = TRUE)

indexBelowAverage = which(data[["Bewegung"]] < averageMovement)

data[indexBelowAverage,][["X"]]



#2a)
splitDataRegion = split(data, f = data$Geo)
belongsEurope = nrow(splitDataRegion$E)
belongsRest = nrow(splitDataRegion$R)
pie(x = (c(belongsRest, belongsEurope)), 
    labels = c(paste("Rest der Welt", belongsRest), paste("Europa", belongsEurope)), 
    col = c("blue", "green"), 
    main = "Länderverteilung")



#2b)
stripchart(data$Lesen ~ data$Geo, 
           group.names = c("Europa", "Rest"), 
           vertical = TRUE,
           method = "jitter", jitter = 0.05,
           main = "PISA-Score Vergleich",
           ylab = "PISA-Score")
data[which.max(data[["Lesen"]]),]

#3a)

boxplot(data$Bildung, main = "Boxplot von Bildung")

#3b)

quantile(data$Bildung, na.rm = TRUE)

#3c)
orderedEducation = lapply(data[4], sort)

#3d)
plot(orderedEducation$Bildung, main = "Anteil Kinder ohne Grundaustattung", xlab = "Länder", ylab = "Anteil Kinder")
abline(v = 23, col = "red")
abline(h = 2.2, col = "red")

#4a)
set.seed(2023)
x1 = rexp(100, rate = 0.1)
x2 = rexp(100, rate = 0.1)
for (i in 1:100) {
  x2[i] = 20-x2[i]
}

#4b)
t.test(x1, x2)

#4c)
wilcox.test(x1,x2)

#5a)
data = read.table("PISA.csv", header = TRUE, sep = ',', dec=".")

#5b)
#R00-R06
boxplot(data$R00, data$R06, names = c("2000", "2006"), col = c("blue", "green"),
        main = "Lesekompetenz")

#M00-M06
boxplot(data$M00, data$M06, names = c("2000", "2006"), col = c("blue", "green"),
        main = "Mathematik-Kompetenz")

#S00-S06
boxplot(data$S00, data$S06, names = c("2000", "2006"), col = c("blue", "green"),
        main = "Naturwissenschaftliche Kompetenz")

#5c)

#Normalverteilung

shapiro.test(data$R00)#p = 0.7609-> Normalverteilt
shapiro.test(data$R06)#p = 0.7889-> Normalverteilt

shapiro.test(data$M00)#p = 0.005166-> nicht Normalverteilt
shapiro.test(data$M06)#p = 0.006494-> nicht Normalverteilt

shapiro.test(data$S00)#p = 0.2752-> Normalverteilt
shapiro.test(data$S06)#p = 0.0009737-> nicht Normalverteilt

#Varianzhomogenität

var.test(data$R00, data$R06) #p = 0.6979 -> Varianzhomogen
ansari.test(data$M00-mean(data$M00),data$M06-mean(data$M06)) #p = 0.03509 -> Varianzheterogen
ansari.test(data$S00-mean(data$S00),data$S06-mean(data$S06)) #p = 0.2861 -> Varianzhomogen

t.test(data$R00, data$R06, conf.level = 0.95) #p-value = 0.3937-> kein signifikanter Unterschied
wilcox.test(data$M00, data$M06) #p-value = 0.6281 -> kein signifikanter Unterschied
wilcox.test(data$S00, data$S06) #p-value = 0.642 -> kein signifikanter Unterschied


#6)

data = read.table("Hustensaft.csv", header = TRUE, dec=".")

#parameterfreier Test, weil Stichprobe < 10
wilcox.test(data$Kon, mu = 40, alternative = "less")
#p-value = 0.08203

#7a)

sues = read.table("Suess.csv", header = TRUE, dec=".", sep = ";")

#7b)

coplot(sues$Geschmack ~ sues$Feuchtigkeit | sues$Suesse, pch = c(5,18), rows = 1, columns = 3,
       xlab = "Feuchtigkeit",
       ylab = "Geschmack")

#7c)
linear.model = lm(sues$Geschmack ~ sues$Feuchtigkeit + sues$Suesse)
summary(linear.model)
#r^2 = 0.9462, Modell erklärt die Varianz fast vollständig, Geschmack und Feuchtigkeit+Suesse sind linear korreliert


#7d)
residuen = resid(linear.model)
plot(sues$Suesse*sues$Feuchtigkeit, residuen,
     main = "Residuen gegen Feuchtigkeit * Suesse",
     xlab = "Suesse * Feuchtigkeit",
     ylab = "Residuen")
abline(0,0)
qqnorm(residuen)
plot(density(residuen))

#7e)

new.linear.model = lm(sues$Geschmack ~ sues$Feuchtigkeit + sues$Suesse + (sues$Feuchtigkeit*sues$Suesse))
summary(new.linear.model)
#r^2 = 0.9742


#7f)
new.residuen = resid(new.linear.model)
plot(sues$Suesse*sues$Feuchtigkeit, new.residuen,
     xlab = "Suesse * Feuchtigkeit",
     ylab = "Residuen (Modell 2)")
abline(0,0)
qqnorm(new.residuen)
plot(density(new.residuen))

#nochmal als Vergleich plot1 und 2 nebeneinander gelegt und die Dichtefunktionen:
par(mfrow=c(1,2))
plot(sues$Suesse*sues$Feuchtigkeit, residuen, col="blue", ylim = c(-6,5),
     xlab = "Suesse * Feuchtigkeit",
     ylab = "Residuen")
abline(0,0)

plot(sues$Suesse*sues$Feuchtigkeit, new.residuen, col = "red", ylim = c(-6,5),
     xlab = "Suesse * Feuchtigkeit",
     ylab = "Residuen Neu")
abline(0,0)

plot(density(residuen), main = "Residuen")
plot(density(new.residuen), main = "Residuen Modell 2")

par(mfrow=c(1,1))

setwd("C:/Users/pmosk/OneDrive/Desktop/Statistik/Statistik Praktikum/Praktikum/Aufgabe 8 Korrelation")
#AUFGABE 8: Korrelationen - Eigene Untersuchung

#Datensatz A : Median-Einkommen pro Jahr in € (2008-2019)
#Datensatz B: Anzahl der Flugpassagiere (bei Abflug) in einem Land pro Jahr (2008-2019)

#8a)

flug = read.table("Flug.csv", sep = ",", header = T)
einkommen = read.table("Einkommen.csv", sep = ",", header = T)
head(flug)
head(einkommen)

#Zeile für DE umwandeln in numerischen Vektor
gerEinkommen = as.numeric(einkommen[5,][-1])

plot(2008:2019, gerEinkommen,
     main = "Deutsches Median-Einkommen p.a",
     sub = "Variable A",
     type = "b",
     xlab = "Jahr", ylab = "Deutsches Median-Einkommen")

gerFlug = as.numeric(flug[5,][-1])

plot(2008:2019, prettyNum(gerFlug/1000000, digits = 2), 
     main = "Passagieranzahl bei Abflug in DE",
     sub = "Variable B",
     xlab = "Jahr", ylab = "Passagieranzahl (in Mio.)", type = "b")

#8b)

plot(gerEinkommen, prettyNum(gerFlug/1000000, digits = 2),
     main = "Passagieranzahl DE ~ Median-Einkommen DE",
     xlab = "Einkommen",
     ylab = "Passagieranzahl (in Mio.)")

cor.test(gerEinkommen, gerFlug)

# stark positive Korrelation, rxy = 0.978065

#8c) 
#Korrelation Einkommen der Deutschen und Flugpassagiere der EU-Länder

correlations = c()
for (i in 1:nrow(flug)) { 
  flugVektor = as.numeric(flug[i,][-1])
  corTest = cor.test(gerEinkommen, flugVektor)
  correlations = c(correlations, corTest$estimate[[1]])
  
}

hist(correlations,
     main = "Korrelation (Einkommen DE ~ Flugpassagiere EU-Länder)",
     ylab = "Anzahl",
     xlab = "Korrelationskoeffizient")

indexMaxFlug = which.max(correlations)
indexMinFlug = which.min(correlations)

#Welches Land ist das mit der kleinsten COR?
flug[indexMinFlug,]

#Welches Land ist das mit der größten COR?
flug[indexMaxFlug,]

slowenienFlug = as.numeric(flug[indexMinFlug,][-1])
frankreichFlug = as.numeric(flug[indexMaxFlug,][-1])

cor.test(gerEinkommen, slowenienFlug)
cor.test(gerEinkommen, frankreichFlug)

plot(gerEinkommen, prettyNum(slowenienFlug/1000, digits = 2),
     main = "Korrelation : Einkommen Deutschland und Flugpassagiere Slowenien",
     xlab = "Deutsches Median-Einkommen",
     ylab = "Flugpassagiere Slowenien (in Tsd.)")

plot(gerEinkommen, prettyNum(frankreichFlug/1000000, digits = 2),
     main = "Korrelation : Einkommen Deutschland und Flugpassagiere Frankreich",
     xlab = "Deutsches Median-Einkommen",
     ylab = "Flugpassagiere Frankreich (in Mio.)")



#9a)
load("C:/Users/pmosk/OneDrive/Desktop/Statistik/Statistik Praktikum/Praktikum/SPECTF.RData")
head(SPECTF)
nrow(SPECTF)

#9b)
vectorSpan = c()
for (i in 1:length(SPECTF)) {
  if (i == 1) {
    next
  }
  span = max(SPECTF[[i]]) - min(SPECTF[[i]])
  vectorSpan = c(vectorSpan, span)
}
summary(vectorSpan)
hist(vectorSpan, 
     xlab = "Spannweite", 
     ylab = "Anzahl", 
     main = "Histogramm der Spannweite")

#9c)
#Liste erstellen mit den Varianzen
varList = lapply(SPECTF, var)

# Namen der Komponenten extrahieren
namesList = names(varList)

# Index der sortierten Werte erhalten
index = order(unlist(varList))

# Liste neu anordnen
varList = varList[namesList][index]

# Ausgabe der sortierten Liste
varList

varList[40:45]

par(mfrow=c(2,3))

boxPlotX25 = boxplot(SPECTF$X25, main = "Boxplot von X25")
boxPlotX30 = boxplot(SPECTF$X30, main = "Boxplot von X30")
boxPlotX26 = boxplot(SPECTF$X26, main = "Boxplot von X26")
boxPlotX41 = boxplot(SPECTF$X41, main = "Boxplot von X41")
boxPlotX44 = boxplot(SPECTF$X44, main = "Boxplot von X44")
boxPlotX42 = boxplot(SPECTF$X42, main = "Boxplot von X42")

par(mfrow=c(1,1))


#10b)
set.seed(123)
load("C:/Users/pmosk/OneDrive/Desktop/Statistik/Statistik Praktikum/Praktikum/SPECTF.RData")
library(caret)
library(pROC)
library(ROCR)

SPECTF$X0 = factor(SPECTF$X0, levels = c(0,1))

dataPartition = createDataPartition(SPECTF$X0, list = T, p = 0.8) 
#p bestimmt Verhältnis der Daten 

#unabhängiger Datensatz an dem das Modell später getestet werden kann
dataToTestLater = SPECTF[-dataPartition[[1]],]

#Daten mit denen trainiert wird
dataToTrain = SPECTF[dataPartition[[1]],]

#10 Folds erstellen
tenKFolds = createFolds(dataToTrain$X0, k = 10, list = T)

results = list()

for (i in 1:10) {
  
  #indexe verteilen auf test und train
  
  train_fold = dataToTrain[-tenKFolds[[i]],]
  test_fold = dataToTrain[tenKFolds[[i]],]
  
  #logistische Regression anwenden
  model = glm(X0 ~ ., data = train_fold, family = "binomial")
  
  #Vorhersage auf Testdaten machen
  preds = predict(model, newdata = test_fold, type = "response")
  
  #Ergebnisse speichern
  results[[i]] = data.frame(obs = test_fold$X0, pred = preds)
  
}

#AUC, ROC, Conf-Intervall

#10c)

#roc

roc = roc(unlist(lapply(results, "[[", "obs")), 
          unlist(lapply(results, "[[", "pred")))

#auc

auc = auc(roc)

#Cl

ci = ci.auc(roc)

#10d)
pdf("ROC SPECTF.pdf")
plot(roc, print.auc = TRUE, main = "ROC-Kurve SPECTF-Daten")
legend("bottomright", paste("CI: [", round(ci[1], 3), ", ", round(ci[3], 3), "]"))
dev.off()