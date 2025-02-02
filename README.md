# Praktikum: Grundlagen der Statistik

## Überblick
Dieses Repository enthält den Praktikumsbericht und dazugehörigen Code zu **Grundlagen der Statistik**, in dem verschiedene statistische Methoden mit **R** angewendet wurden. Die Dokumentation umfasst Datenanalyse, Visualisierung, statistische Tests sowie Modellierung und maschinelles Lernen. Der Bericht schließt mit einer eigenen Untersuchung zur Korrelation zwischen Einkommen und Flugpassagieren in Deutschland ab.

## Inhalt

### 1. Datenanalyse und Visualisierung
- Laden und Vorverarbeitung von Datensätzen
- Berechnung von Mittelwerten, Varianzen und Quantilen
- Erstellung von Boxplots, Stripcharts und Kuchendiagrammen

### 2. Statistische Tests
- t-Test zur Mittelwertuntersuchung
- Wilcoxon-Test für nicht-parametrische Vergleiche
- ANOVA zur Analyse von Gruppenunterschieden
- Produktionskontrolle mittels Hypothesentests

### 3. Regressionsmodelle
- Lineare Regression mit und ohne Interaktionsterme
- Untersuchung der Residuenverteilung
- Interpretation der Modellgüte (R², p-Werte)

### 4. Multivariate Verfahren
- Korrelationen zwischen verschiedenen Variablen
- Kreuzvalidierung und Evaluierung von Modellen
- Erstellung von ROC-Kurven zur Beurteilung der Vorhersagequalität

### 5. Eigene Untersuchung
Zum Abschluss wurde eine eigene Analyse zur Korrelation zwischen dem **jährlichen Einkommen in Deutschland** und der **Anzahl der Flugpassagiere** durchgeführt. Dabei wurden offizielle EU-Daten genutzt, visualisiert und statistisch ausgewertet.

## Anforderungen
Zur Ausführung der Skripte wird **R** benötigt. Die folgenden Pakete sollten installiert sein:
```r
install.packages(c("ggplot2", "dplyr", "pROC", "ROCR", "caret"))
```

