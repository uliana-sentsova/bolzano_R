setwd("~/Desktop/Bolzano/LCR2017/data")

prep.names <- c("in", "null", "on", "with", "at","to","by", "from", "through", "inside", "for")

# Data for initial distribution of prepositions
corpus.distr <- read.csv("prep_distr.txt")
colnames(corpus.distr) <- prep.names

# Data for distribution of prepositions derived from our sample
experimental.distr <- read.csv("prep_rel_freq.txt")
missing <- which(colnames(experimental.distr) %in% c("syn", "among", "NA."))
experimental.distr <- experimental.distr[,-missing]
colnames(experimental.distr) <- prep.names

# Data for students
student.preposition <- read.csv("stud_prep.txt")
studnames <- student.preposition[,1]
rownames(student.preposition) <- studnames
student.preposition <- student.preposition[,-1]
student.preposition <- student.preposition[,-missing]
colnames(student.preposition) <- prep.names

# Data for sentences
sentence.preposition <- read.csv("sent_prep.txt")
sentnames <- sentence.preposition[,1]
rownames(sentence.preposition) <- sentnames
sentence.preposition <- sentence.preposition[,-1]
sentence.preposition <- sentence.preposition[,-missing]
colnames(sentence.preposition) <- prep.names


# Normalization

for (i in 1:length(student.preposition[,1])){
        student.preposition[i,] <- round(student.preposition[i,]/sum(student.preposition[i,]),4)
}

for (i in 1:length(sentence.preposition[,1])){
        sentence.preposition[i,] <- round(sentence.preposition[i,]/sum(sentence.preposition[i,]),4)
}



# ========================================

# Comparing probabilities of prepositions
corpus.vs.experimental <- rbind(corpus.distr, experimental.distr)
rownames(corpus.vs.experimental) <- c("corpus data", "experimental data")

colnames(corpus.vs.experimental) <- prep.names

my_bar=barplot(as.matrix(corpus.vs.experimental), beside=T, ylim = c(0, 0.5),
               las=1 , col=c("royalblue", "midnightblue"), 
               main="corpus vs experiment",  ylab = "Probabilities", xlab="Prepositions")

legend("topright", legend = c("Corpus data","Experimental data" ), col=c("royalblue", "midnightblue"),  
       bty = "o", pch=15 , pt.cex = 2, cex = 0.8, horiz = FALSE, xjust=1, yjust=1)

# --- Display probabilities to the bars if needed ---
bars <- vector(mode="double")
j = 1
for (i in 1:11) {
        bars[j] <- corpus.vs.experimental[1,i]
        bars[j + 1] <- corpus.vs.experimental[2,i]
        j <- j + 2
}
text(my_bar, bars + 0.01, paste("",round(bars,3),sep="") ,cex=0.7, col="black")
# ---------------------------


# ==== CORRELATION ====
# Correlation (preposition from the whole table vs. corpus data)

cor.test(as.numeric(corpus.distr[1,]), as.numeric(experimental.distr[1,]), method="spearman")

# Correlation (professional translation vs. corpus data)
professional <- as.numeric(student.preposition[1,])
cor(as.numeric(corpus.distr[1,]), professional, method="spearman")

prof.vs.corpus <- rbind(professional, corpus.distr)

my_bar=barplot(as.matrix(prof.vs.corpus), beside=T,
               las=1 , col=c("royalblue", "midnightblue"), main="corpus vs experiment",
               ylim = c(0, 0.5))

legend("topright", legend = c("Corpus data","Experimental data" ), col=c("royalblue", "midnightblue"),  
       bty = "o", pch=15 , pt.cex = 2, cex = 0.8, horiz = FALSE, xjust=1, yjust=1)


# --- Display probabilities to the bars if needed ---
bars <- vector(mode="double")
j = 1
for (i in 1:11) {
       bars[j] <- prof.vs.corpus[1,i]
       bars[j + 1] <- prof.vs.corpus[2,i]
       j <- j + 2
}
text(my_bar, bars + 0.01, paste("",round(bars,3),sep="") ,cex=0.7, col="black") 
# ---------------------------


# Distribution of errors
error.distr <- read.csv("errors.txt")
colnames(error.distr) <- c("in", "on", "syn", "with", "at", "from", "among")

names <- intersect(colnames(corpus.distr.restr), colnames(error.distr))
names <- c(names, "among")
error.distr <- error.distr[, which(colnames(error.distr) %in% names)]

corpus.distr.restr <- corpus.distr[, which(colnames(corpus.distr) %in% names)]
corpus.distr.restr[, "among"] <- 0.000001

# Correlation between students' mistakes and corpus
cor.test(as.numeric(corpus.distr.restr[1,]), as.numeric(error.distr[1,]), method="spearman")
corpus.vs.errors <- rbind(corpus.distr.restr, error.distr)
rownames(corpus.vs.errors) <- c("corpus data", "error distribution")
colnames(corpus.vs.errors) <- colnames(error.distr)

barplot(as.matrix(error.distr), horiz=T, xlim=c(0, 0.9),
        main="Prepositions in learner errors", las=1, xlab = "Probabilities")

barplot(as.matrix(corpus.distr.restr), horiz=T, xlim=c(0, 0.9),
        main="Prepositions in corpus", las=1, xlab = "Probabilities")

# ==== STUDENTS ====

# Distance: comparing students to corpus data
distance <- vector(mode="double")

students.only <- student.preposition[2:46,]

for (i in 1:length(students.only[,1])){
        distance[i] <- dist(rbind(students.only[i,], corpus.distr[1,]), method = "canberra")
}

# Z-standartization
distance <- (distance - mean(distance)) / sd(distance)

student.distances <- as.data.frame(distance)
student.distances$name <- rownames(students.only)
row.names(student.distances) <- str_replace(row.names(students.only), "stu", "")

# Ordered plot
plot(student.distances[order(student.distances$distance),1], type="p",
     las=1, ylab="Distance from corpus data (z-scale)",xlab="", main="Distance")
abline(h=mean(student.distances$distance), col = "green", lty = 5)
abline(h=mean(student.distances$distance)-2*sd(student.distances$distance), col = "red", lty = 2)
abline(h=mean(student.distances$distance)+2*sd(student.distances$distance), col = "red", lty = 2)
abline(h=mean(student.distances$distance)+sd(student.distances$distance), col = "grey", lty = 2)
abline(h=mean(student.distances$distance)-sd(student.distances$distance), col = "grey", lty = 2)

text(x=3, y=0.1, "mean", col="green")
text(x=36, y=2.1, "two standard deviations", col="red")

# --- Display student's labels ---
# either all students ...
text(student.distances[order(student.distances$distance),1]+0.1,
     labels = rownames(student.distances[order(student.distances$distance),]), cex=0.7)
#    ... or outlier only
text(x=18, y = 3.25, labels = "18", col="black",cex=0.8)
# ---------------------------

# Simple plot
plot(student.distances$distance, type="p",
     las=1, ylab="Distance from corpus data (z-scale)",xlab="", main="Distance")
abline(h=mean(student.distances$distance), col = "green", lty = 5)
abline(h=mean(student.distances$distance)-2*sd(student.distances$distance), col = "red", lty = 2)
abline(h=mean(student.distances$distance)+2*sd(student.distances$distance), col = "red", lty = 2)
abline(h=mean(student.distances$distance)+sd(student.distances$distance), col = "grey", lty = 2)
abline(h=mean(student.distances$distance)-sd(student.distances$distance), col = "grey", lty = 2)

text(x=3, y=0.1, "mean", col="green")
text(x=33, y=2.1, "two standard deviations", col="red")


# --- Display student's labels ---
# either all students ...
text(student.distances$distance+0.1,
     labels = rownames(student.distances), cex=0.7)
#    ... or outlier only
text(x=18, y = 3.25, labels = "18", col="black",cex=0.8)
# ---------------------------

# Students that are different by two standard deviations:
student.distances[which((student.distances$distance - mean(student.distances$distance)) > 2*sd(student.distances$distance)),2]

# Students that are different by one standard deviation:
student.distances[which((student.distances$distance - mean(student.distances$distance)) > sd(student.distances$distance)),2]

# Scale for students (from typical to non-typical)
student.scale <- student.distances[order(student.distances$distance),2]
student.scale

# ========================================
# Distance: comparing students to professional translators

distance <- vector(mode="double")
for (i in 1:length(students.only[,1])){
        distance[i] <- dist(rbind(students.only[i,], professional), method = "euclidean")
}

distance <- (distance - mean(distance)) / sd(distance)

student.distances.2 <- as.data.frame(distance)
student.distances.2$name <- row.names(students.only)
row.names(student.distances.2) <- str_replace(student.distances.2$name, "stu", "")

# Ordered plot
plot(student.distances.2[order(student.distances.2$distance),1], type="p",
     las=1, ylab="Distance from corpus data (z-scale)",xlab="", main="Distance")
abline(h=mean(student.distances.2$distance), col = "green", lty = 5)
abline(h=mean(student.distances.2$distance)-2*sd(student.distances.2$distance), col = "red", lty = 2)
abline(h=mean(student.distances.2$distance)+2*sd(student.distances.2$distance), col = "red", lty = 2)
abline(h=mean(student.distances.2$distance)+sd(student.distances.2$distance), col = "grey", lty = 2)
abline(h=mean(student.distances.2$distance)-sd(student.distances.2$distance), col = "grey", lty = 2)

text(x=3, y=0.1, "mean", col="green")
text(x=36, y=2.1, "two standard deviations", col="red")


# --- Display student's labels ---
# either all students ...
text(student.distances.2[order(student.distances.2$distance),1]+0.1,
     labels = rownames(student.distances.2[order(student.distances.2$distance),]),
     cex=0.7)
#    ... or outlier only
text(x=18, y = 3.25, labels = "18", col="black",cex=0.8)
# ---------------------------

# Simple plot
plot(student.distances.2$distance, type="p",
     las=1, ylab="Distance from corpus data (z-scale)",xlab="", main="Distance")
abline(h=mean(student.distances.2$distance), col = "green", lty = 5)
abline(h=mean(student.distances.2$distance)-2*sd(student.distances.2$distance), col = "red", lty = 2)
abline(h=mean(student.distances.2$distance)+2*sd(student.distances.2$distance), col = "red", lty = 2)
abline(h=mean(student.distances.2$distance)+sd(student.distances.2$distance), col = "grey", lty = 2)
abline(h=mean(student.distances.2$distance)-sd(student.distances.2$distance), col = "grey", lty = 2)

text(x=3, y=0.1, "mean", col="green")
text(x=33, y=2.1, "two standard deviations", col="red")


# --- Display students' labels ---
# either all students ...
text(student.distances.2$distance+0.1,
     labels = rownames(student.distances.2), cex=0.7)
#    ... or outlier only
text(x=18, y = 2.8, "18", col="black",cex=0.8)
# ---------------------------

# Students that are different by two standard deviations:
student.distances.2[which((student.distances.2$distance - mean(student.distances.2$distance)) > 2*sd(student.distances.2$distance)),2]

# Students that are different by one standard deviation:
student.distances.2[which((student.distances.2$distance - mean(student.distances.2$distance)) > 1*sd(student.distances.2$distance)),2]

# Scale for students
student.scale.2 <- student.distances.2[order(student.distances.2$distance),2]
student.scale.2





# -------- IMPURITY MEASURES -----

gini = function(prob) {
        coef <- 0
        for (i in 1:length(prob)) { coef <- coef + (prob[,i])^2}
        return(1 - coef)
}

gini.coefs <- vector(mode="numeric")

for (i in 1:46) {
        gini.coefs[i] <- gini(student.preposition[i,])
}
gini.coefs <- as.data.frame(gini.coefs)
row.names(gini.coefs) <- row.names(student.preposition)
gini.coefs$names <- row.names(student.preposition)
colnames(gini.coefs) <- "gini"
gini.coefs[order(gini.coefs$gini),2]

