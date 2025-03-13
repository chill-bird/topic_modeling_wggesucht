# TODO: Set your working directory
setwd('/path/to/source/directory')
library("dplyr")
library(stringr)
library(udpipe)

# Read the CSV data
data <- read.csv("./dat/data.csv", sep = ",", encoding = "UTF-8")
col.id <- "anzeigennummer"
col.text <- "description"
col.is_flinta <- "is_flinta"

# German UDPipe model
m_ger <- udpipe::udpipe_download_model(language = "german")
german_model <- udpipe::udpipe_load_model(m_ger$file)

# Words from description column
words <- data[[col.text]]
words <- paste(words, collapse = " ")
words <- str_replace_all(words, "[^[:alpha:]\\s]", " ")
words <- str_squish(words)

# Create a list from words by separating text at " "
list.words <- unique(unlist(str_split(words, "\\s+")))
# Remove words with length < 3 and >= 20
list.words <- list.words[nchar(list.words) >= 3 &
                           nchar(list.words) <= 20]

# Apply lemmatization
annotations <- as.data.frame(udpipe_annotate(german_model, x = list.words))
df.result <- annotations[c("token", "lemma")]
colnames(df.result)[colnames(df.result) == "token"] = "word"
df.result <- na.omit(df.result)
# Remove rows where both values are the same word
df.result <- df.result[df.result[["word"]] != df.result[["lemma"]], , drop = FALSE]
df.result <- df.result[!grepl("\\|", df.result$lemma), ]
df.result$lemma <- gsub("achrich", "achricht", df.result$lemma)
df.result$lemma <- gsub("aktivitäen", "aktivitäten", df.result$lemma)
df.result$lemma <- gsub("chrinken", "chrank", df.result$lemma)
df.result$lemma <- gsub("Garen", "Garten", df.result$lemma)
df.result$lemma <- gsub("garen", "garten", df.result$lemma)
df.result$lemma <- gsub("inue", "inute", df.result$lemma)
df.result$lemma <- gsub("inuten", "inute", df.result$lemma)
df.result$lemma <- gsub("kelle", "keller", df.result$lemma)
df.result$lemma <- gsub("kosen", "kosten", df.result$lemma)
df.result$lemma <- gsub("lltiegen", "lltag", df.result$lemma)
df.result$lemma <- gsub("Mien", "Miete", df.result$lemma)
df.result$lemma <- gsub("mien", "mieten", df.result$lemma)
df.result$lemma <- gsub("nteressiern", "nteressiert", df.result$lemma)
df.result$lemma <- gsub("sonsen", "sonst", df.result$lemma)
df.result$lemma <- gsub("sporen", "sport", df.result$lemma)
df.result$lemma <- gsub("studenen", "studenten", df.result$lemma)
df.result$lemma[df.result$lemma == "liebsen"] <- "liebsten"
df.result$lemma[df.result$lemma == "interessanen"] <- "interessant"
df.result$lemma[df.result$lemma == "meinsamen"] <- "gemeinsam"
df.result$lemma[df.result$word == "Nachricht"] <- "Nachricht"
df.result$lemma[df.result$word == "nachricht"] <- "Nachricht"
df.result$lemma[df.result$word == "Sportarten"] <- "Sportarten"
df.result$lemma[df.result$word %in% c("entspannte", "entspannt")] <- "entspannt"
df.result$lemma[df.result$word %in% c("lehramt", "Lehramt")] <- "Lehramt"
df.result$lemma[df.result$word %in% c("min", "Min")] <- "Minute"
df.result$lemma[df.result$word %in% c("grüße", "Grüße")] <- "Gruß"
df.result$lemma[df.result$word %in% c("interessiere", "interessierst")] <- "interessieren"
df.result$lemma[df.result$word %in% c("inkl", "Inkl")] <- "inklusive"
df.result$lemma[df.result$word %in% c("recht", "rechte")] <- "recht"
df.result$lemma[df.result$word %in% c("schnell", "schnellste", "schnellen")] <- "schnell"
df.result$lemma[df.result$word %in% c("separat", "separate", "Separate")] <- "separat"
df.result$lemma[df.result$word %in% c("solltest", "Solltest")] <- "sollen"
df.result$lemma[df.result$word %in% c("Sport", "sport")] <- "Sport"
df.result$lemma[df.result$word %in% c("student", "Studentinnen", "Studenten")] <- "Student"
df.result$lemma[df.result$word %in% c("teilst", "teile")] <- "teilen"
df.result$lemma[df.result$word %in% c("Trockner", "trockner")] <- "Trockner"
df.result$lemma[df.result$word %in% c("Universität", "Universitäten")] <- "Universität"
df.result$lemma[df.result$word %in% c("Waschmaschine", "Waschmaschinen", "Waschmaschiene")] <- "Waschmaschine"
df.result$lemma[df.result$word %in% c("würdest", "würdet")] <- "werden"
df.result$lemma[df.result$word == "Badewanne"] <- "Badewanne"
df.result$lemma[df.result$word == "bist"] <- "sein"
df.result$lemma[df.result$word == "Diversität"] <- "Diversität"
df.result$lemma[df.result$word == "Dusche"] <- "Dusche"
df.result$lemma[df.result$word == "filme"] <- "Film"
df.result$lemma[df.result$word == "fuß"] <- "Fuß"
df.result$lemma[df.result$word == "garage"] <- "Garage"
df.result$lemma[df.result$word == "Gläschen"] <- "Glas"
df.result$lemma[df.result$word == "gemeinschaft"] <- "Gemeinschaft"
df.result$lemma[df.result$word == "Gemeinschaft"] <- "Gemeinschaft"
df.result$lemma[df.result$word == "interesse"] <- "Interesse"
df.result$lemma[df.result$word == "Interessenten"] <- "Interessent"
df.result$lemma[df.result$word == "internet"] <- "Internet"
df.result$lemma[df.result$word == "könntest"] <- "können"
df.result$lemma[df.result$word == "meldest"] <- "melden"
df.result$lemma[df.result$word == "mietest"] <- "mieten"
df.result$lemma[df.result$word == "Tasse"] <- "Tasse"
df.result$lemma[df.result$word == "Vermieterin"] <- "Vermieter"
df.result$lemma[df.result$word == "vorstellst"] <- "vorstellen"
df.result$lemma[df.result$word == "woche"] <- "Woche"
df.result <- rbind(df.result, data.frame(word = "Freunden", lemma = "Freund"))
df.result <- rbind(df.result, data.frame(word = "melde", lemma = "melden"))
df.result <- rbind(df.result, data.frame(word = "Melde", lemma = "melden"))
df.result <- rbind(df.result, data.frame(word = "Schreib", lemma = "schreiben"))
df.result <- rbind(df.result, data.frame(word = "schreib", lemma = "schreiben"))
df.result <- rbind(df.result, data.frame(word = "Studentin", lemma = "Student"))
df.result <- df.result[df.result[["word"]] != df.result[["lemma"]], , drop = FALSE]


# Sort data frame
df.result <- df.result[order(df.result$word), ]

# Save to CSV file
write.csv(df.result,
          "./dat/lemmas.csv",
          row.names = FALSE,
          fileEncoding = "UTF-8")
