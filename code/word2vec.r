library(devtools)
#install_github("bmschmidt/wordVectors", force = T)
library(wordVectors)
library(tsne)
library(Rtsne)
library(ggplot2)
library(ggrepel)

prep_word2vec("~/Desktop/University/2018 T1/review_data/north_attraction_reviews_only.txt", "processed.txt", lowercase = T)

# training the model
pepys <- train_word2vec("processed.txt", output = "processed_model.bin", threads = 1, vectors = 100, window = 12)

closest_to(pepys, "bathroom", n = 10)

# plot the closest terms using tsne
plot(prpys, pepys[["bathroom"]])
