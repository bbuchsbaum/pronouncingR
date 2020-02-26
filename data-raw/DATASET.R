## code to prepare `DATASET` dataset goes here

##usethis::use_data("DATASET")
library(hashmap)

lines <- readLines("data-raw/cmudict-0.7b-with-vitz-nonce")
lines <- lines[-grep("^[;%']", lines)]
lines <- lines[-grep("^\\#", lines)]
lines <- lines[-grep("^\\.", lines)]
lines <- lines[-(1:49)]
lines <- trimws(lines)
linesplit <- strsplit(lines, "  ")
linesplit <- linesplit[sapply(linesplit, length) == 2]


all_features <- list()
entries <- list()

normalize <- function(x) x/sqrt(sum(x^2))

H <- NULL
for (i in 1:length(linesplit)) {    
  if (i %% 5000 == 0) {
    print(i)
  }
  l <- linesplit[[i]]
  features <- feature_bigrams(strsplit(l[[2]], " ")[[1]])
  entries[[l[[1]]]] <- features
  if (is.null(H)) {
    H <- hashmap(features, rep(1,length(features)))
  } else {
    H[[features]] <- ifelse(is.na(H[[features]]), 1, H[[features]] + 1)
  }
  #all_features <- list(all_features, list(features))
  #a <- list(a, list(i))
}

keys <- H$keys()
counts <- H[[keys]]
feats <- sort(keys[counts >=2])
fmat <- lapply(entries, function(ent) {
  ind <- sort(match(ent, feats))
  fvec <- numeric(length(feats))
  tind <- table(ind)
  fvec[as.integer(names(tind))] <- tind
  normalize(fvec)
})

fmat <- do.call(rbind, fmat)
pcres <- prcomp(fmat, center=TRUE, scale=FALSE)

projection <- pcres$rotation %*% diag(1/(pcres$sdev + epsilon)) 

out <- list(
  centroid=colMeans(fmat),
  words = tolower(names(entries)),
  projmat = pcres$rotation[,1:100],
  whprojmat = projection[,1:100],
  sdev = pcres$sdev,
  components=pcres$x,
  whcomponents=apply(pcres$x[,1:100],2,normalize)
)
  
  


pcres2 <- sk$PCA(n_components=as.integer(50), whiten=TRUE)  
transformed = pcres2$fit_transform(fmat)
#features = Counter(feature_bigrams(phones.split()))
#entries.append((word, features))
#all_features.update(features.keys())