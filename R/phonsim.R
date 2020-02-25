phone_feature_map = list(
  'M' = c('blb', 'nas'),
  'P' = c('vls', 'blb', 'stp'),
  'B' = c('vcd', 'blb', 'stp'),
  'F' = c('vls', 'lbd', 'frc'),
  'V' = c('vcd', 'lbd', 'frc'),
  'TH' = c('vls', 'dnt', 'frc'),
  'DH' = c('vcd', 'dnt', 'frc'),
  'N' = c('alv', 'nas'),
  'T' = c('vls', 'alv', 'stp'),
  'D' = c('vcd', 'alv', 'stp'),
  'S' = c('vls', 'alv', 'frc'),
  'Z' = c('vcd', 'alv', 'frc'),
  'R' = c('alv', 'apr'),
  'L' = c('alv', 'lat'),
  'SH' = c('vls', 'pla', 'frc'),
  'ZH' = c('vcd', 'pla', 'frc'),
  'Y' = c('pal', 'apr'),
  'NG' = c('vel', 'nas'),
  'K' = c('vls', 'vel', 'stp'),
  'G' =  c('vcd', 'vel', 'stp'),
  'W' = c('lbv', 'apr'),
  'HH' =  c('glt', 'apr'),
  'CH'=  c('vls', 'alv', 'stp', 'frc'),
  'JH' =  c('vcd', 'alv', 'stp', 'frc'),
  'AO'= c('lmd', 'bck', 'rnd', 'vwl'),
  'AA'= c('low', 'bck', 'unr', 'vwl'),
  'IY'= c('hgh', 'fnt', 'unr', 'vwl'),
  'UW'= c('hgh', 'bck', 'rnd', 'vwl'),
  'EH'= c('lmd', 'fnt', 'unr', 'vwl'),
  'IH'= c('smh', 'fnt', 'unr', 'vwl'),
  'UH'= c('smh', 'bck', 'rnd', 'vwl'),
  'AH'= c('mid', 'cnt', 'unr', 'vwl'),
  'AE'= c('low', 'fnt', 'unr', 'vwl'),
  'EY'= c('lmd', 'smh', 'fnt', 'unr', 'vwl'),
  'AY'= c('low', 'smh', 'fnt', 'cnt', 'unr', 'vwl'),
  'OW'= c('umd', 'smh', 'bck', 'rnd', 'vwl'),
  'AW'= c('low', 'smh', 'bck', 'cnt', 'unr', 'rnd', 'vwl'),
  'OY'= c('lmd', 'smh', 'bck', 'fnt', 'rnd', 'unr', 'vwl'),
  'ER'= c('umd', 'cnt', 'rzd', 'vwl'),
  '^'= c('beg'),
  '$'= c('end')
)


# Returns a feature tuple for an ARPAbet phone.
# [phone_to_features(p) for p in 'CH IY1 Z'.split()]
# [('vls', 'alv', 'stp', 'frc'), ('hgh', 'fnt', 'unr', 'vwl'), ('vcd', 'alv', 'frc')]

phone_to_features <- function(ph) {
  if (substr(ph, nchar(ph), nchar(ph)) %in% c(0,1,2)) {
    ph <- substr(ph, 1, nchar(ph)-1)
  }
  phone_feature_map[ph]
}

#Takes a list of ARPAbet phones and returns a list of features.
# feature_bigrams("M NG".split())
# ['blb-vel', 'blb-nas', 'nas-vel', 'nas-nas', 'vel-blb', 'vel-nas', 'nas-blb', 'nas-nas']
# feature_bigrams(["OW1"])

feature_bigrams <- function(phones_list, include_reverse=TRUE) {
  # find n-grams of each successive pair
  phones_list = c("^", phones_list, "$")
  doit <- function(phones_list) {
    dfx <- data.frame(ph1=unlist(tail(phones_list,-1)), ph0=unlist(head(phones_list,-1)), stringsAsFactors = FALSE)
    f0 <- phone_to_features(dfx$ph0)
    f1 <- phone_to_features(dfx$ph1)
    #g <- expand.grid(unlist(f1),unlist(f0))
    #paste(g[,2], g[,1], sep="-")
    gg <- do.call(rbind, sapply(1:length(f1), function(i) {
      expand.grid(f0[[i]], f1[[i]])
    }, simplify=FALSE))
    paste(gg[,1], gg[,2], sep="-")
    
  }
  
  grams <- doit(phones_list)
  # backwards too
  if (include_reverse) {
    grams <- c(grams, doit(rev(phones_list)))
  }
  grams
}
            