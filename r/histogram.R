nameYearDistribution <- function(all, name) {
  
  # Filter all the data on the selected name
  filtered <- all[all$Name == name, ]
  filtered.sorted <- filtered[order(filtered$Year),]
  
  # Make sure all years are plotted
  year.min <- min(all$Year)
  year.max <- max(all$Year)
  
  all.years <- seq(year.min, year.max);
  all.years.frame <- data.frame(list(Year=all.years))
  
  merged <- merge(all.years.frame, filtered.sorted, all=TRUE)
  merged$F[which(is.na(merged$F))] <- 0
  merged$M[which(is.na(merged$M))] <- 0
  
  merged$T <- merged$M + merged$F
  
  sum.M <- tapply(all$M, all$Year, sum)
  sum.F <- tapply(all$F, all$Year, sum)
  sum.T <- tapply(all$F+all$M, all$Year, sum)
  
  merged$M.pct <- merged$M / sum.M * 100
  merged$F.pct <- merged$F / sum.F * 100
  merged$T.pct <- merged$T / sum.T * 100
  
  # Return
  merged[c('Year','M', 'M.pct', 'F', 'F.pct', 'T')]
  merged
}

nameUseSD <- function(all, name) {
  dist <- nameYearDistribution(all, name)
  sd(dist$T.pct)
}

filterPct <- function(processed, lower, upper) {
  filtered <- processed[(processed$count.total > quantile(processed$count.total, lower) & processed$count.total < quantile(processed$count.total, upper)), ] 
  filtered
}

filterGender <- function(processed, gender) {
  filtered <- processed[processed$prob.gender == gender,]
  filtered
}

sortProcessed <-function(processed, by) {
  sorted <- processed[order(processed[[by]]),]
  sorted
}

plotName <- function(all, name, column="T.pct") {
  dist <- nameYearDistribution(all, name)
  barplot(dist[[column]], names.arg = dist$Year)
}

plotNames <- function(all, names, column){
  for(i in names) {
    allNames[[names[i]]] <- names[i]
  }
  allNames
  #barplot(distribution[[gender]], names.arg = merged$Year, beside=TRUE)
}
