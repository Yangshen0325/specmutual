# plot stt_table

  stt_table <- island$stt_table
  par(mfrow = c(2, 3))
  plot(stt_table[, 1], stt_table[, 2], type = "l",
       xlab = "Time", ylab = "richness", main = "number of plant immigrants")
  plot(stt_table[, 1], stt_table[, 3], type = "l",
       xlab = "Time", ylab = "richness", main = "number of plant anagenesis")
  plot(stt_table[, 1], stt_table[, 4], type = "l",
       xlab = "Time", ylab = "richness", main = "number of plant cladogenesis")
  plot(stt_table[, 1], stt_table[, 5], type = "l",
       xlab = "Time", ylab = "richness", main = "number of animal immigrants")
  plot(stt_table[, 1], stt_table[, 6], type = "l",
       xlab = "Time", ylab = "richness", main = "number of animal anagenesis")
  plot(stt_table[, 1], stt_table[, 7], type = "l",
       xlab = "Time", ylab = "richness", main = "number of animal cladogenesis")

  par(mfrow = c(1, 1))





