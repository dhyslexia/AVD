# soal 1
kecepatan <- c(4,4,7,7,8,8,9,10,10,11,11,12,12,12,13,13,13,13,114,14,14,14,15,15,15,16,16,17,17,17,17,17,18,18,18,19,19,20,20,20,20,20,21,22,23,24,24,24,25,25)
jarak <- c(2,10,4,20,17,13,18,28,33,18,26,12,24,22,28,24,32,34,43,24,30,58,80,20,24,55,35,40,30,44,50,46,53,70,80,36,46,68,34,48,50,56,60,64,56,72,90,92,110,85)

kecepatan_rata = mean(kecepatan)
jarak_rata = mean(jarak)
sd_jarak = sd(jarak)
print(paste("kecepatan rata-rata = ", kecepatan_rata))
print(paste("jarak rata-rata = ", jarak_rata))
print(paste("sd jarak = ", sd_jarak))

#soal 2
png("scatterplot.png", width = 600, height = 400)
plot(kecepatan, jarak, main = "scatter plot kecepatan vs jarak",xlab = "kecepatan (km/h)", ylab = "jarak (meter)",pch = 19, col = "black")
dev.off()

png("histogram_kecepatan.png", width = 600, height = 400)
hist(kecepatan, main = "histogram kecepatan mobil",xlab = "kecepatan (km/h)",col = "gray", border = "black")
dev.off()


#soal 3
math_mean <- 75 
math_sd <- 10
english_mean <- 70
english_sd <- 8

koefisien_math <- (math_sd / math_mean) * 100
koefisien_english <- (english_sd / english_mean) * 100

print(paste("koefisien math tomas: ", koefisien_math))
print(paste("koefisien english tomas: ", koefisien_english))