#!/usr/bin/env Rscript


# set library path (sesuaikan dengan direktori library R)
lib_path <- "~/Workspace/self/R/lib"
.libPaths(c(lib_path, .libPaths()))

# Load packages dengan error handling
load_package <- function(pkg) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg, lib = lib_path, repos = "https://cloud.r-project.org/")
    library(pkg, character.only = TRUE)
  }
}

# Load packages
cat("Loading packages...\n")
packages <- c("ggplot2", "dplyr", "broom", "ggpubr")
for (pkg in packages) {
  tryCatch({
    load_package(pkg)
    cat("✓", pkg, "loaded\n")
  }, error = function(e) {
    cat("✗", pkg, ":", e$message, "\n")
  })
}

dataset_folder <- "tugas2_dataset"
cat("\n=== LOADING DATA ===\n")

# Cek file CSV
if (file.exists(file.path(dataset_folder, "tugas2.csv"))) {
  cat("file tugas2.csv ditemukan\n")
  data <- read.csv(file.path(dataset_folder, "tugas2.csv"))
  cat("data berhasil dimuat dari CSV\n")
} else {
  cat("file tugas2.csv tidak ditemukan\n")
  quit(save = "no", status = 1)
}

result_folder <- "tugas2_results"

# Tampilkan struktur data
cat("\nstruktur data:\n")
print(str(data))

cat("\n6 baris pertama data:\n")
print(head(data))

cat("\nsummary data:\n")
print(summary(data))

cat("\n=== a. CEK DISTRIBUSI NORMAL ===\n")

# Histogram untuk variabel terikat (kebahagiaan)
mean_kebahagiaan <- mean(data$kebahagiaan, na.rm = TRUE)
hist_kebahagiaan <- ggplot(data, aes(x = kebahagiaan)) +
  geom_histogram(aes(y = after_stat(density)), bins = 15, fill = "lightblue", alpha = 0.7, color = "black") +
  geom_density(color = "darkblue", linewidth = 1.2) +
  geom_vline(aes(xintercept = mean_kebahagiaan), color = "red", linetype = "dashed", linewidth = 1) +
  labs(
    title = "a. Histogram Distribusi Variabel Kebahagiaan",
    subtitle = "Cek Distribusi Normal Variabel Terikat",
    x = "Tingkat Kebahagiaan",
    y = "Density"
  ) +
  theme_minimal() +
  annotate("text", 
           x = mean_kebahagiaan, 
           y = 0.2, 
           label = paste("Mean =", round(mean_kebahagiaan, 2)), 
           color = "red", 
           vjust = -1)

# Tampilkan plot
print(hist_kebahagiaan)

# Simpan
ggsave(file.path(result_folder, "tugas2_histogram_kebahagiaan.png"), 
       plot = hist_kebahagiaan, 
       width = 8, 
       height = 6)

# Interpretasi
cat("Interpretasi Histogram:\n")
cat("- Histogram menunjukkan distribusi frekuensi variabel kebahagiaan\n")
cat("- Garis merah (density) menunjukkan estimasi kurva distribusi\n")
cat("- Jika bentuk menyerupai bell curve, data berdistribusi normal\n")
cat("- Mean kebahagiaan:", mean_kebahagiaan, "\n")

cat("\n=== b. SCATTER PLOT HUBUNGAN LINEAR ===\n")

scatter_plot <- ggplot(data, aes(x = pendapatan, y = kebahagiaan)) +
  geom_point(size = 3, color = "darkgreen", alpha = 0.7) +
  labs(title = "b. Scatter Plot: Pendapatan vs Kebahagiaan",
       subtitle = "Cek Hubungan Linear antara Variabel Bebas dan Terikat",
       x = "Pendapatan",
       y = "Kebahagiaan") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

print(scatter_plot)
ggsave(file.path(result_folder, "tugas2_scatter_plot.png"), scatter_plot, width = 8, height = 6)

# Interpretasi
cat("Interpretasi Scatter Plot:\n")
cat("- Titik-titik menunjukkan hubungan antara pendapatan (x) dan kebahagiaan (y)\n")
cat("- Jika titik membentuk pola linear (naik/turun konsisten), ada hubungan linear\n")
cat("- Pola acak/tersebar menunjukkan tidak ada hubungan linear yang jelas\n")

cat("\n=== c. REGRESI LINEAR SEDERHANA ===\n")

# Melakukan regresi linear
model <- lm(kebahagiaan ~ pendapatan, data = data)
summary_model <- summary(model)

cat("Hasil Regresi Linear:\n")
print(summary_model)

# Tampilkan hasil dalam format yang rapi
cat("\n=== HASIL REGRESI LINEAR ===\n")
cat("Persamaan Regresi: kebahagiaan =", round(coef(model)[1], 4), "+", round(coef(model)[2], 4), "* pendapatan\n")
cat("R-squared:", round(summary_model$r.squared, 4), "\n")
cat("Adjusted R-squared:", round(summary_model$adj.r.squared, 4), "\n")
cat("F-statistic:", round(summary_model$fstatistic[1], 2), "\n")
cat("p-value:", format.pval(pf(summary_model$fstatistic[1], 
                              summary_model$fstatistic[2], 
                              summary_model$fstatistic[3], 
                              lower.tail = FALSE), digits = 4), "\n")

# Interpretasi hasil regresi
cat("\nInterpretasi Hasil Regresi:\n")
cat("- Intercept:", round(coef(model)[1], 4), "(nilai kebahagiaan ketika pendapatan = 0)\n")
cat("- Slope:", round(coef(model)[2], 4), "(setiap kenaikan 1 unit pendapatan, kebahagiaan meningkat", round(coef(model)[2], 4), "unit)\n")
cat("- R-squared:", round(summary_model$r.squared * 100, 2), "% variasi kebahagiaan dapat dijelaskan oleh pendapatan\n")

# Simpan hasil regresi ke file
sink(file.path(result_folder, "tugas2_hasil_regresi.txt"))
cat("HASIL ANALISIS REGRESI LINEAR\n")
cat("Variabel Bebas: Pendapatan\n")
cat("Variabel Terikat: Kebahagiaan\n")
cat("Jumlah Observasi:", nrow(data), "\n")
cat("========================================\n\n")
print(summary_model)
sink()


cat("\n=== d. CEK HOMOSKEDASTISITAS ===\n")

# Plot diagnostik untuk homoskedastisitas
png(file.path(result_folder, "tugas2_diagnostic_plots.png"), width = 1000, height = 800)
par(mfrow = c(2, 2))
plot(model, main = "d. Diagnostic Plots untuk Cek Homoskedastisitas dan Asumsi Lainnya")
dev.off()

# Plot residuals vs fitted values (utama untuk homoskedastisitas)
residual_plot <- ggplot(data.frame(fitted = fitted(model), residuals = resid(model)), 
                       aes(x = fitted, y = residuals)) +
  geom_point(size = 3, color = "purple", alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = 1) +
  geom_smooth(se = FALSE, color = "blue", method = "loess") +
  labs(title = "d. Residuals vs Fitted Values",
       subtitle = "Cek Homoskedastisitas - Residuals harus tersebar acak di sekitar nol",
       x = "Fitted Values",
       y = "Residuals") +
  theme_minimal()

print(residual_plot)
ggsave(file.path(result_folder, "tugas2_homoskedastisitas.png"), residual_plot, width = 8, height = 6)

# Interpretasi homoskedastisitas
cat("Interpretasi Homoskedastisitas:\n")
cat("- Residuals vs Fitted: Titik tersebar acak di sekitar nol = homoskedastisitas terpenuhi\n")
cat("- Pola tertentu (seperti corong/meledak) = heteroskedastisitas (masalah)\n")
cat("- Garis horizontal merah di y=0 sebagai referensi\n")


cat("\n=== e. VISUALISASI HASIL REGRESI ===\n")

# Visualisasi lengkap dengan ggplot2
regression_plot <- ggplot(data, aes(x = pendapatan, y = kebahagiaan)) +
  # 1. Plot data points
  geom_point(size = 3, color = "darkblue", alpha = 0.7) +
  
  # 2. Tambahkan garis regresi linear
  geom_smooth(method = "lm", formula = y ~ x, 
              color = "red", fill = "lightpink", 
              se = TRUE, level = 0.95) +
  
  # 3. Tambah persamaan untuk garis linear
  annotate("text", 
           x = min(data$pendapatan), 
           y = max(data$kebahagiaan),
           label = paste("y =", round(coef(model)[1], 3), "+", round(coef(model)[2], 3), "x",
                        "\nR² =", round(summary_model$r.squared, 3),
                        "\np =", format.pval(summary_model$coefficients[2,4], digits = 3)),
           hjust = 0, vjust = 1,
           color = "darkred", size = 5, fontface = "bold") +
  
  # 4. Beri judul dan label yang sesuai
  labs(title = "e. Hasil Regresi Linear: Pengaruh Pendapatan terhadap Kebahagiaan",
       subtitle = "Visualisasi Lengkap dengan Garis Regresi dan Persamaan",
       x = "Pendapatan",
       y = "Tingkat Kebahagiaan",
       caption = paste("n =", nrow(data), "observasi | sumber: data Tutorial")) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(face = "bold")
  )

print(regression_plot)
ggsave(file.path(result_folder, "tugas2_visualisasi_regresi.png"), regression_plot, width = 10, height = 7)

cat("\n=== ANALISIS SELESAI ===\n")
cat("File output yang dihasilkan:\n")
cat("1. tugas2_results/tugas2_histogram_kebahagiaan.png      - Cek distribusi normal\n")
cat("2. tugas2_results/tugas2_scatter_plot.png               - Cek hubungan linear\n")
cat("3. tugas2_results/tugas2_hasil_regresi.txt              - Hasil regresi linear\n")
cat("4. tugas2_results/tugas2_diagnostic_plots.png           - Diagnostic plots (base R)\n")
cat("5. tugas2_results/tugas2_homoskedastisitas.png          - Cek homoskedastisitas\n")
cat("6. tugas2_results/tugas2_visualisasi_regresi.png        - Visualisasi lengkap regresi\n")

cat("\n=== INTERPRETASI HASIL ===\n")
cat("1. Distribusi Normal: Lihat histogram - apakah berbentuk bell curve?\n")
cat("2. Hubungan Linear: Lihat scatter plot - apakah titik membentuk pola linear?\n")
cat("3. Koefisien Regresi: Setiap kenaikan 1 unit pendapatan, kebahagiaan berubah", round(coef(model)[2], 4), "unit\n")
cat("4. Homoskedastisitas: Residuals vs Fitted - titik tersebar acak = baik\n")
cat("5. Signifikansi: p-value < 0.05 = hubungan signifikan\n")

# Tampilkan ringkasan akhir
cat("\n=== RINGKASAN EKSEKUTIF ===\n")
cat("Persamaan: Kebahagiaan =", round(coef(model)[1], 3), "+", round(coef(model)[2], 3), "* Pendapatan\n")
cat("Kekuatan Hubungan (R²):", round(summary_model$r.squared * 100, 2), "%\n")
cat("Signifikansi Statistik: p =", format.pval(summary_model$coefficients[2,4], digits = 4), "\n")

if (summary_model$coefficients[2,4] < 0.05) {
  cat("Kesimpulan: Terdapat hubungan yang SIGNIFIKAN antara pendapatan dan kebahagiaan.\n")
} else {
  cat("Kesimpulan: Tidak terdapat hubungan yang signifikan antara pendapatan dan kebahagiaan.\n")
}

cat("\nFile data yang digunakan: tugas2.csv\n")
cat("Jumlah observasi:", nrow(data), "\n")