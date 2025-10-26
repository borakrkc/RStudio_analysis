# DOSYA KONUMU KONTROLÜ
# ======================

cat("\n=== DOSYA KONUMU KONTROLÜ ===\n\n")

# Working directory'yi göster
cat("Şu anki working directory:\n")
cat(getwd(), "\n\n")

# Moderasyon ile ilgili dosyaları ara
cat("Moderasyon dosyalarını arıyorum...\n\n")

# Aranacak dosyalar
aranan_dosyalar <- c(
  "moderasyon_trial_data.csv",
  "FINAL_Moderasyon_TrialLevel.png",
  "FINAL_SimpleSlopes_TrialLevel.png",
  "moderasyon_trial_sonuclari.txt",
  "FINAL_Moderasyon_27kisi.png",
  "FINAL_SimpleSlopes_27kisi.png"
)

cat("DOSYA DURUMU:\n")
cat(paste(rep("-", 70), collapse = ""), "\n")

for(dosya in aranan_dosyalar) {
  # Working directory'de var mı?
  wd_path <- file.path(getwd(), dosya)

  if(file.exists(wd_path)) {
    info <- file.info(wd_path)
    boyut <- round(info$size / 1024, 1)  # KB
    zaman <- format(info$mtime, "%Y-%m-%d %H:%M:%S")
    cat(sprintf("✓ %s\n", dosya))
    cat(sprintf("  Konum: %s\n", wd_path))
    cat(sprintf("  Boyut: %.1f KB | Oluşturulma: %s\n\n", boyut, zaman))
  } else {
    cat(sprintf("✗ %s (BULUNAMADI)\n\n", dosya))
  }
}

# İlk İnkar klasöründe var mı kontrol et
inkar_dizin <- "/Users/borakarakoc/Desktop/Ölçek ve yöntem kaynağı makaleler/VERİLER/Eski deney verileri 13 ekim/Yeni klasör/İlk İnkar"

cat("\n")
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("İlk İnkar klasöründe kontrol:\n")
cat(inkar_dizin, "\n")
cat(paste(rep("-", 70), collapse = ""), "\n\n")

if(dir.exists(inkar_dizin)) {
  for(dosya in aranan_dosyalar) {
    inkar_path <- file.path(inkar_dizin, dosya)

    if(file.exists(inkar_path)) {
      info <- file.info(inkar_path)
      boyut <- round(info$size / 1024, 1)
      zaman <- format(info$mtime, "%Y-%m-%d %H:%M:%S")
      cat(sprintf("✓ %s\n", dosya))
      cat(sprintf("  Konum: %s\n", inkar_path))
      cat(sprintf("  Boyut: %.1f KB | Oluşturulma: %s\n\n", boyut, zaman))
    }
  }
} else {
  cat("İlk İnkar klasörü bulunamadı.\n")
}

cat("\n")
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("ÖZET:\n")
cat("Working directory'deki tüm CSV ve PNG dosyalarını listele:\n\n")

# Tüm moderasyon ile ilgili dosyaları listele
csv_dosyalar <- list.files(pattern = ".*moderasyon.*\\.csv$", full.names = TRUE)
png_dosyalar <- list.files(pattern = ".*Moderasyon.*\\.png$|.*SimpleSlopes.*\\.png$", full.names = TRUE)
txt_dosyalar <- list.files(pattern = ".*moderasyon.*\\.txt$", full.names = TRUE)

if(length(csv_dosyalar) > 0) {
  cat("CSV dosyaları:\n")
  for(f in csv_dosyalar) cat("  -", f, "\n")
  cat("\n")
}

if(length(png_dosyalar) > 0) {
  cat("PNG dosyaları:\n")
  for(f in png_dosyalar) cat("  -", f, "\n")
  cat("\n")
}

if(length(txt_dosyalar) > 0) {
  cat("TXT dosyaları:\n")
  for(f in txt_dosyalar) cat("  -", f, "\n")
  cat("\n")
}

cat("\n")
cat("Not: Grafik hatası nedeniyle PNG dosyaları oluşmamış olabilir.\n")
cat("Grafikleri oluşturmak için yukarıdaki kodu çalıştırın.\n")
