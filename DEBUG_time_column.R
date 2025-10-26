# TIME SÜTUNU DEBUG
# ==================

cat("\n=== TIME SÜTUNU KONTROLÜ ===\n\n")

# Dosyayı yükle
gsr_file <- "/Users/borakarakoc/Desktop/Ölçek ve yöntem kaynağı makaleler/VERİLER/Eski deney verileri 13 ekim/Yeni klasör/İlk İnkar/ilkinkar_gsr_zscore.csv"

cat("Dosya yükleniyor...\n")
gsr_data <- read.csv(gsr_file, fileEncoding = "UTF-8")

cat("✓ Yüklendi:", nrow(gsr_data), "satır\n\n")

# Timestamp sütununu kontrol et
cat("Timestamp sütunu özeti:\n")
print(summary(gsr_data$Timestamp))
cat("\n")

cat("İlk 20 Timestamp değeri:\n")
print(head(gsr_data$Timestamp, 20))
cat("\n")

cat("Son 20 Timestamp değeri:\n")
print(tail(gsr_data$Timestamp, 20))
cat("\n")

# Katılımcı ve Stimulus kontrol
cat("Katilimci sütunu var mı?\n")
if("Katilimci" %in% colnames(gsr_data)) {
  cat("✓ VAR\n")
  cat("Benzersiz katılımcılar:\n")
  print(unique(gsr_data$Katilimci))
  cat("\n")
} else {
  cat("✗ YOK\n\n")
  cat("Tüm sütunlar:\n")
  print(colnames(gsr_data))
  cat("\n")
}

# Stimulus kontrol
cat("SourceStimuliName örnekleri:\n")
print(head(unique(gsr_data$SourceStimuliName), 20))
cat("\n")

# Bir katılımcı için Timestamp aralığını kontrol et
if("Katilimci" %in% colnames(gsr_data)) {
  test_kat <- unique(gsr_data$Katilimci)[1]
  test_data <- gsr_data[gsr_data$Katilimci == test_kat, ]

  cat("Test katılımcı:", test_kat, "\n")
  cat("Satır sayısı:", nrow(test_data), "\n")
  cat("Timestamp aralığı:\n")
  cat("  Min:", min(test_data$Timestamp, na.rm = TRUE), "\n")
  cat("  Max:", max(test_data$Timestamp, na.rm = TRUE), "\n")
  cat("  Fark:", max(test_data$Timestamp, na.rm = TRUE) - min(test_data$Timestamp, na.rm = TRUE), "\n\n")

  # İlk stimulus için kontrol
  test_stim <- test_data[1:1000, ]
  cat("İlk 1000 satırın Timestamp'i:\n")
  cat("  Min:", min(test_stim$Timestamp, na.rm = TRUE), "\n")
  cat("  Max:", max(test_stim$Timestamp, na.rm = TRUE), "\n")
  cat("  Range:", max(test_stim$Timestamp) - min(test_stim$Timestamp), "\n\n")
}

cat("SORUN TESPİTİ:\n")
cat("Eğer Timestamp değerleri ÇOK BÜYÜKSE (örn: 1000000+):\n")
cat("  → Timestamp absolute değer, baseline için relative Time gerekli!\n")
cat("  → RecordingTime sütunu olabilir mi kontrol et\n\n")

# RecordingTime var mı?
cat("RecordingTime sütunu var mı?\n")
if("RecordingTime" %in% colnames(gsr_data)) {
  cat("✓ VAR!\n")
  cat("RecordingTime özeti:\n")
  print(summary(gsr_data$RecordingTime))
  cat("\n")
  cat("İlk 20 RecordingTime:\n")
  print(head(gsr_data$RecordingTime, 20))
  cat("\n")

  cat("→ RecordingTime kullanılmalı, Timestamp değil!\n")
} else {
  cat("✗ YOK\n")
  cat("Başka zaman sütunları:\n")
  time_cols <- colnames(gsr_data)[grep("time|Time", colnames(gsr_data), ignore.case = TRUE)]
  print(time_cols)
  cat("\n")
}
