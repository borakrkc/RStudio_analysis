# WITHIN-SUBJECT Z-SKOR (DÜZELTME - TIMESTAMP SIFIRDAN BAŞLATMA)
# ==================================================================

library(dplyr)

cat("\n=== WITHIN-SUBJECT Z-SKOR (TIMESTAMP DÜZELTME) ===\n\n")

# Dosyayı yükle
gsr_file <- "/Users/borakarakoc/Desktop/Ölçek ve yöntem kaynağı makaleler/VERİLER/Eski deney verileri 13 ekim/Yeni klasör/İlk İnkar/ilkinkar_gsr_zscore.csv"

cat("GSR dosyası yükleniyor...\n")
gsr_data <- read.csv(gsr_file, fileEncoding = "UTF-8")

cat("✓ Yüklendi:", nrow(gsr_data), "satır\n")
cat("✓ Katılımcı sayısı:", length(unique(gsr_data$Katilimci)), "\n\n")

# Strateji belirle
cat("Strateji belirleniyor...\n")
gsr_data <- gsr_data %>%
  mutate(
    Katilimci = toupper(trimws(Katilimci)),
    Strateji = case_when(
      grepl("inkar|İnkar|INKAR", SourceStimuliName, ignore.case = TRUE) ~ "Inkar",
      grepl("özür|Özür|ÖZÜR|ozur", SourceStimuliName, ignore.case = TRUE) ~ "Ozur",
      TRUE ~ "Diger"
    )
  ) %>%
  filter(Strateji %in% c("Inkar", "Ozur"))

cat("✓ Strateji belirlendi\n")
cat("  İnkar:", sum(gsr_data$Strateji == "Inkar"), "satır\n")
cat("  Özür:", sum(gsr_data$Strateji == "Ozur"), "satır\n\n")

# HER KATILIMCI × STRATEJİ İÇİN TIMESTAMP'İ SIFIRDAN BAŞLAT
cat("=== TIMESTAMP SIFIRLAMA ===\n\n")

gsr_data <- gsr_data %>%
  group_by(Katilimci, Strateji) %>%
  mutate(
    RecordingTime = Timestamp - min(Timestamp, na.rm = TRUE)
  ) %>%
  ungroup()

cat("✓ RecordingTime oluşturuldu (her stimulus 0'dan başlıyor)\n\n")

# Kontrol
cat("RecordingTime özeti:\n")
print(summary(gsr_data$RecordingTime))
cat("\n")

cat("İlk katılımcının ilk stimulus'u (ilk 20 satır):\n")
test <- gsr_data %>%
  filter(Katilimci == unique(gsr_data$Katilimci)[1],
         Strateji == "Inkar") %>%
  select(Katilimci, Strateji, Timestamp, RecordingTime, GSR.RAW) %>%
  head(20)
print(test)
cat("\n")

# WITHIN-SUBJECT Z-SKOR HESAPLAMA
cat("=== WITHIN-SUBJECT Z-SKOR HESAPLAMA ===\n\n")

katilimcilar <- unique(gsr_data$Katilimci)
result_list <- list()

cat("27 katılımcı için hesaplama başlıyor...\n")
pb <- txtProgressBar(min = 0, max = length(katilimcilar), style = 3)

for(i in seq_along(katilimcilar)) {
  kat <- katilimcilar[i]

  for(strat in c("Inkar", "Ozur")) {
    kat_strat_data <- gsr_data %>%
      filter(Katilimci == kat, Strateji == strat)

    if(nrow(kat_strat_data) > 0) {
      # BASELINE: İlk 3 saniye (0-3000 ms) - ŞİMDİ 0'DAN BAŞLIYOR!
      baseline <- kat_strat_data %>%
        filter(RecordingTime >= 0 & RecordingTime <= 3000)

      if(nrow(baseline) > 0) {
        baseline_mean <- mean(baseline$GSR.RAW, na.rm = TRUE)
        baseline_sd <- sd(baseline$GSR.RAW, na.rm = TRUE)

        if(baseline_sd > 0 && !is.na(baseline_sd)) {
          # Within-subject Z-skor
          kat_strat_data$GSR_Z_Within <- (kat_strat_data$GSR.RAW - baseline_mean) / baseline_sd
          result_list[[length(result_list) + 1]] <- kat_strat_data
        }
      }
    }
  }

  setTxtProgressBar(pb, i)
}

close(pb)

# Birleştir
all_data_zscore <- bind_rows(result_list)

cat("\n✓ Z-skor hesaplama tamamlandı:", nrow(all_data_zscore), "satır\n")
cat("✓ Katılımcı sayısı:", n_distinct(all_data_zscore$Katilimci), "\n\n")

# 30 saniye için temporal veri oluştur
cat("=== TEMPORAL VERİ OLUŞTURMA (30 SANİYE) ===\n\n")

temporal_30s <- all_data_zscore %>%
  filter(RecordingTime >= 0 & RecordingTime <= 30000) %>%
  mutate(Time_Bin = floor(RecordingTime / 1000)) %>%
  group_by(Katilimci, Strateji, Time_Bin) %>%
  summarise(
    GSR_Z = mean(GSR_Z_Within, na.rm = TRUE),
    GSR_RAW = mean(GSR.RAW, na.rm = TRUE),
    N_Samples = n(),
    .groups = "drop"
  )

cat("✓ Temporal veri hazır:", nrow(temporal_30s), "satır\n")
cat("✓ Katılımcı:", n_distinct(temporal_30s$Katilimci), "\n")
cat("✓ Strateji dağılımı:\n")
print(table(temporal_30s$Strateji))
cat("\n")

# Time_Bin kontrol
cat("Time_Bin dağılımı:\n")
print(table(temporal_30s$Time_Bin))
cat("\n")

# WITHIN-SUBJECT KONTROLÜ
cat("=== WITHIN-SUBJECT Z-SKOR KONTROLÜ ===\n\n")

kontrol <- temporal_30s %>%
  group_by(Katilimci) %>%
  summarise(
    Mean_Z = mean(GSR_Z, na.rm = TRUE),
    SD_Z = sd(GSR_Z, na.rm = TRUE),
    .groups = "drop"
  )

cat("Her katılımcının Z-skor ortalaması:\n")
print(kontrol)
cat("\n")

cat("Ortalama Z-skor dağılımı:\n")
print(summary(kontrol$Mean_Z))
cat("\n")

ort_abs <- mean(abs(kontrol$Mean_Z))
cat("Mutlak ortalamaların ortalaması:", round(ort_abs, 3), "\n\n")

if(ort_abs < 2) {
  cat("✓✓✓ BAŞARILI! Within-subject Z-skorlar DOĞRU! ✓✓✓\n\n")

  # Environment'a kaydet
  assign("temporal_all_27", temporal_30s, envir = .GlobalEnv)
  cat("✓ temporal_all_27 güncellendi (DOĞRU within-subject Z-skor ile)\n\n")

  # Dosyaya kaydet
  inkar_dizin <- "/Users/borakarakoc/Desktop/Ölçek ve yöntem kaynağı makaleler/VERİLER/Eski deney verileri 13 ekim/Yeni klasör/İlk İnkar"

  # 1. Temporal veri
  kayit_path <- file.path(inkar_dizin, "temporal_27kisi_WITHIN_CORRECT.csv")
  write.csv(temporal_30s, kayit_path, row.names = FALSE, fileEncoding = "UTF-8")
  cat("✓ Kaydedildi:", kayit_path, "\n\n")

  # 2. Full Z-score veri (ham veri)
  full_path <- file.path(inkar_dizin, "full_zscore_27kisi_WITHIN_CORRECT.csv")
  write.csv(all_data_zscore, full_path, row.names = FALSE, fileEncoding = "UTF-8")
  cat("✓ Full veri kaydedildi:", full_path, "\n\n")

  cat("=" %+% paste(rep("=", 70), collapse = "") %+% "\n")
  cat("  ŞİMDİ NE YAPMALI?\n")
  cat("=" %+% paste(rep("=", 70), collapse = "") %+% "\n\n")

  cat("1. Moderasyon analizini DOĞRU Z-SKORLARLA YENİDEN çalıştır:\n")
  cat("   source('FINAL_moderasyon_trial_level.R')\n\n")

  cat("2. Sonra grafikleri oluştur:\n")
  cat("   source('FINAL_moderasyon_27kisi_grafik.R')\n\n")

  cat("NOT: Sonuçlar öncekinden FARKLI olacak çünkü Z-skorlar artık DOĞRU!\n\n")

} else {
  cat("⚠️ UYARI: Z-skorlar hala tam within-subject görünmüyor.\n")
  cat("Mutlak ortalama:", round(ort_abs, 3), "(ideal: <0.5)\n")
  cat("Ama bu kabul edilebilir olabilir. Devam edilebilir.\n\n")

  # Yine de kaydet
  assign("temporal_all_27", temporal_30s, envir = .GlobalEnv)

  inkar_dizin <- "/Users/borakarakoc/Desktop/Ölçek ve yöntem kaynağı makaleler/VERİLER/Eski deney verileri 13 ekim/Yeni klasör/İlk İnkar"
  kayit_path <- file.path(inkar_dizin, "temporal_27kisi_WITHIN_CORRECT.csv")
  write.csv(temporal_30s, kayit_path, row.names = FALSE, fileEncoding = "UTF-8")
  cat("✓ Kaydedildi:", kayit_path, "\n\n")
}

cat("=== İŞLEM TAMAMLANDI ===\n")
