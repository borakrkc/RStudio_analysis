# WITHIN-SUBJECT Z-SKOR KONTROLÜ VE DÜZELTME
# =============================================

cat("\n=== WITHIN-SUBJECT Z-SKOR KONTROLÜ ===\n\n")

# temporal_all_27 verisindeki Z-skorun nasıl hesaplandığını kontrol et
if(!exists("temporal_all_27")) {
  stop("temporal_all_27 bulunamadı!")
}

cat("Mevcut veri boyutu:", nrow(temporal_all_27), "satır\n")
cat("Katılımcı sayısı:", n_distinct(temporal_all_27$Katilimci), "\n\n")

# Örnek bir katılımcı için kontrol et
ornek_katilimci <- unique(temporal_all_27$Katilimci)[1]
ornek_data <- temporal_all_27 %>%
  filter(Katilimci == ornek_katilimci, Strateji == "Inkar")

cat("Örnek katılımcı:", ornek_katilimci, "\n")
cat("GSR_Z özeti:\n")
print(summary(ornek_data$GSR_Z))
cat("\n")

# Z-skor doğru mu kontrol et
cat("SORU: Bu Z-skor within-subject mi?\n")
cat("Kontrol: Her katılımcının Z-skorunun ortalaması ~0, SD ~1 olmalı\n\n")

# Her katılımcı için Z-skor özetini kontrol et
zskor_kontrol <- temporal_all_27 %>%
  group_by(Katilimci) %>%
  summarise(
    Mean_Z = mean(GSR_Z, na.rm = TRUE),
    SD_Z = sd(GSR_Z, na.rm = TRUE),
    .groups = "drop"
  )

cat("Her katılımcının Z-skor özeti:\n")
print(head(zskor_kontrol, 10))
cat("\n")

cat("Z-skor ortalamaları dağılımı:\n")
print(summary(zskor_kontrol$Mean_Z))
cat("\n")

# KARAR: Eğer ortalamalar 0 civarında DEĞİLSE, yeniden hesaplamamız gerekiyor
if(mean(abs(zskor_kontrol$Mean_Z)) > 5) {
  cat("⚠️ UYARI: Z-skorlar within-subject görünmüyor!\n")
  cat("Yeniden hesaplama gerekiyor.\n\n")

  hesapla <- TRUE
} else {
  cat("✓ Z-skorlar within-subject görünüyor.\n")
  cat("Ama baseline'ın doğru olduğundan emin olalım...\n\n")

  # Baseline kontrolü yap
  cat("BASELINE KONTROLÜ:\n")
  cat("Her stimulus için ilk 3 saniye (0-3000 ms) baseline olarak kullanılmalı\n\n")

  hesapla <- readline("Yeniden hesaplamak ister misiniz? (E/H): ")
  hesapla <- toupper(hesapla) == "E"
}

if(hesapla) {
  cat("\n=== WITHIN-SUBJECT Z-SKOR YENİDEN HESAPLAMA ===\n\n")

  # GSR RAW verisini yükle
  gsr_file <- "/Users/borakarakoc/Desktop/Ölçek ve yöntem kaynağı makaleler/VERİLER/Eski deney verileri 13 ekim/Yeni klasör/İlk İnkar/ilkinkar_gsr_zscore.csv"

  cat("GSR RAW verisi yükleniyor...\n")
  gsr_data <- read.csv(gsr_file, fileEncoding = "UTF-8")
  cat("✓ Yüklendi:", nrow(gsr_data), "satır\n\n")

  # İnkar ve Özür için ayrı hesapla
  cat("İnkar stratejisi için Z-skor hesaplanıyor...\n")
  inkar_data <- gsr_data %>%
    filter(grepl("inkar|İnkar|INKAR", SourceStimuliName, ignore.case = TRUE))

  cat("Özür stratejisi için Z-skor hesaplanıyor...\n")
  ozur_data <- gsr_data %>%
    filter(grepl("özür|Özür|ÖZÜR|ozur", SourceStimuliName, ignore.case = TRUE))

  # Her katılımcı için baseline hesapla ve Z-skor oluştur
  calculate_zscore <- function(data, strateji_adi) {
    katilimcilar <- unique(data$Respondent)
    result_list <- list()

    for(i in seq_along(katilimcilar)) {
      kat_data <- data %>% filter(Respondent == katilimcilar[i])

      # Baseline: ilk 3 saniye (0-3000 ms)
      baseline <- kat_data %>%
        filter(RecordingTime >= 0 & RecordingTime <= 3000)

      if(nrow(baseline) > 0) {
        baseline_mean <- mean(baseline$GSR.RAW, na.rm = TRUE)
        baseline_sd <- sd(baseline$GSR.RAW, na.rm = TRUE)

        # Tüm veri için Z-skor
        kat_data$GSR_Z_Within <- (kat_data$GSR.RAW - baseline_mean) / baseline_sd
        kat_data$Strateji <- strateji_adi

        result_list[[i]] <- kat_data
      }
    }

    return(bind_rows(result_list))
  }

  inkar_zscore <- calculate_zscore(inkar_data, "Inkar")
  cat("✓ İnkar Z-skor tamamlandı:", nrow(inkar_zscore), "satır\n")

  ozur_zscore <- calculate_zscore(ozur_data, "Ozur")
  cat("✓ Özür Z-skor tamamlandı:", nrow(ozur_zscore), "satır\n\n")

  # Birleştir
  all_zscore <- bind_rows(inkar_zscore, ozur_zscore)
  cat("✓ Toplam:", nrow(all_zscore), "satır\n\n")

  # 30 saniye için temporal veri oluştur
  cat("30 saniyelik temporal veri oluşturuluyor...\n")

  temporal_30s <- all_zscore %>%
    filter(RecordingTime >= 0 & RecordingTime <= 30000) %>%
    mutate(
      Katilimci = toupper(trimws(Respondent)),
      Time_Bin = floor(RecordingTime / 1000),
      GSR_Z = GSR_Z_Within
    ) %>%
    group_by(Katilimci, Strateji, Time_Bin) %>%
    summarise(
      GSR_Z = mean(GSR_Z, na.rm = TRUE),
      GSR_RAW = mean(GSR.RAW, na.rm = TRUE),
      N = n(),
      .groups = "drop"
    )

  cat("✓ Temporal veri hazır:", nrow(temporal_30s), "satır\n")
  cat("  Katılımcı:", n_distinct(temporal_30s$Katilimci), "\n")
  cat("  Strateji başına:", table(temporal_30s$Strateji), "\n\n")

  # Kontrol: Her katılımcının Z-skor ortalaması ~0 olmalı
  kontrol_yeni <- temporal_30s %>%
    group_by(Katilimci) %>%
    summarise(Mean_Z = mean(GSR_Z, na.rm = TRUE), .groups = "drop")

  cat("YENİ Z-SKOR KONTROLÜ:\n")
  cat("Katılımcı Z-skor ortalamaları:\n")
  print(summary(kontrol_yeni$Mean_Z))
  cat("\n")

  if(mean(abs(kontrol_yeni$Mean_Z)) < 2) {
    cat("✓ BAŞARILI! Within-subject Z-skorlar doğru görünüyor.\n\n")

    # Temporal_all_27'yi güncelle
    temporal_all_27_YENI <- temporal_30s

    # Environment'a kaydet
    assign("temporal_all_27", temporal_30s, envir = .GlobalEnv)
    cat("✓ temporal_all_27 güncellendi (within-subject Z-skor ile)\n\n")

    # Dosyaya kaydet
    inkar_dizin <- "/Users/borakarakoc/Desktop/Ölçek ve yöntem kaynağı makaleler/VERİLER/Eski deney verileri 13 ekim/Yeni klasör/İlk İnkar"
    kayit_path <- file.path(inkar_dizin, "temporal_27kisi_within_zscore.csv")
    write.csv(temporal_30s, kayit_path, row.names = FALSE, fileEncoding = "UTF-8")
    cat("✓ Kaydedildi:", kayit_path, "\n\n")

  } else {
    cat("⚠️ SORUN VAR: Z-skorlar hala doğru görünmüyor!\n")
  }

} else {
  cat("\nYeniden hesaplama atlandı.\n")
  cat("Mevcut Z-skorlar kullanılacak.\n\n")
}

cat("=== KONTROL TAMAMLANDI ===\n\n")
cat("ŞİMDİ NE YAPMALI?\n")
cat("1. Eğer yeniden hesaplandıysa, moderasyon analizini TEKRAR çalıştır:\n")
cat("   source('FINAL_moderasyon_trial_level.R')\n\n")
cat("2. Eğer değişiklik yoksa, devam et:\n")
cat("   source('FINAL_moderasyon_27kisi_grafik.R')\n\n")
