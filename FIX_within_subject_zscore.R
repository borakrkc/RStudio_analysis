# WITHIN-SUBJECT Z-SKOR DÜZELTME (FIX)
# =====================================

library(dplyr)

cat("\n=== WITHIN-SUBJECT Z-SKOR DÜZELTME ===\n\n")

# 1. Dosya sütunlarını kontrol et
gsr_file <- "/Users/borakarakoc/Desktop/Ölçek ve yöntem kaynağı makaleler/VERİLER/Eski deney verileri 13 ekim/Yeni klasör/İlk İnkar/ilkinkar_gsr_zscore.csv"

cat("GSR dosyası yükleniyor...\n")
gsr_data <- read.csv(gsr_file, fileEncoding = "UTF-8")

cat("✓ Yüklendi:", nrow(gsr_data), "satır\n")
cat("✓ Sütun sayısı:", ncol(gsr_data), "\n\n")

cat("İlk 20 sütun ismi:\n")
print(head(colnames(gsr_data), 20))
cat("\n")

# İhtiyacımız olan sütunları bul
cat("Gerekli sütunlar:\n")
cat("  - Katılımcı: ")
katilimci_sutun <- colnames(gsr_data)[grep("Respondent|Participant|Katilimci", colnames(gsr_data), ignore.case = TRUE)]
if(length(katilimci_sutun) > 0) {
  cat("✓", katilimci_sutun[1], "\n")
} else {
  cat("✗ BULUNAMADI\n")
}

cat("  - Zaman: ")
zaman_sutun <- colnames(gsr_data)[grep("Time|Zaman", colnames(gsr_data), ignore.case = TRUE)]
if(length(zaman_sutun) > 0) {
  cat("✓", zaman_sutun[1], "\n")
} else {
  cat("✗ BULUNAMADI\n")
}

cat("  - GSR RAW: ")
gsr_sutun <- colnames(gsr_data)[grep("GSR.*RAW|GSR\\.RAW", colnames(gsr_data), ignore.case = TRUE)]
if(length(gsr_sutun) > 0) {
  cat("✓", gsr_sutun[1], "\n")
} else {
  cat("✗ BULUNAMADI (tüm GSR sütunlarına bakılıyor...)\n")
  gsr_sutun_tum <- colnames(gsr_data)[grep("GSR", colnames(gsr_data), ignore.case = TRUE)]
  cat("    GSR sütunları:", paste(gsr_sutun_tum, collapse = ", "), "\n")
}

cat("  - Stimulus: ")
stimulus_sutun <- colnames(gsr_data)[grep("Stimulus|SourceStimuli", colnames(gsr_data), ignore.case = TRUE)]
if(length(stimulus_sutun) > 0) {
  cat("✓", stimulus_sutun[1], "\n")
} else {
  cat("✗ BULUNAMADI\n")
}

cat("\n")

# 2. Eğer gerekli sütunlar varsa, within-subject Z-skor hesapla
if(length(katilimci_sutun) > 0 && length(zaman_sutun) > 0 && length(gsr_sutun) > 0) {

  cat("=== Z-SKOR HESAPLAMA BAŞLIYOR ===\n\n")

  # Sütun isimlerini standartlaştır
  kat_col <- katilimci_sutun[1]
  time_col <- zaman_sutun[1]
  gsr_col <- gsr_sutun[1]
  stim_col <- if(length(stimulus_sutun) > 0) stimulus_sutun[1] else NULL

  cat("Kullanılacak sütunlar:\n")
  cat("  Katılımcı:", kat_col, "\n")
  cat("  Zaman:", time_col, "\n")
  cat("  GSR:", gsr_col, "\n")
  if(!is.null(stim_col)) cat("  Stimulus:", stim_col, "\n")
  cat("\n")

  # Veriyi hazırla
  gsr_data <- gsr_data %>%
    rename(
      Katilimci_Raw = !!kat_col,
      Time = !!time_col,
      GSR_RAW = !!gsr_col
    )

  if(!is.null(stim_col)) {
    gsr_data <- gsr_data %>% rename(Stimulus = !!stim_col)
  }

  # Strateji belirle
  gsr_data <- gsr_data %>%
    mutate(
      Katilimci = toupper(trimws(Katilimci_Raw)),
      Strateji = case_when(
        grepl("inkar|İnkar|INKAR", Stimulus, ignore.case = TRUE) ~ "Inkar",
        grepl("özür|Özür|ÖZÜR|ozur", Stimulus, ignore.case = TRUE) ~ "Ozur",
        TRUE ~ "Diger"
      )
    ) %>%
    filter(Strateji %in% c("Inkar", "Ozur"))

  cat("✓ Strateji belirlendi\n")
  cat("  İnkar:", sum(gsr_data$Strateji == "Inkar"), "satır\n")
  cat("  Özür:", sum(gsr_data$Strateji == "Ozur"), "satır\n\n")

  # Her katılımcı × strateji için within-subject Z-skor hesapla
  cat("Within-subject Z-skor hesaplanıyor...\n")

  katilimcilar <- unique(gsr_data$Katilimci)
  result_list <- list()

  pb <- txtProgressBar(min = 0, max = length(katilimcilar), style = 3)

  for(i in seq_along(katilimcilar)) {
    kat <- katilimcilar[i]

    for(strat in c("Inkar", "Ozur")) {
      kat_strat_data <- gsr_data %>%
        filter(Katilimci == kat, Strateji == strat)

      if(nrow(kat_strat_data) > 0) {
        # Baseline: ilk 3 saniye (0-3000 ms)
        baseline <- kat_strat_data %>%
          filter(Time >= 0 & Time <= 3000)

        if(nrow(baseline) > 0) {
          baseline_mean <- mean(baseline$GSR_RAW, na.rm = TRUE)
          baseline_sd <- sd(baseline$GSR_RAW, na.rm = TRUE)

          if(baseline_sd > 0) {
            # Within-subject Z-skor
            kat_strat_data$GSR_Z_Within <- (kat_strat_data$GSR_RAW - baseline_mean) / baseline_sd
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

  cat("\n✓ Z-skor hesaplama tamamlandı:", nrow(all_data_zscore), "satır\n\n")

  # 30 saniye için temporal veri oluştur
  cat("Temporal veri (30 saniye) oluşturuluyor...\n")

  temporal_30s <- all_data_zscore %>%
    filter(Time >= 0 & Time <= 30000) %>%
    mutate(Time_Bin = floor(Time / 1000)) %>%
    group_by(Katilimci, Strateji, Time_Bin) %>%
    summarise(
      GSR_Z = mean(GSR_Z_Within, na.rm = TRUE),
      GSR_RAW = mean(GSR_RAW, na.rm = TRUE),
      N_Samples = n(),
      .groups = "drop"
    )

  cat("✓ Temporal veri hazır:", nrow(temporal_30s), "satır\n")
  cat("  Katılımcı:", n_distinct(temporal_30s$Katilimci), "\n")
  cat("  Strateji dağılımı:\n")
  print(table(temporal_30s$Strateji))
  cat("\n")

  # KONTROL: Her katılımcının Z-skor ortalaması ~0 olmalı
  kontrol <- temporal_30s %>%
    group_by(Katilimci) %>%
    summarise(
      Mean_Z = mean(GSR_Z, na.rm = TRUE),
      SD_Z = sd(GSR_Z, na.rm = TRUE),
      .groups = "drop"
    )

  cat("=== WITHIN-SUBJECT Z-SKOR KONTROLÜ ===\n\n")
  cat("Her katılımcının Z-skor ortalaması (ilk 10):\n")
  print(head(kontrol, 10))
  cat("\n")

  cat("Ortalama Z-skor dağılımı:\n")
  print(summary(kontrol$Mean_Z))
  cat("\n")

  ort_abs <- mean(abs(kontrol$Mean_Z))
  cat("Mutlak ortalamaların ortalaması:", round(ort_abs, 3), "\n")

  if(ort_abs < 2) {
    cat("✓ BAŞARILI! Within-subject Z-skorlar doğru.\n\n")

    # Environment'a kaydet
    assign("temporal_all_27", temporal_30s, envir = .GlobalEnv)
    cat("✓ temporal_all_27 güncellendi (within-subject Z-skor ile)\n\n")

    # Dosyaya kaydet
    inkar_dizin <- "/Users/borakarakoc/Desktop/Ölçek ve yöntem kaynağı makaleler/VERİLER/Eski deney verileri 13 ekim/Yeni klasör/İlk İnkar"
    kayit_path <- file.path(inkar_dizin, "temporal_27kisi_WITHIN_zscore.csv")
    write.csv(temporal_30s, kayit_path, row.names = FALSE, fileEncoding = "UTF-8")
    cat("✓ Kaydedildi:", kayit_path, "\n\n")

    cat("=== ŞİMDİ NE YAPMALI? ===\n\n")
    cat("1. Moderasyon analizini YENİDEN çalıştır (WITHIN-subject Z-skor ile):\n")
    cat("   source('FINAL_moderasyon_trial_level.R')\n\n")
    cat("2. Sonra grafikleri oluştur:\n")
    cat("   source('FINAL_moderasyon_27kisi_grafik.R')\n\n")

  } else {
    cat("⚠️ SORUN VAR: Z-skorlar hala within-subject görünmüyor!\n")
    cat("Ortalama çok yüksek (>2). Baseline hesaplamasında sorun olabilir.\n\n")
  }

} else {
  cat("⚠️ HATA: Gerekli sütunlar bulunamadı!\n")
  cat("Lütfen dosya yapısını kontrol edin.\n\n")

  cat("Tüm sütunlar:\n")
  print(colnames(gsr_data))
}

cat("\n=== İŞLEM TAMAMLANDI ===\n")
