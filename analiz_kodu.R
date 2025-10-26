# Katılımcı ve Stimulus Analizi

# 1. KATILIMCI SAYISI
cat("=== KATILIMCI SAYISI ===\n")
benzersiz_katilimcilar <- unique(birlestirilmis_tam_veri$Respondent)
toplam_katilimci <- length(benzersiz_katilimcilar)
cat("Toplam katılımcı sayısı:", toplam_katilimci, "\n")
cat("Katılımcı ID'leri:", paste(benzersiz_katilimcilar, collapse=", "), "\n\n")

# 2. HER KATILIMCI İÇİN STİMULUS BİLGİSİ
cat("=== KATILIMCI BAZINDA STİMULUS BİLGİSİ ===\n\n")

for(katilimci in benzersiz_katilimcilar) {
  cat("--- Katılımcı:", katilimci, "---\n")

  # Katılımcıya ait veriyi filtrele
  katilimci_verisi <- birlestirilmis_tam_veri[
    birlestirilmis_tam_veri$Respondent == katilimci,
  ]

  # Benzersiz stimulusları bul
  stimuluslar <- unique(katilimci_verisi$SourceStimuliName)
  stimuluslar <- stimuluslar[!is.na(stimuluslar) & stimuluslar != ""]

  cat("Gördüğü stimulus sayısı:", length(stimuluslar), "\n")
  cat("Stimulus sırası:\n")

  # Her stimulus için ilk göründüğü zamanı bul
  stimulus_sira <- data.frame()
  for(stim in stimuluslar) {
    ilk_gorunum <- min(which(katilimci_verisi$SourceStimuliName == stim))
    stimulus_sira <- rbind(stimulus_sira, data.frame(
      Sira = ilk_gorunum,
      Stimulus = stim
    ))
  }

  # Sıraya göre düzenle
  stimulus_sira <- stimulus_sira[order(stimulus_sira$Sira), ]

  for(i in 1:nrow(stimulus_sira)) {
    cat("  ", i, ". ", stimulus_sira$Stimulus[i], "\n", sep="")
  }
  cat("\n")
}

# 3. ÖZET TABLO
cat("\n=== ÖZET TABLO ===\n")
library(dplyr)

ozet <- birlestirilmis_tam_veri %>%
  filter(!is.na(SourceStimuliName) & SourceStimuliName != "") %>%
  group_by(Respondent) %>%
  summarise(
    Stimulus_Sayisi = n_distinct(SourceStimuliName),
    Stimuluslar = paste(unique(SourceStimuliName), collapse = " -> ")
  )

print(ozet)
