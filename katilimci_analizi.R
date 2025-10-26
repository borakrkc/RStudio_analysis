# Katılımcı ve Stimulus Analizi
# =================================

# Eğer veri environment'ta yoksa CSV'den yükleyin:
# birles_tirilmis_tam_veri <- read.csv("birles_tirilmis_tam_veri.csv")

# Veri zaten environment'taysanız aşağıdaki kodları çalıştırın:

# 1. VERİ YAPISI KONTROLÜ
cat("=== VERİ YAPISI ===\n")
cat("Boyut:", dim(birles_tirilmis_tam_veri), "\n")
cat("Satır sayısı:", nrow(birles_tirilmis_tam_veri), "\n")
cat("Sütun sayısı:", ncol(birles_tirilmis_tam_veri), "\n\n")

cat("Sütun isimleri:\n")
print(colnames(birles_tirilmis_tam_veri))
cat("\n")

cat("İlk 10 satır:\n")
print(head(birles_tirilmis_tam_veri, 10))
cat("\n")

cat("Veri yapısı:\n")
str(birles_tirilmis_tam_veri)
cat("\n")

# 2. KATILIMCI SAYISI
cat("\n=== KATILIMCI SAYISI ===\n")

# Veri setinde katılımcı bilgisi hangi sütunda?
# Genellikle "participant", "subject", "katilimci" gibi isimler kullanılır
# Önce sütun isimlerini kontrol edelim

# Katılımcı sütununu bulmak için olası isimler:
katilimci_sutunlari <- grep("katilimci|participant|subject|id|subj",
                             colnames(birles_tirilmis_tam_veri),
                             ignore.case = TRUE, value = TRUE)

if(length(katilimci_sutunlari) > 0) {
  cat("Bulunan katılımcı sütunları:", katilimci_sutunlari, "\n\n")

  # İlk bulunan sütunu kullan
  katilimci_sutun <- katilimci_sutunlari[1]

  benzersiz_katilimcilar <- unique(birles_tirilmis_tam_veri[[katilimci_sutun]])
  toplam_katilimci <- length(benzersiz_katilimcilar)

  cat("Toplam katılımcı sayısı:", toplam_katilimci, "\n")
  cat("Katılımcı ID'leri:\n")
  print(benzersiz_katilimcilar)
} else {
  cat("UYARI: Katılımcı sütunu bulunamadı. Lütfen sütun ismini manuel olarak belirtin.\n")
}

# 3. STIMULUS SAYISI
cat("\n=== STIMULUS BİLGİSİ ===\n")

# Stimulus sütununu bulmak için olası isimler:
stimulus_sutunlari <- grep("stimulus|stim|item|resim|image",
                           colnames(birles_tirilmis_tam_veri),
                           ignore.case = TRUE, value = TRUE)

if(length(stimulus_sutunlari) > 0) {
  cat("Bulunan stimulus sütunları:", stimulus_sutunlari, "\n\n")

  stimulus_sutun <- stimulus_sutunlari[1]

  # Toplam benzersiz stimulus sayısı
  benzersiz_stimulus <- unique(birles_tirilmis_tam_veri[[stimulus_sutun]])
  toplam_stimulus <- length(benzersiz_stimulus)

  cat("Toplam benzersiz stimulus sayısı:", toplam_stimulus, "\n")
  cat("Stimulus listesi:\n")
  print(benzersiz_stimulus)

  # Her katılımcı kaç stimulus görmüş?
  if(exists("katilimci_sutun")) {
    cat("\n--- Her katılımcının gördüğü stimulus sayısı ---\n")
    stimulus_per_participant <- table(birles_tirilmis_tam_veri[[katilimci_sutun]])
    print(stimulus_per_participant)

    cat("\nOrtalama stimulus sayısı:", mean(stimulus_per_participant), "\n")
  }
} else {
  cat("UYARI: Stimulus sütunu bulunamadı. Lütfen sütun ismini manuel olarak belirtin.\n")
}

# 4. STIMULUS SIRASI
cat("\n=== STIMULUS SIRASI ===\n")

if(exists("katilimci_sutun") && exists("stimulus_sutun")) {

  # Sıra bilgisi için trial, order gibi sütunlar
  sira_sutunlari <- grep("trial|order|sira|sequence",
                         colnames(birles_tirilmis_tam_veri),
                         ignore.case = TRUE, value = TRUE)

  # Her katılımcı için stimulus sırası
  for(katilimci in benzersiz_katilimcilar) {
    cat("\n--- Katılımcı:", katilimci, "---\n")

    katilimci_verisi <- birles_tirilmis_tam_veri[
      birles_tirilmis_tam_veri[[katilimci_sutun]] == katilimci,
    ]

    # Eğer sıra sütunu varsa ona göre sırala
    if(length(sira_sutunlari) > 0) {
      sira_sutun <- sira_sutunlari[1]
      katilimci_verisi <- katilimci_verisi[order(katilimci_verisi[[sira_sutun]]), ]

      cat("Stimulus sırası (", sira_sutun, " sütununa göre):\n", sep="")
      print(data.frame(
        Sira = katilimci_verisi[[sira_sutun]],
        Stimulus = katilimci_verisi[[stimulus_sutun]]
      ))
    } else {
      # Sıra sütunu yoksa, veri setindeki sırayla
      cat("Stimulus sırası (veri sırasına göre):\n")
      print(data.frame(
        Satir = 1:nrow(katilimci_verisi),
        Stimulus = katilimci_verisi[[stimulus_sutun]]
      ))
    }
  }

  # Özet tablo
  cat("\n=== ÖZET TABLO ===\n")
  library(dplyr)

  ozet <- birles_tirilmis_tam_veri %>%
    group_by(!!sym(katilimci_sutun)) %>%
    summarise(
      Stimulus_Sayisi = n(),
      Ilk_Stimulus = first(!!sym(stimulus_sutun)),
      Son_Stimulus = last(!!sym(stimulus_sutun))
    )

  print(ozet)
}

cat("\n=== ANALİZ TAMAMLANDI ===\n")
