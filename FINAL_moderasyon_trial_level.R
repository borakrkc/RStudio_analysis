# ===================================================================
# MODERASYON ANALİZİ - TRIAL LEVEL (Aggregasyon Bias YOK!)
# OSS (Linç Eğilimi) × Strateji Etkileşimi
# ===================================================================
#
# SADECE İLK İNKAR GÖRENLER (27 katılımcı)
# Trial-level: Her time bin ayrı gözlem → ~1620 satır
# Aggregation YOK!
#
# ===================================================================

library(lme4)
library(lmerTest)
library(ggplot2)
library(dplyr)
library(emmeans)

cat("\n")
cat("=" %+% paste(rep("=", 60), collapse = "") %+% "\n")
cat("  MODERASYON ANALİZİ: OSS × STRATEJİ (TRIAL-LEVEL)\n")
cat("=" %+% paste(rep("=", 60), collapse = "") %+% "\n\n")

# ===================================================================
# 1. KONTROL: VERİLER ZATEN YÜKLÜ MÜ?
# ===================================================================

cat("=== VERİ KONTROLÜ ===\n\n")

# temporal_all_27 ve oss_veri objeleri environment'ta olmalı
if(!exists("temporal_all_27")) {
  stop("HATA: temporal_all_27 objesi bulunamadı! Önce temporal analizi çalıştırın.")
}

if(!exists("oss_veri")) {
  stop("HATA: oss_veri objesi bulunamadı! OSS verisini yükleyin.")
}

cat("✓ temporal_all_27 bulundu:", nrow(temporal_all_27), "satır\n")
cat("✓ oss_veri bulundu:", nrow(oss_veri), "katılımcı\n\n")

# ===================================================================
# 2. OSS TOPLAM SKORUNU HESAPLA (Eğer yoksa)
# ===================================================================

cat("=== OSS SKORLARI ===\n\n")

if(!"Toplam" %in% colnames(oss_veri)) {
  # OSS maddelerini bul
  oss_sutunlar <- grep("^X[1-9]\\.", colnames(oss_veri), value = TRUE)

  if(length(oss_sutunlar) == 0) {
    stop("HATA: OSS maddeleri bulunamadı! Sütun isimleri doğru mu?")
  }

  cat("OSS maddeleri bulundu:", length(oss_sutunlar), "madde\n")
  oss_veri$Toplam <- rowSums(oss_veri[, oss_sutunlar], na.rm = TRUE)
} else {
  cat("✓ OSS Toplam skoru zaten mevcut\n")
}

cat("OSS skor özeti:\n")
print(summary(oss_veri$Toplam))
cat("\n")

# ===================================================================
# 3. İSİM EŞLEŞTİRME SÖZLÜĞÜ (MANUEL)
# ===================================================================

cat("=== İSİM EŞLEŞTİRME ===\n\n")

# OSS verisindeki isim sütununu bul
oss_isim_sutun <- NULL
if("Adınız.Soyadınız" %in% colnames(oss_veri)) {
  oss_isim_sutun <- "Adınız.Soyadınız"
} else if("Katılımcı" %in% colnames(oss_veri)) {
  oss_isim_sutun <- "Katılımcı"
} else {
  # İlk sütunu kullan
  oss_isim_sutun <- colnames(oss_veri)[1]
  cat("Uyarı: İsim sütunu bulunamadı, ilk sütun kullanılıyor:", oss_isim_sutun, "\n")
}

# OSS isimlerini temizle
oss_veri$Isim_Clean <- toupper(trimws(oss_veri[[oss_isim_sutun]]))

# Temporal isimlerini temizle
temporal_all_27$Isim_Clean <- toupper(trimws(temporal_all_27$Katilimci))

cat("OSS'deki benzersiz isimler:", length(unique(oss_veri$Isim_Clean)), "\n")
cat("Temporal'deki benzersiz isimler:", length(unique(temporal_all_27$Isim_Clean)), "\n\n")

# Doğrudan eşleşenleri kontrol et
dogrudan_eslesme <- intersect(
  unique(temporal_all_27$Isim_Clean),
  unique(oss_veri$Isim_Clean)
)

cat("Doğrudan eşleşen katılımcı sayısı:", length(dogrudan_eslesme), "\n")

if(length(dogrudan_eslesme) < 20) {
  cat("\n⚠️ UYARI: Çok az isim eşleşti!\n")
  cat("Manuel eşleştirme sözlüğü oluşturuluyor...\n\n")

  # OSS ve Temporal'deki isimleri göster
  cat("OSS'deki ilk 15 isim:\n")
  print(head(sort(unique(oss_veri$Isim_Clean)), 15))
  cat("\nTemporal'deki ilk 15 isim:\n")
  print(head(sort(unique(temporal_all_27$Isim_Clean)), 15))
  cat("\n")

  # TAM Manuel eşleştirme sözlüğü (27 katılımcının TÜMÜ)
  # Türkçe karakter farkları (İ/I, Ü/U) ve eksik soyadlar
  isim_eslestirme <- data.frame(
    OSS_Isim = c(
      # Türkçe karakter farkları (İ → I)
      "AYŞENUR ÖZISKENDER",
      "SÜMEYYE DILMAÇ",
      "BERRAK FAIZOĞLU",
      "ESRA YIĞITER",
      "IREM ADAR",
      "SENA ÇETIN",
      "ECRIN BALABAN",
      "KERIM BULAGAY",
      "BERKAY KARADENIZ",

      # Eksik/fazla soyadlar
      "FATMA ZEHRA ŞIMŞEKLIEL",
      "IRMAK",
      "AYŞE MÜBERRA EROL",
      "CENK",

      # Ü → U farkı
      "TUBA ZEHRA ATEŞ"
    ),
    Temporal_Isim = c(
      # Temporal'deki karşılıkları
      "AYŞENUR ÖZİSKENDER",
      "SÜMEYYE DİLMAÇ",
      "BERRAK FAİZOĞLU",
      "ESRA YİĞİTER",
      "İREM ADAR",
      "SENA ÇETİN",
      "ECRİN BALABAN",
      "KERİM BULAGAY",
      "BERKAY KARADENİZ",

      "FATMA ZEHRA ŞİMŞEK",
      "IRMAK KAYA",
      "AYŞE EROL",
      "CENK EREN KUZU",

      "TUĞBA ZEHRA ATEŞ"
    ),
    stringsAsFactors = FALSE
  )

  cat("Manuel eşleştirme sözlüğü:\n")
  print(isim_eslestirme)
  cat("\n")

  # Manuel eşleştirmeyi uygula
  # OSS'deki isimleri Temporal'e dönüştür
  oss_veri$Isim_Mapped <- oss_veri$Isim_Clean

  for(i in 1:nrow(isim_eslestirme)) {
    oss_veri$Isim_Mapped[oss_veri$Isim_Clean == isim_eslestirme$OSS_Isim[i]] <-
      isim_eslestirme$Temporal_Isim[i]
  }

  # Eşleşen sayıyı tekrar kontrol et
  eslesen_isimler <- intersect(
    unique(temporal_all_27$Isim_Clean),
    unique(oss_veri$Isim_Mapped)
  )

  cat("Manuel eşleştirme sonrası eşleşen:", length(eslesen_isimler), "katılımcı\n\n")

} else {
  cat("✓ Yeterli isim eşleşti, manuel eşleştirme gerekmiyor\n\n")
  oss_veri$Isim_Mapped <- oss_veri$Isim_Clean
}

# ===================================================================
# 4. VERİLERİ BİRLEŞTİR
# ===================================================================

cat("=== VERİLERİ BİRLEŞTİRME ===\n\n")

# OSS skorlarını temporal veriye ekle
temporal_oss <- temporal_all_27 %>%
  left_join(
    oss_veri %>% select(Isim_Mapped, OSS_Toplam = Toplam),
    by = c("Isim_Clean" = "Isim_Mapped")
  )

cat("Birleştirme sonucu:\n")
cat("Toplam satır:", nrow(temporal_oss), "\n")
cat("OSS verisi olan satır:", sum(!is.na(temporal_oss$OSS_Toplam)), "\n")
cat("Benzersiz katılımcı (OSS ile):",
    n_distinct(temporal_oss$Katilimci[!is.na(temporal_oss$OSS_Toplam)]), "\n\n")

# NA'ları kaldır
temporal_oss_clean <- temporal_oss %>%
  filter(!is.na(OSS_Toplam))

cat("Temiz veri (NA'lar kaldırıldı):\n")
cat("Satır sayısı:", nrow(temporal_oss_clean), "\n")
cat("Katılımcı sayısı:", n_distinct(temporal_oss_clean$Katilimci), "\n")
cat("Strateji başına:\n")
print(table(temporal_oss_clean$Strateji))
cat("\n\n")

# Minimum veri kontrolü
if(nrow(temporal_oss_clean) < 100) {
  stop("HATA: Çok az veri eşleşti (", nrow(temporal_oss_clean), " satır).\n",
       "İsim eşleştirme sözlüğünü güncellemeniz gerekiyor!")
}

# ===================================================================
# 5. OSS'Yİ MERKEZLE (Moderasyon analizi için)
# ===================================================================

cat("=== OSS MERKEZLENDİRME ===\n\n")

temporal_oss_clean <- temporal_oss_clean %>%
  mutate(
    OSS_Centered = OSS_Toplam - mean(OSS_Toplam, na.rm = TRUE),
    OSS_Standardized = scale(OSS_Toplam, center = TRUE, scale = TRUE)[,1]
  )

cat("OSS özeti:\n")
cat("Ortalama:", mean(temporal_oss_clean$OSS_Toplam), "\n")
cat("SD:", sd(temporal_oss_clean$OSS_Toplam), "\n")
cat("Min:", min(temporal_oss_clean$OSS_Toplam), "\n")
cat("Max:", max(temporal_oss_clean$OSS_Toplam), "\n\n")

# ===================================================================
# 6. LMM MODERASYON ANALİZİ
# ===================================================================

cat("=" %+% paste(rep("=", 60), collapse = "") %+% "\n")
cat("  LMM MODERASYON ANALİZİ\n")
cat("=" %+% paste(rep("=", 60), collapse = "") %+% "\n\n")

# Model 1: Ana etkiler (Strateji + OSS + Zaman)
cat("Model 1: Ana Etkiler\n")
cat("Formula: GSR_Z ~ Strateji + OSS_Centered + Time_Bin + (1|Katilimci)\n\n")

model1 <- lmer(
  GSR_Z ~ Strateji + OSS_Centered + Time_Bin + (1|Katilimci),
  data = temporal_oss_clean,
  REML = FALSE
)

summary1 <- summary(model1)
print(summary1)
cat("\n")

anova1 <- anova(model1)
cat("ANOVA Tablosu (Model 1):\n")
print(anova1)
cat("\n\n")

# Model 2: Moderasyon (Strateji × OSS etkileşimi)
cat("Model 2: Moderasyon (Strateji × OSS Etkileşimi)\n")
cat("Formula: GSR_Z ~ Strateji * OSS_Centered + Time_Bin + (1|Katilimci)\n\n")

model2 <- lmer(
  GSR_Z ~ Strateji * OSS_Centered + Time_Bin + (1|Katilimci),
  data = temporal_oss_clean,
  REML = FALSE
)

summary2 <- summary(model2)
print(summary2)
cat("\n")

anova2 <- anova(model2)
cat("ANOVA Tablosu (Model 2):\n")
print(anova2)
cat("\n\n")

# ===================================================================
# 7. MODEL KARŞILAŞTIRMA
# ===================================================================

cat("=" %+% paste(rep("=", 60), collapse = "") %+% "\n")
cat("  MODEL KARŞILAŞTIRMA\n")
cat("=" %+% paste(rep("=", 60), collapse = "") %+% "\n\n")

model_comparison <- anova(model1, model2)
print(model_comparison)
cat("\n")

# Etkileşim p-değeri
interact_p <- anova2["Strateji:OSS_Centered", "Pr(>F)"]
cat("Etkileşim p-değeri:", round(interact_p, 4), "\n")

if(interact_p < 0.05) {
  cat("✓ ANLamlı moderasyon etkisi bulundu! (p < 0.05)\n\n")
} else {
  cat("✗ Moderasyon etkisi anlamlı değil (p > 0.05)\n\n")
}

# ===================================================================
# 8. SIMPLE SLOPES ANALİZİ
# ===================================================================

cat("=" %+% paste(rep("=", 60), collapse = "") %+% "\n")
cat("  SIMPLE SLOPES ANALİZİ\n")
cat("=" %+% paste(rep("=", 60), collapse = "") %+% "\n\n")

# OSS'nin -1 SD, Mean, +1 SD değerlerinde strateji etkisi
oss_sd <- sd(temporal_oss_clean$OSS_Toplam)
oss_mean <- mean(temporal_oss_clean$OSS_Toplam)

oss_levels <- c(
  Dusuk = oss_mean - oss_sd,
  Orta = oss_mean,
  Yuksek = oss_mean + oss_sd
)

cat("OSS seviyeleri:\n")
cat(sprintf("  Düşük (-1 SD): %.2f\n", oss_levels["Dusuk"]))
cat(sprintf("  Orta (Mean):   %.2f\n", oss_levels["Orta"]))
cat(sprintf("  Yüksek (+1 SD): %.2f\n\n", oss_levels["Yuksek"]))

# emmeans ile simple slopes
emm_slopes <- emtrends(
  model2,
  pairwise ~ Strateji,
  var = "OSS_Centered",
  at = list(
    OSS_Centered = oss_levels - oss_mean,
    Time_Bin = mean(as.numeric(as.character(temporal_oss_clean$Time_Bin)))
  )
)

cat("Simple Slopes (Strateji etkisi farklı OSS seviyelerinde):\n")
print(summary(emm_slopes))
cat("\n\n")

# Strateji karşılaştırması farklı OSS seviyelerinde
emm_strateji <- emmeans(
  model2,
  pairwise ~ Strateji | OSS_Centered,
  at = list(
    OSS_Centered = oss_levels - oss_mean,
    Time_Bin = mean(as.numeric(as.character(temporal_oss_clean$Time_Bin)))
  )
)

cat("Strateji karşılaştırması (farklı OSS seviyelerinde):\n")
print(summary(emm_strateji))
cat("\n\n")

# ===================================================================
# 9. GRAFİKLER
# ===================================================================

cat("=" %+% paste(rep("=", 60), collapse = "") %+% "\n")
cat("  GRAFİKLER OLUŞTURULUYOR\n")
cat("=" %+% paste(rep("=", 60), collapse = "") %+% "\n\n")

# Tahmin için grid oluştur
pred_grid <- expand.grid(
  Strateji = c("İnkar", "Özür"),
  OSS_Centered = seq(min(temporal_oss_clean$OSS_Centered),
                      max(temporal_oss_clean$OSS_Centered),
                      length.out = 50),
  Time_Bin = mean(as.numeric(as.character(temporal_oss_clean$Time_Bin)))
)

pred_grid$Katilimci <- NA  # Random effect için
pred_grid$GSR_Z_Pred <- predict(model2, newdata = pred_grid, re.form = NA)

# OSS_Centered'ı OSS_Toplam'a geri dönüştür
pred_grid$OSS_Toplam <- pred_grid$OSS_Centered + oss_mean

# GRAFIK 1: Moderasyon scatter + regression
cat("Grafik 1: Moderasyon scatter plot...\n")

# Temporal veriden her katılımcı için ortalama al (görselleştirme için)
plot_data <- temporal_oss_clean %>%
  group_by(Katilimci, Strateji, OSS_Toplam) %>%
  summarise(
    Mean_GSR_Z = mean(GSR_Z, na.rm = TRUE),
    .groups = "drop"
  )

p1 <- ggplot() +
  # Gerçek veri noktaları
  geom_point(data = plot_data,
             aes(x = OSS_Toplam, y = Mean_GSR_Z, color = Strateji),
             size = 4, alpha = 0.6) +
  # Tahmin edilen regresyon çizgileri
  geom_line(data = pred_grid,
            aes(x = OSS_Toplam, y = GSR_Z_Pred, color = Strateji),
            linewidth = 2) +
  scale_color_manual(
    values = c("İnkar" = "#E63946", "Özür" = "#06A77D"),
    name = "Strateji"
  ) +
  labs(
    title = "Moderasyon: Strateji × Linç Eğilimi (OSS)",
    subtitle = sprintf(
      "Trial-Level LMM: n=%d katılımcı, %d gözlem | p(etkileşim)=%.4f",
      n_distinct(temporal_oss_clean$Katilimci),
      nrow(temporal_oss_clean),
      interact_p
    ),
    x = "OSS Toplam Skoru (Linç Eğilimi)",
    y = "GSR Z-Skoru",
    caption = sprintf(
      "Model: GSR_Z ~ Strateji × OSS + Time + (1|Katılımcı)\n%s",
      ifelse(interact_p < 0.05, "** Anlamlı moderasyon", "n.s.")
    )
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

ggsave("FINAL_Moderasyon_TrialLevel.png", p1, width = 10, height = 7, dpi = 300)
cat("✓ Grafik 1 kaydedildi: FINAL_Moderasyon_TrialLevel.png\n")

# GRAFIK 2: Simple slopes
cat("Grafik 2: Simple slopes...\n")

# Tahmin için veri hazırla
simple_pred <- data.frame(
  OSS_Level = rep(c("Düşük (-1 SD)", "Orta", "Yüksek (+1 SD)"), each = 2),
  Strateji = rep(c("İnkar", "Özür"), 3),
  OSS_Centered = rep(oss_levels - oss_mean, each = 2),
  Time_Bin = mean(as.numeric(as.character(temporal_oss_clean$Time_Bin))),
  Katilimci = NA
)

simple_pred$GSR_Z_Pred <- predict(model2, newdata = simple_pred, re.form = NA)
simple_pred$OSS_Level <- factor(simple_pred$OSS_Level,
                                 levels = c("Düşük (-1 SD)", "Orta", "Yüksek (+1 SD)"))

p2 <- ggplot(simple_pred, aes(x = Strateji, y = GSR_Z_Pred, fill = Strateji)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7, alpha = 0.8) +
  facet_wrap(~ OSS_Level, ncol = 3) +
  scale_fill_manual(
    values = c("İnkar" = "#E63946", "Özür" = "#06A77D")
  ) +
  labs(
    title = "Simple Slopes: Farklı OSS Seviyelerinde Strateji Etkisi",
    subtitle = sprintf(
      "Trial-Level LMM: n=%d katılımcı, %d gözlem",
      n_distinct(temporal_oss_clean$Katilimci),
      nrow(temporal_oss_clean)
    ),
    x = "İletişim Stratejisi",
    y = "Tahmin Edilen GSR Z-Skoru",
    fill = "Strateji",
    caption = sprintf("p(etkileşim) = %.4f", interact_p)
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    strip.text = element_text(face = "bold", size = 12),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

ggsave("FINAL_SimpleSlopes_TrialLevel.png", p2, width = 12, height = 6, dpi = 300)
cat("✓ Grafik 2 kaydedildi: FINAL_SimpleSlopes_TrialLevel.png\n\n")

# ===================================================================
# 10. SONUÇLARI KAYDET
# ===================================================================

cat("=== SONUÇLARI KAYDETME ===\n\n")

# Veriyi kaydet
write.csv(temporal_oss_clean, "moderasyon_trial_data.csv",
          row.names = FALSE, fileEncoding = "UTF-8")
cat("✓ Trial-level veri kaydedildi: moderasyon_trial_data.csv\n")

# Model sonuçlarını kaydet
sink("moderasyon_trial_sonuclari.txt")
cat("MODERASYON ANALİZİ SONUÇLARI (TRIAL-LEVEL)\n")
cat(paste(rep("=", 60), collapse = ""), "\n\n")
cat("Tarih:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("Katılımcı sayısı:", n_distinct(temporal_oss_clean$Katilimci), "\n")
cat("Toplam gözlem sayısı:", nrow(temporal_oss_clean), "\n\n")

cat("MODEL 1: ANA ETKİLER\n")
cat(paste(rep("-", 60), collapse = ""), "\n")
print(summary1)
cat("\n")
print(anova1)
cat("\n\n")

cat("MODEL 2: MODERASYON (Strateji × OSS)\n")
cat(paste(rep("-", 60), collapse = ""), "\n")
print(summary2)
cat("\n")
print(anova2)
cat("\n\n")

cat("MODEL KARŞILAŞTIRMA\n")
cat(paste(rep("-", 60), collapse = ""), "\n")
print(model_comparison)
cat("\n\n")

cat("SIMPLE SLOPES\n")
cat(paste(rep("-", 60), collapse = ""), "\n")
print(summary(emm_slopes))
cat("\n\n")
print(summary(emm_strateji))

sink()
cat("✓ Model sonuçları kaydedildi: moderasyon_trial_sonuclari.txt\n\n")

cat("=" %+% paste(rep("=", 60), collapse = "") %+% "\n")
cat("  ANALİZ TAMAMLANDI!\n")
cat("=" %+% paste(rep("=", 60), collapse = "") %+% "\n\n")

cat("Oluşturulan dosyalar:\n")
cat("  1. moderasyon_trial_data.csv - Trial-level veri\n")
cat("  2. FINAL_Moderasyon_TrialLevel.png - Moderasyon grafiği\n")
cat("  3. FINAL_SimpleSlopes_TrialLevel.png - Simple slopes grafiği\n")
cat("  4. moderasyon_trial_sonuclari.txt - Model sonuçları\n\n")

cat("Katılımcı sayısı:", n_distinct(temporal_oss_clean$Katilimci), "\n")
cat("Toplam gözlem:", nrow(temporal_oss_clean), "\n")
cat("Etkileşim p-değeri:", round(interact_p, 4), "\n")
if(interact_p < 0.05) {
  cat("SONUÇ: OSS linç eğilimi, strateji-GSR ilişkisini MODERE EDİYOR! **\n")
} else {
  cat("SONUÇ: Moderasyon etkisi anlamlı değil.\n")
}
