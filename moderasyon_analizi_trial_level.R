# MODERASYON ANALİZİ - TRIAL LEVEL (Aggregasyon Bias Yok!)
# OSS (Online Social Shaming) × Strateji etkileşimi
# ===================================================================

library(dplyr)
library(lme4)
library(lmerTest)
library(ggplot2)
library(emmeans)

# 1. VERİLERİ YÜKLE
# ===================================================================

cat("=== VERİLERİ YÜKLEME ===\n")

# GSR Z-skoru trial-level verisi (520,933 satır)
gsr_file <- "/Users/borakarakoc/Desktop/Ölçek ve yöntem kaynağı makaleler/VERİLER/Eski deney verileri 13 ekim/Yeni klasör/İlk İnkar/ilkinkar_gsr_zscore.csv"

# OSS ölçek verileri (27 katılımcı)
oss_file <- "/Users/borakarakoc/Desktop/Ölçek ve yöntem kaynağı makaleler/VERİLER/Eski deney verileri 13 ekim/Yeni klasör/İlk İnkar/oss_inkar_ters_kodlu.csv"

cat("GSR verisi yükleniyor...\n")
gsr_data <- read.csv(gsr_file, fileEncoding = "UTF-8")

cat("OSS verisi yükleniyor...\n")
oss_data <- read.csv(oss_file, fileEncoding = "UTF-8")

cat("GSR veri boyutu:", nrow(gsr_data), "satır\n")
cat("OSS veri boyutu:", nrow(oss_data), "katılımcı\n\n")

# 2. İSİM EŞLEŞTİRME SORUNUNU ÇÖZ
# ===================================================================

cat("=== İSİM EŞLEŞTİRME ===\n")

# OSS verilerindeki isimleri düzenle
oss_data$Katilimci_Clean <- toupper(trimws(oss_data$Katılımcı))

# Manuel isim eşleştirme sözlüğü
isim_eslestirme <- data.frame(
  OSS_Isim = c(
    "AYŞE MÜBERRA EROL",
    "CENK",
    "IRMAK",
    "TUBA ZEHRA ATEŞ",
    "FATMA ZEHRA ŞIMŞEKLIEL",
    "IREM ADAR",
    "ESRA",
    "HILAL",
    "BEYZA",
    "ZEYNEP",
    "AYŞE",
    "MERVE",
    "ALI",
    "MEHMET"
  ),
  GSR_Isim = c(
    "AYŞE EROL",
    "CENK EREN KUZU",
    "IRMAK KAYA",
    "TUĞBA ZEHRA ATEŞ",
    "FATMA ZEHRA ŞİMŞEK",
    "İREM ADAR",
    "ESRA [SOYAD]",
    "HİLAL [SOYAD]",
    "BEYZA [SOYAD]",
    "ZEYNEP [SOYAD]",
    "AYŞE [SOYAD]",
    "MERVE [SOYAD]",
    "ALİ [SOYAD]",
    "MEHMET [SOYAD]"
  ),
  stringsAsFactors = FALSE
)

# GSR verisindeki benzersiz katılımcıları kontrol et
gsr_katilimcilar <- unique(gsr_data$Respondent)
cat("GSR verisinde", length(gsr_katilimcilar), "benzersiz katılımcı var:\n")
print(head(gsr_katilimcilar, 20))
cat("\n")

# OSS verisindeki katılımcıları kontrol et
oss_katilimcilar <- unique(oss_data$Katilimci_Clean)
cat("OSS verisinde", length(oss_katilimcilar), "katılımcı var:\n")
print(oss_katilimcilar)
cat("\n")

# Doğrudan eşleşenleri bul
dogrudan_eslesme <- intersect(
  toupper(trimws(gsr_katilimcilar)),
  oss_katilimcilar
)
cat("Doğrudan eşleşen:", length(dogrudan_eslesme), "katılımcı\n")
print(dogrudan_eslesme)
cat("\n")

# 3. OSS SKORLARINI GSR VERİSİNE EKLE
# ===================================================================

cat("=== OSS SKORLARINI EKLEME ===\n")

# GSR verisine temiz isim sütunu ekle
gsr_data$Katilimci_Clean <- toupper(trimws(gsr_data$Respondent))

# OSS toplam skorunu ekle
gsr_with_oss <- gsr_data %>%
  left_join(
    oss_data %>% select(Katilimci_Clean, OSS_Toplam = Toplam),
    by = "Katilimci_Clean"
  )

# Kaç katılımcı eşleşti?
eslesen_katilimcilar <- gsr_with_oss %>%
  filter(!is.na(OSS_Toplam)) %>%
  pull(Katilimci_Clean) %>%
  unique()

cat("Eşleşen katılımcı sayısı:", length(eslesen_katilimcilar), "\n")
cat("Eşleşen katılımcılar:\n")
print(eslesen_katilimcilar)
cat("\n")

# Eşleşmeyen katılımcıları göster
eslesmeyen_oss <- setdiff(oss_katilimcilar, toupper(trimws(gsr_katilimcilar)))
eslesmeyen_gsr <- setdiff(toupper(trimws(gsr_katilimcilar)), oss_katilimcilar)

cat("OSS'de var ama GSR'de eşleşmeyen:", length(eslesmeyen_oss), "katılımcı\n")
print(eslesmeyen_oss)
cat("\n")

cat("GSR'de var ama OSS'de eşleşmeyen:", length(eslesmeyen_gsr), "katılımcı\n")
print(head(eslesmeyen_gsr, 20))
cat("\n")

# 4. TEMPORAL ANALİZ İÇİN VERİYİ HAZIRLA (30 saniye)
# ===================================================================

cat("=== TEMPORAL VERİ HAZIRLIĞI ===\n")

# Sadece OSS verisi olanları al
temporal_data <- gsr_with_oss %>%
  filter(!is.na(OSS_Toplam))

cat("OSS verisi olan toplam satır sayısı:", nrow(temporal_data), "\n")

# İnkar ve Özür stratejileri için ayrı veri setleri oluştur
inkar_data <- temporal_data %>%
  filter(grepl("inkar|İnkar|INKAR", SourceStimuliName, ignore.case = TRUE))

ozur_data <- temporal_data %>%
  filter(grepl("özür|Özür|ÖZÜR|ozur", SourceStimuliName, ignore.case = TRUE))

cat("İnkar satır sayısı:", nrow(inkar_data), "\n")
cat("Özür satır sayısı:", nrow(ozur_data), "\n")

# Her katılımcı için baseline hesapla (ilk 3 saniye = 0-3000 ms)
inkar_baseline <- inkar_data %>%
  filter(RecordingTime >= 0 & RecordingTime <= 3000) %>%
  group_by(Katilimci_Clean) %>%
  summarise(
    Baseline_Mean = mean(GSR.RAW, na.rm = TRUE),
    Baseline_SD = sd(GSR.RAW, na.rm = TRUE),
    .groups = "drop"
  )

ozur_baseline <- ozur_data %>%
  filter(RecordingTime >= 0 & RecordingTime <= 3000) %>%
  group_by(Katilimci_Clean) %>%
  summarise(
    Baseline_Mean = mean(GSR.RAW, na.rm = TRUE),
    Baseline_SD = sd(GSR.RAW, na.rm = TRUE),
    .groups = "drop"
  )

# Z-skorları hesapla (0-30000 ms)
inkar_30s <- inkar_data %>%
  filter(RecordingTime >= 0 & RecordingTime <= 30000) %>%
  left_join(inkar_baseline, by = "Katilimci_Clean") %>%
  mutate(
    GSR_Z = (GSR.RAW - Baseline_Mean) / Baseline_SD,
    Time_Bin = cut(RecordingTime,
                   breaks = seq(0, 30000, by = 1000),
                   labels = 0:29,
                   include.lowest = TRUE),
    Strateji = "İnkar"
  )

ozur_30s <- ozur_data %>%
  filter(RecordingTime >= 0 & RecordingTime <= 30000) %>%
  left_join(ozur_baseline, by = "Katilimci_Clean") %>%
  mutate(
    GSR_Z = (GSR.RAW - Baseline_Mean) / Baseline_SD,
    Time_Bin = cut(RecordingTime,
                   breaks = seq(0, 30000, by = 1000),
                   labels = 0:29,
                   include.lowest = TRUE),
    Strateji = "Özür"
  )

# Birleştir
temporal_combined <- bind_rows(inkar_30s, ozur_30s) %>%
  filter(!is.na(Time_Bin) & !is.na(GSR_Z) & !is.na(OSS_Toplam))

cat("Birleştirilmiş temporal veri:", nrow(temporal_combined), "satır\n")
cat("Benzersiz katılımcı sayısı:", n_distinct(temporal_combined$Katilimci_Clean), "\n\n")

# 5. TRIAL-LEVEL AGGREGASYON (saniye bazında ortalama)
# ===================================================================

cat("=== TRIAL-LEVEL AGGREGASYON ===\n")

# Her katılımcı × strateji × time_bin için ortalama al
temporal_trial <- temporal_combined %>%
  group_by(Katilimci_Clean, Strateji, Time_Bin, OSS_Toplam) %>%
  summarise(
    GSR_Z = mean(GSR_Z, na.rm = TRUE),
    GSR_RAW = mean(GSR.RAW, na.rm = TRUE),
    N_Samples = n(),
    .groups = "drop"
  ) %>%
  mutate(
    Time_Bin_Numeric = as.numeric(as.character(Time_Bin)),
    OSS_Centered = scale(OSS_Toplam, center = TRUE, scale = FALSE)[,1],
    OSS_Standardized = scale(OSS_Toplam, center = TRUE, scale = TRUE)[,1]
  )

cat("Trial-level veri boyutu:", nrow(temporal_trial), "satır\n")
cat("Katılımcı sayısı:", n_distinct(temporal_trial$Katilimci_Clean), "\n")
cat("Strateji başına:", table(temporal_trial$Strateji), "\n\n")

# 6. LMM MODERASYON ANALİZİ
# ===================================================================

cat("=== LMM MODERASYON ANALİZİ ===\n\n")

# Model 1: Ana etkiler (Strateji + OSS)
cat("Model 1: Ana Etkiler\n")
cat("Formula: GSR_Z ~ Strateji + OSS_Centered + Time_Bin_Numeric + (1|Katilimci_Clean)\n\n")

model1 <- lmer(
  GSR_Z ~ Strateji + OSS_Centered + Time_Bin_Numeric + (1|Katilimci_Clean),
  data = temporal_trial,
  REML = FALSE
)

print(summary(model1))
cat("\n")
print(anova(model1))
cat("\n\n")

# Model 2: Moderasyon (Strateji × OSS etkileşimi)
cat("Model 2: Moderasyon (Strateji × OSS Etkileşimi)\n")
cat("Formula: GSR_Z ~ Strateji * OSS_Centered + Time_Bin_Numeric + (1|Katilimci_Clean)\n\n")

model2 <- lmer(
  GSR_Z ~ Strateji * OSS_Centered + Time_Bin_Numeric + (1|Katilimci_Clean),
  data = temporal_trial,
  REML = FALSE
)

print(summary(model2))
cat("\n")
print(anova(model2))
cat("\n\n")

# Model karşılaştırma
cat("=== MODEL KARŞILAŞTIRMA ===\n")
model_comparison <- anova(model1, model2)
print(model_comparison)
cat("\n\n")

# 7. SIMPLE SLOPES ANALİZİ
# ===================================================================

cat("=== SIMPLE SLOPES ANALİZİ ===\n\n")

# OSS_Centered'ın -1 SD, ortalama, +1 SD değerlerinde strateji etkisi
emm_simple <- emtrends(
  model2,
  pairwise ~ Strateji,
  var = "OSS_Centered",
  at = list(
    OSS_Centered = c(-sd(temporal_trial$OSS_Centered),
                     0,
                     sd(temporal_trial$OSS_Centered))
  )
)

cat("Simple Slopes (Strateji etkisi farklı OSS seviyelerinde):\n")
print(summary(emm_simple))
cat("\n\n")

# OSS'nin farklı seviyelerinde strateji karşılaştırması
emm_interaction <- emmeans(
  model2,
  pairwise ~ Strateji | OSS_Centered,
  at = list(
    OSS_Centered = c(-sd(temporal_trial$OSS_Centered),
                     0,
                     sd(temporal_trial$OSS_Centered)),
    Time_Bin_Numeric = mean(temporal_trial$Time_Bin_Numeric)
  )
)

cat("Strateji karşılaştırması (farklı OSS seviyelerinde):\n")
print(summary(emm_interaction))
cat("\n\n")

# 8. EFFECT SIZE HESAPLAVERİ
# ===================================================================

cat("=== EFFECT SIZE ===\n\n")

# R-squared değişimi
r2_model1 <- as.data.frame(VarCorr(model1))
r2_model2 <- as.data.frame(VarCorr(model2))

cat("Model 1 varyans bileşenleri:\n")
print(r2_model1)
cat("\n")

cat("Model 2 varyans bileşenleri:\n")
print(r2_model2)
cat("\n\n")

# 9. MODERASYON GRAFİĞİ (İnteraktif Etki)
# ===================================================================

cat("=== MODERASYON GRAFİĞİ OLUŞTURMA ===\n")

# OSS'nin -1 SD, ortalama, +1 SD değerlerinde tahmin edilen değerleri al
oss_sd <- sd(temporal_trial$OSS_Centered)
oss_levels <- data.frame(
  OSS_Centered = c(-oss_sd, 0, oss_sd),
  OSS_Label = c("Düşük Linç Eğilimi (-1 SD)",
                "Ortalama Linç Eğilimi",
                "Yüksek Linç Eğilimi (+1 SD)")
)

# Tahmin için grid oluştur
prediction_grid <- expand.grid(
  Strateji = c("İnkar", "Özür"),
  OSS_Centered = oss_levels$OSS_Centered,
  Time_Bin_Numeric = mean(temporal_trial$Time_Bin_Numeric)
)

# Tahminleri ekle
prediction_grid$Katilimci_Clean <- NA  # Random effect için
prediction_grid$GSR_Z_Pred <- predict(model2, newdata = prediction_grid, re.form = NA)

# OSS label ekle
prediction_grid <- prediction_grid %>%
  left_join(oss_levels, by = "OSS_Centered")

# Moderasyon grafiği
png("MODERASYON_Strateji_OSS.png", width = 800, height = 600, res = 100)

ggplot(prediction_grid, aes(x = Strateji, y = GSR_Z_Pred,
                             group = OSS_Label, color = OSS_Label)) +
  geom_line(size = 1.2) +
  geom_point(size = 4) +
  scale_color_manual(
    values = c("Düşük Linç Eğilimi (-1 SD)" = "#2E86AB",
               "Ortalama Linç Eğilimi" = "#A23B72",
               "Yüksek Linç Eğilimi (+1 SD)" = "#F18F01"),
    name = "Linç Eğilimi (OSS)"
  ) +
  labs(
    title = "Moderasyon: Strateji × Linç Eğilimi Etkileşimi",
    subtitle = sprintf("LMM Trial-Level Analiz (n=%d katılımcı, %d gözlem)",
                       n_distinct(temporal_trial$Katilimci_Clean),
                       nrow(temporal_trial)),
    x = "İletişim Stratejisi",
    y = "GSR Z-Skoru (Tahmin Edilen)",
    caption = sprintf("Model: GSR_Z ~ Strateji × OSS + Zaman + (1|Katılımcı)\np(etkileşim) = %.4f",
                      anova(model2)["Strateji:OSS_Centered", "Pr(>F)"])
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

dev.off()
cat("Moderasyon grafiği kaydedildi: MODERASYON_Strateji_OSS.png\n\n")

# 10. SIMPLE SLOPES GRAFİĞİ
# ===================================================================

cat("=== SIMPLE SLOPES GRAFİĞİ ===\n")

# Her OSS seviyesi için strateji farkını hesapla
simple_slopes_data <- data.frame()

for(oss_level in c(-oss_sd, 0, oss_sd)) {
  for(strateji in c("İnkar", "Özür")) {
    pred_data <- data.frame(
      Strateji = strateji,
      OSS_Centered = oss_level,
      Time_Bin_Numeric = mean(temporal_trial$Time_Bin_Numeric),
      Katilimci_Clean = NA
    )

    pred_value <- predict(model2, newdata = pred_data, re.form = NA)

    simple_slopes_data <- rbind(
      simple_slopes_data,
      data.frame(
        OSS_Level = oss_level,
        Strateji = strateji,
        GSR_Z_Pred = pred_value
      )
    )
  }
}

# OSS label ekle
simple_slopes_data <- simple_slopes_data %>%
  mutate(
    OSS_Label = case_when(
      OSS_Level < -0.5 ~ "Düşük Linç Eğilimi",
      OSS_Level > 0.5 ~ "Yüksek Linç Eğilimi",
      TRUE ~ "Ortalama Linç Eğilimi"
    ),
    OSS_Label = factor(OSS_Label,
                       levels = c("Düşük Linç Eğilimi",
                                  "Ortalama Linç Eğilimi",
                                  "Yüksek Linç Eğilimi"))
  )

png("SIMPLE_SLOPES_Strateji_OSS.png", width = 1000, height = 600, res = 100)

ggplot(simple_slopes_data, aes(x = Strateji, y = GSR_Z_Pred,
                                fill = Strateji, group = OSS_Label)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7, alpha = 0.8) +
  facet_wrap(~ OSS_Label, ncol = 3) +
  scale_fill_manual(
    values = c("İnkar" = "#E63946", "Özür" = "#06A77D")
  ) +
  labs(
    title = "Simple Slopes: Farklı Linç Eğilimi Seviyelerinde Strateji Etkisi",
    subtitle = sprintf("LMM Trial-Level Analiz (n=%d katılımcı, %d gözlem)",
                       n_distinct(temporal_trial$Katilimci_Clean),
                       nrow(temporal_trial)),
    x = "İletişim Stratejisi",
    y = "GSR Z-Skoru (Tahmin Edilen)",
    fill = "Strateji",
    caption = sprintf("Model: GSR_Z ~ Strateji × OSS + Zaman + (1|Katılımcı)\np(etkileşim) = %.4f",
                      anova(model2)["Strateji:OSS_Centered", "Pr(>F)"])
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    strip.text = element_text(face = "bold", size = 12),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

dev.off()
cat("Simple slopes grafiği kaydedildi: SIMPLE_SLOPES_Strateji_OSS.png\n\n")

# 11. SONUÇLARI KAYDET
# ===================================================================

cat("=== SONUÇLARI KAYDETME ===\n")

# Trial-level veriyi kaydet
write.csv(temporal_trial, "temporal_trial_oss.csv", row.names = FALSE, fileEncoding = "UTF-8")
cat("Trial-level veri kaydedildi: temporal_trial_oss.csv\n")

# Model sonuçlarını kaydet
sink("moderasyon_model_sonuclari.txt")
cat("MODERASYON ANALİZİ SONUÇLARI\n")
cat("=" %+% paste(rep("=", 50), collapse = "") %+% "\n\n")
cat("Tarih:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("Katılımcı sayısı:", n_distinct(temporal_trial$Katilimci_Clean), "\n")
cat("Toplam gözlem sayısı:", nrow(temporal_trial), "\n\n")

cat("MODEL 1: ANA ETKİLER\n")
cat(paste(rep("-", 50), collapse = ""), "\n")
print(summary(model1))
cat("\n")
print(anova(model1))
cat("\n\n")

cat("MODEL 2: MODERASYON (Strateji × OSS)\n")
cat(paste(rep("-", 50), collapse = ""), "\n")
print(summary(model2))
cat("\n")
print(anova(model2))
cat("\n\n")

cat("MODEL KARŞILAŞTIRMA\n")
cat(paste(rep("-", 50), collapse = ""), "\n")
print(model_comparison)
cat("\n\n")

cat("SIMPLE SLOPES ANALİZİ\n")
cat(paste(rep("-", 50), collapse = ""), "\n")
print(summary(emm_simple))
cat("\n\n")
print(summary(emm_interaction))

sink()
cat("Model sonuçları kaydedildi: moderasyon_model_sonuclari.txt\n\n")

cat("=== ANALİZ TAMAMLANDI! ===\n")
cat("Oluşturulan dosyalar:\n")
cat("1. temporal_trial_oss.csv - Trial-level veri\n")
cat("2. MODERASYON_Strateji_OSS.png - Moderasyon grafiği\n")
cat("3. SIMPLE_SLOPES_Strateji_OSS.png - Simple slopes grafiği\n")
cat("4. moderasyon_model_sonuclari.txt - Model sonuçları\n")
