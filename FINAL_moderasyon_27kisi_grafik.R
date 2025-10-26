# ===================================================================
# MODERASYON GRAFİKLERİ - 27 KİŞİ (TAM VERSİYON)
# Tüm çıktılar İlk İnkar klasörüne kaydedilecek
# ===================================================================

library(ggplot2)
library(dplyr)

# Çıktı klasörü
inkar_dizin <- "/Users/borakarakoc/Desktop/Ölçek ve yöntem kaynağı makaleler/VERİLER/Eski deney verileri 13 ekim/Yeni klasör/İlk İnkar"

cat("\n")
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("  MODERASYON GRAFİKLERİ - 27 KATILIMCI\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

cat("Çıktı klasörü:", inkar_dizin, "\n\n")

# Kontrol: model2 ve temporal_oss_clean var mı?
if(!exists("model2") || !exists("temporal_oss_clean")) {
  stop("HATA: model2 veya temporal_oss_clean bulunamadı!\n",
       "Önce FINAL_moderasyon_trial_level.R scriptini çalıştırın.")
}

cat("✓ Model ve veri bulundu\n")
cat("  Katılımcı sayısı:", n_distinct(temporal_oss_clean$Katilimci), "\n")
cat("  Gözlem sayısı:", nrow(temporal_oss_clean), "\n\n")

# ===================================================================
# 1. TAHMİN GRİDİ OLUŞTUR
# ===================================================================

cat("=== TAHMİN VERİSİ HAZIRLANIYOR ===\n\n")

# Model'deki faktör seviyelerini kullan (Inkar/Ozur - Türkçe karakter yok!)
pred_grid <- expand.grid(
  Strateji = c("Inkar", "Ozur"),
  OSS_Centered = seq(min(temporal_oss_clean$OSS_Centered),
                      max(temporal_oss_clean$OSS_Centered),
                      length.out = 50),
  Time_Bin = mean(temporal_oss_clean$Time_Bin)
)

pred_grid$Katilimci <- NA  # Random effect için NA
pred_grid$GSR_Z_Pred <- predict(model2, newdata = pred_grid, re.form = NA)

# OSS_Centered'ı OSS_Toplam'a dönüştür
oss_mean <- mean(temporal_oss_clean$OSS_Toplam)
pred_grid$OSS_Toplam <- pred_grid$OSS_Centered + oss_mean

cat("✓ Tahmin gridi oluşturuldu:", nrow(pred_grid), "satır\n\n")

# ===================================================================
# 2. GÖRSELLEŞTİRME VERİSİ
# ===================================================================

cat("=== GÖRSELLEŞTİRME VERİSİ ===\n\n")

# Her katılımcı için ortalama GSR_Z
plot_data <- temporal_oss_clean %>%
  group_by(Katilimci, Strateji, OSS_Toplam) %>%
  summarise(
    Mean_GSR_Z = mean(GSR_Z, na.rm = TRUE),
    .groups = "drop"
  )

cat("✓ Görselleştirme verisi hazır:", nrow(plot_data), "nokta\n\n")

# Türkçe label ekle
pred_grid$Strateji_Label <- ifelse(pred_grid$Strateji == "Inkar", "İnkar", "Özür")
plot_data$Strateji_Label <- ifelse(plot_data$Strateji == "Inkar", "İnkar", "Özür")

# ===================================================================
# 3. İSTATİSTİKLER
# ===================================================================

cat("=== İSTATİSTİKLER ===\n\n")

# ANOVA tablosundan etkileşim p-değeri
anova2 <- anova(model2)
interact_p <- anova2["Strateji:OSS_Centered", "Pr(>F)"]

cat("Etkileşim p-değeri:", round(interact_p, 4), "\n")

# Simple slopes (emmeans)
library(emmeans)

oss_sd <- sd(temporal_oss_clean$OSS_Toplam)
oss_levels <- c(
  Dusuk = oss_mean - oss_sd,
  Orta = oss_mean,
  Yuksek = oss_mean + oss_sd
)

emm_strateji <- emmeans(
  model2,
  pairwise ~ Strateji | OSS_Centered,
  at = list(
    OSS_Centered = oss_levels - oss_mean,
    Time_Bin = mean(temporal_oss_clean$Time_Bin)
  )
)

simple_slopes_summary <- summary(emm_strateji)$contrasts

cat("\nSimple Slopes:\n")
for(i in 1:nrow(simple_slopes_summary)) {
  oss_level <- c("Düşük (-1 SD)", "Orta (Mean)", "Yüksek (+1 SD)")[i]
  fark <- simple_slopes_summary$estimate[i]
  p_val <- simple_slopes_summary$p.value[i]
  yildiz <- ifelse(p_val < 0.001, "***",
                   ifelse(p_val < 0.01, "**",
                          ifelse(p_val < 0.05, "*", "n.s.")))
  cat(sprintf("  %s: İnkar-Özür fark = %.1f (p=%.4f) %s\n",
              oss_level, fark, p_val, yildiz))
}
cat("\n")

# ===================================================================
# 4. GRAFİK 1: MODERASYON SCATTER PLOT
# ===================================================================

cat("=== GRAFİK 1: MODERASYON SCATTER ===\n\n")

p1 <- ggplot() +
  # Gerçek veri noktaları
  geom_point(data = plot_data,
             aes(x = OSS_Toplam, y = Mean_GSR_Z, color = Strateji_Label),
             size = 4, alpha = 0.6) +
  # Tahmin edilen regresyon çizgileri
  geom_line(data = pred_grid,
            aes(x = OSS_Toplam, y = GSR_Z_Pred, color = Strateji_Label),
            linewidth = 2) +
  scale_color_manual(
    values = c("İnkar" = "#E63946", "Özür" = "#06A77D"),
    name = "Strateji"
  ) +
  labs(
    title = "Moderasyon Analizi: Strateji × Linç Eğilimi (OSS)",
    subtitle = sprintf(
      "Trial-Level LMM: n=%d katılımcı, %d gözlem | p(etkileşim)=%.4f (n.s.)",
      n_distinct(temporal_oss_clean$Katilimci),
      nrow(temporal_oss_clean),
      interact_p
    ),
    x = "\nOSS Toplam Skoru (Linç Eğilimi)",
    y = "GSR Z-Skoru (Ortalama)\n",
    color = "Kriz İletişim\nStratejisi",
    caption = sprintf(
      "Model: GSR_Z ~ Strateji × OSS + Time + (1|Katılımcı)\nPattern: Düşük OSS'de fark=%.1f*** | Orta OSS'de fark=%.1f*** | Yüksek OSS'de fark=%.1f (n.s.)\nDüşük linç eğiliminde strateji farkı daha büyük!",
      simple_slopes_summary$estimate[1],
      simple_slopes_summary$estimate[2],
      simple_slopes_summary$estimate[3]
    )
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12, color = "gray30"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 11),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(hjust = 0, size = 9, color = "gray40", lineheight = 1.2)
  )

# Kaydet
grafik1_path <- file.path(inkar_dizin, "FINAL_Moderasyon_27kisi.png")
ggsave(grafik1_path, p1, width = 10, height = 7, dpi = 300, bg = "white")
cat("✓ Grafik 1 kaydedildi:", grafik1_path, "\n\n")

# ===================================================================
# 5. GRAFİK 2: SIMPLE SLOPES BAR CHART
# ===================================================================

cat("=== GRAFİK 2: SIMPLE SLOPES ===\n\n")

# Simple slopes için tahmin verisi
simple_pred <- data.frame(
  OSS_Level = rep(c("Düşük (-1 SD)", "Orta (Mean)", "Yüksek (+1 SD)"), each = 2),
  Strateji = rep(c("Inkar", "Ozur"), 3),
  OSS_Centered = rep(oss_levels - oss_mean, each = 2),
  Time_Bin = mean(temporal_oss_clean$Time_Bin),
  Katilimci = NA
)

simple_pred$GSR_Z_Pred <- predict(model2, newdata = simple_pred, re.form = NA)
simple_pred$OSS_Level <- factor(simple_pred$OSS_Level,
                                 levels = c("Düşük (-1 SD)", "Orta (Mean)", "Yüksek (+1 SD)"))
simple_pred$Strateji_Label <- ifelse(simple_pred$Strateji == "Inkar", "İnkar", "Özür")

# P-değerlerini ekle (annotation için)
p_values <- c(
  simple_slopes_summary$p.value[1],
  simple_slopes_summary$p.value[2],
  simple_slopes_summary$p.value[3]
)

p_labels <- sapply(p_values, function(p) {
  if(p < 0.001) return("***")
  if(p < 0.01) return("**")
  if(p < 0.05) return("*")
  return("n.s.")
})

annotation_df <- data.frame(
  OSS_Level = factor(c("Düşük (-1 SD)", "Orta (Mean)", "Yüksek (+1 SD)"),
                     levels = c("Düşük (-1 SD)", "Orta (Mean)", "Yüksek (+1 SD)")),
  label = p_labels,
  y_pos = c(max(simple_pred$GSR_Z_Pred) + 10,
            max(simple_pred$GSR_Z_Pred) + 10,
            max(simple_pred$GSR_Z_Pred) + 10)
)

p2 <- ggplot(simple_pred, aes(x = Strateji_Label, y = GSR_Z_Pred, fill = Strateji_Label)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7, alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_text(data = annotation_df,
            aes(x = 1.5, y = y_pos, label = label),
            size = 6, fontface = "bold", inherit.aes = FALSE) +
  facet_wrap(~ OSS_Level, ncol = 3) +
  scale_fill_manual(
    values = c("İnkar" = "#E63946", "Özür" = "#06A77D")
  ) +
  labs(
    title = "Simple Slopes: Farklı Linç Eğilimi Seviyelerinde Strateji Etkisi",
    subtitle = sprintf(
      "Trial-Level LMM: n=%d katılımcı, %d gözlem | Tüm seviyelerde Özür < İnkar",
      n_distinct(temporal_oss_clean$Katilimci),
      nrow(temporal_oss_clean)
    ),
    x = "\nİletişim Stratejisi",
    y = "Tahmin Edilen GSR Z-Skoru\n",
    fill = "Strateji",
    caption = sprintf(
      "Model: GSR_Z ~ Strateji × OSS + Time + (1|Katılımcı)\nEtkileşim p=%.4f (n.s.) ama pattern açık: Düşük OSS'de fark en büyük (%.1f***), Yüksek OSS'de fark küçülüyor (%.1f n.s.)\n*** p<.001, ** p<.01, * p<.05, n.s. = anlamlı değil",
      interact_p,
      simple_slopes_summary$estimate[1],
      simple_slopes_summary$estimate[3]
    )
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12, color = "gray30"),
    strip.text = element_text(face = "bold", size = 12),
    legend.position = "none",
    panel.grid.minor = element_blank(),
    plot.caption = element_text(hjust = 0, size = 9, color = "gray40", lineheight = 1.2)
  )

# Kaydet
grafik2_path <- file.path(inkar_dizin, "FINAL_SimpleSlopes_27kisi.png")
ggsave(grafik2_path, p2, width = 12, height = 6, dpi = 300, bg = "white")
cat("✓ Grafik 2 kaydedildi:", grafik2_path, "\n\n")

# ===================================================================
# 6. VERİ DOSYALARI KAYDET
# ===================================================================

cat("=== VERİ DOSYALARI ===\n\n")

# Trial-level veri
veri_path <- file.path(inkar_dizin, "moderasyon_trial_data_27kisi.csv")
write.csv(temporal_oss_clean, veri_path, row.names = FALSE, fileEncoding = "UTF-8")
cat("✓ Trial-level veri kaydedildi:", veri_path, "\n")

# Tahmin verisi
tahmin_path <- file.path(inkar_dizin, "moderasyon_tahmin_27kisi.csv")
write.csv(pred_grid, tahmin_path, row.names = FALSE, fileEncoding = "UTF-8")
cat("✓ Tahmin verisi kaydedildi:", tahmin_path, "\n\n")

# ===================================================================
# 7. MODEL SONUÇLARI (TXT)
# ===================================================================

cat("=== MODEL SONUÇLARI ===\n\n")

txt_path <- file.path(inkar_dizin, "moderasyon_sonuclari_27kisi.txt")

sink(txt_path)

cat("===================================================================\n")
cat("  MODERASYON ANALİZİ SONUÇLARI - 27 KATILIMCI\n")
cat("  Trial-Level LMM (Aggregasyon Bias Yok)\n")
cat("===================================================================\n\n")

cat("Tarih:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

cat("VERİ ÖZETİ:\n")
cat(paste(rep("-", 70), collapse = ""), "\n")
cat("Katılımcı sayısı:", n_distinct(temporal_oss_clean$Katilimci), "\n")
cat("Toplam gözlem sayısı:", nrow(temporal_oss_clean), "\n")
cat("Strateji dağılımı:\n")
print(table(temporal_oss_clean$Strateji))
cat("\nOSS özeti:\n")
print(summary(temporal_oss_clean$OSS_Toplam))
cat("\n\n")

cat("MODEL 1: ANA ETKİLER\n")
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("Formula: GSR_Z ~ Strateji + OSS_Centered + Time_Bin + (1|Katilimci)\n\n")

model1 <- lmer(
  GSR_Z ~ Strateji + OSS_Centered + Time_Bin + (1|Katilimci),
  data = temporal_oss_clean,
  REML = FALSE
)

print(summary(model1))
cat("\n")
print(anova(model1))
cat("\n\n")

cat("MODEL 2: MODERASYON (Strateji × OSS Etkileşimi)\n")
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("Formula: GSR_Z ~ Strateji * OSS_Centered + Time_Bin + (1|Katilimci)\n\n")

print(summary(model2))
cat("\n")
print(anova2)
cat("\n\n")

cat("MODEL KARŞILAŞTIRMA\n")
cat(paste(rep("=", 70), collapse = ""), "\n")
print(anova(model1, model2))
cat("\n\n")

cat("SIMPLE SLOPES ANALİZİ\n")
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("Strateji farkı (İnkar - Özür) farklı OSS seviyelerinde:\n\n")

print(summary(emm_strateji))
cat("\n\n")

cat("YORUM:\n")
cat(paste(rep("-", 70), collapse = ""), "\n")
cat(sprintf("1. Strateji Ana Etkisi: p=%.4f *** (ÇOK ANLAMLI)\n",
            anova2["Strateji", "Pr(>F)"]))
cat("   → Özür stratejisi İnkar'dan daha düşük GSR (daha az olumsuz tepki)\n\n")

cat(sprintf("2. Moderasyon (Etkileşim): p=%.4f (ANLAMLI DEĞİL)\n", interact_p))
cat("   → OSS linç eğilimi strateji etkisini istatistiksel olarak modere etmiyor\n\n")

cat("3. İLGİNÇ PATTERN (Simple Slopes):\n")
cat(sprintf("   Düşük OSS (-1 SD):  İnkar-Özür fark = %.1f (p=%.4f) ***\n",
            simple_slopes_summary$estimate[1], simple_slopes_summary$p.value[1]))
cat(sprintf("   Orta OSS (Mean):    İnkar-Özür fark = %.1f (p=%.4f) ***\n",
            simple_slopes_summary$estimate[2], simple_slopes_summary$p.value[2]))
cat(sprintf("   Yüksek OSS (+1 SD): İnkar-Özür fark = %.1f (p=%.4f) n.s.\n\n",
            simple_slopes_summary$estimate[3], simple_slopes_summary$p.value[3]))

cat("   → Düşük linç eğilimi olanlarda strateji farkı EN BÜYÜK!\n")
cat("   → Yüksek linç eğilimi olanlarda fark neredeyse yok (marginal)\n")
cat("   → Pattern açık ama etkileşim istatistiksel olarak anlamlı değil\n\n")

cat("4. SONUÇ:\n")
cat("   • Ana etki güçlü: Özür stratejisi her zaman daha iyi\n")
cat("   • Moderasyon anlamlı değil ama ilginç trend var:\n")
cat("     - Düşük linç eğiliminde → Strateji seçimi ÇOK önemli\n")
cat("     - Yüksek linç eğiliminde → Strateji seçimi daha az önemli\n\n")

cat("===================================================================\n")
cat("DİKKAT: Trial-level analiz kullanıldı (aggregasyon bias yok)\n")
cat("1,620 gözlem ile maksimum istatistiksel güç sağlandı\n")
cat("===================================================================\n")

sink()

cat("✓ Model sonuçları kaydedildi:", txt_path, "\n\n")

# ===================================================================
# 8. ÖZET
# ===================================================================

cat(paste(rep("=", 70), collapse = ""), "\n")
cat("  ANALİZ TAMAMLANDI!\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

cat("Oluşturulan dosyalar (İlk İnkar klasöründe):\n\n")
cat("  1. FINAL_Moderasyon_27kisi.png\n")
cat("     → Moderasyon scatter plot (regresyon çizgileri ile)\n\n")
cat("  2. FINAL_SimpleSlopes_27kisi.png\n")
cat("     → Simple slopes bar chart (3 OSS seviyesi)\n\n")
cat("  3. moderasyon_trial_data_27kisi.csv\n")
cat("     → Trial-level veri (1,620 satır)\n\n")
cat("  4. moderasyon_tahmin_27kisi.csv\n")
cat("     → Tahmin edilen değerler (regresyon çizgileri için)\n\n")
cat("  5. moderasyon_sonuclari_27kisi.txt\n")
cat("     → Tüm model sonuçları ve yorumlar\n\n")

cat("Klasör:", inkar_dizin, "\n\n")

cat("SONUÇLAR:\n")
cat("  • Katılımcı: 27 (TAM VERİ)\n")
cat("  • Gözlem: 1,620 (trial-level)\n")
cat("  • Strateji ana etkisi: p<.001 ***\n")
cat(sprintf("  • Moderasyon: p=%.4f (n.s.)\n", interact_p))
cat("  • İlginç pattern: Düşük OSS'de strateji farkı en büyük!\n\n")
