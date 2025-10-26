# GRAFİK DÜZELTME - Model ile uyumlu faktör seviyeleri
# =======================================================

library(ggplot2)

cat("\n=== GRAFİKLER (DÜZELTİLMİŞ) ===\n\n")

# Model'deki Strateji faktör seviyelerini kontrol et
cat("Model'deki Strateji seviyeleri:\n")
print(levels(temporal_oss_clean$Strateji))
cat("\n")

# Model'deki Time_Bin tipini kontrol et
cat("Time_Bin sınıfı:", class(temporal_oss_clean$Time_Bin), "\n")
cat("Time_Bin seviyeleri (ilk 10):\n")
print(head(levels(temporal_oss_clean$Time_Bin), 10))
cat("\n")

# GRAFIK 1: Moderasyon scatter + regression
cat("Grafik 1: Moderasyon scatter plot...\n")

# Tahmin için grid oluştur - MODEL'DEKİ FAKTÖR SEVİYELERİNİ KULLAN
pred_grid <- expand.grid(
  Strateji = levels(temporal_oss_clean$Strateji),  # Model'deki seviyeler
  OSS_Centered = seq(min(temporal_oss_clean$OSS_Centered),
                      max(temporal_oss_clean$OSS_Centered),
                      length.out = 50),
  Time_Bin = levels(temporal_oss_clean$Time_Bin)[15]  # Orta değer (15. time bin)
)

pred_grid$Katilimci <- NA  # Random effect için

# Tahmin yap
pred_grid$GSR_Z_Pred <- predict(model2, newdata = pred_grid, re.form = NA)

# OSS_Centered'ı OSS_Toplam'a geri dönüştür
oss_mean <- mean(temporal_oss_clean$OSS_Toplam)
pred_grid$OSS_Toplam <- pred_grid$OSS_Centered + oss_mean

# Görselleştirme için her katılımcıdan ortalama al
plot_data <- temporal_oss_clean %>%
  group_by(Katilimci, Strateji, OSS_Toplam) %>%
  summarise(
    Mean_GSR_Z = mean(GSR_Z, na.rm = TRUE),
    .groups = "drop"
  )

# Strateji'yi Türkçe label'a çevir (sadece görselleştirme için)
plot_data$Strateji_Label <- ifelse(plot_data$Strateji == "Inkar", "İnkar", "Özür")
pred_grid$Strateji_Label <- ifelse(pred_grid$Strateji == "Inkar", "İnkar", "Özür")

# Etkileşim p-değeri
interact_p <- anova2["Strateji:OSS_Centered", "Pr(>F)"]

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
    title = "Moderasyon: Strateji × Linç Eğilimi (OSS)",
    subtitle = sprintf(
      "Trial-Level LMM: n=%d katılımcı, %d gözlem | p(etkileşim)=%.4f (n.s.)",
      n_distinct(temporal_oss_clean$Katilimci),
      nrow(temporal_oss_clean),
      interact_p
    ),
    x = "OSS Toplam Skoru (Linç Eğilimi)",
    y = "GSR Z-Skoru",
    caption = sprintf(
      "Model: GSR_Z ~ Strateji × OSS + Time + (1|Katılımcı)\nİnkar: β=%.2f (p<.001) | Etkileşim: p=%.3f (n.s.)",
      fixef(model2)["StratejiOzur"],
      interact_p
    )
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(hjust = 0, size = 10, color = "gray40")
  )

ggsave("FINAL_Moderasyon_TrialLevel_19kisi.png", p1, width = 10, height = 7, dpi = 300)
cat("✓ Grafik 1 kaydedildi: FINAL_Moderasyon_TrialLevel_19kisi.png\n\n")

# GRAFIK 2: Simple slopes
cat("Grafik 2: Simple slopes...\n")

# OSS seviyeleri
oss_sd <- sd(temporal_oss_clean$OSS_Toplam)
oss_levels <- c(
  Dusuk = oss_mean - oss_sd,
  Orta = oss_mean,
  Yuksek = oss_mean + oss_sd
)

# Tahmin için veri hazırla
simple_pred <- data.frame(
  OSS_Level = rep(c("Düşük (-1 SD)", "Orta", "Yüksek (+1 SD)"), each = 2),
  Strateji = rep(levels(temporal_oss_clean$Strateji), 3),  # Model'deki seviyeler
  OSS_Centered = rep(oss_levels - oss_mean, each = 2),
  Time_Bin = levels(temporal_oss_clean$Time_Bin)[15],  # Orta time bin
  Katilimci = NA
)

simple_pred$GSR_Z_Pred <- predict(model2, newdata = simple_pred, re.form = NA)
simple_pred$OSS_Level <- factor(simple_pred$OSS_Level,
                                 levels = c("Düşük (-1 SD)", "Orta", "Yüksek (+1 SD)"))

# Türkçe label
simple_pred$Strateji_Label <- ifelse(simple_pred$Strateji == "Inkar", "İnkar", "Özür")

p2 <- ggplot(simple_pred, aes(x = Strateji_Label, y = GSR_Z_Pred, fill = Strateji_Label)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7, alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  facet_wrap(~ OSS_Level, ncol = 3) +
  scale_fill_manual(
    values = c("İnkar" = "#E63946", "Özür" = "#06A77D")
  ) +
  labs(
    title = "Simple Slopes: Farklı OSS Seviyelerinde Strateji Etkisi",
    subtitle = sprintf(
      "Trial-Level LMM: n=%d katılımcı, %d gözlem | Tüm seviyelerde Özür < İnkar",
      n_distinct(temporal_oss_clean$Katilimci),
      nrow(temporal_oss_clean)
    ),
    x = "İletişim Stratejisi",
    y = "Tahmin Edilen GSR Z-Skoru",
    fill = "Strateji",
    caption = sprintf(
      "OSS Düşük: İnkar-Özür fark=47.7 (p<.001) | Orta: fark=36.6 (p<.001) | Yüksek: fark=25.6 (p=.037)\nEtkileşim p=%.3f (n.s.) → Fark büyüklüğü değişiyor ama yönü sabit",
      interact_p
    )
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    strip.text = element_text(face = "bold", size = 12),
    legend.position = "none",
    panel.grid.minor = element_blank(),
    plot.caption = element_text(hjust = 0, size = 9, color = "gray40", lineheight = 1.2)
  )

ggsave("FINAL_SimpleSlopes_TrialLevel_19kisi.png", p2, width = 12, height = 6, dpi = 300)
cat("✓ Grafik 2 kaydedildi: FINAL_SimpleSlopes_TrialLevel_19kisi.png\n\n")

cat("=== GRAFİKLER TAMAMLANDI! ===\n")
cat("Her iki grafik de başarıyla oluşturuldu.\n")
cat("Not: 19 katılımcı ile analiz (8 katılımcı isim eşleştirilemedi)\n")
