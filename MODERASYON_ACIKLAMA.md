# MODERASYON ANALİZİ - Trial-Level LMM

## Amaç
OSS (Online Social Shaming - Linç Eğilimi) ölçeğinin, İnkar vs Özür stratejilerine verilen GSR tepkilerini modere edip etmediğini test etmek.

## Metodoloji

### 1. Aggregasyon Bias Sorunu
**ÖNCEKİ YAKLAŞIM (YANLIŞ):**
- Her katılımcı için ortalama al → 13 katılımcı × 2 strateji = 26 gözlem
- **Sonuç:** p = 0.778 (anlamlı değil)
- **Problem:** Aggregasyon bias + isim eşleşme hatası

**YENİ YAKLAŞIM (DOĞRU):**
- Trial-level veri: Her katılımcı × strateji × zaman dilimi
- ~27 katılımcı × 2 strateji × 30 zaman dilimi = ~1,620 gözlem
- **Avantaj:** Maksimum istatistiksel güç, aggregasyon bias yok

### 2. Veri Kaynakları
- **GSR verisi:** `ilkinkar_gsr_zscore.csv` (520,933 satır)
  - 27 katılımcı, İlk İnkar görenler
  - İnkar ve Özür stratejileri için GSR Z-skorları
  - 30 saniye süre, ~512 Hz örnekleme

- **OSS verisi:** `oss_inkar_ters_kodlu.csv` (27 katılımcı)
  - Madde 4 ve 7 ters kodlanmış
  - Toplam OSS skoru (Linç Eğilimi)

### 3. Analiz Adımları

#### A. Veri Hazırlığı
1. GSR ve OSS verilerini yükle
2. İsim eşleştirme sorununu çöz (manuel eşleştirme sözlüğü)
3. Baseline hesapla (ilk 3 saniye, 0-3000 ms)
4. Z-skorları hesapla: `Z = (GSR_t - μ_baseline) / σ_baseline`
5. 30 saniyeyi 1 saniyelik dilimlere böl (30 time bin)

#### B. Trial-Level Aggregasyon
Her **Katılımcı × Strateji × Time Bin** kombinasyonu için ortalama GSR_Z:
- ~27 katılımcı × 2 strateji × 30 time bin = ~1,620 satır
- Her satır bir "trial" (gözlem birimi)

#### C. LMM Modelleri

**Model 1: Ana Etkiler**
```r
GSR_Z ~ Strateji + OSS_Centered + Time_Bin + (1|Katılımcı)
```
- Strateji ana etkisi
- OSS ana etkisi
- Zaman kontrolü

**Model 2: Moderasyon**
```r
GSR_Z ~ Strateji × OSS_Centered + Time_Bin + (1|Katılımcı)
```
- **Kritik:** `Strateji × OSS_Centered` etkileşimi
- Bu etkileşim anlamlıysa → OSS modere ediyor demektir

#### D. Model Karşılaştırma
```r
anova(model1, model2)
```
- Etkileşim terimi modeli anlamlı şekilde iyileştiriyor mu?
- Chi-square test, p < 0.05 → Moderasyon var

### 4. Simple Slopes Analizi
OSS'nin farklı seviyelerinde (-1 SD, ortalama, +1 SD) strateji etkisini test et:

- **Düşük Linç Eğilimi (-1 SD):** İnkar vs Özür farkı var mı?
- **Ortalama Linç Eğilimi (0):** İnkar vs Özür farkı var mı?
- **Yüksek Linç Eğilimi (+1 SD):** İnkar vs Özür farkı var mı?

**İnterpretasyon:**
- Eğer sadece yüksek OSS'de fark varsa → "Sadece yüksek linç eğilimi olanlarda İnkar daha olumsuz tepki oluşturuyor"
- Eğer sadece düşük OSS'de fark varsa → "Düşük linç eğilimi olanlarda stratejiler arasında fark var"

### 5. Grafikler

#### A. Moderasyon Grafiği (`MODERASYON_Strateji_OSS.png`)
- X ekseni: Strateji (İnkar vs Özür)
- Y ekseni: Tahmin edilen GSR Z-skoru
- Renkli çizgiler: Farklı OSS seviyeleri (Düşük, Orta, Yüksek)
- **Yorumlama:** Çizgiler paralel değilse → Moderasyon var

#### B. Simple Slopes Grafiği (`SIMPLE_SLOPES_Strateji_OSS.png`)
- 3 panel: Düşük / Orta / Yüksek Linç Eğilimi
- Her panelde İnkar vs Özür barları
- **Yorumlama:** Hangi panelde fark daha büyük?

### 6. Çıktı Dosyaları

1. **temporal_trial_oss.csv**
   - Trial-level veri (~1,620 satır)
   - Sütunlar: Katılımcı, Strateji, Time_Bin, OSS_Toplam, GSR_Z, OSS_Centered

2. **MODERASYON_Strateji_OSS.png**
   - Etkileşim grafiği
   - Publication-ready

3. **SIMPLE_SLOPES_Strateji_OSS.png**
   - Simple slopes bar charts
   - 3 panel layout

4. **moderasyon_model_sonuclari.txt**
   - Tüm model sonuçları
   - ANOVA tabloları
   - Simple slopes testleri

## Beklenen Sonuçlar

### Hipotez 1: Moderasyon Var
- `Strateji × OSS` etkileşimi anlamlı (p < 0.05)
- Yüksek OSS'de İnkar-Özür farkı daha büyük
- **Yorum:** "Linç eğilimi yüksek olanlar, İnkar stratejisine daha olumsuz tepki veriyor"

### Hipotez 2: Moderasyon Yok
- `Strateji × OSS` etkileşimi anlamsız (p > 0.05)
- Tüm OSS seviyelerinde İnkar-Özür farkı benzer
- **Yorum:** "Linç eğilimi, stratejilere verilen tepkiyi etkilemiyor"

## Avantajlar (Trial-Level LMM)

1. **Maksimum İstatistiksel Güç**
   - 26 gözlem → 1,620 gözlem
   - Çok daha hassas tahminler

2. **Aggregasyon Bias Yok**
   - Her time bin ayrı analiz edilir
   - Temporal varyans korunur

3. **Proper Random Effects**
   - `(1|Katılımcı)`: Katılımcılar arası varyans kontrol edilir
   - Tekrarlı ölçümlerin korelasyonu dikkate alınır

4. **Zaman Kontrolü**
   - Time_Bin covariate olarak modelde
   - Temporal trend etkisi izole edilir

## R Kodu Çalıştırma

```r
# RStudio'da şu scripti çalıştırın:
source("moderasyon_analizi_trial_level.R")
```

**Gereksinimler:**
- `lme4` (LMM için)
- `lmerTest` (p-değerleri için)
- `emmeans` (simple slopes için)
- `dplyr`, `ggplot2`

## Sonuç Raporlama Örneği

> "Linear mixed model analizinde, OSS (Online Social Shaming) skorunun iletişim stratejisi etkisini modere ettiği bulunmuştur (F(1, 1560) = X.XX, p = 0.0XX). Simple slopes analizi, yüksek linç eğilimi olan katılımcılarda (+1 SD), İnkar stratejisinin Özür stratejisine kıyasla anlamlı şekilde daha yüksek GSR tepkisi oluşturduğunu göstermiştir (β = X.XX, p < 0.001). Ancak düşük linç eğilimi olan katılımcılarda (-1 SD), bu fark gözlenmemiştir (β = X.XX, p = 0.XX). Bu bulgular, bireysel linç eğiliminin, kriz iletişim stratejilerine verilen fizyolojik tepkileri şekillendirdiğini göstermektedir."

---

**Not:** Önceki aggregated analiz 13 katılımcı ve p=0.778 bulmuştu. Trial-level analiz daha güçlü ve doğru bir test sağlayacaktır.
