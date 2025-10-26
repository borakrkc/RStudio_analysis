# GSR Moderasyon Analizi - Hızlı Başlangıç

## Ne Yapılacak?
OSS (Linç Eğilimi) ölçeğinin, İnkar vs Özür stratejilerine verilen GSR tepkilerini **modere edip etmediğini** trial-level LMM ile test edeceğiz.

## Önemli Not: Aggregasyon Bias
⚠️ **Önceki analiz:** Aggregated data kullandı → 13 katılımcı, p=0.778 (anlamsız)

✅ **Bu analiz:** Trial-level data kullanıyor → ~27 katılımcı × 60 trial = ~1,620 gözlem

## Hızlı Çalıştırma

### RStudio'da:
```r
# Scripti çalıştır
source("moderasyon_analizi_trial_level.R")
```

### Terminal'de:
```bash
Rscript moderasyon_analizi_trial_level.R
```

## Gerekli Paketler
Script otomatik olarak şunları yükler:
- `lme4` - LMM için
- `lmerTest` - p-değerleri için
- `emmeans` - Simple slopes için
- `dplyr`, `ggplot2` - Veri ve grafik için

Eğer kurulu değilse:
```r
install.packages(c("lme4", "lmerTest", "emmeans", "dplyr", "ggplot2"))
```

## Girdiler (Input)
Script şu dosyaları otomatik okur:
1. `ilkinkar_gsr_zscore.csv` - GSR Z-skoru verileri (520,933 satır)
2. `oss_inkar_ters_kodlu.csv` - OSS ölçek skorları (27 katılımcı)

## Çıktılar (Output)

### 1. Veri Dosyası
- **temporal_trial_oss.csv**
  - Trial-level veri (~1,620 satır)
  - Her satır: Katılımcı × Strateji × Time_Bin

### 2. Grafikler
- **MODERASYON_Strateji_OSS.png**
  - Etkileşim grafiği
  - 3 renkli çizgi: Düşük/Orta/Yüksek OSS

- **SIMPLE_SLOPES_Strateji_OSS.png**
  - 3 panel bar chart
  - Her OSS seviyesinde İnkar vs Özür karşılaştırması

### 3. İstatistik Sonuçları
- **moderasyon_model_sonuclari.txt**
  - Model 1: Ana etkiler
  - Model 2: Moderasyon (Strateji × OSS)
  - Model karşılaştırma
  - Simple slopes testleri

## Sonuçları Nasıl Yorumlarım?

### 1. Console Output'a Bak
Script çalışırken şunları gösterecek:
- Kaç katılımcı eşleşti? (hedef: 27)
- Toplam kaç gözlem? (hedef: ~1,620)
- Model sonuçları ve p-değerleri

### 2. Kritik p-Değeri
`moderasyon_model_sonuclari.txt` dosyasında:
```
Strateji:OSS_Centered    Pr(>F)
```
- **p < 0.05** → Moderasyon VAR! 🎉
- **p > 0.05** → Moderasyon YOK

### 3. Grafiği İncele
**MODERASYON_Strateji_OSS.png:**
- Çizgiler **paralel değilse** → Moderasyon var
- Çizgiler **paralel** → Moderasyon yok

**SIMPLE_SLOPES_Strateji_OSS.png:**
- Hangi panelde İnkar-Özür farkı daha büyük?
- Yüksek OSS panelinde fark büyükse → "Yüksek linç eğilimi olanlarda İnkar daha olumsuz"

## Sorun Giderme

### Hata: "cannot open file"
→ Dosya yolları doğru mu kontrol et
→ Script içinde `/Users/borakarakoc/Desktop/...` yollarını değiştir

### Hata: "object not found"
→ Sütun isimleri değişmiş olabilir
→ CSV dosyalarını kontrol et

### Eşleşen Katılımcı 27'den Az
→ İsim eşleştirme sözlüğünü güncelle
→ Script'in "İSİM EŞLEŞTİRME" bölümüne bak

## Detaylı Açıklama
Daha fazla bilgi için:
```
MODERASYON_ACIKLAMA.md
```

## Önceki Analizlerle Fark

| Özellik | Önceki (Yanlış) | Şimdi (Doğru) |
|---------|----------------|---------------|
| Veri tipi | Aggregated means | Trial-level |
| Gözlem sayısı | 26 | ~1,620 |
| Katılımcı sayısı | 13 | 27 |
| Analiz | t-test / basic LMM | LMM + Simple slopes |
| Sonuç | p=0.778 | TBD |

---

**Hazır mısın?** RStudio'da `source("moderasyon_analizi_trial_level.R")` çalıştır! 🚀
