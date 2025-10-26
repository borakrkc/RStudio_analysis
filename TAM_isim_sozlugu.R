# TAM İSİM EŞLEŞTİRME SÖZLÜĞÜ - 27 KATILIMCI
# ================================================
# Her iki veri setinde de 27 katılımcı var!
# Sadece Türkçe karakter farkları ve eksik soyadlar var

cat("\n=== TAM MANUEL EŞLEŞTİRME SÖZLÜĞÜ ===\n\n")

# DOĞRUDAN EŞLEŞENLER (13 katılımcı)
dogrudan_eslesenler <- c(
  "İREM EROL",
  "SATINUR BULUT",
  "HAMS EISSA",
  "BORA KOCAMAZ",
  "ENES YÜKSEK",
  "ÖMER HASTÜRK",
  "HAKAN KAYITMAZ",
  "BERAT ATMACA",
  "ORKUN KOZANOĞLU",
  "GÖKTUĞ TALHA COŞKUNSOY",
  "ERKAM KARA",
  "NUH ENES SAKALLI",
  "SUDENAZ KILIÇ"
)

cat("Doğrudan eşleşenler:", length(dogrudan_eslesenler), "\n")

# MANUEL EŞLEŞTİRME GEREKLİ (14 katılımcı)
isim_eslestirme_tam <- data.frame(
  OSS_Isim = c(
    # Türkçe karakter farkları (İ → I)
    "AYŞENUR ÖZISKENDER",      # İ → I
    "SÜMEYYE DILMAÇ",          # İ → I
    "BERRAK FAIZOĞLU",         # İ → I
    "ESRA YIĞITER",            # İ → I
    "IREM ADAR",               # İ → I
    "SENA ÇETIN",              # İ → I
    "ECRIN BALABAN",           # İ → I
    "KERIM BULAGAY",           # İ → I
    "BERKAY KARADENIZ",        # İ → I

    # Eksik/fazla soyadlar
    "FATMA ZEHRA ŞIMŞEKLIEL",  # Soyadı farklı yazılmış
    "IRMAK",                   # Soyadı yok
    "AYŞE MÜBERRA EROL",       # İkinci isim fazla
    "CENK",                    # Soyadı yok

    # Ü → U farkı
    "TUBA ZEHRA ATEŞ"          # U → Ü
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

cat("Manuel eşleştirme gerekli:", nrow(isim_eslestirme_tam), "\n\n")

cat("TOPLAM EŞLEŞTİRME:\n")
cat("  Doğrudan: ", length(dogrudan_eslesenler), "\n")
cat("  Manuel:   ", nrow(isim_eslestirme_tam), "\n")
cat("  TOPLAM:   ", length(dogrudan_eslesenler) + nrow(isim_eslestirme_tam), " / 27 ✓\n\n")

cat("Manuel eşleştirme sözlüğü:\n")
cat(sprintf("%-30s → %s\n", "OSS İsmi", "Temporal İsmi"))
cat(paste(rep("-", 65), collapse = ""), "\n")
for(i in 1:nrow(isim_eslestirme_tam)) {
  cat(sprintf("%-30s → %s\n",
              isim_eslestirme_tam$OSS_Isim[i],
              isim_eslestirme_tam$Temporal_Isim[i]))
}

cat("\n")
cat("Bu sözlüğü kullanarak 27 katılımcının HEPSİ eşleştirilecek!\n")
