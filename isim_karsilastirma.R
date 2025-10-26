# Tüm isimleri karşılaştırma scripti
# =====================================

cat("\n=== TAM İSİM LİSTESİ KARŞILAŞTIRMA ===\n\n")

# OSS'deki TÜM isimler
cat("OSS'deki TÜM isimler (", length(unique(oss_veri$Isim_Clean)), "katılımcı):\n")
oss_isimler <- sort(unique(oss_veri$Isim_Clean))
for(i in seq_along(oss_isimler)) {
  cat(sprintf("%2d. %s\n", i, oss_isimler[i]))
}

cat("\n")

# Temporal'deki TÜM isimler
cat("Temporal'deki TÜM isimler (", length(unique(temporal_all_27$Isim_Clean)), "katılımcı):\n")
temporal_isimler <- sort(unique(temporal_all_27$Isim_Clean))
for(i in seq_along(temporal_isimler)) {
  cat(sprintf("%2d. %s\n", i, temporal_isimler[i]))
}

cat("\n")

# Eşleşmeyen isimleri göster
cat("=== EŞLEŞMEDİ ===\n\n")

# OSS'de olup Temporal'de olmayan
oss_olmayan <- setdiff(oss_isimler, temporal_isimler)
cat("OSS'de var ama Temporal'de YOK (", length(oss_olmayan), "):\n")
for(i in seq_along(oss_olmayan)) {
  cat(sprintf("%2d. %s\n", i, oss_olmayan[i]))
}

cat("\n")

# Temporal'de olup OSS'de olmayan
temporal_olmayan <- setdiff(temporal_isimler, oss_isimler)
cat("Temporal'de var ama OSS'de YOK (", length(temporal_olmayan), "):\n")
for(i in seq_along(temporal_olmayan)) {
  cat(sprintf("%2d. %s\n", i, temporal_olmayan[i]))
}

cat("\n")

# Benzerlik analizi (ilk kelimeye göre)
cat("=== BENZERLİK ANALİZİ (İlk Kelime) ===\n\n")
for(oss_isim in oss_olmayan) {
  ilk_kelime_oss <- strsplit(oss_isim, " ")[[1]][1]

  # Temporal'de benzer başlangıca sahip isimleri bul
  benzerler <- temporal_olmayan[grepl(paste0("^", ilk_kelime_oss), temporal_olmayan)]

  if(length(benzerler) > 0) {
    cat(sprintf("OSS: %-30s → Temporal: %s\n", oss_isim, benzerler[1]))
  }
}
