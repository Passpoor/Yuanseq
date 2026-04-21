# =====================================================
# 转录因子活性模块 v2.0
# 增强功能：
#   A. 多数据库整合：CollecTRI + DoRothEA（A-D 置信度分级）
#   B. TF 家族富集分析（超几何检验 / Fisher 精确检验）
# =====================================================

# =====================================================
# TF 家族分类数据（基于 Lambert et al. 2018 Cell + AnimalTFDB）
# =====================================================

#' 获取 TF 家族映射表
#' @param species "human" 或 "mouse"
#' @return data.frame (TF_symbol, Family)
get_tf_family_map <- function(species = "human") {
  if (species == "human") {
    tf_families <- list(
      bZIP = c("JUN", "FOS", "ATF1", "ATF2", "ATF3", "ATF4", "ATF5", "ATF6", "ATF7",
               "CREB1", "CREB3", "CREB3L1", "CREB3L2", "CREB3L3", "CREB3L4", "CREB5",
               "CREM", "BATF", "BATF2", "BATF3", "MAF", "MAFA", "MAFB", "MAFF", "MAFG",
               "MAFK", "NFE2", "NFE2L1", "NFE2L2", "NFE2L3", "JUNB", "JUND", "FOSL1",
               "FOSL2", "FOSB", "XBP1", "CEBPA", "CEBPB", "CEBPD", "CEBPE", "CEBPG",
               "CEBPZ", "DDIT3", "EPC1", "NRF1", "NRF2"),
      bHLH = c("MYC", "MYCN", "MYCL", "MYCNOS", "MAX", "MXI1", "MNT", "MLX",
               "MLXIP", "MLXIPL", "TFE3", "TFEB", "TFEC", "MITF",
               "ARNT", "ARNT2", "ARNTL", "ARNTL2", "CLOCK", "NPAS1", "NPAS2", "NPAS3", "NPAS4",
               "HES1", "HES2", "HES3", "HES4", "HES5", "HES6", "HES7",
               "HEY1", "HEY2", "HEYL",
               "ASCL1", "ASCL2", "ASCL3", "ATOH1", "ATOH7", "ATOH8",
               "NEUROD1", "NEUROD2", "NEUROD4", "NEUROD6", "NEUROG1", "NEUROG2", "NEUROG3",
               "OLIG1", "OLIG2", "OLIG3",
               "MYOD1", "MYOG", "MYF5", "MYF6",
               "TCF3", "TCF4", "TCF12", "TCF15", "TCF21", "TCF24",
               "HAND1", "HAND2", "TWIST1", "TWIST2", "SREBF1", "SREBF2",
               "AHR", "SIM1", "SIM2", "SOHLH1", "SOHLH2", "FIGLA",
               "BHLHA15", "BHLHE22", "BHLHE23", "BHLHE40", "BHLHE41",
               "FERD3L", "MSGN1", "PTF1A", "TAL1", "TAL2", "LYL1"),
      Homeobox = c("HOXA1", "HOXA2", "HOXA3", "HOXA4", "HOXA5", "HOXA6", "HOXA7", "HOXA9", "HOXA10", "HOXA11", "HOXA13",
                   "HOXB1", "HOXB2", "HOXB3", "HOXB4", "HOXB5", "HOXB6", "HOXB7", "HOXB8", "HOXB9", "HOXB13",
                   "HOXC4", "HOXC5", "HOXC6", "HOXC8", "HOXC9", "HOXC10", "HOXC11", "HOXC12", "HOXC13",
                   "HOXD1", "HOXD3", "HOXD4", "HOXD8", "HOXD9", "HOXD10", "HOXD11", "HOXD12", "HOXD13",
                   "POU1F1", "POU2F1", "POU2F2", "POU2F3", "POU3F1", "POU3F2", "POU3F3", "POU3F4", "POU4F1", "POU4F2", "POU4F3", "POU5F1", "POU6F1", "POU6F2",
                   "PAX1", "PAX2", "PAX3", "PAX4", "PAX5", "PAX6", "PAX7", "PAX8", "PAX9",
                   "LHX1", "LHX2", "LHX3", "LHX4", "LHX5", "LHX6", "LHX8", "LHX9",
                   "NKX2-1", "NKX2-2", "NKX2-5", "NKX2-8", "NKX3-1", "NKX3-2", "NKX6-1", "NKX6-2",
                   "IRX1", "IRX2", "IRX3", "IRX4", "IRX5", "IRX6",
                   "MEIS1", "MEIS2", "MEIS3", "TGIF1", "TGIF2", "PKNOX1", "PKNOX2",
                   "PBX1", "PBX2", "PBX3", "PBX4",
                   "PROX1", "PROX2", "GSX1", "GSX2", "DBX1", "DBX2",
                   "PHOX2A", "PHOX2B", "OTP", "EMX1", "EMX2",
                   "HLX", "HMX1", "HMX2", "HMX3", "ISL1", "ISL2",
                   "ONECUT1", "ONECUT2", "ONECUT3",
                   "OTX1", "OTX2", "CRX",
                   "VSX1", "VSX2",
                   "CDX1", "CDX2", "CDX4",
                   "PDX1", "NHLH1", "NHLH2", "NOBOX"),
      `Nuclear_Receptor` = c("NR1H2", "NR1H3", "NR1H4", "NR1H5", "NR1D1", "NR1D2", "NR1I2", "NR1I3",
                             "NR2C1", "NR2C2", "NR2E1", "NR2E3", "NR2F1", "NR2F2", "NR2F6",
                             "NR3C1", "NR3C2", "NR4A1", "NR4A2", "NR4A3",
                             "NR5A1", "NR5A2",
                             "NR6A1",
                             "ESR1", "ESR2", "ESRRA", "ESRRB", "ESRRG",
                             "RARA", "RARB", "RARG",
                             "THRA", "THRB",
                             "PPARA", "PPARD", "PPARG",
                             "RXRA", "RXRB", "RXRG",
                             "VDR", "NR0B1", "NR0B2",
                             "AR", "NR1I2",
                             "HNF4A", "HNF4G",
                             "LRH-1", "SHP", "FXR", "LXR", "PXR", "CAR"),
      ETS = c("ETS1", "ETS2", "ERG", "FLI1", "ETV1", "ETV2", "ETV3", "ETV4", "ETV5", "ETV6", "ETV7",
              "ELK1", "ELK3", "ELK4",
              "SPI1", "SPIB", "SPIC", "SPDEF", "SPI1",
              "GABPA", "GABPB1", "GABPB2",
              "EHF", "ELF1", "ELF2", "ELF3", "ELF4", "ELF5",
              "ERF", "ETV3L", "FEV",
              "PEA3"),
      Forkhead = c("FOXA1", "FOXA2", "FOXA3",
                   "FOXB1", "FOXB2",
                   "FOXC1", "FOXC2",
                   "FOXD1", "FOXD2", "FOXD3", "FOXD4",
                   "FOXE1", "FOXE3",
                   "FOXF1", "FOXF2",
                   "FOXG1",
                   "FOXH1", "FOXI1", "FOXI2", "FOXI3",
                   "FOXJ1", "FOXJ2", "FOXJ3",
                   "FOXK1", "FOXK2",
                   "FOXL1", "FOXL2",
                   "FOXM1",
                   "FOXN1", "FOXN2", "FOXN3",
                   "FOXO1", "FOXO3", "FOXO4", "FOXO6",
                   "FOXP1", "FOXP2", "FOXP3", "FOXP4",
                   "FOXQ1",
                   "FOXR1", "FOXR2",
                   "FOXS1"),
      `C2H2_ZF` = c("WT1", "EGR1", "EGR2", "EGR3", "EGR4",
                    "KLF1", "KLF2", "KLF3", "KLF4", "KLF5", "KLF6", "KLF7", "KLF8", "KLF9", "KLF10",
                    "KLF11", "KLF12", "KLF13", "KLF14", "KLF15", "KLF16", "KLF17",
                    "SP1", "SP2", "SP3", "SP4", "SP5", "SP6", "SP7", "SP8", "SP9",
                    "ZEB1", "ZEB2", "ZBTB7A", "ZBTB7B",
                    "GFI1", "GFI1B", "PRDM1", "PRDM5",
                    "ZNF281", "ZNF423", "ZNF536",
                    "YY1", "YY2",
                    "THAP1", "THAP2", "THAP3", "THAP4", "THAP5", "THAP6", "THAP7", "THAP8", "THAP9",
                    "TFCP2", "TFCP2L1", "Ubp1"),
      HMG = c("SOX1", "SOX2", "SOX3", "SOX4", "SOX5", "SOX6", "SOX7", "SOX8", "SOX9", "SOX10",
              "SOX11", "SOX12", "SOX13", "SOX14", "SOX15", "SOX17", "SOX18", "SOX21", "SOX30",
              "TCF3", "TCF4", "TCF7", "TCF7L1", "TCF7L2",
              "LEF1",
              "HMG20A", "HMG20B",
              "HMGA1", "HMGA2", "HMGB1", "HMGB2", "HMGB3",
              "TCF12", "TCF15", "TCF20", "TCF21", "TCF23", "TCF24", "TCF25", "TCF27"),
      STAT = c("STAT1", "STAT2", "STAT3", "STAT4", "STAT5A", "STAT5B", "STAT6"),
      GATA = c("GATA1", "GATA2", "GATA3", "GATA4", "GATA5", "GATA6"),
      SMAD = c("SMAD1", "SMAD2", "SMAD3", "SMAD4", "SMAD5", "SMAD6", "SMAD7", "SMAD9"),
      IRF = c("IRF1", "IRF2", "IRF3", "IRF4", "IRF5", "IRF6", "IRF7", "IRF8", "IRF9"),
      NFKB = c("RELA", "RELB", "NFKB1", "NFKB2", "REL"),
      AP2 = c("TFAP2A", "TFAP2B", "TFAP2C", "TFAP2D", "TFAP2E"),
      Grainyhead = c("GRHL1", "GRHL2", "GRHL3"),
      P53 = c("TP53", "TP63", "TP73"),
      MADS = c("MEF2A", "MEF2B", "MEF2C", "MEF2D", "SRF"),
      TALE = c("MEIS1", "MEIS2", "MEIS3", "TGIF1", "TGIF2", "TGIF2LX", "PKNOX1", "PKNOX2"),
      SANDF = c("RUNX1", "RUNX2", "RUNX3", "CBFB"),
      HSF = c("HSF1", "HSF2", "HSF4", "HSF5"),
      T_Box = c("TBX1", "TBX2", "TBX3", "TBX4", "TBX5", "TBX6", "TBX15", "TBX18", "TBX19", "TBX21", "TBX22", "TBXT", "EOMES", "MGA"),
      RHR = c("NFATC1", "NFATC2", "NFATC3", "NFATC4", "NFAT5"),
      DM = c("DMRT1", "DMRT2", "DMRT3", "DMRTA1", "DMRTA2", "DMRTB1", "DMRTC2"),
      COE = c("EBF1", "EBF2", "EBF3", "EBF4"),
      E2F = c("E2F1", "E2F2", "E2F3", "E2F4", "E2F5", "E2F6", "E2F7", "E2F8",
              "TFDP1", "TFDP2", "TFDP3"),
      CENPB = c("CENPA", "CENPB", "CENPC", "CENPT"),
      AT_hook = c("HMGA1", "HMGA2"),
      `BZIP_LZ` = c("ATF1", "ATF2", "ATF3", "ATF4", "ATF5", "ATF6", "ATF7", "CREB1",
                    "CREB3", "CREB3L1", "CREB3L2", "CREB3L3", "CREB3L4", "CREB5",
                    "CREM", "JUN", "JUNB", "JUND", "FOS", "FOSB", "FOSL1", "FOSL2",
                    "NFE2", "NFE2L1", "NFE2L2", "NFE2L3"),
      CSD = c("YBX1", "YBX2", "YBX3", "DBPB"),
      ZZ = c("ZNF277", "SNAI1", "SNAI2", "SNAI3"),
      `CxxC` = c("KDM2A", "KDM2B", "TET1", "TET2", "TET3"),
      `BTB-POZ` = c("KLF13", "KLF16", "PLZF", "BCL6", "BAZF", "ZBTB7A"),
      MYB = c("MYB", "MYBL1", "MYBL2"),
      `Tryptophan_cluster` = c("MYB", "MYBL1", "MYBL2", "MYPOP"),
      ARID = c("ARID1A", "ARID1B", "ARID2", "ARID3A", "ARID3B", "ARID3C", "ARID4A", "ARID4B", "ARID5A", "ARID5B", "JARID2"),
      CSDF = c("CSDA", "YBX1", "YBX2", "YBX3", "MSY1"),
      CUT = c("CUX1", "CUX2", "ONECUT1", "ONECUT2", "ONECUT3", "SATB1", "SATB2", "CDP"),
      SRF = c("SRF"),
      LIM = c("LHX1", "LHX2", "LHX3", "LHX4", "LHX5", "LHX6", "LHX8", "LHX9",
              "LMO1", "LMO2", "LMO3", "LMO4"),
      CAAT = c("NFYA", "NFYB", "NFYC", "CEBPA", "CEBPB", "CEBPD", "CEBPE", "CEBPG", "CEBPZ", "NFIL3"),
      AF4 = c("AFF1", "AFF2", "AFF3", "AFF4", "MLLT2"),
      FAST = c("FOXL1", "FOXL2"),
      TCF_LEF = c("TCF7", "TCF7L1", "TCF7L2", "LEF1"),
      FLYWCH = c("FLII", "FLYWCH1", "FLYWCH2"),
      Plus3 = c("RBM14", "RBM4", "RBM4B"),
      `Pseudo_NR` = c("CGBP", "HMGA1", "HMGA2"),
      RB = c("RBL1", "RBL2", "RBL3"),
      SCAN = c("SCAN1", "SCAN2", "SCAN3", "ZNF174", "ZNF263", "ZNF300", "ZFP28"),
      GCM = c("GCM1", "GCM2"),
      HLH = c("TCF3", "TCF4", "TCF12", "HEY1", "HEY2", "HEYL", "HES1", "HES2", "HES3", "HES4", "HES5", "HES6", "HES7"),
      GTF2I = c("GTF2I", "GTF2IRD1", "GTF2IRD2"),
      SIX = c("SIX1", "SIX2", "SIX3", "SIX4", "SIX5", "SIX6"),
      `A_T_hook` = c("HMGA1", "HMGA2"),
      CXXC = c("KDM2A", "KDM2B", "TET1", "TET2", "TET3"),
      Homeo = c("IRX1", "IRX2", "IRX3", "IRX4", "IRX5", "IRX6",
                "MEIS1", "MEIS2", "MEIS3", "TGIF1", "TGIF2", "PKNOX1", "PKNOX2",
                "PITX1", "PITX2", "PITX3", "PROP1"),
      DMRT = c("DMRT1", "DMRT2", "DMRT3", "DMRTA1", "DMRTA2", "DMRTB1", "DMRTC2"),
      FBP = c("FUBP1", "FUBP3"),
      `COUP-TFII` = c("NR2F2"),
      `Other_TF` = c("CHD1", "CHD2", "CHD3", "CHD4", "CHD5", "CHD6", "CHD7", "CHD8", "CHD9",
                     "REST", "NSD1", "SETDB1", "SETDB2",
                     "TRPS1", "ZFHX3", "ZFHX4",
                     "CIC", "PATZ1", "PATZ2",
                     "CAPNS2", "TEAD1", "TEAD2", "TEAD3", "TEAD4",
                     "MGA", "MAX", "MXI1", "MNT",
                     "TFAM", "TTF1", "TTF2",
                     "HMBOX1", "HOMEZ", "HOPX", "HIVEP1", "HIVEP2", "HIVEP3",
                     "IKZF1", "IKZF2", "IKZF3", "IKZF4", "IKZF5",
                     "LIN28A", "LIN28B",
                     "MSC", "MSGN1", "MYF5", "MYF6", "MYOD1", "MYOG",
                     "NR1D1", "NR1D2", "NR2F1", "NR2F2", "NR2F6",
                     "PGR", "ESR1", "ESR2", "AR", "PPARD",
                     "TFCP2", "UBP1", "ZNF100", "ZNF101")
    )
  } else {
    # Mouse TF families (map to human orthologs)
    tf_families <- list(
      bZIP = c("Jun", "Fos", "Atf1", "Atf2", "Atf3", "Atf4", "Atf5", "Atf6", "Atf7",
               "Creb1", "Creb3", "Creb3l1", "Creb3l2", "Creb3l3", "Creb3l4", "Creb5",
               "Crem", "Batf", "Batf2", "Batf3", "Maf", "Mafa", "Mafb", "Maff", "Mafg",
               "Mafk", "Nfe2", "Nfe2l1", "Nfe2l2", "Nfe2l3", "Junb", "Jund", "Fosl1",
               "Fosl2", "Fosb", "Xbp1", "Cebpa", "Cebpb", "Cebpd", "Cebpe", "Cebpg", "Cebpz", "Ddit3"),
      bHLH = c("Myc", "Mycn", "Mycl", "Max", "Mxi1", "Mnt", "Mlx", "Mlxip", "Mlxipl",
               "Tfe3", "Tfeb", "Tfec", "Mitf",
               "Arnt", "Arnt2", "Arntl", "Arntl2", "Clock", "Npas1", "Npas2", "Npas3", "Npas4",
               "Hes1", "Hes2", "Hes3", "Hes4", "Hes5", "Hes6", "Hes7",
               "Hey1", "Hey2", "Heyl",
               "Ascl1", "Ascl2", "Ascl3", "Atoh1", "Atoh7", "Atoh8",
               "Neurod1", "Neurod2", "Neurod4", "Neurod6", "Neurog1", "Neurog2", "Neurog3",
               "Olig1", "Olig2", "Olig3",
               "Myod1", "Myog", "Myf5", "Myf6",
               "Tcf3", "Tcf4", "Tcf12", "Tcf15", "Tcf21", "Tcf24",
               "Hand1", "Hand2", "Twist1", "Twist2", "Srebf1", "Srebf2"),
      Homeobox = c("Hoxa1", "Hoxa2", "Hoxa3", "Hoxa4", "Hoxa5", "Hoxa6", "Hoxa7", "Hoxa9", "Hoxa10", "Hoxa11", "Hoxa13",
                   "Hoxb1", "Hoxb2", "Hoxb3", "Hoxb4", "Hoxb5", "Hoxb6", "Hoxb7", "Hoxb8", "Hoxb9", "Hoxb13",
                   "Hoxc4", "Hoxc5", "Hoxc6", "Hoxc8", "Hoxc9", "Hoxc10", "Hoxc11", "Hoxc12", "Hoxc13",
                   "Hoxd1", "Hoxd3", "Hoxd4", "Hoxd8", "Hoxd9", "Hoxd10", "Hoxd11", "Hoxd12", "Hoxd13",
                   "Pou1f1", "Pou2f1", "Pou2f2", "Pou2f3", "Pou3f1", "Pou3f2", "Pou3f3", "Pou3f4",
                   "Pou4f1", "Pou4f2", "Pou4f3", "Pou5f1", "Pou6f1", "Pou6f2",
                   "Pax1", "Pax2", "Pax3", "Pax4", "Pax5", "Pax6", "Pax7", "Pax8", "Pax9",
                   "Lhx1", "Lhx2", "Lhx3", "Lhx4", "Lhx5", "Lhx6", "Lhx8", "Lhx9",
                   "Nkx2-1", "Nkx2-2", "Nkx2-5", "Nkx2-8", "Nkx3-1", "Nkx3-2", "Nkx6-1", "Nkx6-2",
                   "Irx1", "Irx2", "Irx3", "Irx4", "Irx5", "Irx6",
                   "Meis1", "Meis2", "Meis3", "Tgif1", "Tgif2", "Pknox1", "Pknox2",
                   "Pbx1", "Pbx2", "Pbx3", "Pbx4",
                   "Prox1", "Prox2", "Gsx1", "Gsx2", "Dbx1", "Dbx2",
                   "Phox2a", "Phox2b", "Otp", "Emx1", "Emx2",
                   "Hlx", "Hmx1", "Hmx2", "Hmx3", "Isl1", "Isl2",
                   "Onecut1", "Onecut2", "Onecut3",
                   "Otx1", "Otx2", "Crx",
                   "Cdx1", "Cdx2", "Cdx4", "Pdx1"),
      `Nuclear_Receptor` = c("Nr1h2", "Nr1h3", "Nr1h4", "Nr1d1", "Nr1d2", "Nr1i2", "Nr1i3",
                             "Nr2c1", "Nr2c2", "Nr2e1", "Nr2e3", "Nr2f1", "Nr2f2", "Nr2f6",
                             "Nr3c1", "Nr3c2", "Nr4a1", "Nr4a2", "Nr4a3",
                             "Nr5a1", "Nr5a2", "Nr6a1",
                             "Esr1", "Esr2", "Esrra", "Esrrb", "Esrrg",
                             "Rara", "Rarb", "Rarg", "Thra", "Thrb",
                             "Ppara", "Ppard", "Pparg",
                             "Rxra", "Rxrb", "Rxrg", "Vdr", "Nr0b1", "Nr0b2", "Ar"),
      ETS = c("Ets1", "Ets2", "Erg", "Fli1", "Etv1", "Etv2", "Etv3", "Etv4", "Etv5", "Etv6", "Etv7",
              "Elk1", "Elk3", "Elk4", "Spi1", "Spib", "Spic", "Spdef",
              "Gabpa", "Gabpb1", "Gabpb2", "Ehf", "Elf1", "Elf2", "Elf3", "Elf4", "Elf5"),
      Forkhead = c("Foxa1", "Foxa2", "Foxa3", "Foxb1", "Foxb2", "Foxc1", "Foxc2",
                   "Foxd1", "Foxd2", "Foxd3", "Foxd4",
                   "Foxe1", "Foxe3", "Foxf1", "Foxf2", "Foxg1",
                   "Foxh1", "Foxi1", "Foxi2", "Foxi3",
                   "Foxj1", "Foxj2", "Foxj3",
                   "Foxk1", "Foxk2", "Foxl1", "Foxl2", "Foxm1",
                   "Foxn1", "Foxn2", "Foxn3",
                   "Foxo1", "Foxo3", "Foxo4", "Foxo6",
                   "Foxp1", "Foxp2", "Foxp3", "Foxp4", "Foxq1", "Foxr1", "Foxr2", "Foxs1"),
      `C2H2_ZF` = c("Wt1", "Egr1", "Egr2", "Egr3", "Egr4",
                    "Klf1", "Klf2", "Klf3", "Klf4", "Klf5", "Klf6", "Klf7", "Klf8", "Klf9", "Klf10",
                    "Klf11", "Klf12", "Klf13", "Klf14", "Klf15", "Klf16", "Klf17",
                    "Sp1", "Sp2", "Sp3", "Sp4", "Sp5", "Sp6", "Sp7", "Sp8", "Sp9",
                    "Zeb1", "Zeb2", "Zbtb7a", "Zbtb7b",
                    "Gfi1", "Gfi1b", "Prdm1", "Prdm5",
                    "Zfp281", "Zfp423", "Zfp536", "Yy1", "Yy2"),
      HMG = c("Sox1", "Sox2", "Sox3", "Sox4", "Sox5", "Sox6", "Sox7", "Sox8", "Sox9", "Sox10",
              "Sox11", "Sox12", "Sox13", "Sox14", "Sox15", "Sox17", "Sox18", "Sox21", "Sox30",
              "Tcf3", "Tcf4", "Tcf7", "Tcf7l1", "Tcf7l2", "Lef1"),
      STAT = c("Stat1", "Stat2", "Stat3", "Stat4", "Stat5a", "Stat5b", "Stat6"),
      GATA = c("Gata1", "Gata2", "Gata3", "Gata4", "Gata5", "Gata6"),
      SMAD = c("Smad1", "Smad2", "Smad3", "Smad4", "Smad5", "Smad6", "Smad7", "Smad9"),
      IRF = c("Irf1", "Irf2", "Irf3", "Irf4", "Irf5", "Irf6", "Irf7", "Irf8", "Irf9"),
      NFKB = c("Rela", "Relb", "Nfkb1", "Nfkb2", "Rel"),
      AP2 = c("Tfap2a", "Tfap2b", "Tfap2c", "Tfap2d", "Tfap2e"),
      Grainyhead = c("Grhl1", "Grhl2", "Grhl3"),
      P53 = c("Trp53", "Trp63", "Trp73"),
      MADS = c("Mef2a", "Mef2b", "Mef2c", "Mef2d", "Srf"),
      SANDF = c("Runx1", "Runx2", "Runx3", "Cbfb"),
      HSF = c("Hsf1", "Hsf2", "Hsf4", "Hsf5"),
      T_Box = c("Tbx1", "Tbx2", "Tbx3", "Tbx4", "Tbx5", "Tbx6", "Tbx15", "Tbx18", "Tbx19", "Tbx21", "Tbx22", "Tbxt", "Eomes"),
      RHR = c("Nfatc1", "Nfatc2", "Nfatc3", "Nfatc4", "Nfat5"),
      E2F = c("E2f1", "E2f2", "E2f3", "E2f4", "E2f5", "E2f6", "E2f7", "E2f8", "Tfdp1", "Tfdp2"),
      MYB = c("Myb", "Mybl1", "Mybl2"),
      TEAD = c("Tead1", "Tead2", "Tead3", "Tead4"),
      GCM = c("Gcm1", "Gcm2"),
      SIX = c("Six1", "Six2", "Six3", "Six4", "Six5", "Six6"),
      ARID = c("Arid1a", "Arid1b", "Arid2", "Arid3a", "Arid3b", "Arid3c", "Arid4a", "Arid4b", "Arid5a", "Arid5b", "Jarid2"),
      `Other_TF` = c("Rest", "Nsd1", "Setdb1", "Setdb2", "Trps1",
                     "Cic", "Patz1",
                     "Hmbox1", "Homez", "Hopx", "Hivep1", "Hivep2", "Hivep3",
                     "Ikzf1", "Ikzf2", "Ikzf3", "Ikzf4", "Ikzf5",
                     "Lin28a", "Lin28b",
                     "Msc", "Msgn1", "Myf5", "Myf6", "Myod1", "Myog",
                     "Pgr", "Tfcp2", "Ubp1")
    )
  }

  # 转换为 data.frame
  family_df <- do.call(rbind, lapply(names(tf_families), function(fam) {
    data.frame(TF = tf_families[[fam]], Family = fam, stringsAsFactors = FALSE)
  }))

  # 去重：一个 TF 可能属于多个家族，取第一个
  family_df <- family_df %>%
    distinct(TF, .keep_all = TRUE)

  return(family_df)
}


#' 运行 TF 家族富集分析
#' @param tf_results TF 活性推断结果 (data.frame with source, score, p_value)
#' @param tf_family_map TF 家族映射表
#' @param active_top_n 选择 Top N 活性 TF 作为 foreground
#' @param pvalue_cutoff 显著性阈值
#' @return data.frame with enrichment results
tf_family_enrichment <- function(tf_results, tf_family_map,
                                  active_top_n = 20, pvalue_cutoff = 0.05) {

  # 获取 foreground TF（top active + top inactive）
  top_tfs <- tf_results %>%
    arrange(rnk) %>%
    head(active_top_n) %>%
    pull(source)

  # 背景：所有被推断的 TF
  background_tfs <- unique(tf_results$source)

  # 只保留有家族注释的 TF
  annotated_tfs <- intersect(background_tfs, tf_family_map$TF)

  if (length(annotated_tfs) < 5) {
    return(NULL)
  }

  foreground <- intersect(top_tfs, annotated_tfs)
  background <- annotated_tfs

  # 获取涉及的家族列表
  families <- unique(tf_family_map$Family[tf_family_map$TF %in% background])

  results <- lapply(families, function(fam) {
    family_tfs <- tf_family_map$TF[tf_family_map$Family == fam]

    # 构建列联表
    a <- sum(foreground %in% family_tfs)  # foreground 中属于该家族
    b <- sum(!(foreground %in% family_tfs))  # foreground 中不属于
    c <- sum(background %in% family_tfs) - a  # background 中属于（排除 foreground）
    d <- sum(!(background %in% family_tfs)) - b  # background 中不属于

    if ((a + c) < 2) return(NULL)  # 家族 TF 太少

    # Fisher 精确检验
    mat <- matrix(c(a, b, c, d), nrow = 2)
    test <- fisher.test(mat, alternative = "greater")

    data.frame(
      Family = fam,
      N_Family_TFs = a + c,
      N_Foreground = a,
      N_Background = c,
      Expected = length(foreground) * (a + c) / length(background),
      Fold_Enrichment = ifelse((a + c) > 0, a / max(length(foreground) * (a + c) / length(background), 0.001), NA),
      PValue = test$p.value,
      stringsAsFactors = FALSE
    )
  })

  results_df <- do.call(rbind, results)
  if (is.null(results_df) || nrow(results_df) == 0) return(NULL)

  # 计算 padj
  results_df$Padj <- p.adjust(results_df$PValue, method = "BH")

  # 标记显著性
  results_df$Significance <- ifelse(
    results_df$Padj < 0.001, "***",
    ifelse(results_df$Padj < 0.01, "**",
           ifelse(results_df$Padj < 0.05, "*", "ns"))
  )

  results_df <- results_df %>%
    arrange(PValue)

  return(results_df)
}


# =====================================================
# 转录因子活性模块 Server
# =====================================================

tf_activity_server <- function(input, output, session, deg_results) {

  # =====================================================
  # 数据库加载（CollecTRI + DoRothEA）
  # =====================================================

  # 统一的数据库加载入口
  tf_network_data <- reactive({
    req(input$tf_database, input$data_source)

    # 确定物种
    if (input$data_source == "counts") {
      species_code <- input$species_select
    } else {
      species_code <- input$deg_species
    }
    organism_code <- if(species_code == "Hs") "human" else "mouse"

    db_type <- input$tf_database

    # 根据选择加载对应数据库
    if (db_type == "collectri") {
      return(load_collectri(organism_code))
    } else if (db_type == "dorothea") {
      return(load_dorothea(organism_code))
    } else {
      showNotification(paste("未知数据库类型:", db_type), type = "error")
      return(NULL)
    }
  })

  # 加载 CollecTRI 数据库
  load_collectri <- function(organism_code) {
    current_file <- paste0("collectri_", organism_code, ".rds")

    if (file.exists(current_file)) {
      showNotification(paste0("正在从本地加载 CollecTRI (", organism_code, ")..."),
                       type = "message", duration = 3)
      net <- readRDS(current_file)
    } else {
      showNotification(paste0("正在下载 CollecTRI (", organism_code, ")..."),
                       type = "warning", duration = 5)
      tryCatch({
        net <- decoupleR::get_collectri(organism = organism_code, split_complexes = FALSE)
        saveRDS(net, current_file)
        showNotification(paste0("CollecTRI (", organism_code, ") 下载成功!"),
                         type = "message", duration = 3)
      }, error = function(e) {
        showNotification(paste("下载 CollecTRI 失败:", e$message), type = "error")
        return(NULL)
      })
    }

    if (!is.null(net)) {
      # 添加数据库来源标记
      net$database <- "CollecTRI"
      net$confidence <- NA  # CollecTRI 没有置信度分级
    }

    return(net)
  }

  # 加载 DoRothEA 数据库（带置信度分级）
  load_dorothea <- function(organism_code) {
    cache_file <- paste0("dorothea_", organism_code, ".rds")

    if (file.exists(cache_file)) {
      showNotification(paste0("正在从本地加载 DoRothEA (", organism_code, ")..."),
                       type = "message", duration = 3)
      net <- readRDS(cache_file)
    } else {
      showNotification(paste0("正在下载 DoRothEA (", organism_code, ")..."),
                       type = "warning", duration = 5)
      tryCatch({
        net <- decoupleR::get_dorothea(organism = organism_code,
                                       levels = c("A", "B", "C", "D"))
        saveRDS(net, cache_file)
        showNotification(paste0("DoRothEA (", organism_code, ") 下载成功!"),
                         type = "message", duration = 3)
      }, error = function(e) {
        showNotification(paste("下载 DoRothEA 失败:", e$message), type = "error")
        return(NULL)
      })
    }

    if (!is.null(net)) {
      # 确保 confidence 列存在
      if (!("confidence" %in% colnames(net))) {
        if ("mor" %in% colnames(net)) {
          # 如果 DoRothEA 返回的格式不同，尝试适配
          showNotification("DoRothEA 数据格式适配中...", type = "message")
        }
        net$confidence <- "C"  # 默认 C 级
      }
      net$database <- "DoRothEA"
    }

    return(net)
  }

  # 获取过滤后的网络（应用置信度筛选）
  filtered_network <- reactive({
    req(tf_network_data())
    net <- tf_network_data()

    # 只有 DoRothEA 需要置信度过滤
    if (input$tf_database == "dorothea" && "confidence" %in% colnames(net)) {
      selected_levels <- input$dorothea_confidence
      if (is.null(selected_levels)) selected_levels <- c("A", "B", "C")

      net <- net %>%
        filter(confidence %in% selected_levels)

      showNotification(
        paste0("DoRothEA 过滤后: ", nrow(net), " 条调控关系 (置信度: ",
               paste(selected_levels, collapse = ", "), ")"),
        type = "message", duration = 3
      )
    }

    return(net)
  })

  # =====================================================
  # TF 家族映射表（响应式，根据物种变化）
  # =====================================================

  tf_family_map_data <- reactive({
    req(input$data_source)

    if (input$data_source == "counts") {
      species_code <- input$species_select
    } else {
      species_code <- input$deg_species
    }
    organism_code <- if(species_code == "Hs") "human" else "mouse"

    return(get_tf_family_map(organism_code))
  })

  # =====================================================
  # 运行 TF 活性分析
  # =====================================================

  tf_activity_results <- eventReactive(input$run_tf_activity, {
    req(deg_results(), filtered_network())

    method <- input$tf_method
    db_type <- input$tf_database

    showNotification(
      paste("正在推断 TF 活性 (数据库:", toupper(db_type),
            "| 算法:", toupper(method), ")..."),
      type = "message"
    )

    # 获取数据
    deg_data <- deg_results()
    deg_res <- deg_data$deg_df
    net <- filtered_network()

    # 1. 构造输入矩阵
    stats_df <- deg_res %>%
      filter(!is.na(SYMBOL), !is.na(t_stat)) %>%
      group_by(SYMBOL) %>%
      filter(abs(log2FoldChange) == max(abs(log2FoldChange))) %>%
      ungroup() %>%
      distinct(SYMBOL, .keep_all = TRUE)

    if (nrow(stats_df) < 5) {
      showNotification(
        paste0("TF 分析失败: 有效基因数量 (", nrow(stats_df), ") 不足。"),
        type = "error", duration = 15
      )
      return(NULL)
    }

    # 2. ID 兼容性检查
    input_genes <- stats_df$SYMBOL
    net_targets <- unique(net$target)
    shared_genes <- intersect(input_genes, net_targets)

    min_size <- input$tf_min_size
    if(length(shared_genes) < min_size) {
      showNotification(
        paste0("TF 分析失败: 共享靶基因 (", length(shared_genes),
               ") 少于最小要求 (", min_size, ")。"),
        type = "error", duration = 15
      )
      return(NULL)
    }

    # 3. 数据清洗
    stats_df_clean <- stats_df %>%
      filter(SYMBOL %in% shared_genes) %>%
      filter(!is.na(t_stat), is.finite(t_stat), t_stat != 0)

    cat(sprintf("📊 TF分析: %d 基因 -> 清洗后 %d 基因 (数据库: %s)\n",
                nrow(stats_df), nrow(stats_df_clean), db_type))

    if (nrow(stats_df_clean) < 5) {
      showNotification("TF 分析失败: 清洗后有效基因不足。", type = "error", duration = 15)
      return(NULL)
    }

    mat_input <- stats_df_clean %>%
      select(SYMBOL, t_stat) %>%
      column_to_rownames(var = "SYMBOL") %>%
      as.matrix()

    # 最终清理
    if (any(is.na(mat_input)) || any(!is.finite(mat_input))) {
      mat_input <- mat_input[is.finite(rowSums(mat_input)), , drop = FALSE]
      mat_input <- mat_input[, is.finite(colSums(mat_input)), drop = FALSE]
    }

    # 4. 运行推断算法
    tryCatch({
      contrast_acts <- switch(method,
        "ulm" = decoupleR::run_ulm(
          mat = mat_input, net = net,
          .source = 'source', .target = 'target', .mor = 'mor',
          minsize = min_size
        ),
        "mlm" = decoupleR::run_mlm(
          mat = mat_input, net = net,
          .source = 'source', .target = 'target', .mor = 'mor',
          minsize = min_size
        ),
        "wmean" = decoupleR::run_wmean(
          mat = mat_input, net = net,
          .source = 'source', .target = 'target', .mor = 'mor',
          minsize = min_size
        ),
        "wsum" = decoupleR::run_wsum(
          mat = mat_input, net = net,
          .source = 'source', .target = 'target', .mor = 'mor',
          minsize = min_size
        ),
        decoupleR::run_ulm(
          mat = mat_input, net = net,
          .source = 'source', .target = 'target', .mor = 'mor',
          minsize = min_size
        )
      )

      # 标准化返回结果
      if (is.data.frame(contrast_acts)) {
        result_df <- contrast_acts
      } else if (is.list(contrast_acts) && "statistic" %in% names(contrast_acts)) {
        result_df <- contrast_acts$statistic
      } else {
        result_df <- as.data.frame(contrast_acts)
      }

      # 添加排名
      result_df <- result_df %>% mutate(rnk = NA)
      msk_pos <- result_df$score > 0
      result_df[msk_pos, 'rnk'] <- rank(-result_df[msk_pos, 'score'])
      msk_neg <- result_df$score < 0
      result_df[msk_neg, 'rnk'] <- rank(-abs(result_df[msk_neg, 'score']))

      # 添加元信息
      result_df$method <- toupper(method)
      result_df$database <- toupper(db_type)

      showNotification(
        paste("TF 活性推断完成! (", nrow(result_df), " TFs | ", toupper(db_type), ")"),
        type = "message"
      )

      return(result_df)

    }, error = function(e) {
      error_msg <- e$message
      if (grepl("colinear", error_msg, ignore.case = TRUE)) {
        showNotification(
          paste("TF 分析失败 (共线性错误): 请尝试 ULM/WMEAN/WSUM 算法。"),
          type = "error", duration = 10
        )
      } else {
        showNotification(paste("TF 分析失败:", error_msg), type = "error")
      }
      return(NULL)
    })
  })

  # =====================================================
  # 原有输出：TF 活性柱状图
  # =====================================================

  output$tf_activity_bar_plot <- renderPlot({
    req(tf_activity_results())

    df_acts <- tf_activity_results()
    n_tfs <- input$tf_top_n

    tfs_to_plot <- df_acts %>%
      arrange(rnk) %>%
      head(n_tfs) %>%
      pull(source)

    f_contrast_acts <- df_acts %>%
      filter(source %in% tfs_to_plot)

    txt_col <- if(input$theme_toggle) "white" else "black"
    grid_col <- if(input$theme_toggle) "#444444" else "#cccccc"

    ggplot(f_contrast_acts, aes(x = reorder(source, score), y = score)) +
      geom_bar(aes(fill = score), stat = "identity", width = input$tf_bar_width %||% 0.7) +
      scale_fill_gradient2(
        low = input$tf_inactive_col,
        high = input$tf_active_col,
        mid = "whitesmoke",
        midpoint = 0,
        name = "TF 活性分数",
        limits = c(-max(abs(f_contrast_acts$score), 1), max(abs(f_contrast_acts$score), 1))
      ) +
      geom_hline(yintercept = 0, linetype = 'dashed', color = txt_col, linewidth = 0.5) +
      labs(
        x = "转录因子 (TFs)",
        y = "活性分数",
        title = paste("Top", n_tfs, "转录因子活性变化")
      ) +
      theme_minimal(base_size = input$tf_bar_font_size %||% 11) +
      theme(
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        plot.title = element_text(color = txt_col, face = "bold", hjust = 0.5,
                                 size = (input$tf_bar_font_size %||% 11) * 1.3),
        plot.title.position = "plot",
        axis.title = element_text(color = txt_col, face = "bold",
                                  size = (input$tf_bar_font_size %||% 11) * 1.05),
        axis.text.x = element_text(angle = input$tf_bar_angle %||% 45, hjust = 1,
                                    size = (input$tf_bar_font_size %||% 11) * 0.9,
                                    face = "bold", color = txt_col),
        axis.text.y = element_text(size = (input$tf_bar_font_size %||% 11) * 0.9,
                                    face = "bold", color = txt_col),
        legend.text = element_text(color = txt_col,
                                    size = (input$tf_bar_font_size %||% 11) * 0.85),
        legend.title = element_text(color = txt_col, face = "bold",
                                    size = (input$tf_bar_font_size %||% 11) * 0.9),
        axis.line = element_line(color = txt_col, linewidth = 0.5),
        panel.grid.major = element_line(color = grid_col, linewidth = 0.3),
        panel.grid.minor = element_blank()
      )
  })

  # =====================================================
  # 原有输出：结果下载
  # =====================================================

  output$download_tf_results <- downloadHandler(
    filename = function() {
      db <- ifelse(is.null(input$tf_database), "TF", toupper(input$tf_database))
      paste0(db, "_Activity_Results_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(tf_activity_results())
      df <- tf_activity_results() %>%
        select(source, score, p_value, rnk, database) %>%
        rename(TF = source, Score = score, P.Value = p_value, Rank = rnk, Database = database)
      write.csv(df, file, row.names = FALSE)
    }
  )

  # =====================================================
  # 原有输出：TF 活性表格
  # =====================================================

  output$tf_activity_table <- DT::renderDataTable({
    req(tf_activity_results())

    df <- tf_activity_results() %>%
      select(source, score, p_value, rnk, database) %>%
      rename(TF = source, Score = score, P.Value = p_value, Rank = rnk, Database = database) %>%
      arrange(Rank)

    DT::datatable(df, selection = 'single', options = list(scrollX=T, pageLength=10), rownames=F) %>%
      formatRound(c("Score", "P.Value"), 4)
  })

  # =====================================================
  # 原有输出：靶基因分析（适配多数据库）
  # =====================================================

  selected_tf_targets <- reactive({
    req(tf_activity_results(), filtered_network(), deg_results())

    selected_row <- input$tf_activity_table_rows_selected
    if (length(selected_row) == 0) return(NULL)

    tf_res <- tf_activity_results()
    if (!is.data.frame(tf_res)) {
      showNotification("TF结果格式错误", type = "error")
      return(NULL)
    }

    net <- filtered_network()
    if (!is.data.frame(net)) {
      showNotification("网络格式错误", type = "error")
      return(NULL)
    }

    deg_res <- deg_results()
    if (!is.list(deg_res) || is.null(deg_res$deg_df)) {
      showNotification("差异分析结果格式错误", type = "error")
      return(NULL)
    }

    tf_name <- tf_res %>%
      arrange(rnk) %>%
      slice(selected_row) %>%
      pull(source)

    tf_net <- net %>%
      filter(source == tf_name) %>%
      select(target, mor) %>%
      rename(SYMBOL = target, Mode_of_Regulation = mor)

    deg_data <- deg_res$deg_df %>%
      select(SYMBOL, log2FoldChange, pvalue, padj, t_stat, Status)

    final_table <- tf_net %>%
      left_join(deg_data, by = "SYMBOL") %>%
      mutate(
        Predicted_Change = case_when(
          Mode_of_Regulation > 0 ~ "Activator (Up)",
          Mode_of_Regulation < 0 ~ "Repressor (Down)",
          TRUE ~ "Unknown"
        ),
        Actual_Change = case_when(
          log2FoldChange > 0 ~ "Up",
          log2FoldChange < 0 ~ "Down",
          TRUE ~ "Not DE"
        ),
        Match_Status = case_when(
          Predicted_Change == "Activator (Up)" & Actual_Change == "Up" ~ "Consistent",
          Predicted_Change == "Repressor (Down)" & Actual_Change == "Down" ~ "Consistent",
          Predicted_Change == "Activator (Up)" & Actual_Change == "Down" ~ "Inconsistent",
          Predicted_Change == "Repressor (Down)" & Actual_Change == "Up" ~ "Inconsistent",
          TRUE ~ "Neutral/Unknown"
        )
      ) %>%
      mutate(Is_DE = ifelse(Status != "Not DE", "Yes", "No")) %>%
      select(SYMBOL, Mode_of_Regulation, Is_DE, Status, log2FoldChange, t_stat, pvalue, padj,
             Predicted_Change, Actual_Change, Match_Status) %>%
      arrange(desc(abs(log2FoldChange)))

    return(list(tf_name = tf_name, data = final_table))
  })

  output$tf_target_table <- DT::renderDataTable({
    req(selected_tf_targets())
    data_list <- selected_tf_targets()

    DT::datatable(
      data_list$data,
      caption = paste0("TF: ", data_list$tf_name, " 的靶基因"),
      options = list(scrollX=T, pageLength=10), rownames=F
    ) %>%
      formatRound(c("log2FoldChange", "t_stat", "pvalue", "padj"), 4)
  })

  # 一致性统计
  output$tf_consistency_summary <- renderText({
    req(selected_tf_targets())
    df <- selected_tf_targets()$data

    n_consistent <- sum(df$Match_Status == "Consistent", na.rm = TRUE)
    n_inconsistent <- sum(df$Match_Status == "Inconsistent", na.rm = TRUE)
    n_neutral <- sum(df$Match_Status == "Neutral/Unknown", na.rm = TRUE)
    n_total <- n_consistent + n_inconsistent + n_neutral

    if (n_total > 0) {
      pct_consistent <- round(100 * n_consistent / n_total, 1)
      pct_inconsistent <- round(100 * n_inconsistent / n_total, 1)
      pct_neutral <- round(100 * n_neutral / n_total, 1)

      paste0(
        "📊 调控一致性统计 | ",
        "✅ 一致: ", n_consistent, " (", pct_consistent, "%) | ",
        "❌ 不一致: ", n_inconsistent, " (", pct_inconsistent, "%) | ",
        "⚪ 未知: ", n_neutral, " (", pct_neutral, "%)"
      )
    } else {
      "📊 无数据"
    }
  })

  # =====================================================
  # 原有输出：靶基因散点图
  # =====================================================

  output$tf_target_plot <- renderPlot({
    req(selected_tf_targets())

    # Explicit reactive dependencies (fix: %||% alone doesn't trigger re-render)
    input$tf_scatter_point_size
    input$tf_scatter_alpha
    input$tf_scatter_label_size
    input$tf_scatter_n_labels
    input$tf_scatter_point_shape
    input$tf_scatter_label_repel
    input$tf_scatter_legend_pos
    input$tf_scatter_title_size
    input$tf_scatter_axis_size
    input$tf_scatter_consis_col
    input$tf_scatter_incon_col
    input$tf_scatter_neutral_col

    point_size <- input$tf_scatter_point_size %||% 3
    point_alpha <- input$tf_scatter_alpha %||% 0.7
    label_size <- input$tf_scatter_label_size %||% 3
    n_labels <- input$tf_scatter_n_labels %||% 15
    point_shape <- input$tf_scatter_point_shape %||% 19
    use_repel <- input$tf_scatter_label_repel %||% TRUE
    legend_pos <- input$tf_scatter_legend_pos %||% "right"
    title_size <- input$tf_scatter_title_size %||% 14
    axis_size <- input$tf_scatter_axis_size %||% 10
    consis_col <- input$tf_scatter_consis_col %||% "#2ecc71"
    incon_col <- input$tf_scatter_incon_col %||% "#e74c3c"
    neutral_col <- input$tf_scatter_neutral_col %||% "#95a5a6"

    data_list <- selected_tf_targets()
    df <- data_list$data
    tf_name <- data_list$tf_name

    txt_col <- if(input$theme_toggle) "white" else "black"
    grid_col <- if(input$theme_toggle) "#444444" else "#cccccc"

    p <- ggplot(df, aes(x = log2FoldChange, y = -log10(pvalue))) +
      geom_point(aes(color = Match_Status), size = point_size, alpha = point_alpha, shape = point_shape) +
      scale_color_manual(
        values = c("Consistent" = consis_col, "Inconsistent" = incon_col, "Neutral/Unknown" = neutral_col),
        name = "调控一致性"
      ) +
      geom_vline(xintercept = 0, linetype = "dashed", color = txt_col, alpha = 0.5) +
      geom_hline(yintercept = -log10(input$pval_cutoff), linetype = "dotted", color = txt_col, alpha = 0.5) +
      labs(title = paste("TF:", tf_name, "靶基因差异表达"),
           x = expression(log[2](Fold~Change)), y = expression(-log[10](italic(P)~value))) +
      theme_minimal() +
      theme(
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        plot.title = element_text(color = txt_col, face = "bold", hjust = 0.5, size = title_size),
        plot.subtitle = element_text(color = txt_col, size = title_size * 0.85),
        axis.title = element_text(color = txt_col, face = "bold", size = axis_size),
        axis.text = element_text(color = txt_col, size = axis_size * 0.9),
        axis.text.x = element_text(color = txt_col, size = axis_size * 0.9),
        axis.text.y = element_text(color = txt_col, size = axis_size * 0.9),
        legend.text = element_text(color = txt_col, size = axis_size * 0.85),
        legend.title = element_text(color = txt_col, face = "bold", size = axis_size * 0.9),
        legend.position = legend_pos,
        axis.line = element_line(color = txt_col, linewidth = 0.5),
        panel.grid.major = element_line(color = grid_col, linewidth = 0.3),
        panel.grid.minor = element_blank()
      )

    if (n_labels > 0) {
      top_genes <- df %>%
        filter(!is.na(pvalue)) %>%
        arrange(pvalue) %>%
        head(min(n_labels, nrow(df)))

      if (nrow(top_genes) > 0) {
        if (use_repel && requireNamespace("ggrepel", quietly = TRUE)) {
          p <- p + ggrepel::geom_text_repel(
            data = top_genes, aes(label = SYMBOL),
            size = label_size, color = txt_col, fontface = "bold",
            max.overlaps = 20, box.padding = 0.5, point.padding = 0.3,
            segment.color = txt_col, segment.alpha = 0.4,
            min.segment.length = 0.1,
            force = 0.5, force_pull = 0.3
          )
        } else {
          top_genes <- top_genes %>%
            mutate(label_x = log2FoldChange + ifelse(log2FoldChange > 0, 0.15, -0.15),
                   label_y = -log10(pvalue) + 0.3)

          p <- p +
            geom_text(data = top_genes, aes(x = label_x, y = label_y, label = SYMBOL),
                      size = label_size, color = txt_col, fontface = "bold",
                      check_overlap = TRUE, vjust = 0.5,
                      hjust = ifelse(top_genes$log2FoldChange > 0, 0, 1))
        }
      }
    }

    print(p)
  })


  # =====================================================
  # 原有输出：交互式网络图
  # =====================================================

  output$tf_network_plot_interactive <- renderPlotly({
    req(selected_tf_targets())

    # 强制响应自定义选项
    input$tf_network_node_size
    input$tf_network_label_size
    input$tf_tf_node_col
    input$tf_consistent_act_col
    input$tf_consistent_rep_col
    input$tf_inconsistent_act_col
    input$tf_inconsistent_rep_col
    input$tf_neutral_col
    input$theme_toggle

    node_size_mult <- input$tf_network_node_size %||% 1
    label_size <- input$tf_network_label_size %||% 3.5

    data_list <- selected_tf_targets()
    df <- data_list$data
    tf_name <- data_list$tf_name

    top_targets <- df %>%
      filter(!is.na(pvalue)) %>%
      arrange(pvalue, abs(log2FoldChange)) %>%
      head(min(30, nrow(df))) %>%
      mutate(
        node_type = "target",
        edge_color = ifelse(Mode_of_Regulation > 0, "#e74c3c", "#3498db"),
        node_size = ifelse(Is_DE == "Yes", 8, 5)
      )

    if (nrow(top_targets) < 2) return(NULL)

    n_targets <- nrow(top_targets)
    angles <- seq(0, 2*pi, length.out = n_targets + 1)[1:n_targets]
    top_targets$x <- 2 * cos(angles)
    top_targets$y <- 2 * sin(angles)

    top_targets$color <- ifelse(top_targets$Match_Status == "Consistent",
                                 ifelse(top_targets$Mode_of_Regulation > 0,
                                        input$tf_consistent_act_col,
                                        input$tf_consistent_rep_col),
                                 ifelse(top_targets$Match_Status == "Inconsistent",
                                        ifelse(top_targets$Mode_of_Regulation > 0,
                                               input$tf_inconsistent_act_col,
                                               input$tf_inconsistent_rep_col),
                                        input$tf_neutral_col))

    edges_list <- list()
    for (i in 1:n_targets) {
      edges_list[[i]] <- list(
        x = c(0, top_targets$x[i], NA),
        y = c(0, top_targets$y[i], NA),
        color = top_targets$edge_color[i],
        linetype = ifelse(top_targets$Match_Status[i] == "Consistent", "solid", "dashed")
      )
    }

    tf_node_data <- data.frame(x = 0, y = 0)

    p <- plot_ly() %>%
      add_trace(
        data = tf_node_data,
        x = ~x, y = ~y,
        type = 'scatter', mode = 'markers+text',
        name = tf_name,
        text = ~tf_name,
        textfont = list(size = label_size, color = if(input$theme_toggle) "white" else "black"),
        textposition = 'top center',
        marker = list(size = 12 * node_size_mult, color = input$tf_tf_node_col,
                      line = list(color = 'white', width = 2)),
        hoverinfo = 'text', showlegend = FALSE
      ) %>%
      add_trace(
        data = top_targets,
        x = ~x, y = ~y,
        type = 'scatter', mode = 'markers+text',
        name = 'Target Genes',
        text = ~SYMBOL,
        textfont = list(size = label_size * 0.8, color = if(input$theme_toggle) "white" else "black"),
        textposition = 'top center',
        hovertext = ~paste("Gene:", SYMBOL, "<br>",
                           "log2FC:", round(log2FoldChange, 3), "<br>",
                           "p-value:", format(pvalue, scientific = TRUE, digits = 3), "<br>",
                           "Status:", Match_Status),
        marker = list(size = ~node_size * node_size_mult, color = ~color,
                      line = list(color = 'white', width = 1)),
        hoverinfo = 'text', showlegend = FALSE
      ) %>%
      layout(
        title = paste("TF:", tf_name, "的靶基因调控网络"),
        showlegend = FALSE,
        xaxis = list(title = "", showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE, range = c(-3.5, 3.5)),
        yaxis = list(title = "", showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE, scaleanchor = "x", range = c(-3.5, 3.5)),
        plot_bgcolor = if(input$theme_toggle) "#2b2b2b" else "white",
        paper_bgcolor = if(input$theme_toggle) "#1a1a1a" else "white",
        font = list(color = if(input$theme_toggle) "white" else "black"),
        hovermode = 'closest', dragmode = 'move'
      )

    for (i in 1:length(edges_list)) {
      edge_data <- data.frame(x = edges_list[[i]]$x, y = edges_list[[i]]$y)
      p <- p %>% add_trace(
        data = edge_data, x = ~x, y = ~y,
        type = 'scatter', mode = 'lines',
        line = list(color = edges_list[[i]]$color, width = 2),
        showlegend = FALSE, hoverinfo = 'skip', inherit = FALSE
      )
    }

    p
  })

  # =====================================================
  # 原有输出：静态网络图
  # =====================================================

  output$tf_network_plot <- renderPlot({
    req(selected_tf_targets())

    # Explicit reactive dependencies
    input$tf_network_node_size
    input$tf_network_label_size
    input$tf_tf_node_col
    input$tf_consistent_act_col
    input$tf_consistent_rep_col
    input$tf_inconsistent_act_col
    input$tf_inconsistent_rep_col
    input$tf_neutral_col

    node_size_mult <- input$tf_network_node_size %||% 1
    label_size <- input$tf_network_label_size %||% 3.5

    data_list <- selected_tf_targets()
    df <- data_list$data
    tf_name <- data_list$tf_name

    top_targets <- df %>%
      filter(!is.na(pvalue)) %>%
      arrange(pvalue, abs(log2FoldChange)) %>%
      head(min(30, nrow(df))) %>%
      mutate(
        node_type = "target",
        edge_color = ifelse(Mode_of_Regulation > 0, "#e74c3c", "#3498db"),
        node_size = ifelse(Is_DE == "Yes", 8, 5)
      )

    if (nrow(top_targets) < 2) {
      plot.new()
      title(main = paste("TF:", tf_name, "- 数据不足以绘制网络图"))
      return()
    }

    tf_node <- data.frame(name = tf_name, node_type = "tf", x = 0, y = 0,
                          node_size = 12, color = input$tf_tf_node_col)

    n_targets <- nrow(top_targets)
    angles <- seq(0, 2*pi, length.out = n_targets + 1)[1:n_targets]
    top_targets$x <- 2 * cos(angles)
    top_targets$y <- 2 * sin(angles)

    top_targets$color <- ifelse(top_targets$Match_Status == "Consistent",
                                 ifelse(top_targets$Mode_of_Regulation > 0,
                                        input$tf_consistent_act_col,
                                        input$tf_consistent_rep_col),
                                 ifelse(top_targets$Match_Status == "Inconsistent",
                                        ifelse(top_targets$Mode_of_Regulation > 0,
                                               input$tf_inconsistent_act_col,
                                               input$tf_inconsistent_rep_col),
                                        input$tf_neutral_col))

    all_nodes <- rbind(
      tf_node[, c("name", "x", "y", "node_size", "color")],
      top_targets[, c("SYMBOL", "x", "y", "node_size", "color")] %>% rename(name = SYMBOL)
    )

    edges <- data.frame(
      x = rep(0, n_targets), y = rep(0, n_targets),
      xend = top_targets$x, yend = top_targets$y,
      color = top_targets$edge_color,
      linetype = ifelse(top_targets$Match_Status == "Consistent", "solid", "dashed")
    )

    txt_col <- if(input$theme_toggle) "white" else "black"
    all_nodes$node_size_scaled <- all_nodes$node_size * node_size_mult

    p <- ggplot() +
      geom_segment(data = edges, aes(x = x, xend = xend, y = y, yend = yend,
                                      color = color, linetype = linetype),
                   linewidth = 0.8, alpha = 0.6) +
      geom_point(data = all_nodes, aes(x = x, y = y, size = node_size_scaled, color = color), alpha = 0.9) +
      geom_text(data = all_nodes, aes(x = x, y = y, label = name),
                size = label_size, fontface = "bold", color = txt_col,
                vjust = ifelse(all_nodes$y > 0, -0.5, 1.5), check_overlap = TRUE) +
      scale_color_identity() + scale_linetype_identity() + scale_size_identity() +
      labs(title = paste("TF:", tf_name, "的靶基因调控网络"),
           subtitle = paste("Top", n_targets, "靶基因 | 红色=激活, 蓝色=抑制, 实线=一致, 虚线=不一致")) +
      theme_minimal() +
      theme(
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        plot.title = element_text(color = txt_col, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(color = txt_col, hjust = 0.5),
        axis.title = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), panel.grid = element_blank(),
        legend.position = "none"
      ) +
      coord_equal() + xlim(-3, 3) + ylim(-3, 3)

    print(p)
  })

  # =====================================================
  # 原有输出：SVG 导出功能
  # =====================================================

  output$download_tf_network_svg <- downloadHandler(
    filename = function() {
      req(selected_tf_targets())
      paste0("TF_Network_", selected_tf_targets()$tf_name, "_", Sys.Date(), ".svg")
    },
    content = function(file) {
      req(selected_tf_targets())

      data_list <- selected_tf_targets()
      df <- data_list$data
      tf_name <- data_list$tf_name

      top_targets <- df %>%
        filter(!is.na(pvalue)) %>%
        arrange(pvalue, abs(log2FoldChange)) %>%
        head(min(30, nrow(df))) %>%
        mutate(node_type = "target",
               edge_color = ifelse(Mode_of_Regulation > 0, "#e74c3c", "#3498db"),
               node_size = ifelse(Is_DE == "Yes", 8, 5))

      if (nrow(top_targets) < 2) return()

      tf_node <- data.frame(name = tf_name, node_type = "tf", x = 0, y = 0,
                            node_size = 12, color = input$tf_tf_node_col)

      n_targets <- nrow(top_targets)
      angles <- seq(0, 2*pi, length.out = n_targets + 1)[1:n_targets]
      top_targets$x <- 2 * cos(angles)
      top_targets$y <- 2 * sin(angles)
      top_targets$color <- ifelse(top_targets$Match_Status == "Consistent",
                                   ifelse(top_targets$Mode_of_Regulation > 0,
                                          input$tf_consistent_act_col,
                                          input$tf_consistent_rep_col),
                                   ifelse(top_targets$Match_Status == "Inconsistent",
                                          ifelse(top_targets$Mode_of_Regulation > 0,
                                                 input$tf_inconsistent_act_col,
                                                 input$tf_inconsistent_rep_col),
                                          input$tf_neutral_col))

      all_nodes <- rbind(
        tf_node[, c("name", "x", "y", "node_size", "color")],
        top_targets[, c("SYMBOL", "x", "y", "node_size", "color")] %>% rename(name = SYMBOL)
      )

      edges <- data.frame(
        x = rep(0, n_targets), y = rep(0, n_targets),
        xend = top_targets$x, yend = top_targets$y,
        color = top_targets$edge_color,
        linetype = ifelse(top_targets$Match_Status == "Consistent", "solid", "dashed")
      )

      all_nodes$node_size_scaled <- all_nodes$node_size * input$tf_network_node_size

      p <- ggplot() +
        geom_segment(data = edges, aes(x = x, xend = xend, y = y, yend = yend,
                                        color = color, linetype = linetype),
                     linewidth = 0.8, alpha = 0.6) +
        geom_point(data = all_nodes, aes(x = x, y = y, size = node_size_scaled, color = color), alpha = 0.9) +
        geom_text(data = all_nodes, aes(x = x, y = y, label = name),
                  size = input$tf_network_label_size, fontface = "bold",
                  color = "black", vjust = ifelse(all_nodes$y > 0, -0.5, 1.5), check_overlap = TRUE) +
        scale_color_identity() + scale_linetype_identity() + scale_size_identity() +
        labs(title = paste("TF:", tf_name, "的靶基因调控网络"),
             subtitle = paste("Top", n_targets, "靶基因")) +
        theme_minimal() +
        theme(panel.background = element_rect(fill = "white", colour = NA),
              plot.title = element_text(face = "bold", hjust = 0.5),
              axis.title = element_blank(), axis.text = element_blank(),
              axis.ticks = element_blank(), panel.grid = element_blank()) +
        coord_equal() + xlim(-3, 3) + ylim(-3, 3)

      svg(file, width = 10, height = 8)
      print(p)
      dev.off()
    },
    contentType = "image/svg+xml"
  )

  output$download_tf_scatter_svg <- downloadHandler(
    filename = function() {
      req(selected_tf_targets())
      paste0("TF_Scatter_", selected_tf_targets()$tf_name, "_", Sys.Date(), ".svg")
    },
    content = function(file) {
      req(selected_tf_targets())

      data_list <- selected_tf_targets()
      df <- data_list$data
      tf_name <- data_list$tf_name

      point_size <- input$tf_scatter_point_size %||% 3
      point_alpha <- input$tf_scatter_alpha %||% 0.7
      label_size <- input$tf_scatter_label_size %||% 3
      n_labels <- input$tf_scatter_n_labels %||% 15

      p <- ggplot(df, aes(x = log2FoldChange, y = -log10(pvalue))) +
        geom_point(aes(color = Match_Status), size = point_size, alpha = point_alpha) +
        scale_color_manual(
          values = c("Consistent" = "#2ecc71", "Inconsistent" = "#e74c3c", "Neutral/Unknown" = "#95a5a6"),
          name = "调控一致性"
        ) +
        geom_vline(xintercept = 0, linetype = "dashed", color = "black", alpha = 0.7) +
        labs(title = paste("TF:", tf_name, "的靶基因差异表达"),
             x = "log2(Fold Change)", y = "-log10(P Value)") +
        theme_minimal() +
        theme(panel.background = element_rect(fill = "white", colour = NA),
              plot.title = element_text(face = "bold", hjust = 0.5),
              axis.title = element_text(face = "bold"))

      if (n_labels > 0) {
        top_genes <- df %>% filter(!is.na(pvalue)) %>% arrange(pvalue) %>% head(min(n_labels, nrow(df)))
        if (nrow(top_genes) > 0) {
          top_genes <- top_genes %>%
            mutate(label_x = log2FoldChange + ifelse(log2FoldChange > 0, 0.2, -0.2),
                   label_y = -log10(pvalue) + 0.5)
          p <- p + geom_text(data = top_genes, aes(x = label_x, y = label_y, label = SYMBOL),
                             size = label_size, color = "black", fontface = "bold",
                             check_overlap = TRUE, vjust = 0.5,
                             hjust = ifelse(top_genes$log2FoldChange > 0, 0, 1))
        }
      }

      svg(file, width = 10, height = 8)
      print(p)
      dev.off()
    },
    contentType = "image/svg+xml"
  )

  output$download_tf_scatter_data <- downloadHandler(
    filename = function() {
      req(selected_tf_targets())
      paste0("TF_Target_Genes_", selected_tf_targets()$tf_name, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(selected_tf_targets())
      data_list <- selected_tf_targets()
      df <- data_list$data
      tf_name <- data_list$tf_name

      write.csv(df, file, row.names = FALSE)
    },
    contentType = "text/csv"
  )

  # =====================================================
  # 🆕 新增输出：TF 家族富集分析
  # =====================================================

  # 运行 TF 家族富集分析
  tf_family_results <- eventReactive(input$run_tf_family_enrichment, {
    req(tf_activity_results(), tf_family_map_data())

    showNotification("正在运行 TF 家族富集分析...", type = "message")

    results <- tf_family_enrichment(
      tf_results = tf_activity_results(),
      tf_family_map = tf_family_map_data(),
      active_top_n = input$tf_family_top_n %||% 20,
      pvalue_cutoff = input$tf_family_pvalue %||% 0.05
    )

    if (is.null(results)) {
      showNotification("TF 家族富集分析失败: 数据不足或家族注释太少。", type = "error")
      return(NULL)
    }

    showNotification(
      paste("TF 家族富集分析完成! (", nrow(results), " 个家族)"),
      type = "message"
    )

    return(results)
  })

  # 🆕 TF 家族富集结果表
  output$tf_family_table <- DT::renderDataTable({
    req(tf_family_results())

    df <- tf_family_results() %>%
      select(Family, N_Family_TFs, N_Foreground, N_Background, Fold_Enrichment, PValue, Padj, Significance) %>%
      rename(
        家族 = Family,
        家族TF数 = N_Family_TFs,
        前景命中 = N_Foreground,
        背景命中 = N_Background,
        富集倍数 = Fold_Enrichment,
        P值 = PValue,
        校正P值 = Padj,
        显著性 = Significance
      )

    DT::datatable(df, options = list(scrollX = TRUE, pageLength = 15), rownames = FALSE) %>%
      formatRound(c("富集倍数"), 3) %>%
      formatRound(c("P值", "校正P值"), 4) %>%
      formatStyle(
        "显著性",
        backgroundColor = styleEqual(
          c("***", "**", "*", "ns"),
          c("#d4edda", "#fff3cd", "#fff3cd", "#f8f9fa")
        ),
        fontWeight = styleEqual("***", "bold")
      )
  })

  # 🆕 TF 家族富集柱状图
  output$tf_family_bar_plot <- renderPlot({
    req(tf_family_results())

    df <- tf_family_results() %>%
      filter(Padj < 0.1) %>%
      head(20)

    if (nrow(df) == 0) {
      plot.new()
      title(main = "没有显著的 TF 家族 (Padj < 0.1)")
      return()
    }

    txt_col <- if(input$theme_toggle) "white" else "black"
    grid_col <- if(input$theme_toggle) "#444444" else "#cccccc"

    input$tf_family_font_size
    input$tf_family_bar_width

    ggplot(df, aes(x = reorder(Family, -log10(Padj)), y = Fold_Enrichment)) +
      geom_col(aes(fill = -log10(Padj)), width = input$tf_family_bar_width %||% 0.7) +
      geom_hline(yintercept = 1, linetype = "dashed", color = "gray50", linewidth = 0.5) +
      scale_fill_gradient(low = "#3498db", high = "#e74c3c", name = expression(-log[10](italic(Padj)))) +
      labs(
        title = "TF 家族富集分析",
        subtitle = paste("显示 Top", nrow(df), "个显著家族"),
        x = "TF 家族",
        y = "富集倍数 (Fold Enrichment)"
      ) +
      theme_minimal(base_size = input$tf_family_font_size %||% 11) +
      theme(
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        plot.title = element_text(color = txt_col, face = "bold", hjust = 0.5,
                                   size = (input$tf_family_font_size %||% 11) * 1.3),
        plot.subtitle = element_text(color = txt_col, hjust = 0.5,
                                      size = (input$tf_family_font_size %||% 11) * 0.9),
        axis.title = element_text(color = txt_col, face = "bold",
                                   size = (input$tf_family_font_size %||% 11) * 1.05),
        axis.text.x = element_text(angle = 45, hjust = 1, color = txt_col, face = "bold",
                                    size = (input$tf_family_font_size %||% 11) * 0.9),
        axis.text.y = element_text(color = txt_col,
                                    size = (input$tf_family_font_size %||% 11) * 0.9),
        legend.text = element_text(color = txt_col,
                                    size = (input$tf_family_font_size %||% 11) * 0.85),
        legend.title = element_text(color = txt_col, face = "bold",
                                    size = (input$tf_family_font_size %||% 11) * 0.9),
        panel.grid.major = element_line(color = grid_col, linewidth = 0.3),
        panel.grid.minor = element_blank()
      )
  })

  # 🆕 TF 家族点图（气泡图）
  output$tf_family_dot_plot <- renderPlot({
    req(tf_family_results())

    df <- tf_family_results() %>%
      filter(Padj < 0.1) %>%
      head(20)

    if (nrow(df) == 0) {
      plot.new()
      title(main = "没有显著的 TF 家族 (Padj < 0.1)")
      return()
    }

    txt_col <- if(input$theme_toggle) "white" else "black"
    grid_col <- if(input$theme_toggle) "#444444" else "#cccccc"

    input$tf_family_font_size

    ggplot(df, aes(x = Fold_Enrichment, y = reorder(Family, Fold_Enrichment))) +
      geom_point(aes(size = N_Family_TFs, color = -log10(Padj)), alpha = 0.8, shape = 21, stroke = 0.5) +
      scale_color_gradient(low = "#3498db", high = "#e74c3c", name = expression(-log[10](italic(Padj)))) +
      scale_size_continuous(name = "家族TF数", range = c(3, 15)) +
      scale_fill_gradient(low = "#3498db", high = "#e74c3c", guide = "none") +
      geom_vline(xintercept = 1, linetype = "dashed", color = "gray50", linewidth = 0.5) +
      labs(
        title = "TF 家族富集气泡图",
        x = "富集倍数 (Fold Enrichment)",
        y = "TF 家族"
      ) +
      theme_minimal(base_size = input$tf_family_font_size %||% 11) +
      theme(
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        plot.title = element_text(color = txt_col, face = "bold", hjust = 0.5,
                                   size = (input$tf_family_font_size %||% 11) * 1.3),
        axis.title = element_text(color = txt_col, face = "bold",
                                   size = (input$tf_family_font_size %||% 11) * 1.05),
        axis.text = element_text(color = txt_col, face = "bold",
                                  size = (input$tf_family_font_size %||% 11) * 0.9),
        legend.text = element_text(color = txt_col,
                                    size = (input$tf_family_font_size %||% 11) * 0.85),
        legend.title = element_text(color = txt_col, face = "bold",
                                    size = (input$tf_family_font_size %||% 11) * 0.9),
        panel.grid.major = element_line(color = grid_col, linewidth = 0.3),
        panel.grid.minor = element_blank()
      )
  })

  # 🆕 TF 家族活性概览（Lollipop 图）
  output$tf_family_lollipop_plot <- renderPlot({
    req(tf_activity_results(), tf_family_map_data())

    tf_res <- tf_activity_results()
    fam_map <- tf_family_map_data()

    # 合并 TF 活性与家族信息
    tf_with_family <- tf_res %>%
      left_join(fam_map, by = c("source" = "TF")) %>%
      filter(!is.na(Family))

    if (nrow(tf_with_family) == 0) {
      plot.new()
      title(main = "没有足够的 TF 家族注释数据")
      return()
    }

    # 计算每个家族的平均活性
    family_activity <- tf_with_family %>%
      group_by(Family) %>%
      summarise(
        Mean_Score = mean(score, na.rm = TRUE),
        N_TFs = n(),
        N_Active = sum(score > 0, na.rm = TRUE),
        N_Inactive = sum(score < 0, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      filter(N_TFs >= 2) %>%
      arrange(desc(abs(Mean_Score))) %>%
      head(25)

    txt_col <- if(input$theme_toggle) "white" else "black"
    grid_col <- if(input$theme_toggle) "#444444" else "#cccccc"

    input$tf_family_font_size

    ggplot(family_activity, aes(x = reorder(Family, Mean_Score), y = Mean_Score)) +
      geom_segment(aes(x = Family, xend = Family, y = 0, yend = Mean_Score),
                   linewidth = 0.8, color = if(input$theme_toggle) "#666666" else "gray60") +
      geom_point(aes(color = Mean_Score > 0, size = N_TFs), alpha = 0.9, shape = 21, stroke = 0.8) +
      scale_color_manual(
        values = c("TRUE" = "#e74c3c", "FALSE" = "#3498db"),
        name = "活性方向",
        labels = c("抑制", "激活")
      ) +
      scale_size_continuous(name = "TF 数量", range = c(3, 12)) +
      scale_fill_manual(values = c("TRUE" = "#e74c3c", "FALSE" = "#3498db"), guide = "none") +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.5) +
      coord_flip() +
      labs(
        title = "TF 家族平均活性 (Lollipop 图)",
        x = "TF 家族",
        y = "平均活性分数"
      ) +
      theme_minimal(base_size = input$tf_family_font_size %||% 11) +
      theme(
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        plot.title = element_text(color = txt_col, face = "bold", hjust = 0.5,
                                   size = (input$tf_family_font_size %||% 11) * 1.3),
        axis.title = element_text(color = txt_col, face = "bold",
                                   size = (input$tf_family_font_size %||% 11) * 1.05),
        axis.text = element_text(color = txt_col, face = "bold",
                                  size = (input$tf_family_font_size %||% 11) * 0.9),
        legend.text = element_text(color = txt_col,
                                    size = (input$tf_family_font_size %||% 11) * 0.85),
        legend.title = element_text(color = txt_col, face = "bold",
                                    size = (input$tf_family_font_size %||% 11) * 0.9),
        panel.grid.major.y = element_line(color = grid_col, linewidth = 0.3),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()
      )
  })

  # 🆕 下载 TF 家族富集结果
  output$download_tf_family_results <- downloadHandler(
    filename = function() {
      paste0("TF_Family_Enrichment_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(tf_family_results())
      write.csv(tf_family_results(), file, row.names = FALSE)
    }
  )

  # =====================================================
  # 返回 TF 活性结果供其他模块使用
  # =====================================================

  return(tf_activity_results)
}
