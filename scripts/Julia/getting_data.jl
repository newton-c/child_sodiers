using WorldBankData

wb_data = wdi(["SI.POV.GINI", "VC.IHR.PSRC.P5", "SL.UEM.TOTL.NE.ZS",
              "SE.XPD.TOTL.GD.ZS"], "all", 1989, 2018)

rename!(wb_data, Dict(:SI_POV_GINI => :gini, :VC_IHR_PSRC_P5 => :hom_rate,
                      :SL_UEM_TOTL_NE_ZS => :unemployment,
                      :SE_XPD_TOTL_GD_ZS => :education))
