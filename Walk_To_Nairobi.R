library(httr)
library(jsonlite)
library(googleway)
library(geosphere)
library(dplyr)




# Get Data From Kobo
form_uid <- "auntyPb3NY5QiFnPNUXkVn"
kpi_url <- "https://kf.kobotoolbox.org/assets/"
user = "ciaramc"
password = "BP074912"
resp <- GET(
  paste(kpi_url, form_uid, "submissions", sep="/"),
  authenticate(user, password),
  content_type("text/json"),
  accept_json()
)
dataAsText <- content(resp, as="text")
dataAsLists <- content(resp, as="parsed")
dataAsFrame <- fromJSON(dataAsText)




# Distance Calculations
dataAsFrame$steps_daily_total <- as.numeric(dataAsFrame$steps_daily_total)
dataAsFrame$steps_weekly_total <- as.numeric(dataAsFrame$steps_weekly_total)
dataAsFrame$cycle_km <- as.numeric(dataAsFrame$cycle_km)
dataAsFrame$swin_meters <- as.numeric(dataAsFrame$swin_meters)

distance_progress <- dataAsFrame %>% 
    summarise(total_steps = sum(steps_daily_total, na.rm = TRUE) + sum(steps_weekly_total, na.rm = TRUE), total_steps_km = total_steps/1500, total_cycle_km = sum(cycle_km, na.rm = TRUE), total_swim_km = (sum(swin_meters, na.rm = TRUE)/1000), total_distance = (total_steps_km + total_cycle_km + total_swim_km), distance_left = (11503.010 - total_distance))

distance_progress$total_distance <- round(distance_progress$total_distance, 0)
distance_progress$distance_left <- round(distance_progress$distance_left, 0)

# Polylines
api_key <- ("AIzaSyB1G-GepQCSqNUZ7KcZ--YokEHUJt8OPOE")

# Route Polyline Part 1

#google_directions(origin = "6 South Street, Reading, UK",
#                  destination = "Niamey, Niger",
#                  mode = "walking",
#                  waypoints = list(stop = c(50.909764, -1.404223), via = "Orleans, France", via = "Barcelona", via = "Bamako, Mali", via = "Ouagadougou, Burkina Faso"),
#                  key = api_key)

pl1 <- "mh`yHxo{Dd~{@f_w@zt^bfYhfR}bQnqLwhg@bsEfp@dmjCccnBtouCsru@jh@xNde@qEnhFz_Cbe@y}@vk@miIMybEvr@_wWjaJgtVtsF}{f@lnhAk_sBb{v@efmBxhyAihnAh~ZoeId`w@~dTzfp@liTdlh@hpGhts@_sG|g\\~sQjvmAn{a@ludAfpRrn~Brba@nfs@}gUlyr@ksJrnn@ouLdzl@x`L~fnBaaT`}q@omBxnk@cza@hse@}[xuZvf@jbToz^`pRguK`rNbtGlq`@{}YxhOerObcMah\\hnU`wFv|n@wuB~dmA{rGbmh@dsh@xcFj{^ho_@hkgCjja@bpeA|evAtc_Alk{AbhoA`|y@hzv@t{eB~b_@fxfAjv\\bx\\~uFpbn@{kUpxEilBtqBrH~l@jPzsnA_ib@lnlLdsz@`gA|Sfr@qdBrpDqoBru]sd@|t[|zJjwZ`Bref@{wClvr@i|b@pwk@og_@lv_A_lx@`oc@qpFjvpAhww@ha\\bpJbt[j}A|xkAtlv@~k[n\\prOhuc@tc{@bpE`gj@jab@~hXxs|AfvTz||@`ox@wtFt{cCvcv@zuqAfwr@vnPhaAnmS~nMdyp@kuPfz|@wwfBfnv@o|~@xsbB{ufBpxUqlfBtexAygi@jebBr}Xtgv@pAtpnB_fn@lq`Aivm@djnDmlQ`yiFg~wArdfDcax@`slAvrAhtpAmwRp{zCoi@xqdAxm[deoAhxG`k]}eV~kZunFtq_B`qSj{cBtkJhtmArwFznw@bvBrb~@p`KpyjAmy@h`~@d~e@|lvAtcv@b_n@jaYzbiAsdA`zfC~gLbin@du`@`wSxoFkkCjrq@fxE~g}@zs\\jgjAlfp@bes@jdlAvc\\lxRvtz@l|_@tsoA_cD`fcAryTlecAefIx|iAqpKhsj@lyU~p]zpe@~`x@puaA|g_@x`|@`cBtxb@`zSr}RpyMjrEnoe@mbOdy]rrk@jmuBf{UjiKifDvpVlHjtx@xnQvu`Ahua@fy~@bzw@|oq@pkp@`tv@lsMllzAhxYbadAevGrw_@hzE~oBrgAaef@aoSojhApqCksVlt[eulAdwGsntAxcHkqVriLc_]i}CqrQgTawWsyBkj]pqI{c~@fkQoxj@ewRmdvAsZicmDq|Qe}lAbpZ_cw@ff[aqwAkpLonbAqoXgl{@aaAgopA_xGi`w@h|Gs`u@~cPy|d@hkIqvv@bdc@uegEdcBoqcAkfaAm`z@o{Ou`tAopy@qd]ozkBc}v@}y_@om\\}xAeyE"
polyline1 <- decode_pl(pl1)
polyline1

# Route Polyline Part 2

#google_directions(origin = "Niamey, Niger",
#                  destination = "Nairobi, Kenya",
#                  mode = "walking",
#                  waypoints = list(via = "Kampala, Uganda"),
#                  key = api_key)  

pl2 <- "y}mqA{a~Kyti@uzkCkPkgm@psUssZryM{md@x|J{eW{fH}fJ{pUa}`AqfAcj\\sx^c_bBgjD_po@xrI}le@_tDusm@q{Ywzv@_vQadiAvxe@wx|@xwH_f]~t]m|Unu@otTeqFkgS`fCuz\\f|Nex_@tuV}qL|c}@wtVvdVw_|@t}QigNdq{@wxYx}\\iqh@d`|@cqiAzuh@o{p@rj\\amg@xhXqpKvxKk`c@leX_ffApqLuiIfgNchfAbxTkqz@v|Mib_@xrVy`L~r]_zc@~tCy|a@f{eAub~@z_]c{JroCwdL|t`@ssjAdiOwiEllC_|HetIcmTnoWed[zwyAuoPjkc@}xj@xtF_vc@j}Mii]{_Bg}UnjKiq]pjm@loC|kl@zmGd_Nsxx@vkN}am@wl@_maAm`G{}e@f~A{``@lxKq}ZvvJkRta@cmN~uRa`_@bst@_rd@vjZwwj@btNah`@z~Bw{l@`rK{cO~c^yfLr_Ssei@bsRqlU`hJexKc`BohXzjMkd`ApjH{wUjyOzcA~ey@upe@jvOsxa@n~XyfYfuPsiZnBabSxvFuua@j\\qgu@|qAoly@eyKu{z@yk@mhu@xrGe}w@n|X{eu@~tImzj@|x_A_`eB|bYw__@vjFkda@_hIoxr@faJwdr@~kj@af`@xkUwbOdqPq|Wzdh@_tDv}Pyf]pgy@yw`@`oWsqeAmwLe_p@xuDwyq@xu@qrd@oYqrU_zHkjLyxFopSxmMsdLfu^n_DfvWuqNj{Yz|HhhEyrPjdIeza@lhSgcr@||j@en|@`f[axInf_@q`BtmXaxV|pFu|OhzKwpZfuo@ujEzxMorD~_K{ae@j`Gs~`@zH}{MhvFe`Xt`[atN~abA{|^vtVetp@blKira@_Fyi]bwKsqUggAmz^nqTm}WluYg}PurHksWlyS_pv@dmE{~OxnUm}OimBak_AxpVadJ|{`@vvDno{@_d\\khJy}_@ckIg`Ucp_@cmWeiOeh`@xfAmjp@kzD_pr@|uCuj_@~nB_~e@ei@i|Rl_JwkV{jE{`^veL{uRjah@ilMlt{AyyVpaSg~Qc|AwaU{zMo^wnLmvIge@e_Et{@ugK_oM_xVzyNweWnTute@zxCynVwyAe|S`cHg_fAfrUskjAcxGqimAxwVuinBd_KckiBu|KmykBo{Uk~lAc_C{w~@xcOyaj@bwJsfgAhcXyns@bdQsaw@cvBub{@`nTuwc@reNgbDfcGwkO~lf@ys[rcJqb|@`|h@kfm@rcs@smYv`lAawg@|bYoo_@"  
polyline2 <- decode_pl(pl2)
polyline2

#Total Route Polyline
route_polyline <- rbind(polyline1, polyline2[2:193,])
route_polyline

# Progress Polyline
start_locations <- route_polyline[1:376,]
end_locations <- route_polyline[2:377,]
end_locations <- end_locations %>% rename(lat2 = lat, lon2 = lon)
coordinates <-cbind(start_locations, end_locations)
coordinates <- coordinates[c(2,1,4,3)]
bitsOfDistances <- apply(coordinates, 1, function(row){distm(row [1:2], row [3:4])})
coordinates$cumDistance <- (cumsum(bitsOfDistances))/1000
coordinates

current_location <- coordinates %>%
  filter(cumDistance >= distance_progress$total_distance) %>%
  select(lat, lon, lat2, lon2, cumDistance) %>%
  arrange(cumDistance) %>%
  head(1)
current_location

progress_polyline <- coordinates %>%
  filter(cumDistance <= current_location$cumDistance) %>%
  select(lat, lon)
progress_polyline

# Route Still to Complete Polyline
other_polyline <- coordinates %>%
  filter(cumDistance >= current_location$cumDistance) %>%
  select(lat, lon)