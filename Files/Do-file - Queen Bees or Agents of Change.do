* ##################### Paper Information
* Journal: Submission to AMJ - Academy of Management Journal (2024)
* Title: Queen Bees or Agents of Change? Unpacking the Effect of Female Leadership on Subordinate Women
* Authors: Henrique Minetto Brabo; Fernando Deodato Domingos; Paulo Roberto Arvate 
* Corresponding Author: Paulo Roberto Arvate: paulo.arvate@fgv.br 

* ##################### Technical Information
* Github Repository: https://github.com/brbhm/Queen_Bees_or_Agents_of_Change
* Stata Version: Stata 17 - 64 MP
* RAM Memory used to perform calculations: 32GB
* This version's date: 29th Nov 2024

* ##################### Disclaimer About the Dataset
* The dataset mainly consists in agregated data from the brazilian work labor census (RAIS - "Relação Anual de Informações Sociais"). This dataset used to be publicly available but was removed from public access due to the LGPD ("Lei Geral de Proteção de Dados"), the data protection law (similar to the GDPR). This is the reason that the researchers are not allowed to share the original versions (identified or identifiable) of those datasets.
* If you have access to the original dataset (tables with identified individuals, by year and company - "RAIS Vínculos"), we are glad to help replicating the construction of the consolidated databases.

* ############################################################################################################################################################################################
* ############################################################################################################################################################################################
* ############################################################################################################################################################################################
* ############################################################################################################################################################################################
* ############################################################################################################################################################################################
* ############################################################################################################################################################################################
* ############################################################################################################################################################################################
* ############################################################################################################################################################################################
* ######################################## PACKAGES

	clear
	set more off, permanently

	clear all
	capture log close

	adopath + "../ado/"

* ############################################################################################################################################################################################
* ############################################################################################################################################################################################
* ############################################################################################################################################################################################
* ############################################################################################################################################################################################
* ############################################################################################################################################################################################
* ############################################################################################################################################################################################
* ############################################################################################################################################################################################
* ############################################################################################################################################################################################
* ######################################## PROPENSITY SCORE MATCHING
{
use econometry_database.dta , clear

 * ############################################### COMPANY SIZE

capture drop media_colab
capture drop d_tam_emp_acima1500	
bysort CNPJ_raiz: egen media_colab = mean(qtd_colab_ativ) if qtd_colab_ativ > 0
replace media_colab = 0 if media_colab == .
bysort CNPJ_raiz: gen d_tam_emp_acima1500 = 1 if media_colab >= 1500
tab d_tam_emp_acima1500
drop if d_tam_emp_acima1500 == .
capture drop media_colab			

* Perform the PSmatchs for the econometrics

capture log close
log using log_ps_scores.log, replace

* Admissões
capture drop  _pscore
capture drop _treated 
capture drop _support 
capture drop _wt1const 
capture drop _weight
capture drop _perc_mul_cargo_diretoria 
capture drop _perc_mul_adm_no_ano 
capture drop _wage_gap_fx_0 
capture drop _mov_vert_mulher_ano_ant 
capture drop _perc_mul_demitida_no_ano 
capture drop _id 
capture drop _n1 
capture drop _nn 
capture drop _pdif
capture drop aux_wt_admissoes
capture drop _wt_admissoes

psmatch2 tratamento_alguma_hora perc_mulheres qtd_colab_ativ perc_raca_cor_branca_mul perc_idade_20_a_30_mul perc_idade_30_a_40_mul perc_idade_40_a_50_mul i.ef_industria if tempo == 0 & trat_cont == 1, out(perc_mul_adm_no_ano) n(1) norepl cal(0.01) 
pstest, detail onlysig
rename _weight aux_wt_admissoes
bysort CNPJ_raiz: egen _wt_admissoes = max(aux_wt_admissoes)
replace _wt_admissoes=0 if _wt_admissoes==.

tab _wt_admissoes
capture drop aux_wt_admissoes


* Demissões
capture drop  _pscore
capture drop _treated 
capture drop _support 
capture drop _wt1const 
capture drop _weight
capture drop _perc_mul_cargo_diretoria 
capture drop _perc_mul_adm_no_ano 
capture drop _wage_gap_fx_0 
capture drop _mov_vert_mulher_ano_ant 
capture drop _perc_mul_demitida_no_ano 
capture drop _id 
capture drop _n1 
capture drop _nn 
capture drop _pdif
capture drop aux_wt_demissoes
capture drop wt_demissoes

psmatch2 tratamento_alguma_hora perc_mulheres qtd_colab_ativ perc_raca_cor_branca_mul perc_idade_20_a_30_mul perc_idade_30_a_40_mul perc_idade_40_a_50_mul i.ef_industria if tempo == 0 & trat_cont == 1, out(perc_mul_demitida_no_ano) n(1) norepl  cal(0.01) 
pstest, detail onlysig
rename _weight aux_wt_demissoes
bysort CNPJ_raiz: egen _wt_demissoes = max(aux_wt_demissoes)
replace _wt_demissoes=0 if _wt_demissoes==.
 
tab _wt_demissoes
capture drop aux_wt_demissoes

* Wage Gap
capture drop  _pscore
capture drop _treated 
capture drop _support 
capture drop _wt1const 
capture drop _weight
capture drop _perc_mul_cargo_diretoria 
capture drop _perc_mul_adm_no_ano 
capture drop _wage_gap_fx_0 
capture drop _mov_vert_mulher_ano_ant 
capture drop _perc_mul_demitida_no_ano 
capture drop _id 
capture drop _n1 
capture drop _nn 
capture drop _pdif
capture drop aux_wt_wagegap
capture drop _wt_wagegap

psmatch2 tratamento_alguma_hora perc_mulheres qtd_colab_ativ perc_raca_cor_branca_mul perc_idade_20_a_30_mul perc_idade_30_a_40_mul perc_idade_40_a_50_mul i.ef_industria if tempo == 0 & trat_cont == 1, out(wage_gap_fx_0) n(1) norepl cal(0.01) 
pstest, detail onlysig
rename _weight aux_wt_wagegap
bysort CNPJ_raiz: egen _wt_wagegap = max(aux_wt_wagegap)
replace _wt_wagegap=0 if _wt_wagegap==.
 
tab _wt_wagegap
capture drop aux_wt_wagegap

* Vertical Mobility
capture drop  _pscore
capture drop _treated 
capture drop _support 
capture drop _wt1const 
capture drop _weight
capture drop _perc_mul_cargo_diretoria 
capture drop _perc_mul_adm_no_ano 
capture drop _wage_gap_fx_0 
capture drop _mov_vert_mul_fx_0 
capture drop _perc_mul_demitida_no_ano 
capture drop _id 
capture drop _n1 
capture drop _nn 
capture drop _pdif
capture drop aux_wt_mobvert
capture drop _wt_mobvert

psmatch2 tratamento_alguma_hora perc_mulheres qtd_colab_ativ perc_raca_cor_branca_mul perc_idade_20_a_30_mul perc_idade_30_a_40_mul perc_idade_40_a_50_mul i.ef_industria if tempo == 0 & trat_cont == 1, out(mov_vert_mul_fx_0) n(1) norepl cal(0.01) 
pstest, detail onlysig
rename _weight aux_wt_mobvert
bysort CNPJ_raiz: egen _wt_mobvert = max(aux_wt_mobvert)
replace _wt_mobvert=0 if _wt_mobvert==.
 
tab _wt_mobvert
capture drop aux_wt_mobvert

capture drop  _pscore
capture drop _treated 
capture drop _support 
capture drop _wt1const 
capture drop _weight
capture drop _perc_mul_cargo_diretoria 
capture drop _perc_mul_adm_no_ano 
capture drop _wage_gap_fx_0 
capture drop _mov_vert_mul_fx_0 
capture drop _perc_mul_demitida_no_ano 
capture drop _id 
capture drop _n1 
capture drop _nn 
capture drop _pdif

keep if _wt_mobvert + _wt_wagegap + _wt_demissoes + _wt_admissoes > 0 


compress

save matched_database, replace 

capture log close
}
* ############################################################################################################################################################################################
* ############################################################################################################################################################################################
* ############################################################################################################################################################################################
* ############################################################################################################################################################################################
* ############################################################################################################################################################################################
* ######################################## GALIANI
{			
*  Check if the women CEO is exogenous to the composition of C-level
capture log close
log using log_galiani.log, replace
	
	use matched_database, clear
	capture drop galiani_mul_diretoria
	capture drop aux_controle
	capture drop aux_tratamento
				
	bysort CNPJ_raiz: egen aux_controle = mean(perc_mul_cargo_diretoria) if grupo_controle_homem_troca == 1
	bysort CNPJ_raiz: egen aux_tratamento = mean(perc_mul_cargo_diretoria) if tratamento_alguma_hora == 1 & grupo_tratamento_mulher_fim == 0
	
	gen galiani_mul_diretoria = aux_controle
	replace galiani_mul_diretoria = aux_tratamento if aux_controle == . & tratamento_alguma_hora == 1
	
	capture drop aux_controle
	capture drop aux_tratamento
	
	compress
	
	probit tratamento_alguma_hora galiani_mul_diretoria i.Ano i.ef_industria i.Municipio if d_tam_emp_acima1500 == 1, vce(cluster Municipio)
	reghdfe tratamento_alguma_hora galiani_mul_diretoria				if d_tam_emp_acima1500 == 1, absorb(Ano ef_industria Municipio Municipio#Ano  Ano#ef_industria) vce(cluster Municipio)	
	reghdfe tratamento_alguma_hora galiani_mul_diretoria perc_mulheres	if d_tam_emp_acima1500 == 1, absorb(Ano ef_industria Municipio Municipio#Ano  Ano#ef_industria) vce(cluster Municipio)

	capture drop galiani_mul_diretoria
			

	use matched_database, clear
	capture drop galiani_perc_mul
	capture drop aux_controle
	capture drop aux_tratamento
				
	bysort CNPJ_raiz: egen aux_controle = mean(perc_mulheres) if grupo_controle_homem_troca == 1
	bysort CNPJ_raiz: egen aux_tratamento = mean(perc_mulheres) if tratamento_alguma_hora == 1 & grupo_tratamento_mulher_fim == 0
	
	gen galiani_perc_mul = aux_controle
	replace galiani_perc_mul = aux_tratamento if aux_controle == . & tratamento_alguma_hora == 1
	
	capture drop aux_controle
	capture drop aux_tratamento
	
	compress
	
	probit tratamento_alguma_hora galiani_perc_mul i.Ano i.ef_industria i.Municipio if d_tam_emp_acima1500 == 1, vce(cluster Municipio)
	reghdfe tratamento_alguma_hora galiani_perc_mul				if d_tam_emp_acima1500 == 1, absorb(Ano ef_industria Municipio Municipio#Ano  Ano#ef_industria) vce(cluster Municipio)	

	capture drop galiani_perc_mul
		
	
capture log close
}

* ############################################################################################################################################################################################
* ############################################################################################################################################################################################
* ############################################################################################################################################################################################
* ############################################################################################################################################################################################
* ############################################################################################################################################################################################
* ############################################################################################################################################################################################
* ############################################################################################################################################################################################
* ############################################################################################################################################################################################
* ######################################## CALLOWAY SANTANNA - DIFF IN DIFF - MAIN RESULTS
{
 * ######################################################################################## CSDID - Acima de 1500 funcionários
capture log close

log using log_csdid_log_size1500plus.log, replace

clear
use matched_database

keep CNPJ_raiz Ano ef_industria csdid_tratamento Municipio tratamento_alguma_hora grupo_tratamento_mulher_fim perc_mul_adm_no_ano perc_mul_demitida_no_ano mov_vert_mul_fx_0 wage_gap_fx_0 _wt_mobvert _wt_wagegap _wt_demissoes _wt_admissoes d_tam_emp_acima1500
compress

**** Wage Gap																							// FIGURE 1
estimates clear
csdid2 wage_gap_fx_0 i.ef_industria if _wt_wagegap == 1 & d_tam_emp_acima1500 == 1, cluster(Municipio) time(Ano) gvar(csdid_tratamento) method(reg) short 
csdid2_estat event
csdid2_plot, title("Wage Gap") style(rcap) legend(label(1 "95% CI") label(2 "Pre-Treatment Coefficient") label(3 "95% CI") label(4 "Post-Treatment Coefficient") region(lstyle(foreground)) rows(2) position(6)) level(95)
*graph save csdid_wage_gap_size1500plus.gph, replace 

**** Mob Vertical																							// FIGURE 2
estimates clear
csdid2 mov_vert_mul_fx_0 i.ef_industria if _wt_mobvert == 1 & d_tam_emp_acima1500 == 1, cluster(Municipio) time(Ano) gvar(csdid_tratamento) method(reg) short 
csdid2_estat event
csdid2_plot, title("Vertical Mobility") style(rcap) legend(label(1 "95% CI") label(2 "Pre-Treatment Coefficient") label(3 "95% CI") label(4 "Post-Treatment Coefficient") region(lstyle(foreground)) rows(2) position(6)) level(95)
*graph save csdid_mob_vertical_size1500plus.gph, replace 

**** Per Mul Dem																							// FIGURE 3A
estimates clear
csdid2 perc_mul_demitida_no_ano i.ef_industria if _wt_demissoes == 1 & d_tam_emp_acima1500 == 1, cluster(Municipio) time(Ano) gvar(csdid_tratamento) method(reg) short 
csdid2_estat event
csdid2_plot, title("Percentage of Women Fired") style(rcap) legend(label(1 "95% CI") label(2 "Pre-Treatment Coefficient") label(3 "95% CI") label(4 "Post-Treatment Coefficient") region(lstyle(foreground)) rows(2) position(6)) level(95)
*graph save csdid_perc_demitidas_size1500plus.gph, replace 

**** Per Mul Adm																							// FIGURE 3B
estimates clear
csdid2 perc_mul_adm_no_ano i.ef_industria if _wt_admissoes == 1 & d_tam_emp_acima1500 == 1, cluster(Municipio) time(Ano) gvar(csdid_tratamento) method(reg) short 
csdid2_estat event
csdid2_plot, title("Percentage of Women Hired") style(rcap) legend(label(1 "95% CI") label(2 "Pre-Treatment Coefficient") label(3 "95% CI") label(4 "Post-Treatment Coefficient") region(lstyle(foreground)) rows(2) position(6)) level(95) xscale(range(-7 7))
*graph save csdid_perc_admitidas_size1500plus.gph, replace 

capture log close


* ############################################################################################################################################################################################
* ############################################################################################################################################################################################
* ############################################################################################################################################################################################
* ############################################################################################################################################################################################
* ############################################################################################################################################################################################
* ############################################################################################################################################################################################
* ############################################################################################################################################################################################
* ############################################################################################################################################################################################
* ######################################## CALLOWAY SANTANNA - DIFF IN DIFF - RANGE HETEROGENEITIES

 * ######################################################################################## CSDID - HETEROGENEIDADES - SIZE 1500 - fx_1
capture log close

log using log_csdid_log_fx_1_1500plus.log, replace

clear
use matched_database

keep CNPJ_raiz Ano ef_industria csdid_tratamento Municipio tratamento_alguma_hora grupo_tratamento_mulher_fim perc_mul_adm_no_ano_fx_1 perc_mul_demitida_no_ano_fx_1 mov_vert_mul_fx_1 wage_gap_fx_1 _wt_mobvert _wt_wagegap _wt_demissoes _wt_admissoes d_tam_emp_acima1500
compress

**** Wage Gap																							// FIGURE 1
estimates clear
csdid2 wage_gap_fx_1 i.ef_industria if _wt_wagegap == 1 & d_tam_emp_acima1500 == 1, cluster(Municipio) time(Ano) gvar(csdid_tratamento) method(reg) short 
csdid2_estat event
csdid2_plot, title("Wage Gap") style(rcap) legend(label(1 "95% CI") label(2 "Pre-Treatment Coefficient") label(3 "95% CI") label(4 "Post-Treatment Coefficient") region(lstyle(foreground)) rows(2) position(6)) level(95)
*graph save csdid_wage_gap_fx_1_1500plus.gph, replace

**** Mob Vertical																							// FIGURE 2
estimates clear
csdid2 mov_vert_mul_fx_1 i.ef_industria if _wt_mobvert == 1 & d_tam_emp_acima1500 == 1, cluster(Municipio) time(Ano) gvar(csdid_tratamento) method(reg) short 
csdid2_estat event
csdid2_plot, title("Vertical Mobility") style(rcap) legend(label(1 "95% CI") label(2 "Pre-Treatment Coefficient") label(3 "95% CI") label(4 "Post-Treatment Coefficient") region(lstyle(foreground)) rows(2) position(6)) level(95)
*graph save csdid_mob_vertical_fx_1_1500plus.gph, replace 

**** Per Mul Dem																							// FIGURE 3A
estimates clear
csdid2 perc_mul_demitida_no_ano_fx_1 i.ef_industria if _wt_demissoes == 1 & d_tam_emp_acima1500 == 1, cluster(Municipio) time(Ano) gvar(csdid_tratamento) method(reg) short 
csdid2_estat event
csdid2_plot, title("Percentage of Women Fired") style(rcap) legend(label(1 "95% CI") label(2 "Pre-Treatment Coefficient") label(3 "95% CI") label(4 "Post-Treatment Coefficient") region(lstyle(foreground)) rows(2) position(6)) level(95)
*graph save csdid_perc_demitidas_fx_1_1500plus.gph, replace 

**** Per Mul Adm																							// FIGURE 3B
estimates clear
csdid2 perc_mul_adm_no_ano_fx_1 i.ef_industria if _wt_admissoes == 1 & d_tam_emp_acima1500 == 1, cluster(Municipio) time(Ano) gvar(csdid_tratamento) method(reg) short 
csdid2_estat event
csdid2_plot, title("Percentage of Women Hired") style(rcap) legend(label(1 "95% CI") label(2 "Pre-Treatment Coefficient") label(3 "95% CI") label(4 "Post-Treatment Coefficient") region(lstyle(foreground)) rows(2) position(6)) level(95) xscale(range(-7 7))
*graph save csdid_perc_admitidas_fx_1_1500plus.gph, replace 

capture log close


 * ######################################################################################## CSDID - HETEROGENEIDADES - SIZE 1500 - fx_2
capture log close

log using log_csdid_log_fx_2_1500plus.log, replace

clear
use matched_database

keep CNPJ_raiz Ano ef_industria csdid_tratamento Municipio tratamento_alguma_hora grupo_tratamento_mulher_fim perc_mul_adm_no_ano_fx_2 perc_mul_demitida_no_ano_fx_2 mov_vert_mul_fx_2 wage_gap_fx_2 _wt_mobvert _wt_wagegap _wt_demissoes _wt_admissoes d_tam_emp_acima1500
compress

**** Wage Gap																							// FIGURE 1
estimates clear
csdid2 wage_gap_fx_2 i.ef_industria if _wt_wagegap == 1 & d_tam_emp_acima1500 == 1, cluster(Municipio) time(Ano) gvar(csdid_tratamento) method(reg) short 
csdid2_estat event
csdid2_plot, title("Wage Gap") style(rcap) legend(label(1 "95% CI") label(2 "Pre-Treatment Coefficient") label(3 "95% CI") label(4 "Post-Treatment Coefficient") region(lstyle(foreground)) rows(2) position(6)) level(95)
*graph save csdid_wage_gap_fx_2_1500plus.gph, replace

**** Mob Vertical																							// FIGURE 2
estimates clear
csdid2 mov_vert_mul_fx_2 i.ef_industria if _wt_mobvert == 1 & d_tam_emp_acima1500 == 1, cluster(Municipio) time(Ano) gvar(csdid_tratamento) method(reg) short 
csdid2_estat event
csdid2_plot, title("Vertical Mobility") style(rcap) legend(label(1 "95% CI") label(2 "Pre-Treatment Coefficient") label(3 "95% CI") label(4 "Post-Treatment Coefficient") region(lstyle(foreground)) rows(2) position(6)) level(95)
*graph save csdid_mob_vertical_fx_2_1500plus.gph, replace 

**** Per Mul Dem																							// FIGURE 3A
estimates clear
csdid2 perc_mul_demitida_no_ano_fx_2 i.ef_industria if _wt_demissoes == 1 & d_tam_emp_acima1500 == 1, cluster(Municipio) time(Ano) gvar(csdid_tratamento) method(reg) short 
csdid2_estat event
csdid2_plot, title("Percentage of Women Fired") style(rcap) legend(label(1 "95% CI") label(2 "Pre-Treatment Coefficient") label(3 "95% CI") label(4 "Post-Treatment Coefficient") region(lstyle(foreground)) rows(2) position(6)) level(95)
*graph save csdid_perc_demitidas_fx_2_1500plus.gph, replace 

**** Per Mul Adm																							// FIGURE 3B
estimates clear
csdid2 perc_mul_adm_no_ano_fx_2 i.ef_industria if _wt_admissoes == 1 & d_tam_emp_acima1500 == 1, cluster(Municipio) time(Ano) gvar(csdid_tratamento) method(reg) short 
csdid2_estat event
csdid2_plot, title("Percentage of Women Hired") style(rcap) legend(label(1 "95% CI") label(2 "Pre-Treatment Coefficient") label(3 "95% CI") label(4 "Post-Treatment Coefficient") region(lstyle(foreground)) rows(2) position(6)) level(95) xscale(range(-7 7))
*graph save csdid_perc_admitidas_fx_2_1500plus.gph, replace 

capture log close


 * ######################################################################################## CSDID - HETEROGENEIDADES - SIZE 1500 - fx_3
capture log close

log using log_csdid_log_fx_3_1500plus.log, replace

clear
use matched_database

keep CNPJ_raiz Ano ef_industria csdid_tratamento Municipio tratamento_alguma_hora grupo_tratamento_mulher_fim perc_mul_adm_no_ano_fx_3 perc_mul_demitida_no_ano_fx_3 mov_vert_mul_fx_3 wage_gap_fx_3 _wt_mobvert _wt_wagegap _wt_demissoes _wt_admissoes d_tam_emp_acima1500
compress

**** Wage Gap																							// FIGURE 1
estimates clear
csdid2 wage_gap_fx_3 i.ef_industria if _wt_wagegap == 1 & d_tam_emp_acima1500 == 1, cluster(Municipio) time(Ano) gvar(csdid_tratamento) method(reg) short 
csdid2_estat event
csdid2_plot, title("Wage Gap") style(rcap) legend(label(1 "95% CI") label(2 "Pre-Treatment Coefficient") label(3 "95% CI") label(4 "Post-Treatment Coefficient") region(lstyle(foreground)) rows(2) position(6)) level(95)
*graph save csdid_wage_gap_fx_3_1500plus.gph, replace

**** Mob Vertical																							// FIGURE 2
estimates clear
csdid2 mov_vert_mul_fx_3 i.ef_industria if _wt_mobvert == 1 & d_tam_emp_acima1500 == 1, cluster(Municipio) time(Ano) gvar(csdid_tratamento) method(reg) short 
csdid2_estat event
csdid2_plot, title("Vertical Mobility") style(rcap) legend(label(1 "95% CI") label(2 "Pre-Treatment Coefficient") label(3 "95% CI") label(4 "Post-Treatment Coefficient") region(lstyle(foreground)) rows(2) position(6)) level(95)
*graph save csdid_mob_vertical_fx_3_1500plus.gph, replace 

**** Per Mul Dem																							// FIGURE 3A
estimates clear
csdid2 perc_mul_demitida_no_ano_fx_3 i.ef_industria if _wt_demissoes == 1 & d_tam_emp_acima1500 == 1, cluster(Municipio) time(Ano) gvar(csdid_tratamento) method(reg) short 
csdid2_estat event
csdid2_plot, title("Percentage of Women Fired") style(rcap) legend(label(1 "95% CI") label(2 "Pre-Treatment Coefficient") label(3 "95% CI") label(4 "Post-Treatment Coefficient") region(lstyle(foreground)) rows(2) position(6)) level(95)
*graph save csdid_perc_demitidas_fx_3_1500plus.gph, replace 

**** Per Mul Adm																							// FIGURE 3B
estimates clear
csdid2 perc_mul_adm_no_ano_fx_3 i.ef_industria if _wt_admissoes == 1 & d_tam_emp_acima1500 == 1, cluster(Municipio) time(Ano) gvar(csdid_tratamento) method(reg) short 
csdid2_estat event
csdid2_plot, title("Percentage of Women Hired") style(rcap) legend(label(1 "95% CI") label(2 "Pre-Treatment Coefficient") label(3 "95% CI") label(4 "Post-Treatment Coefficient") region(lstyle(foreground)) rows(2) position(6)) level(95) xscale(range(-7 7))
*graph save csdid_perc_admitidas_fx_3_1500plus.gph, replace 

capture log close


 * ######################################################################################## CSDID - HETEROGENEIDADES - SIZE 1500 - fx_4
capture log close

log using log_csdid_log_fx_4_1500plus.log, replace

clear
use matched_database

keep CNPJ_raiz Ano ef_industria csdid_tratamento Municipio tratamento_alguma_hora grupo_tratamento_mulher_fim perc_mul_adm_no_ano_fx_4 perc_mul_demitida_no_ano_fx_4 mov_vert_mul_fx_4 wage_gap_fx_4 _wt_mobvert _wt_wagegap _wt_demissoes _wt_admissoes d_tam_emp_acima1500
compress

**** Wage Gap																							// FIGURE 1
estimates clear
csdid2 wage_gap_fx_4 i.ef_industria if _wt_wagegap == 1 & d_tam_emp_acima1500 == 1, cluster(Municipio) time(Ano) gvar(csdid_tratamento) method(reg) short 
csdid2_estat event
csdid2_plot, title("Wage Gap") style(rcap) legend(label(1 "95% CI") label(2 "Pre-Treatment Coefficient") label(3 "95% CI") label(4 "Post-Treatment Coefficient") region(lstyle(foreground)) rows(2) position(6)) level(95)
*graph save csdid_wage_gap_fx_4_1500plus.gph, replace

**** Mob Vertical																							// FIGURE 2
estimates clear
csdid2 mov_vert_mul_fx_4 i.ef_industria if _wt_mobvert == 1 & d_tam_emp_acima1500 == 1, cluster(Municipio) time(Ano) gvar(csdid_tratamento) method(reg) short 
csdid2_estat event
csdid2_plot, title("Vertical Mobility") style(rcap) legend(label(1 "95% CI") label(2 "Pre-Treatment Coefficient") label(3 "95% CI") label(4 "Post-Treatment Coefficient") region(lstyle(foreground)) rows(2) position(6)) level(95)
*graph save csdid_mob_vertical_fx_4_1500plus.gph, replace 

**** Per Mul Dem																							// FIGURE 3A
estimates clear
csdid2 perc_mul_demitida_no_ano_fx_4 i.ef_industria if _wt_demissoes == 1 & d_tam_emp_acima1500 == 1, cluster(Municipio) time(Ano) gvar(csdid_tratamento) method(reg) short 
csdid2_estat event
csdid2_plot, title("Percentage of Women Fired") style(rcap) legend(label(1 "95% CI") label(2 "Pre-Treatment Coefficient") label(3 "95% CI") label(4 "Post-Treatment Coefficient") region(lstyle(foreground)) rows(2) position(6)) level(95)
*graph save csdid_perc_demitidas_fx_4_1500plus.gph, replace 

**** Per Mul Adm																							// FIGURE 3B
estimates clear
csdid2 perc_mul_adm_no_ano_fx_4 i.ef_industria if _wt_admissoes == 1 & d_tam_emp_acima1500 == 1, cluster(Municipio) time(Ano) gvar(csdid_tratamento) method(reg) short 
csdid2_estat event
csdid2_plot, title("Percentage of Women Hired") style(rcap) legend(label(1 "95% CI") label(2 "Pre-Treatment Coefficient") label(3 "95% CI") label(4 "Post-Treatment Coefficient") region(lstyle(foreground)) rows(2) position(6)) level(95) xscale(range(-7 7))
*graph save csdid_perc_admitidas_fx_4_1500plus.gph, replace 

capture log close


 * ######################################################################################## CSDID - HETEROGENEIDADES - SIZE 1500 - fx_5
capture log close

log using log_csdid_log_fx_5_1500plus.log, replace

clear
use matched_database

keep CNPJ_raiz Ano ef_industria csdid_tratamento Municipio tratamento_alguma_hora grupo_tratamento_mulher_fim perc_mul_adm_no_ano_fx_5 perc_mul_demitida_no_ano_fx_5 mov_vert_mul_fx_5 wage_gap_fx_5 _wt_mobvert _wt_wagegap _wt_demissoes _wt_admissoes d_tam_emp_acima1500
compress

**** Wage Gap																							// FIGURE 1
estimates clear
csdid2 wage_gap_fx_5 i.ef_industria if _wt_wagegap == 1 & d_tam_emp_acima1500 == 1, cluster(Municipio) time(Ano) gvar(csdid_tratamento) method(reg) short 
csdid2_estat event
csdid2_plot, title("Wage Gap") style(rcap) legend(label(1 "95% CI") label(2 "Pre-Treatment Coefficient") label(3 "95% CI") label(4 "Post-Treatment Coefficient") region(lstyle(foreground)) rows(2) position(6)) level(95)
*graph save csdid_wage_gap_fx_5_1500plus.gph, replace 

**** Mob Vertical																							// FIGURE 2
estimates clear
csdid2 mov_vert_mul_fx_5 i.ef_industria if _wt_mobvert == 1 & d_tam_emp_acima1500 == 1, cluster(Municipio) time(Ano) gvar(csdid_tratamento) method(reg) short 
csdid2_estat event
csdid2_plot, title("Vertical Mobility") style(rcap) legend(label(1 "95% CI") label(2 "Pre-Treatment Coefficient") label(3 "95% CI") label(4 "Post-Treatment Coefficient") region(lstyle(foreground)) rows(2) position(6)) level(95)
*graph save csdid_mob_vertical_fx_5_1500plus.gph, replace 

**** Per Mul Dem																							// FIGURE 3A
estimates clear
csdid2 perc_mul_demitida_no_ano_fx_5 i.ef_industria if _wt_demissoes == 1 & d_tam_emp_acima1500 == 1, cluster(Municipio) time(Ano) gvar(csdid_tratamento) method(reg) short 
csdid2_estat event
csdid2_plot, title("Percentage of Women Fired") style(rcap) legend(label(1 "95% CI") label(2 "Pre-Treatment Coefficient") label(3 "95% CI") label(4 "Post-Treatment Coefficient") region(lstyle(foreground)) rows(2) position(6)) level(95)
*graph save csdid_perc_demitidas_fx_5_1500plus.gph, replace 

**** Per Mul Adm																							// FIGURE 3B
estimates clear
csdid2 perc_mul_adm_no_ano_fx_5 i.ef_industria if _wt_admissoes == 1 & d_tam_emp_acima1500 == 1, cluster(Municipio) time(Ano) gvar(csdid_tratamento) method(reg) short 
csdid2_estat event
csdid2_plot, title("Percentage of Women Hired") style(rcap) legend(label(1 "95% CI") label(2 "Pre-Treatment Coefficient") label(3 "95% CI") label(4 "Post-Treatment Coefficient") region(lstyle(foreground)) rows(2) position(6)) level(95) xscale(range(-7 7))
*graph save csdid_perc_admitidas_fx_5_1500plus.gph, replace 

capture log close
}
*/
* ############################################################################################################################################################################################
* ############################################################################################################################################################################################
* ############################################################################################################################################################################################
* ############################################################################################################################################################################################
* ############################################################################################################################################################################################
* ############################################################################################################################################################################################
* ############################################################################################################################################################################################
* ############################################################################################################################################################################################
* ######################################## "FULL SAMPLE" HETEROGENEITY - COMMENTED BECAUSE NOT SIGNIFICANT
/*
{
use econometry_database.dta , clear

 * ############################################### COMPANY SIZE 

capture drop media_colab
capture drop d_tam_emp_fullsample_het	
bysort CNPJ_raiz: egen media_colab = mean(qtd_colab_ativ) if qtd_colab_ativ > 0
replace media_colab = 0 if media_colab == .
bysort CNPJ_raiz: gen d_tam_emp_fullsample_het = 1 if media_colab >= 15
tab d_tam_emp_fullsample_het
drop if d_tam_emp_fullsample_het == .
capture drop media_colab			

* Perform the PSmatchs for the econometrics

capture log close
log using log_ps_scores_fullsample_het.log, replace

* Admissões
capture drop  _pscore
capture drop _treated 
capture drop _support 
capture drop _wt1const 
capture drop _weight
capture drop _perc_mul_cargo_diretoria 
capture drop _perc_mul_adm_no_ano 
capture drop _wage_gap_fx_0 
capture drop _mov_vert_mulher_ano_ant 
capture drop _perc_mul_demitida_no_ano 
capture drop _id 
capture drop _n1 
capture drop _nn 
capture drop _pdif
capture drop aux_wt_admissoes
capture drop _wt_admissoes

psmatch2 tratamento_alguma_hora perc_mulheres qtd_colab_ativ perc_raca_cor_branca_mul perc_idade_20_a_30_mul perc_idade_30_a_40_mul perc_idade_40_a_50_mul i.ef_industria if tempo == 0 & trat_cont == 1, out(perc_mul_adm_no_ano) n(1) norepl cal(0.01) 
pstest, detail onlysig
rename _weight aux_wt_admissoes
bysort CNPJ_raiz: egen _wt_admissoes = max(aux_wt_admissoes)
replace _wt_admissoes=0 if _wt_admissoes==.

tab _wt_admissoes
capture drop aux_wt_admissoes


* Demissões
capture drop  _pscore
capture drop _treated 
capture drop _support 
capture drop _wt1const 
capture drop _weight
capture drop _perc_mul_cargo_diretoria 
capture drop _perc_mul_adm_no_ano 
capture drop _wage_gap_fx_0 
capture drop _mov_vert_mulher_ano_ant 
capture drop _perc_mul_demitida_no_ano 
capture drop _id 
capture drop _n1 
capture drop _nn 
capture drop _pdif
capture drop aux_wt_demissoes
capture drop wt_demissoes

psmatch2 tratamento_alguma_hora perc_mulheres qtd_colab_ativ perc_raca_cor_branca_mul perc_idade_20_a_30_mul perc_idade_30_a_40_mul perc_idade_40_a_50_mul i.ef_industria if tempo == 0 & trat_cont == 1, out(perc_mul_demitida_no_ano) n(1) norepl  cal(0.01) 
pstest, detail onlysig
rename _weight aux_wt_demissoes
bysort CNPJ_raiz: egen _wt_demissoes = max(aux_wt_demissoes)
replace _wt_demissoes=0 if _wt_demissoes==.
 
tab _wt_demissoes
capture drop aux_wt_demissoes

* Wage Gap
capture drop  _pscore
capture drop _treated 
capture drop _support 
capture drop _wt1const 
capture drop _weight
capture drop _perc_mul_cargo_diretoria 
capture drop _perc_mul_adm_no_ano 
capture drop _wage_gap_fx_0 
capture drop _mov_vert_mulher_ano_ant 
capture drop _perc_mul_demitida_no_ano 
capture drop _id 
capture drop _n1 
capture drop _nn 
capture drop _pdif
capture drop aux_wt_wagegap
capture drop _wt_wagegap

psmatch2 tratamento_alguma_hora perc_mulheres qtd_colab_ativ perc_raca_cor_branca_mul perc_idade_20_a_30_mul perc_idade_30_a_40_mul perc_idade_40_a_50_mul i.ef_industria if tempo == 0 & trat_cont == 1, out(wage_gap_fx_0) n(1) norepl cal(0.01) 
pstest, detail onlysig
rename _weight aux_wt_wagegap
bysort CNPJ_raiz: egen _wt_wagegap = max(aux_wt_wagegap)
replace _wt_wagegap=0 if _wt_wagegap==.
 
tab _wt_wagegap
capture drop aux_wt_wagegap

* Vertical Mobility
capture drop  _pscore
capture drop _treated 
capture drop _support 
capture drop _wt1const 
capture drop _weight
capture drop _perc_mul_cargo_diretoria 
capture drop _perc_mul_adm_no_ano 
capture drop _wage_gap_fx_0 
capture drop _mov_vert_mul_fx_0 
capture drop _perc_mul_demitida_no_ano 
capture drop _id 
capture drop _n1 
capture drop _nn 
capture drop _pdif
capture drop aux_wt_mobvert
capture drop _wt_mobvert

psmatch2 tratamento_alguma_hora perc_mulheres qtd_colab_ativ perc_raca_cor_branca_mul perc_idade_20_a_30_mul perc_idade_30_a_40_mul perc_idade_40_a_50_mul i.ef_industria if tempo == 0 & trat_cont == 1, out(mov_vert_mul_fx_0) n(1) norepl cal(0.01) 
pstest, detail onlysig
rename _weight aux_wt_mobvert
bysort CNPJ_raiz: egen _wt_mobvert = max(aux_wt_mobvert)
replace _wt_mobvert=0 if _wt_mobvert==.
 
tab _wt_mobvert
capture drop aux_wt_mobvert

capture drop  _pscore
capture drop _treated 
capture drop _support 
capture drop _wt1const 
capture drop _weight
capture drop _perc_mul_cargo_diretoria 
capture drop _perc_mul_adm_no_ano 
capture drop _wage_gap_fx_0 
capture drop _mov_vert_mul_fx_0 
capture drop _perc_mul_demitida_no_ano 
capture drop _id 
capture drop _n1 
capture drop _nn 
capture drop _pdif

keep if _wt_mobvert + _wt_wagegap + _wt_demissoes + _wt_admissoes > 0 

compress

save matched_database_fullsample_het, replace 

capture log close
	
log using log_csdid_log_fullsample_het.log, replace

use matched_database_fullsample_het, clear

keep CNPJ_raiz Ano ef_industria csdid_tratamento Municipio tratamento_alguma_hora grupo_tratamento_mulher_fim perc_mul_adm_no_ano perc_mul_demitida_no_ano mov_vert_mul_fx_0 wage_gap_fx_0 _wt_mobvert _wt_wagegap _wt_demissoes _wt_admissoes d_tam_emp_fullsample_het
compress

**** Per Mul Adm
estimates clear
csdid2 perc_mul_adm_no_ano i.ef_industria if _wt_admissoes == 1 & d_tam_emp_fullsample_het == 1, cluster(Municipio) time(Ano) gvar(csdid_tratamento) method(reg) short 
csdid2_estat event
csdid2_plot, title("Percentage of Women Hired") style(rcap) legend(label(1 "95% CI") label(2 "Pre-Treatment Coefficient") label(3 "95% CI") label(4 "Post-Treatment Coefficient") region(lstyle(foreground)) rows(2) position(6)) level(95) xscale(range(-7 7))
*graph save csdid_perc_admitidas_fullsample_het.gph, replace 

**** Per Mul Dem
estimates clear
csdid2 perc_mul_demitida_no_ano i.ef_industria if _wt_demissoes == 1 & d_tam_emp_fullsample_het == 1, cluster(Municipio) time(Ano) gvar(csdid_tratamento) method(reg) short 
csdid2_estat event
csdid2_plot, title("Percentage of Women Fired") style(rcap) legend(label(1 "95% CI") label(2 "Pre-Treatment Coefficient") label(3 "95% CI") label(4 "Post-Treatment Coefficient") region(lstyle(foreground)) rows(2) position(6)) level(95)
*graph save csdid_perc_demitidas_fullsample_het.gph, replace 

**** Mob Vertical
estimates clear
csdid2 mov_vert_mul_fx_0 i.ef_industria if _wt_mobvert == 1 & d_tam_emp_fullsample_het == 1, cluster(Municipio) time(Ano) gvar(csdid_tratamento) method(reg) short 
csdid2_estat event
csdid2_plot, title("Vertical Mobility") style(rcap) legend(label(1 "95% CI") label(2 "Pre-Treatment Coefficient") label(3 "95% CI") label(4 "Post-Treatment Coefficient") region(lstyle(foreground)) rows(2) position(6)) level(95)
*graph save csdid_mob_vertical_fullsample_het.gph, replace 

**** Wage Gap
estimates clear
csdid2 wage_gap_fx_0 i.ef_industria if _wt_wagegap == 1 & d_tam_emp_fullsample_het == 1, cluster(Municipio) time(Ano) gvar(csdid_tratamento) method(reg) short 
csdid2_estat event
csdid2_plot, title("Wage Gap") style(rcap) legend(label(1 "95% CI") label(2 "Pre-Treatment Coefficient") label(3 "95% CI") label(4 "Post-Treatment Coefficient") region(lstyle(foreground)) rows(2) position(6)) level(95)
*graph save csdid_wage_gap_fullsample_het.gph, replace 

capture log close
	
}	
*/
* ############################################################################################################################################################################################
* ############################################################################################################################################################################################
* ############################################################################################################################################################################################
* ############################################################################################################################################################################################
* ############################################################################################################################################################################################
* ############################################################################################################################################################################################
* ############################################################################################################################################################################################
* ############################################################################################################################################################################################
* ######################################## DESCRIPTIVE STATISTICS - TABLE 2
{
use matched_database, clear

capture log close
log using log_DescriptiveStatistics.log, replace
capture erase DS_TreatControl.csv
capture erase DS_Control.csv
capture erase DS_Treatment.csv

estpost summarize  perc_mul_demitida_no_ano perc_mul_demitida_no_ano_fx_1 perc_mul_demitida_no_ano_fx_2 perc_mul_demitida_no_ano_fx_3 perc_mul_demitida_no_ano_fx_4 perc_mul_demitida_no_ano_fx_5 wage_gap_fx_0 wage_gap_fx_1 wage_gap_fx_2 wage_gap_fx_3 wage_gap_fx_4 wage_gap_fx_5 mov_vert_mul_fx_0 mov_vert_mul_fx_1 mov_vert_mul_fx_2 mov_vert_mul_fx_3 mov_vert_mul_fx_4 mov_vert_mul_fx_5 perc_mul_adm_no_ano perc_mul_adm_no_ano_fx_1 perc_mul_adm_no_ano_fx_2 perc_mul_adm_no_ano_fx_3 perc_mul_adm_no_ano_fx_4 perc_mul_adm_no_ano_fx_5 Ano TimeToEvent csdid_tratamento d_tam_emp_acima1500  perc_idade_20_a_30_mul perc_idade_30_a_40_mul perc_idade_40_a_50_mul perc_raca_cor_branca_mul perc_mulheres qtd_colab_ativ 
esttab using DS_TreatControl.csv, cells ("count mean sd min max")
 
estpost summarize  perc_mul_demitida_no_ano perc_mul_demitida_no_ano_fx_1 perc_mul_demitida_no_ano_fx_2 perc_mul_demitida_no_ano_fx_3 perc_mul_demitida_no_ano_fx_4 perc_mul_demitida_no_ano_fx_5 wage_gap_fx_0 wage_gap_fx_1 wage_gap_fx_2 wage_gap_fx_3 wage_gap_fx_4 wage_gap_fx_5 mov_vert_mul_fx_0 mov_vert_mul_fx_1 mov_vert_mul_fx_2 mov_vert_mul_fx_3 mov_vert_mul_fx_4 mov_vert_mul_fx_5 perc_mul_adm_no_ano perc_mul_adm_no_ano_fx_1 perc_mul_adm_no_ano_fx_2 perc_mul_adm_no_ano_fx_3 perc_mul_adm_no_ano_fx_4 perc_mul_adm_no_ano_fx_5 Ano TimeToEvent csdid_tratamento d_tam_emp_acima1500  perc_idade_20_a_30_mul perc_idade_30_a_40_mul perc_idade_40_a_50_mul perc_raca_cor_branca_mul perc_mulheres qtd_colab_ativ if  tratamento_alguma_hora == 0
esttab using DS_Control.csv, cells ("count mean sd min max")
 
estpost summarize  perc_mul_demitida_no_ano perc_mul_demitida_no_ano_fx_1 perc_mul_demitida_no_ano_fx_2 perc_mul_demitida_no_ano_fx_3 perc_mul_demitida_no_ano_fx_4 perc_mul_demitida_no_ano_fx_5 wage_gap_fx_0 wage_gap_fx_1 wage_gap_fx_2 wage_gap_fx_3 wage_gap_fx_4 wage_gap_fx_5 mov_vert_mul_fx_0 mov_vert_mul_fx_1 mov_vert_mul_fx_2 mov_vert_mul_fx_3 mov_vert_mul_fx_4 mov_vert_mul_fx_5 perc_mul_adm_no_ano perc_mul_adm_no_ano_fx_1 perc_mul_adm_no_ano_fx_2 perc_mul_adm_no_ano_fx_3 perc_mul_adm_no_ano_fx_4 perc_mul_adm_no_ano_fx_5 Ano TimeToEvent csdid_tratamento d_tam_emp_acima1500  perc_idade_20_a_30_mul perc_idade_30_a_40_mul perc_idade_40_a_50_mul perc_raca_cor_branca_mul perc_mulheres qtd_colab_ativ if tratamento_alguma_hora == 1
esttab using DS_Treatment.csv, cells ("count mean sd min max")
 
capture log close
}

* ############################################################################################################################################################################################
* ############################################################################################################################################################################################
* ############################################################################################################################################################################################
* ############################################################################################################################################################################################
* ######################################## EOF
