#!/usr/bin/env ruby

input = <<END
netload str netload_filename
xy2pic int xy2pic_x, int xy2pic_y
hantozen str hantozen_arg1, var hantozen_arg2, int hantozen_arg3
zentohan str zentohan_arg1, var zentohan_arg2, int zentohan_arg3
tmset
tmend
lang str lang_jp, str lang_en
cnvrank int cnvrank_rank
cnvtalk str cnvtalk_str
cnvarticle str cnvarticle_str
cnvitemname int cnvitemname_itemid
cnven str cnven_str
cbit int cbit_bit, int cbit_charid
cbitmod int cbitmod_bit, int cbitmod_charid, int cbitmod_seton
refchara int refchara_dbid, int refchara_dbspec, int refchara_charid
refclass int refclass_dbid, int refclass_dbspec
refrace str refrace_dbidn, int refrace_dbspec
ranktitle int ranktitle_rank
guildname
calcbuff int calcbuff_charid, int calcbuff_buffid, int calcbuff_power
calcskill int calcskill_skillid, int calcskill_charid, int calcskill_spellpower
sorg int sorg_skillid, int sorg_charid
randskill
randattb
ibit int ibit_bit, int ibit_itemid
ibitmod int ibitmod_bit, int ibitmod_itemid, int ibitmod_seton
refitem int refitem_dbid, int refitem_dbspec
iequiploc int iequiploc_itemid
setunid int setunid_charid, int setunid_unid
getunid int getunid_charid
findunid str findunid_arg1
synccheck int synccheck_arg1, int synccheck_arg2
evid
evfind int evfind_eventid
evadd int evadd_arg1, int evadd_arg2, int evadd_arg3
sndload str sndload_arg1, int sndload_arg2
musicload str musicload_arg1, int musicload_arg2
snd int snd_arg1, int snd_arg2, int snd_arg3
GetTStatus int TweakCategory, int TweakNumber
key_check int key_check_arg1
keyrelease
press int press_arg1
bmes str bmes_arg1, int bmes_arg2, int bmes_arg3, int bmes_arg4
talk_conv var talk_conv_arg1, int talk_conv_arg2
msg_write var msg_write_arg1
txtmore
txtcontinue
anime_halt
msg_halt
help_halt
txtef int txtef_color
msg_newlog
msg_newline
txtnew
msg_clear
txt_conv
name int name_arg1
aln int aln_charid
npcn int npcn_charid
_s int _s_charid, int _s_arg2
_s2 int _s2_arg1
is2 int is2_arg1
is int is_charid
was int was_charid
have int have_charid
does int does_arg1
he int he_charid, int he_arg2
_s3 int _s3_num
his2 int EntityID
him2 int him2_EntityID
his int his_charid, int his_arg2
him int him_charid, int him_arg2
your2 int EntityID2
your int your_charid, int
yourself int yourself_charid
txt_check int txt_check_arg1
txt_select str txt_select_arg1, str txt_select_arg2, str txt_select_arg3, str txt_select_arg4, str txt_select_arg5, str txt_select_arg6, str txt_select_arg7, str txt_select_arg8, str txt_select_arg9
cnvfix int cnvfix_arg1
stxt int stxt_arg1, str stxt_arg2
cnvdate int cnvdate_arg1, int cnvdate_arg2
cnvplaytime int cnvplaytime_arg1
roundmargin int roundmargin_arg1, int roundmargin_arg2
at int at_waittime
text_set
elename int elename_ele
txttargetlevel
txttargetsp
sncnv var sncnv_arg1
sngeneral var sngeneral_arg1
sninn var sninn_arg1
sntrade var sntrade_arg1
sngoods var sngoods_arg1
snbakery var snbakery_arg1
snmagic var snmagic_arg1
snarmor var snarmor_arg1
sntrainer var sntrainer_arg1
snfish var snfish_arg1
snblack var snblack_arg1
snfood var snfood_arg1
txtsetlastword
txtsetwinword int txtsetwinword_arg1
txtsetwinwordc int txtsetwinwordc_arg1
txtsetquiz0 int txtsetquiz0_arg1
txtsetquiz1 int txtsetquiz1_arg1
txtsetquiz2 int txtsetquiz2_arg1
txtsetquiz3 int txtsetquiz3_arg1
txtsetquiz4 int txtsetquiz4_arg1
txtsetquiz5 int txtsetquiz5_arg1
txtsetquiz6 int txtsetquiz6_arg1
txtsetquiz7 int txtsetquiz7_arg1
txtsetquiz8 int txtsetquiz8_arg1
txtsetquiz9 int txtsetquiz9_arg1
txtsetquiz10 int txtsetquiz10_arg1
txtsetquiz11 int txtsetquiz11_arg1
txtsetquiz12 int txtsetquiz12_arg1
txtsetquiz13 int txtsetquiz13_arg1
txtsetquiz14 int txtsetquiz14_arg1
txtsetquiz15 int txtsetquiz15_arg1
txtsetquiz16 int txtsetquiz16_arg1
txtsetquiz17 int txtsetquiz17_arg1
txtsetquiz18 int txtsetquiz18_arg1
txtsetquiz19 int txtsetquiz19_arg1
txtsetquiz20 int txtsetquiz20_arg1
txtsetquiz21 int txtsetquiz21_arg1
txtsetquiz22 int txtsetquiz22_arg1
txtsetquiz23 int txtsetquiz23_arg1
txtsetquiz24 int txtsetquiz24_arg1
txtsetquiz25 int txtsetquiz25_arg1
txtsetquiz26 int txtsetquiz26_arg1
txtsetquiz27 int txtsetquiz27_arg1
txtsetquiz28 int txtsetquiz28_arg1
txtsetquiz29 int txtsetquiz29_arg1
txtsetquiz30 int txtsetquiz30_arg1
txtsetquiz31 int txtsetquiz31_arg1
txtsetquiz32 int txtsetquiz32_arg1
txtsetquiz33 int txtsetquiz33_arg1
txtsetquiz34 int txtsetquiz34_arg1
txtsetnecrom int txtsetnecrom_arg1
txtsettamer int txtsettamer_arg1
txtsetshop int txtsetshop_arg1
txtsetshopkazu int txtsetshopkazu_arg1
txtsetshopfeat int txtsetshopfeat_arg1
txtsetybank int txtsetybank_arg1
txtsetride int txtsetride_arg1
txtsettezca int txtsettezca_arg1
txtselecttc int txtselecttc_arg1
txtselectride int txtselectride_arg1
txtseteyes int txtseteyes_arg1
txtsetbirth int txtsetbirth_arg1
txtsetnecro int txtsetnecro_arg1
txtsetnefia int txtsetnefia_arg1
txtplusbody int txtplusbody_arg1
txtsettactical int txtsettactical_arg1
txtsetequipw int txtsetequipw_arg1
txtsetchocoword int txtsetchocoword_arg1
txtsetchocoword2 int txtsetchocoword2_arg1
txtsetchocoword3 int txtsetchocoword3_arg1
limitmax int limitmax_arg1, int limitmax_arg2
maplevel int
mapfile int mapfile_mapid
mapname int mapname_mapid, int mapname_arg2
txtbuilding int txtbuilding_arg1, int txtbuilding_arg2
txtskillchange int txtskillchange_skillid, int txtskillchange_negative, int txtskillchange_charid
foodname int foodname_arg1, str foodname_arg2, int foodname_arg3, int foodname_arg4
_yoro int _yoro_arg1
_dozo int _dozo_arg1
_thanks int _thanks_arg1
_birth int _birth_arg1
_rob int _rob_arg1
_ka int _ka_arg1
_da int _da_arg1
_nda int _nda_arg1
_noka int _noka_arg1
_kana int _kana_arg1
_kimi int _kimi_arg1
_miruna int _miruna_arg1
_ru int _ru_arg1
_tanomu int _tanomu_arg1
_ore int _ore_arg1
_ga int _ga_arg1
_dana int _dana_arg1
_siro int _siro_arg1
_kure int _kure_arg1
_daro int _daro_arg1
_yo int _yo_arg1
_aru int _aru_arg1
_u int _u_arg1
_na int _na_arg1
_ta int _ta_arg1
_trick int _trick_arg1
cnvweight int cnvweight_arg1
fltname int fltname_type
addnews2 str addnews2_arg1, int addnews2_arg2
addnewstopic str addnewstopic_arg1, str addnewstopic_arg2
addnews int addnews_arg1, int addnews_arg2
txtgod int txtgod_godid, int txtgod_flavor
create_pcpic int create_pcpic_arg1, int create_pcpic_arg2
addefmap int addefmap_arg1, int addefmap_arg2, int addefmap_arg3, int addefmap_arg4, int addefmap_arg5, int addefmap_arg6
cardplayerinit int cardplayerinit_arg1, int cardplayerinit_arg2
cardplayeradd int cardplayeradd_arg1, int cardplayeradd_arg2, int cardplayeradd_arg3
initcard int initcard_arg1, int initcard_arg2, int
showcard2 int showcard2_arg1, int showcard2_arg2
showcardpile
showcard
servecard int servecard_arg1
showcardholder
opencard2 int opencard2_arg1, int opencard2_arg2
trashcard int trashcard_arg1
cpscore int cpscore_arg1
lastcard int lastcard_arg1
cpblackcard int cpblackcard_arg1
cpcardnum int cpcardnum_arg1
pileremain
csvsort array csvsort_arg1, var csvsort_arg2, int csvsort_arg3
randomname int
random_title int random_title_arg1
getinheritance int getinheritance_arg1, array getinheritance_arg2, var getinheritance_arg3
calcobjlv int calcobjlv_arg1
calcfixlv int calcfixlv_startquality
flt int flt_objlv, int flt_qualitylv
fltn str fltn_arg1
discsetmc
fltsetdungeon
randomele
putenclv int putenclv_arg1
encflt int encflt_filter1, int encflt_filter2
randomenc int randomenc_arg1
randomenclv int randomenclv_arg1
randomencp int randomencp_arg1
sortenc int sortenc_arg1
encremove int encremove_arg1, int encremove_arg2, int encremove_arg3
encadd int encadd_arg1, int encadd_arg2, int encadd_arg3, int encadd_arg4, int encadd_arg5, int encadd_arg6, int encadd_arg7
egoadd int egoadd_arg1, int egoadd_arg2
itemcreate int itemcreate_charid, int itemcreate_dbid, int itemcreate_x, int itemcreate_y, int itemcreate_num
inv_getheader int inv_getheader_arg1
inv_getowner int inv_getowner_arg1
inv_find int inv_find_arg1, int inv_find_arg2
fix_find int fix_find_itemid, int fix_find_charid
item_find int item_find_itemid, int item_find_mode, int item_find_inv
encfind int encfind_arg1, int encfind_arg2
encfindpower int encfindpower_arg1, int encfindpower_arg2
encfindspec int encfindspec_arg1, int encfindspec_arg2
itemlist int itemlist_arg1, int itemlist_arg2
itemusingfind int itemusingfind_arg1, int itemusingfind_arg2
itemfind int itemfind_arg1, int itemfind_arg2, int itemfind_arg3
allitemfind int allitemfind_arg1
mapfoodtypefind int mapfoodtypefind_x, int mapfoodtypefind_y, int mapfoodtypefind_arg3
mapitemfind int mapitemfind_x, int mapitemfind_y, int mapitemfind_itemid
cell_refresh int cell_refresh_x, int cell_refresh_y
itemturn int itemturn_itemid
removeitem int removeitem_itemid, int removeitem_amount
item_copy int item_copy_src, int item_copy_dst
item_exchange int item_exchange_itemid1, int item_exchange_itemid2
item_delete int item_delete_itemid
inv_getspace int inv_getspace_charid
inv_sum int inv_sum_charid
item_compress int
inv_getfreeid int inv_getfreeid_charid
inv_weight int inv_weight_charid
item_num int item_num_itemid, int item_num_amount
item_separate var item_separate_itemid
item_separatefish var item_separatefish_itemid
chara_unequip int chara_unequip_itemid
item_identify int item_identify_itemid, int item_identify_idlevel, int item_identify_power
item_checkknown int item_checkknown_itemid
rpname int rpname_arg1
itemowner int itemowner_itemid
itemname int itemname_itemid, int itemname_arg2, int itemname_arg3
remain_make int remain_make_itemid, int remain_make_arg2
make_dish int make_dish_arg1, int make_dish_arg2
item_stack int item_stack_charid, int item_stack_itemid, int item_stack_mode
colorres int
equipinfo int equipinfo_arg1, int equipinfo_arg2, int equipinfo_arg3
csvstr2 array csvstr2_arg1, str csvstr2_arg2
cargocheck
rndshuffle array rndshuffle_arg1
dist int dist_arg1, int dist_arg2, int dist_arg3, int dist_arg4
winposy int winposy_arg1, int winposy_arg2
cutname var cutname_arg1, int cutname_arg2
cs_list str cs_list_arg1, int cs_list_arg2, int cs_list_arg3, int cs_list_arg4, int cs_list_arg5, int cs_list_arg6, int cs_list_arg7
cs_listbk
gohostile
modkarma int modkarma_arg1, int modkarma_arg2
modrank int modrank_arg1, int modrank_arg2, int modrank_arg3
sexp int sexp_skillid, int sexp_charid
sgrowth int sgrowth_skillid, int sgrowth_charid
modgrowth int modgrowth_charid, int modgrowth_skillid, int modgrowth_arg3
skillgain int skillgain_charid, int skillgain_skillid, int skillgain_arg3, int skillgain_arg4
skillmod int skillmod_skillid, int skillmod_charid, int skillmod_arg3
skillexp int skillexp_skillid, int skillexp_charid, int skillexp_amount, int skillexp_arg4, int skillexp_arg5
calcfame int calcfame_charid, int calcfame_amount
decfame int decfame_charid, int decfame_amount
getworker int getworker_arg1, int getworker_arg2
removeworker int removeworker_arg1
calcshopreform
delmef int delmef_arg1
addmef int addmef_arg1, int addmef_arg2, int addmef_arg3, int addmef_arg4, int addmef_arg5, int addmef_arg6, int addmef_arg7, int addmef_arg8, int addmef_arg9, int addmef_arg10
cell_featset int cell_featset_arg1, int cell_featset_arg2, int cell_featset_arg3, int cell_featset_arg4, int cell_featset_arg5, int cell_featset_arg6
cell_featread int cell_featread_arg1, int cell_featread_arg2, int
cell_featclear int cell_featclear_arg1, int cell_featclear_arg2
cell_check int cell_check_arg1, int cell_check_arg2
cell_swap int cell_swap_arg1, int cell_swap_arg2, int cell_swap_arg3, int cell_swap_arg4
cell_movechara int cell_movechara_arg1, int cell_movechara_arg2, int cell_movechara_arg3
route_info var route_info_arg1, var route_info_arg2, int route_info_arg3
breath_list
draw_emo int draw_emo_arg1, int draw_emo_arg2, int draw_emo_arg3
chara_preparepic int chara_preparepic_arg1, int chara_preparepic_arg2
cell_shownull int cell_shownull_x, int cell_shownull_y
cell_show int cell_show_x, int cell_show_y, int cell_show_see
cell_draw
cell_itemlist int cell_itemlist_arg1, int cell_itemlist_arg2
cell_itemoncell int cell_itemoncell_arg1, int cell_itemoncell_arg2
fov_los int fov_los_arg1, int fov_los_arg2, int fov_los_arg3, int fov_los_arg4
get_route int get_route_arg1, int get_route_arg2, int get_route_arg3, int get_route_arg4
display_customkey str display_customkey_arg1, int display_customkey_arg2, int display_customkey_arg3, int
display_key int display_key_arg1, int display_key_arg2, int display_key_arg3
cursor_check
lenfix var lenfix_arg1, int lenfix_arg2
showscroll int showscroll_arg1, int showscroll_arg2, int showscroll_arg3, int showscroll_arg4
window int window_arg1, int window_arg2, int window_arg3, int window_arg4, int, int window_arg5
window2 int window2_arg1, int window2_arg2, int window2_arg3, int window2_arg4, int window2_arg5, int window2_arg6
display_window2 int display_window2_arg1, int display_window2_arg2, int display_window2_arg3, int display_window2_arg4, int display_window2_arg5, int display_window2_arg6
display_window int display_window_arg1, int display_window_arg2, int display_window_arg3, int display_window_arg4, int display_window_arg5, int display_window_arg6
display_note str display_note_arg1, int display_note_arg2
display_topic str display_topic_arg1, int display_topic_arg2, int display_topic_arg3, int
display_msg int display_msg_arg1, int display_msg_arg2
id_crlf var id_crlf_arg1
conv_crlf var conv_crlf_arg1, int conv_crlf_arg2
rm_crlf var rm_crlf_arg1
role int role_arg1, int role_arg2, int role_arg3
rolemax int rolemax_arg1, int rolemax_arg2, int rolemax_arg3
page_save
page_load
fileadd str fileadd_arg1, int fileadd_arg2
arrayfile
validatetmp str validatetmp_arg1, int validatetmp_arg2
zopenwrapper var zopenwrapper_arg1, str zopenwrapper_arg2, int zopenwrapper_arg3, int zopenwrapper_arg4
existwrapper str existwrapper_arg1
deletewrapper str deletewrapper_arg1
arrayfilewrapper
dirlistwrapper var dirlistwrapper_arg1, str dirlistwrapper_arg2
fmode13replacer int fmode13replacer_arg1
del_str str del_str_arg1, str del_str_arg2
cnv_str var cnv_str_arg1, str cnv_str_arg2, str cnv_str_arg3
fix_wish var fix_wish_arg1
fix_input_chat var fix_input_chat_arg1
fix_input_chat2 var fix_input_chat2_arg1
cnv_filestr var cnv_filestr_arg1
imeset int imeset_arg1
imeget
_fdialog array _fdialog_arg1, int _fdialog_arg2, array _fdialog_arg3, str _fdialog_arg4, str _fdialog_arg5
gmes str gmes_arg1
boxl int boxl_arg1, int boxl_arg2, int boxl_arg3, int boxl_arg4
topicbox int topicbox_arg1, int topicbox_arg2, int topicbox_arg3, int topicbox_arg4
fixtxt str fixtxt_arg1, int fixtxt_arg2
windowanime int windowanime_arg1, int windowanime_arg2, int windowanime_arg3, int windowanime_arg4, int windowanime_arg5, int windowanime_arg6
windowanimecorner int windowanimecorner_arg1, int windowanimecorner_arg2, int windowanimecorner_arg3, int windowanimecorner_arg4, int windowanimecorner_arg5, int windowanimecorner_arg6
showtitle str, str showtitle_arg1, int showtitle_arg2, int
drawmenu int drawmenu_arg1
fillbg int fillbg_arg1, int fillbg_arg2, int fillbg_arg3, int fillbg_arg4, int fillbg_arg5
direction int direction_arg1, int direction_arg2, int direction_arg3, int direction_arg4
prodcheck
calcweaponfix int calcweaponfix_itemid
fixaiact int fixaiact_arg1
eleinfo int eleinfo_skillid, int eleinfo_mode
characreate int characreate_arg1, int characreate_arg2, int characreate_arg3, int characreate_arg4
relationbetween int relationbetween_arg1, int
calcage int calcage_arg1
rowactend int rowactend_arg1
customtalk int customtalk_charid, int customtalk_dbmode
findchara int findchara_charid
findcharaaz int findcharaaz_charid
findcharala int findcharala_charid
findallyguard int findallyguard_charid
findally int findally_charid
implevel int implevel_arg1
modimp int modimp_charid, int modimp_arg2
implevel2 int implevel2_arg1
modimp2 int modimp2_charid, int modimp2_arg2
put_questtarget
exist_questtarget
check_quest
refreshspeed int refreshspeed_arg1
tag_begin int tag_begin_arg1, int tag_begin_arg2
tag_end int tag_end_arg1
ride_begin int ride_begin_arg1
ride_end
chara_vanquish int chara_vanquish_charid
turn_aggro int turn_aggro_arg1, int turn_aggro_arg2, int turn_aggro_arg3
make_sound int make_sound_arg1, int make_sound_arg2, int make_sound_arg3, int make_sound_arg4, int make_sound_arg5, int make_sound_arg6
get_freechara
get_freeally
get_freeallyne
del_chara int del_chara_arg1
relocate_chara int relocate_chara_arg1, int relocate_chara_arg2, int relocate_chara_arg3
hostileaction int hostileaction_target, int hostileaction_source
rowact_check int rowact_check_charid
rowact_item int rowact_item_arg1
wake_up
incognitobegin
incognitoend
cell_setchara int cell_setchara_arg1, int cell_setchara_arg2, int cell_setchara_arg3
cell_removechara int cell_removechara_arg1, int cell_removechara_arg2
cell_findspace int cell_findspace_arg1, int cell_findspace_arg2, int cell_findspace_arg3
findbuff int findbuff_charid, int findbuff_buffid
addbuff int addbuff_charid, int addbuff_buffid, int addbuff_arg3, int addbuff_arg4
delbuff int delbuff_charid, int delbuff_buffid
animeload int animeload_arg1, int animeload_arg2
animeblood int animeblood_arg1, int animeblood_arg2, int animeblood_arg3
resistmod int resistmod_charid, int resistmod_ele, int resistmod_arg3
resistmodh int resistmodh_charid, int resistmodh_ele, int resistmodh_arg3
modcorrupt int modcorrupt_arg1
wet int wet_arg1, int wet_arg2
dmgcon int dmgcon_charid, int dmgcon_cond, int dmgcon_arg3
healhp int healhp_charid, int healhp_amount
healmp int healmp_charid, int healmp_amount
healsp int healsp_charid, int healsp_amount
healcon int healcon_charid, int healcon_cond, int healcon_arg3
spillblood int spillblood_arg1, int spillblood_arg2, int spillblood_arg3
spillfrag int spillfrag_arg1, int spillfrag_arg2, int spillfrag_arg3
check_talk int check_talk_arg1, int check_talk_arg2
check_kill int check_kill_arg1, int check_kill_arg2
item_acid int item_acid_arg1, int item_acid_arg2
item_fire int item_fire_arg1, int item_fire_arg2
mapitem_fire int mapitem_fire_arg1, int mapitem_fire_arg2
item_cold int item_cold_arg1, int item_cold_arg2
mapitem_cold int mapitem_cold_arg1, int mapitem_cold_arg2
copy_chara int copy_chara_arg1
txteledmg int txteledmg_arg1, int txteledmg_arg2, int txteledmg_arg3, int txteledmg_ele
dmghp int dmghp_charid, int dmghp_origin, int dmghp_source, int dmghp_element, int dmghp_power
dmgmp int dmgmp_charid, int dmgmp_arg2
dmgsp int dmgsp_charid, int dmgsp_arg2
dmgspt int dmgspt_charid, int dmgspt_amount
dmgsptalk int dmgsptalk_charid, int dmgsptalk_amount
healsan int healsan_charid, int healsan_arg2
dmgsan int dmgsan_charid, int dmgsan_arg2
actionsp int actionsp_charid, int actionsp_arg2
advfavoriteskill int advfavoriteskill_arg1
advfavoritestat int advfavoritestat_arg1
modweight int modweight_charid, int modweight_arg2, int modweight_arg3
modheight int modheight_charid, int modheight_arg2
cure_anorexia int cure_anorexia_charid
chara_vomit int chara_vomit_charid
chara_morasi int chara_morasi_arg1
eatstatus int eatstatus_arg1, int eatstatus_arg2
chara_anorexia int chara_anorexia_arg1
sickifcursed int sickifcursed_arg1, int sickifcursed_arg2, int sickifcursed_arg3
net_send str net_send_arg1, int net_send_arg2
net_read int net_read_arg1
net_dllist str net_dllist_arg1, int net_dllist_arg2
net_dl str net_dl_arg1, str net_dl_arg2
carmor int carmor_arg1
cnveqweight int cnveqweight_arg1
calcexpalive int calcexpalive_arg1
calcattackhit int calcattackhit_arg1
calcattackdmg int calcattackdmg_arg1
cnvbonus int cnvbonus_arg1, int cnvbonus_arg2
calcmedalvalue int calcmedalvalue_itemid
calcmusicticketvalue int calcmusicticketvalue_itemid
calcitemvalue int calcitemvalue_itemid, int calcitemvalue_mode
calcinvestvalue int
calcinvestvalue50 int
calcguiltvalue int
calchireadv int calchireadv_arg1
calchirecost int calchirecost_charid
generatemoney int generatemoney_charid
calccosthire
calccostbuilding
calccosttax
calcmealvalue
calccostreload int calccostreload_arg1, int calccostreload_arg2
calccargoupdate
calccargoupdatecost
calcidentifyvalue int calcidentifyvalue_arg1
calctraincost int calctraincost_arg1, int calctraincost_arg2, int calctraincost_arg3
calclearncost int, int, int calclearncost_arg1
calcresurrectvalue int calcresurrectvalue_arg1
calcslavevalue int calcslavevalue_arg1
calcrestorecost
calcrestorecostc
calcinitgold int calcinitgold_arg1
calcspellpower int calcspellpower_skillid, int calcspellpower_charid
calcspellfail int calcspellfail_skillid, int calcspellfail_charid
calcspellcostmp int calcspellcostmp_skillid, int calcspellcostmp_charid
calcspellcoststock int calcspellcoststock_skillid, int calcspellcoststock_charid
skillinit int skillinit_skillid, int skillinit_charid, int skillinit_arg3
calcscore
calcpartyscore
calcpartyscore2
eqweaponlight
eqweaponheavy
eqrandweaponmage int eqrandweaponmage_arg1
convertartifact int convertartifact_arg1, int convertartifact_arg2
lovemiracle int lovemiracle_arg1
lovemiracle2 int lovemiracle2_arg1
map_converttile
map_tileset int map_tileset_arg1
map_initcustom str map_initcustom_mapname
map_reload str map_reload_arg1
map_initialize
map_placecharaonentrance int map_placecharaonentrance_arg1, int map_placecharaonentrance_arg2, int map_placecharaonentrance_arg3
map_placearena int map_placearena_arg1, int map_placearena_arg2
map_placeplayer
map_randomtile int map_randomtile_arg1, int map_randomtile_arg2
map_line int map_line_arg1, int map_line_arg2, int map_line_arg3, int map_line_arg4
map_makesimpleroom int map_makesimpleroom_arg1, int map_makesimpleroom_arg2, int map_makesimpleroom_arg3, int map_makesimpleroom_arg4, int map_makesimpleroom_arg5, int map_makesimpleroom_arg6
map_digcheck int map_digcheck_arg1, int map_digcheck_arg2
map_nextdir1 int map_nextdir1_arg1, int map_nextdir1_arg2
map_nextdir2 int map_nextdir2_arg1, int map_nextdir2_arg2
map_ovservemap
map_digtoentrance1 int map_digtoentrance1_arg1, int map_digtoentrance1_arg2, int map_digtoentrance1_arg3, int map_digtoentrance1_arg4, int map_digtoentrance1_arg5
map_setfog int, int
map_createroomdoor
map_createroom int map_createroom_arg1
map_placeupstairs int map_placeupstairs_arg1, int map_placeupstairs_arg2
map_placedownstairs int map_placedownstairs_arg1, int map_placedownstairs_arg2
map_randsite int map_randsite_arg1, int map_randsite_arg2
map_fever int map_fever_arg1, int map_fever_arg2
map_trap int map_trap_arg1, int map_trap_arg2, int, int map_trap_arg3
map_web int map_web_arg1, int map_web_arg2, int map_web_arg3
map_barrel int map_barrel_arg1, int map_barrel_arg2
map_connectroom
map_makedoor
selectcoast int selectcoast_arg1
dimmix array dimmix_arg1
GetTabbedParameter str FilePath, str SearchString
RemoveTabs str RTInput
RemoveLineBreaks str RLBInput
AddLineBreaks str ALBInput
AddTabs str ATInput
ParseString str InputString
equipinfo2 int eq2_itempos, int eq2_posx, int eq2_posy, int eq2_setnum
cbreeder int cbreeder_charid
cdbit int cdbit_bit, int cdbit_charid
cdbitmod int cdbitmod_bit, int cdbitmod_charid, int cdbitmod_seton
cpflip
cpisplayer
cpisme
cpisenemy
cnvrare int cnvrare_arg1
card_ref int card_ref_arg1
dbghit
makecardlist
cardhelp str cardhelp_arg1, int cardhelp_arg2
tcgdrawcard int tcgdrawcard_arg1, int tcgdrawcard_arg2
tcgdraw
efllistadd int efllistadd_arg1, int efllistadd_arg2, int efllistadd_arg3, int efllistadd_arg4, int efllistadd_arg5, int efllistadd_arg6
create_card int create_card_arg1, int create_card_arg2
cardpos int cardpos_arg1, int cardpos_arg2
gravecard int gravecard_arg1
dmgcard int dmgcard_arg1, int dmgcard_arg2
dmgplayer int dmgplayer_arg1, int dmgplayer_arg2
delbottomcard int delbottomcard_arg1
gameover
getholdersum int getholdersum_arg1
getspotsum int getspotsum_arg1
getdecksum int getdecksum_arg1
cardcandeclareattack int cardcandeclareattack_arg1
cardcanblock int cardcanblock_arg1
cardcanuseskill int cardcanuseskill_arg1
getrandomcard int getrandomcard_arg1
saccard int saccard_arg1, int saccard_arg2
opencard int opencard_arg1
activatecard int activatecard_arg1, int
actionchain
actionproc
putcard int putcard_arg1, int putcard_arg2
tcgdrawbg
tcginit
calcstartcard int calcstartcard_arg1
calcstartattb int calcstartattb_arg1
calcdomain
calcdecksize
decktest
tcgdeck
tcgmain
csfix
cslineup
cslinedown
random_material int random_material_matlv, int random_material_matrare
matgetmain int matgetmain_arg1, int matgetmain_arg2, int matgetmain_arg3
matdelmain int matdelmain_arg1, int matdelmain_arg2
atxinit
begintempinv
exittempinv
modpiety int modpiety_arg1
calcincome int calcincome_arg1
rpmatname int rpmatname_arg1
blendcheckext int blendcheckext_arg1, int blendcheckext_arg2
rpsuccessrate int rpsuccessrate_rate
rpdiff int, int rpdiff_arg1, int rpdiff_arg2
blendcheckmat int blendcheckmat_arg1
blendmatnum int blendmatnum_arg1, int blendmatnum_arg2
blendlist array blendlist_arg1, int blendlist_arg2
window_recipe2
window_recipe array, int window_recipe_itemid, int window_recipe_arg2, int window_recipe_arg3, int window_recipe_arg4, int window_recipe_arg5
clear_rprefmat
txtitemoncell int txtitemoncell_arg1, int txtitemoncell_arg2
txttargetnpc int txttargetnpc_arg1, int txttargetnpc_arg2, int txttargetnpc_arg3
key_direction
savecycle
trimdesc str trimdesc_arg1, int trimdesc_arg2
zipadd str zipadd_arg1
zipinit2 str zipinit2_arg1, str zipinit2_arg2
zipadd2 str zipadd2_arg1
zipend2
unzip2 str unzip2_arg1, str unzip2_arg2
getnpctxt str getnpctxt_arg1, str getnpctxt_arg2
cnvvar var cnvvar_arg1, str cnvvar_arg2
efstatusfix int efstatusfix_arg1, int efstatusfix_arg2, int efstatusfix_arg3, int efstatusfix_arg4
calcmagiccontrol int calcmagiccontrol_arg1, int calcmagiccontrol_arg2
dipcursed int dipcursed_item, int
clientguide int
tradecheck int tradecheck_charid
randname int
randnameinit
ai_check
cnvjkey str cnvjkey_arg1
END

input.lines.each do |line| 
  s = line.split(" ", 2)
  name = s[0]
  args = s[1].split(",")
  unless args.empty?
    args.last.strip! 
  end
  if args.empty?
    s = <<END
"#{name}": (
  args: {}
),
END
  else
    argstring = args.each_with_index.map do |a, i| 
      s = a.split(" ")
      argtype = s[0].capitalize
      argname = s[1] || "#{name}_arg#{i}"
      if argname.start_with? (name + "_")
        argname.delete_prefix! (name + "_")
      end
      "    #{i}: (name: \"#{argname}\", type: #{argtype}),"
    end.join("\n")
    s = <<END
"#{name}": (
  args: {
    #{argstring}
  }
),
END
    puts s
  end
end
