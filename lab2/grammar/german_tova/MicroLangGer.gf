--# -path=.:../abstract
concrete MicroLangGer of MicroLang = open MicroResGer, Prelude in {

-----------------------------------------------------
---------------- Grammar part -----------------------
-----------------------------------------------------

  lincat
    Utt = {s : Str} ;
    
    S  = {s : Str} ;
    VP = {verb : Verb ; compl : Str} ; ---s special case of Mini
    Comp = {s : Str} ;
    AP = Adjective ;
    CN = Noun ;
    NP = {s : Case => Str ; a : Agreement} ;
    Pron = {s : Case => Str ; a : Agreement} ;
    Det = {s : Str ; n : Number} ;
    Prep = {s : Str} ;
    V = Verb ;
    V2 = Verb2 ; --transitive? 
    A = Adjective ;
    N = Noun ;
    Adv = {s : Str} ;

  lin
    UttS s = s ;
    UttNP np = {s = np.s ! Acc} ;

    PredVPS np vp = {
      s = np.s ! Nom ++ vp.verb.s ! agr2vform np.a ++ vp.compl
      } ;
      
    UseV v = {
      verb = v ;
      compl = [] ;
      } ;
      
    ComplV2 v2 np = {
      verb = v2 ;
      compl = v2.c ++ np.s ! Acc  -- NP object in the accusative, preposition first
      } ;
      
    UseComp comp = {
      verb = be_Verb ;     -- the verb is the copula "be"
      compl = comp.s
      } ;
      
    CompAP ap = ap ;
      
    AdvVP vp adv =
      vp ** {compl = vp.compl ++ adv.s} ;
      
    DetCN det cn = {
      s = \\c => det.s ++ cn.s ! det.n ;
      a = Agr det.n ;
      } ;
      
    UsePron p = p ;
            
    a_Det = {s = pre {"a"|"e"|"i"|"o" => "an" ; _ => "a"} ; n = Sg} ; --- a/an can get wrong
    aPl_Det = {s = "" ; n = Pl} ;
    the_Det = {s = "the" ; n = Sg} ;
    thePl_Det = {s = "the" ; n = Pl} ;
    
    UseN n = n ;
    
    AdjCN ap cn = {
      s = table {n => ap.s ++ cn.s ! n}
      } ;

    PositA a = a ;

    PrepNP prep np = {s = prep.s ++ np.s ! Acc} ;

    in_Prep = {s = "in"} ;
    on_Prep = {s = "on"} ;
    with_Prep = {s = "with"} ;

    he_Pron = {
      s = table {Nom => "he" ; Acc => "him"} ;
      a = Agr Sg ;
      } ;
    she_Pron = {
      s = table {Nom => "she" ; Acc => "her"} ;
      a = Agr Sg ;
      } ;
    they_Pron = {
      s = table {Nom => "they" ; Acc => "them"} ;
      a = Agr Pl ;
      } ;

-----------------------------------------------------
---------------- Lexicon part -----------------------
-----------------------------------------------------

lin already_Adv = mkAdv "schon" ;
lin animal_N = mkN "tier" ;
lin apple_N = mkN "Apfel" "Äpfel" ;
lin baby_N = mkN "Baby" ;
lin bad_A = mkA "schlecht" ;
lin beer_N = mkN "Bier" ;
lin big_A = mkA "gross" ;
lin bike_N = mkN "Fahrrad" "Fahrräder" ;
lin bird_N = mkN "Vogel" "Vögel" ;
lin black_A = mkA "schwarz" ;
lin blood_N = mkN "Blut" ;
lin blue_A = mkA "blau" ;
lin boat_N = mkN "Boot" ;
lin book_N = mkN "Buch" "Bücher" ;
lin boy_N = mkN "Junge" ;
lin bread_N = mkN "Brot" ;
lin break_V2 = mkV2 (mkV "brechen" "brach" "gebrochen") ; 
lin buy_V2 = mkV2 (mkV "kaufen" "kaufte" "gekauft") ;
lin car_N = mkN "Auto" ;
lin cat_N = mkN "Katze" ;
lin child_N = mkN "Kind" "Kinder" ;
lin city_N = mkN "Stadt" "Städte" ;
lin clean_A = mkA "sauber" ;
lin clever_A = mkA "schlau" ;
lin cloud_N = mkN "Wolke" ;
lin cold_A = mkA "kalt" ;
lin come_V = mkV "kommen" "kam" "gekommen" ;
lin computer_N = mkN "Computer" ;
lin cow_N = mkN "Kuh" "Kühe" ;
lin dirty_A = mkA "schmutzig" ;
lin dog_N = mkN "Hund" ;
lin drink_V2 = mkV2 (mkV "trinken" "trank" "getrunken") ;
lin eat_V2 = mkV2 (mkV "essen" "ass" "gegessen") ;
lin find_V2 = mkV2 (mkV "finden" "fand" "gefunden") ;
lin fire_N = mkN "Feuer" ;
lin fish_N = mkN "Fisch";
lin flower_N = mkN "Blume" ;
lin friend_N = mkN "Freund" ;
lin girl_N = mkN "Mädchen" ;
lin good_A = mkA "gut" ;
lin go_V = mkV "gehen" "ging" "gegangen" ;
lin grammar_N = mkN "Grammatik" ;
lin green_A = mkA "grün" ;
lin heavy_A = mkA "schwer" ;
lin horse_N = mkN "Pferd" ;
lin hot_A = mkA "warm" ;
lin house_N = mkN "Haus" ;
-- lin john_PN = mkPN "John" ;
lin jump_V = mkV "springen" "sprang" "gesprungen" ;
lin kill_V2 = mkV2 "töten" ;
-- lin know_VS = mkVS (mkV "wissen" "wusste" "gewusst") ;
lin language_N = mkN "Sprache" ;
lin live_V = mkV "leben" ;
lin love_V2 = mkV2 (mkV "lieben") ;
lin man_N = mkN "Mann" "Männer" ;
lin milk_N = mkN "Milch" ;
lin music_N = mkN "Musik" ;
lin new_A = mkA "neu" ;
lin now_Adv = mkAdv "jetzt" ;
lin old_A = mkA "alt" ;
-- lin paris_PN = mkPN "Paris" ;
lin play_V = mkV "spielen" ;
lin read_V2 = mkV2 (mkV "lesen" "las" "gelesen") ;
lin ready_A = mkA "bereit" ;
lin red_A = mkA "rot" ;
lin river_N = mkN "Fluss" "Flüsse" ;
lin run_V = mkV "laufen" "lief" "gelaufen" ;
lin sea_N = mkN "See" ;
lin see_V2 = mkV2 (mkV "sehen" "sah" "gesehen") ;
lin ship_N = mkN "Schiff" ;
lin sleep_V = mkV "schlafen" "schlief" "geschlafen" ;
lin small_A = mkA "klein" ;
lin star_N = mkN "Stern" ;
lin swim_V = mkV "schwimmen" "schwamm" "geschwommen" ;
lin teach_V2 = mkV2 (mkV "lehren" "lehrte" "gelehrt") ;
lin train_N = mkN "Zug" "Züge" ;
lin travel_V = mkV "reisen" ;
lin tree_N = mkN "Baum" "Bäume" ;
lin understand_V2 = mkV2 (mkV "verstehen" "verstand" "verstanden") ;
lin wait_V2 = mkV2 "warten" "auf" ;
lin walk_V = mkV "gehen";
lin warm_A = mkA "warm" ;
lin water_N = mkN "Wasser" ;
lin white_A = mkA "weiss" ;
lin wine_N = mkN "Wein" ;
lin woman_N = mkN "Frau" ;
lin yellow_A = mkA "gelb" ;
lin young_A = mkA "jung" ;

---------------------------
-- Paradigms part ---------
---------------------------

oper
  mkN = overload {
    mkN : Str -> Noun   -- predictable noun, e.g. car-cars, boy-boys, fly-flies, bush-bushes
      = \n -> lin N (smartNoun n) ;
    mkN : Str -> Str -> Noun  -- irregular noun, e.g. man-men
      = \sg,pl -> lin N (mkNoun sg pl) ;
    } ;

  mkA : Str -> A
    = \s -> lin A {s = s} ;

  mkV = overload {
    mkV : (inf : Str) -> V  -- predictable verb, e.g. play-plays, cry-cries, wash-washes
      = \s -> lin V (smartVerb s) ;
    mkV : (inf,pres,part : Str) -> V  -- irregular verb, e.g. drink-drank-drunk
      = \inf,pres,part -> lin V (irregVerb inf pres part) ;
    } ;

  mkV2 = overload {
    mkV2 : Str -> V2          -- predictable verb with direct object, e.g. "wash"
      = \s   -> lin V2 (smartVerb s ** {c = []}) ;
    mkV2 : Str  -> Str -> V2  -- predictable verb with preposition, e.g. "wait - for"
      = \s,p -> lin V2 (smartVerb s ** {c = p}) ;
    mkV2 : V -> V2            -- any verb with direct object, e.g. "drink"
      = \v   -> lin V2 (v ** {c = []}) ;
    mkV2 : V -> Str -> V2     -- any verb with preposition
      = \v,p -> lin V2 (v ** {c = p}) ;
    } ;

  mkAdv : Str -> Adv
    = \s -> lin Adv {s = s} ;
  
  mkPrep : Str -> Prep
    = \s -> lin Prep {s = s} ;

}
