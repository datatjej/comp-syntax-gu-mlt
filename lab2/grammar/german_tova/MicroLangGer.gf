--# -path=.:../abstract
concrete MicroLangGer of MicroLang = open MicroResGer, Prelude in {

-----------------------------------------------------
---------------- Grammar part -----------------------
-----------------------------------------------------

  lincat
    Utt = {s : Str} ;
    
    S  = {s : Str} ;
    VP = {verb : Verb ; compl : UseAP => Str; isPron : Bool } ;
    Pron = {s : Case => Str ; g : Gender ; n : Number } ;
    Det = Determiner ;
    Prep = Preposition; -- {s : Str} ;
    V = Verb ;
    V2 = Verb2 ;
    N,CN = Noun;
    NP = {s : Case => Str ; g : Gender ; n : Number; isPron : Bool} ; 
    Adv = {s : Str} ;
    A,AP,Comp = Adjective ;

  lin
    UttS s = s ;
    
    UttNP np = {s = np.s ! Acc} ;
    
    PredVPS np vp = {                      --- John walks
    s = case np.isPron of {
	  True => case np.n of { Sg => np.s ! Nom ; Pl => "" } ;
	  False => np.s ! Nom
	  } ++ vp.verb.s ! Pres np.n P3 ++ vp.compl ! Pred -- ÄNDRA TILL ATTR??
    };

    UseV v = {           --sleep      V --> VP      
    verb = v ;                      -- Verb : Type = {s : VForm => Str} ;   -- VForm = Inf | Pres Number Person ;
    compl = \\_ => [] ;             --  VP = {verb : Verb ; compl : UseAP => Str; isPron : Bool } ;
    isPron = False 
    } ;
      
    ComplV2 v2 np = {           -- V2 -> NP -> VP        -- love it                      
      verb = v2 ;                                         -- Verb2 : Type = Verb ** {c : Gender => Number => Str} ; 
      compl = \\_ => v2.c ++ np.s ! Acc;         -- NP = {s : Case => Str ; g : Gender ; n : Number; isPron : Bool} ; 
      isPron = np.isPron
	   } ;
    
    UseComp comp = {
      verb = be_Verb ;
      compl = \\_ => comp.s ! Pred ;
      isPron = False 
    };

    CompAP ap = {s = \\_ => ap.s ! Pred} ;  -- Afrikans lib/src/afrikaans/VerbAfr.gf:  CompAP ap = {s = \\_ => ap.s ! APred} ;

    AdvVP vp adv =                --sleep here         -- VP -> Adv -> VP       
      vp ** {compl = \\a => vp.compl ! a ++ adv.s} ;  


    DetCN det cn = {            -- Det -> CN -> NP ;
    s = \\c => det.s ! cn.g ! c ++ cn.s ! det.n ! c ;
    g = cn.g;
    n = det.n;
    isPron = False
    } ;

   UsePron p = p ** { det = "" ; isPron = True } ;
   
  a_Det = {s = table {Masc => table          {Nom => "ein" ;
                                              Gen => "eines" ;
                                              Dat => "einem" ;
                                              Acc => "einen" } ;
                      
                      Fem => table {Nom => "eine" ;
                                              Gen => "einer" ;
                                              Dat => "einer" ;
                                              Acc => "eine" } ;
                      
                      Neut => table         {Nom => "ein" ;
                                              Gen => "eines" ;
                                              Dat => "einem" ;
                                              Acc => "ein" } 
                      } ;
          n = Sg;
          af = Mixed };          

   aPl_Det = {s = table {
		 _ => table {
       _ => ""
     }
    };
    n = Pl;
    af = Strong
    };

  -----------------------

      the_Det = {s = table {Masc => table 
                                {Nom => "der" ; 
                                Gen => "des" ;
                                Dat => "dem" ;
                                Acc => "den" } ;
                          Fem => table 
                                {Nom => "die" ;
                                Gen => "der" ;
                                Dat => "der" ;
                                Acc => "die" } ;
                           Neut => table 
                                {Nom => "das" ;
                                Gen => "des" ;
                                Dat => "dem" ;
                                Acc => "das" }} ;
                n = Sg;
                af = Weak };

      thePl_Det = {s = table {Masc => table 
                            {Nom => "die" ; 
                            Gen => "der" ;
                            Dat => "den" ;
                            Acc => "die" } ;
                      Fem => table 
                            {Nom => "die" ;
                            Gen => "der" ;
                            Dat => "den" ;
                            Acc => "die" } ;
                        Neut => table 
                            {Nom => "die" ;
                            Gen => "der" ;
                            Dat => "den" ;
                            Acc => "die" }};
                n = Pl;
                af = Weak }; 

  UseN n = n ;

  -- funkar:
  -- AdjCN ap cn = {     -- AP -> CN -> CN    "big house"                -- inspo: https://www.grammaticalframework.org/lib/doc/rgl-tutorial/index.html
  --   s = \\n,c => ap.s ! Attr (sgA Strong cn.g Nom) ++ cn.s ! n ! c;
  --   g = cn.g 
  -- } ; 

-- funkar:
  AdjCN ap cn = {     -- AP -> CN -> CN    "big house"                -- inspo: https://www.grammaticalframework.org/lib/doc/rgl-tutorial/index.html
     s = \\n,c =>
      ap.s ! Attr (case n of {Pl => plA Strong c;  
                        Sg => sgA Strong cn.g c}) 
                        ++ cn.s ! n ! c;                   -- CN : Type = {s : Number => Case => Str ; g : Gender};
      g = cn.g 
      } ; 

 -- funkar ej:
-- AdjCN ap cn = {     -- AP -> CN -> CN    "big house"                -- inspo: https://www.grammaticalframework.org/lib/doc/rgl-tutorial/index.html
--      s = \\n,d,c =>
--      ap.s ! Attr (case n of {Pl => plA Strong c; 
--                        Sg => case d of {Strong => sgA Strong cn.g c}}) 
--                        ++ cn.s ! n ! c;                                -- CN : Type = {s : Number => Case => Str ; g : Gender};
--      g = cn.g 
--      } ; 

-- funkar ej:
--    AdjCN ap cn = {     -- AP -> CN -> CN    "big house"                -- inspo: https://www.grammaticalframework.org/lib/doc/rgl-tutorial/index.html
--      s = \\n,d,c =>
--      ap.s ! Attr (case n of {Pl => case d of {Strong => plA Strong c;        -- Adjective : Type = {s : UseAP => Str} ; 
--                                        Weak => plA Weak c;              -- UseAP = Attr FormA | Pred ; -- FormA = sgA AForm Gender Case | plA AForm Case ;
--                                        Mixed => plA Mixed c}; 
--                        Sg => case d of {Strong => sgA Strong cn.g c; 
--                                        Weak => sgA Weak cn.g c;
--                                        Mixed => sgA Mixed cn.g c}}) 
--                                          ++ cn.s ! d ! n ! c;            -- CN : Type = {s : Number => Case => Str ; g : Gender};
--      g = cn.g 
--      } ; 


    PositA a = a ;

    -- PrepNP prep np = {s = prep.s ++ np.s ! Acc} ;

     PrepNP prep np = {
      s = prep.s ++ np.s ! prep.c ; -- mit ihm: dat, auf ihn: acc
      c = prep.c;
      g = np.g ;
      n = np.n ;
      isPron = False
      } ;

    in_Prep = {
      s = "in" ;
      c = Dat ; 
      } ;

    on_Prep = {
      s = "auf" ;
      c = Acc ; 
      } ;

    with_Prep = {
      s = "auf" ;
      c = Dat ; 
    } ;

    he_Pron = {
      s = table {Nom => "er" ; Acc => "ihn"; Dat => "ihm"; Gen => "seiner"} ;
      g = Masc ;
      n = Sg
      } ;
    she_Pron = {
      s = table {Nom => "sie" ; Acc => "sie"; Dat => "ihr"; Gen => "ihrer"} ;
      g = Masc ;
      n = Sg
      } ;
    they_Pron = {
      s = table {Nom => "sie" ; Acc => "sie"; Dat => "ihnen"; Gen => "ihrer"} ;
      g = Neut ;
      n = Pl
      } ;

-----------------------------------------------------
---------------- Lexicon part -----------------------
-----------------------------------------------------

lin already_Adv = mkAdv "schon" ;
lin animal_N = mkN "Tier";
lin apple_N = mkN "Apfel" "Apfel" "Apfel" "Apfels" "Äpfel" "Äpfel" "Äpfeln" "Äpfel" Masc;
lin baby_N = mkN "Baby";
lin bad_A = mkA "schlecht" ;
lin beer_N = mkN "Bier" "Bier" "Bier" "Bieres" "Biere" "Biere" "Bieren" "Biere" Neut;
lin big_A = mkA "gross" ;
lin bike_N = mkN "Fahrrad" "Fahrrad" "Fahrrad" "Fahrrads" "Fahrräder" "Fahrräder" "Fahrrädern" "Fahrräder" Masc;
lin bird_N = mkN "Vogel" "Vogel" "Vogel" "Vogels" "Vögel" "Vögel" "Vögeln" "Vögel" Masc;
lin black_A = mkA "schwarz" ;
lin blood_N = mkN "Blut" "Blut" "Blut" "Blutes" "Blute" "Blute" "Bluten" "Blute" Neut;
lin blue_A = mkA "blau" ;
lin boat_N = mkN "Boot" "Boot" "Boot" "Bootes" "Boote" "Boote" "Booten" "Boote" Neut;
lin book_N = mkN "Buch" "Buch" "Buch" "Buches" "Bücher" "Bücher" "Büchern" "Bücher" Neut;
lin boy_N = mkN "Junge" "Jungen" "Jungen" "Jungen" "Jungen" "Jungen" "Jungen" "Jungen" Masc; 
lin bread_N = mkN "Brot" "Brot" "Brot" "Brotes" "Brote" "Brote" "Broten" "Brote" Neut;
lin break_V2 = mkV2 (mkV "brechen" "breche" "brichst" "bricht" "brechen" "brecht" "brechen") ; 
lin buy_V2 = mkV2 (mkV "kaufen") ;
lin car_N = mkN "Auto" ;
lin cat_N = mkN "Katze" "Katze" "Katze" "Katze" "Katzen" "Katzen" "Katzen" "Katzen" Fem ;
lin child_N = mkN "Kind" "Kind" "Kind" "Kindes" "Kinder" "Kinder" "Kindern" "Kinder" Neut;
lin city_N = mkN "Stadt" "Stadt" "Stadt" "Stadt" "Städte" "Städte" "Städten" "Städte" Fem;
lin clean_A = mkA "sauber" ;
lin clever_A = mkA "schlau" ;
lin cloud_N = mkN "Wolke" "Wolke" "Wolke" "Wolke" "Wolken" "Wolken" "Wolken" "Wolken" Fem;
lin cold_A = mkA "kalt" ;
lin come_V = mkV "kommen";
lin computer_N = mkN "Computer" "Computer" "Computer" "Computers" "Computer" "Computer" "Computern" "Computer" Neut;
lin cow_N = mkN "Kuh" "Kuh" "Kuh" "Kuh" "Kühe" "Kühe" "Kühe" "Kühe" Fem;
lin dirty_A = mkA "schmutzig" ;
lin dog_N = mkN "Hund";
lin drink_V2 = mkV2 (mkV "trinken") ;
lin eat_V2 = mkV2 (mkV "essen" "esse" "isst" "isst" "essen" "isst" "essen") ;
lin find_V2 = mkV2 (mkV "finden") ;
lin fire_N = mkN "Feuer" "Feuer" "Feuer" "Feuers" "Feuer" "Feuer" "Feuern" "Feuer" Neut;
lin fish_N = mkN "Fisch" "Fisch" "Fisch" "Fisches" "Fische" "Fische" "Fischen" "Fische" Masc;
lin flower_N = mkN "Blume";
lin friend_N = mkN "Freund";
lin girl_N = mkN "Mädchen" "Mädchen" "Mädchen" "Mädchens" "Mädchen" "Mädchen" "Mädchen" "Mädchen" Neut;
lin good_A = mkA "gut" ;
lin go_V = mkV "gehen";
lin grammar_N = mkN "Grammatik";
lin green_A = mkA "grün" ;
lin heavy_A = mkA "schwer" ;
lin horse_N = mkN "Pferd";
lin hot_A = mkA "warm" ;
lin house_N = mkN "Haus" "Haus" "Haus" "Hauses" "Häuser" "Häuser" "Häusern" "Häuser" Neut;
-- lin john_PN = mkPN "John" ;
lin jump_V = mkV "springen";
lin kill_V2 = mkV2 "töten" ;
-- lin know_VS = mkVS (mkV "wissen" "weiß" "weißt" "weiß" "wissen" "wisst" "wissen") ;
lin language_N = mkN "Sprache" "Sprache" "Sprache" "Sprache"  "Sprachen" "Sprachen" "Sprachen" "Sprachen" Fem;
lin live_V = mkV "leben" ;
lin love_V2 = mkV2 (mkV "lieben") ;
lin man_N = mkN "Mann" "Mann" "Mann" "Mannes" "Männer" "Männer" "Männern" "Männer" Masc;
lin milk_N = mkN "Milch";
lin music_N = mkN "Musik" ;
lin new_A = mkA "neu" ;
lin now_Adv = mkAdv "jetzt" ;
lin old_A = mkA "alt" ;
-- lin paris_PN = mkPN "Paris" ;
lin play_V = mkV "spielen" ;
lin read_V2 = mkV2 (mkV "lesen" "lese" "liesst" "liesst" "lesen" "lest" "lesen") ;
lin ready_A = mkA "bereit" ;
lin red_A = mkA "rot" ;
lin river_N = mkN "Fluss" "Fluss" "Fluss" "Flusses" "Flüsse" "Flüsse" "Flüssen" "Flüsse" Masc;
lin run_V = mkV "laufen" "laufe" "läufst" "läuft" "laufen" "lauft" "laufen";
lin sea_N = mkN "See" "See" "See" "See" "Seen" "Seen" "Seen" "Seen" Fem;
lin see_V2 = mkV2 (mkV "sehen" "sehe" "sehst" "sieht" "sehen" "seht" "sehen") ;
lin ship_N = mkN "Schiff";
lin sleep_V = mkV "schlafen" "schlafe" "schläfst" "schläft" "schlafen" "schlaft" "schlafen";
lin small_A = mkA "klein" ;
lin star_N = mkN "Stern";
lin swim_V = mkV "schwimmen";
lin teach_V2 = mkV2 (mkV "lehren") ;
lin train_N = mkN "Zug" "Zug" "Zug" "Zuges" "Züge" "Züge" "Zügen" "Züge" Masc;
lin travel_V = mkV "reisen" ;
lin tree_N = mkN "Baum" "Baum" "Baum" "Baums" "Bäume" "Bäume" "Bäumen" "Bäume" Masc;
lin understand_V2 = mkV2 (mkV "verstehen") ;
lin wait_V2 = mkV2 "warten" "auf" ;
lin walk_V = mkV "gehen";
lin warm_A = mkA "warm" ;
lin water_N = mkN "Wasser" "Wasser" "Wasser" "Wassers" "Wasser" "Wasser" "Wassern" "Wasser" Neut;
lin white_A = mkA "weiss" ;
lin wine_N = mkN "Wein";
lin woman_N = mkN "Frau";
lin yellow_A = mkA "gelb" ;
lin young_A = mkA "jung" ;

---------------------------
-- Paradigms part ---------
---------------------------

oper

  mkN = overload {
    mkN : Str ->  N 
      = \n -> lin N (smartNoun n) ;
    mkN : Str -> Str -> Str -> Str -> Str -> Str -> Str -> Str -> Gender -> N 
      = \sgNom,sgAcc,sgDat,sgGen,plNom,plAcc,plDat,plGen,g -> lin N (mkNoun sgNom sgAcc sgDat sgGen plNom plAcc plDat plGen g) ;
   };

  mkA = overload {
  mkA : Str -> A = \a -> lin A (mkAdjective a) ;
  --mkA : Str -> Bool -> A = \a,p -> lin A (smartAdjective a ** { isPre = p }) ;
  } ;

   mkV = overload {
    mkV : (inf : Str) -> V  -- predictable verbs, e.g. "gehen"
      = \s -> lin V (smartVerb s) ;
    mkV : (inf,sg1,sg2,sg3,pl1,pl2,pl3: Str) -> V  -- irregular verbs, or verbs with umlaut, or verbs with sein as auxiliary verb 
      = \inf,sg1,sg2,sg3,pl1,pl2,pl3 -> lin V (irregVerb inf sg1 sg2 sg3 pl1 pl2 pl3) ;
    } ;


   mkV2 = overload {
    mkV2 : Str -> V2                                              -- predictable verb with direct object, e.g. "wash"
      = \s   -> lin V2 (smartVerb s ** {c = []}); -- {c = \\g, n => []}) ;
    mkV2 : Str -> Str -> V2                                      -- predictable verb with preposition, e.g. "wait - for"
      = \s,p -> lin V2 (smartVerb s ** {c = p}) ; -- \\g, n => p}) ;
    mkV2 : V -> V2                                                -- any verb with direct object, e.g. "drink"
      = \v   -> lin V2 (v ** {c = []}); -- \\g, n => []}) ;
    mkV2 : V -> Str -> V2                                         -- any verb with preposition
      = \v,p -> lin V2 ( v ** {c = p}); --\\g, n => p}) ;
    } ;

-----------------------------------------------------

  mkAdv : Str -> Adv
    = \s -> lin Adv {s = s} ;
  
  
  mkPrep : Str -> Case -> Prep
    = \s,c -> lin Prep {s = s; c = c} ;

}