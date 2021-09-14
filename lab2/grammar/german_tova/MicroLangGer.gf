--# -path=.:../abstract
concrete MicroLangGer of MicroLang = open MicroResGer, Prelude in {

-----------------------------------------------------
---------------- Grammar part -----------------------
-----------------------------------------------------

  lincat
    Utt = {s : Str} ;
    
    S  = {s : Str} ;
    VP = {verb : Verb ; compl : Gender => Number => Str ; isPron : Bool } ; --hl
    -- VP = {verb : Verb ; compl : Gender => Number => Str ; isPron : Bool ; adv : Str } ; --julia
    Comp = Adjective;
    AP = Adjective ;
    CN = Noun ;
   -- NP =  {s : Case => Str ; det : Str ; g : Gender ; n : Number ; isPron : Bool } ; -- hl
    NP = {s : Case => Str ; g : Gender ; n : Number; isPron : Bool} ;
    Pron = {s : Case => Str ; g : Gender ; n : Number } ;
    Det = Determiner ;
    Prep = {s : Str} ;
    V = Verb ;
    V2 = Verb2 ; --transitive? 
    A = Adjective ;
    N = Noun ;
    Adv = {s : Str} ;

  lin
    UttS s = s ;
    UttNP np = {s = np.s ! Acc} ;

 -- PredVPS np vp = {
--	s = np.s ! Nom ++
--	case vp.isPron of {
--	  True => vp.compl ! np.g ! np.n ++ vp.verb.s ! agr2vform np.n ++ vp.adv;
--	  False => vp.verb.s ! agr2vform np.n ++ vp.compl ! np.g ! np.n ++ vp.adv
--	}
--	};

  -- ENGELSKA:
  -- PredVPS np vp = {
  --    s = np.s ! Nom ++ vp.verb.s ! agr2vform np.a ++ vp.compl -- Nom subject + NP.agreement + vp.compl
   --   } ;

     PredVPS np vp = {
      s = case np.isPron of {
	  True => case np.n of { Sg => np.s ! Nom ; Pl => "" } ;
	  False => np.s ! Nom
	  } ++ vp.verb.s ! Pres np.n P3 ++ vp.compl ! np.g ! np.n
    };
  
  -- HERBERT:
  -- PredVPS np vp = {
  --    s = case np.isPron of { -- om NP är pronomen:
	-- True => case np.n of { Sg => np.s ! Nom ; Pl => "" } ; --
	-- False => np.det ++ np.s ! Nom
	--  } ++
	-- case vp.isPron of {
	--  True => vp.compl ! np.g ! np.n ++ vp.verb.s ! Ind Present np.n P3 ;
	--  False => 
	--    vp.verb.s ! Ind Present np.n P3 ++ vp.compl ! np.g ! np.n
	-- }
  --    };

    UseV v = {
    verb = v ;
    compl = \\g, n => [] ;
	  isPron = False ; 
	  adv = []
      } ;
      
     ComplV2 v2 np = {
      verb = v2 ;
      compl = \\g, n => v2.c ! g ! n ++ np.s ! Acc ; -- NP object in the accusative, preposition first  
      isPron = np.isPron ;
	    adv = []
	   } ;


      -- LIINA:
      -- ComplV2 v2 np = {
     -- verb = v2 ;
     -- compl = \\g,n => v2.c ! g ! n ++ np.s ! Acc ; -- NP object in the accusative, preposition first
     -- isPron = np.isPron ;
     -- adv = []
     --  } ;



    -- herbert 
    --  ComplV2 v2 np = {
    --  verb = v2 ;
    --  compl = \\_,_ => v2.c.con ! np.g ! np.n ++ np.det ++ np.s ! Acc ;  -- NP object in the accusative, preposition first
    --  isPron = np.isPron 
    --  } ;

    -- engelska
    --  ComplV2 v2 np = {
    --  verb = v2 ;
    --  compl = v2.c ++ np.s ! Acc  -- NP object in the accusative, preposition first
    --  } ;

    
    UseComp comp = {
      verb = be_Verb ;     -- the verb is the copula "be"
      compl = \\g,n => comp.s ! g ! n ;
	  isPron = False ;
	  adv = []
      } ;

    CompAP ap = ap ;
    --AdvVP vp adv =
    --  vp ** {compl = vp.compl ++ adv.s} ;

    --AdvVP vp adv =
    --vp ** {compl = \\g,n => vp.compl ! g ! n ++ adv.s} ;  

    -- ENGELSKA:
    -- AdvVP vp adv =
    --  vp ** {compl = vp.compl ++ adv.s} ;

    -- HERBERT:
    AdvVP vp adv =
      vp ** {compl = \\g,n => vp.compl ! g ! n ++ adv.s} ;  

    DetCN det cn = {
    s = \\c => det.s ! cn.g ! c ++ cn.s ! det.n ! c ;
    n = det.n;
    g = cn.g;
    isPron = False
    } ;

   UsePron p = p ** { det = "" ; isPron = True } ;
            

     a_Det = {s = table {
       Fem => table {
           Nom | Acc => "eine" ; 
           Dat | Gen => "einer" 
        } ;
        Masc => table {
          Nom => "ein" ; 
          Acc => "einen" ; 
          Dat | Gen => "einem"
        } ;
        Neut => table {
          Nom | Acc => "ein" ; 
          Dat => "einem"; 
          Gen => "eines"
        }
    };
    n = Sg
    };

   aPl_Det = {s = table {
		 _ => table {
       _ => ""
     }
    };
    n = Pl
    };

    the_Det = {s = table {
       Fem => table {
           Nom | Acc => "die" ; 
           Dat | Gen => "der" 
        } ;
        Masc => table {
          Nom => "der" ; 
          Acc => "den" ; 
          Dat | Gen => "dem"
        } ;
        Neut => table {
          Nom | Acc => "das" ; 
          Dat => "dem"; 
          Gen => "des"
        }
    };
    n = Sg
    };

       thePl_Det = {s = table {
       Fem => table {
           Nom | Acc => "die" ; 
           Dat => "den";
           Gen => "der" 
        } ;
        Masc => table {
          Nom | Acc => "die" ; 
          Dat => "den" ; 
          Gen => "der"
        } ;
        Neut => table {
          Nom | Acc => "die" ; 
          Dat => "den" ; 
          Gen => "der"
        }
    };
    n = Pl
    };

  UseN n = n ;

    AdjCN ap cn = {  -- inspo: https://www.grammaticalframework.org/lib/doc/rgl-tutorial/index.html
    s = \\n,c => ap.s ! cn.g ! n ++ cn.s ! n ! c ;
    g = cn.g
    } ;

    PositA a = a ;

    PrepNP prep np = {s = prep.s ++ np.s ! Acc} ;

    in_Prep = {s = "in"} ;
    on_Prep = {s = "auf"} ;
    with_Prep = {s = "mit"} ;

    he_Pron = {
      s = table {Nom => "er" ; Acc => "ihn"; Dat => "ihm"; Gen => "seiner"} ;
      g = Masc ;
      n = Sg
      -- a = Agr Sg ;
      } ;
    she_Pron = {
      s = table {Nom => "sie" ; Acc => "sie"; Dat => "ihr"; Gen => "ihrer"} ;
      g = Masc ;
      n = Sg
      --a = Agr Sg ;
      } ;
    they_Pron = {
      s = table {Nom => "sie" ; Acc => "sie"; Dat => "ihnen"; Gen => "ihrer"} ;
      g = Neut ;
      n = Pl
      --a = Agr Pl ;
      } ;

-----------------------------------------------------
---------------- Lexicon part -----------------------
-----------------------------------------------------

lin already_Adv = mkAdv "schon" ;
lin animal_N = mkN "Tier"; -- FIXA!
lin apple_N = mkN "Apfel" "Apfels" "Äpfel" Masc;
lin baby_N = mkN "Baby";
lin bad_A = mkA "schlecht" ;
lin beer_N = mkN "Bier" "Bieres" "Biere" Neut;
lin big_A = mkA "gross" ;
lin bike_N = mkN "Fahrrad" "Fahrrads" "Fahrräder" Masc;
lin bird_N = mkN "Vogel" "Vogels" "Vögel" Masc;
lin black_A = mkA "schwarz" ;
lin blood_N = mkN "Blut" "Blutes" "Blute" Neut;
lin blue_A = mkA "blau" ;
lin boat_N = mkN "Boot" "Bootes" "Boote" Neut;
lin book_N = mkN "Buch" "Buches" "Bücher" Neut;
lin boy_N = mkN "Junge" "Jungen" "Jungen" Masc; -- TODO- modellera Junge Mensch-gruppen
lin bread_N = mkN "Brot" "Brotes" "Brote" Neut;
lin break_V2 = mkV2 (mkV "brechen" "breche" "brichst" "bricht" "brechen" "brecht" "brechen") ; 
lin buy_V2 = mkV2 (mkV "kaufen") ;
lin car_N = mkN "Auto" ;
lin cat_N = mkN "Katze" ;
lin child_N = mkN "Kind" "Kindes" "Kinder" Neut;
lin city_N = mkN "Stadt" "Stadt" "Städte" Fem;
lin clean_A = mkA "sauber" ;
lin clever_A = mkA "schlau" ;
lin cloud_N = mkN "Wolke" "Wolke" "Wolken" Fem;
lin cold_A = mkA "kalt" ;
lin come_V = mkV "kommen";
lin computer_N = mkN "Computer" "Computers" "Computer" Neut;
lin cow_N = mkN "Kuh" "Kuh" "Kühe" Fem;
lin dirty_A = mkA "schmutzig" ;
lin dog_N = mkN "Hund";
lin drink_V2 = mkV2 (mkV "trinken") ;
lin eat_V2 = mkV2 (mkV "essen" "esse" "isst" "isst" "essen" "isst" "essen") ;
lin find_V2 = mkV2 (mkV "finden") ;
lin fire_N = mkN "Feuer" "Feuers" "Feuer" Neut;
lin fish_N = mkN "Fisch" "Fisches" "Fische" Masc;
lin flower_N = mkN "Blume";
lin friend_N = mkN "Freund";
lin girl_N = mkN "Mädchen" "Mädchens" "Mädchen" Neut;
lin good_A = mkA "gut" ;
lin go_V = mkV "gehen";
lin grammar_N = mkN "Grammatik";
lin green_A = mkA "grün" ;
lin heavy_A = mkA "schwer" ;
lin horse_N = mkN "Pferd";
lin hot_A = mkA "warm" ;
lin house_N = mkN "Haus" "Hauses" "Häuser" Neut;
-- lin john_PN = mkPN "John" ;
lin jump_V = mkV "springen";
lin kill_V2 = mkV2 "töten" ;
-- lin know_VS = mkVS (mkV "wissen" "weiß" "weißt" "weiß" "wissen" "wisst" "wissen") ;
lin language_N = mkN "Sprache" "Sprache" "Sprachen" Fem;
lin live_V = mkV "leben" ;
lin love_V2 = mkV2 (mkV "lieben") ;
lin man_N = mkN "Mann" "Mannes" "Männer" Masc;
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
lin river_N = mkN "Fluss" "Flusses" "Flüsse" Masc;
lin run_V = mkV "laufen" "laufe" "läufst" "läuft" "laufen" "lauft" "laufen";
lin sea_N = mkN "See" "See" "Seen" Fem;
lin see_V2 = mkV2 (mkV "sehen") ;
lin ship_N = mkN "Schiff";
lin sleep_V = mkV "schlafen" "schlafe" "schläfst" "schläft" "schlafen" "schlaft" "schlafen";
lin small_A = mkA "klein" ;
lin star_N = mkN "Stern";
lin swim_V = mkV "schwimmen";
lin teach_V2 = mkV2 (mkV "lehren") ;
lin train_N = mkN "Zug" "Zuges" "Züge" Masc;
lin travel_V = mkV "reisen" ;
lin tree_N = mkN "Baum" "Baums" "Bäume" Masc;
lin understand_V2 = mkV2 (mkV "verstehen") ;
lin wait_V2 = mkV2 "warten" "auf" ;
lin walk_V = mkV "gehen";
lin warm_A = mkA "warm" ;
lin water_N = mkN "Wasser" "Wassers" "Wasser" Neut;
lin white_A = mkA "weiss" ;
lin wine_N = mkN "Wein";
lin woman_N = mkN "Frau";
lin yellow_A = mkA "gelb" ;
lin young_A = mkA "jung" ;

---------------------------
-- Paradigms part ---------
---------------------------

oper

-- engelska:
  --mkN = overload {
  --  mkN : Str -> Noun                                           -- predictable noun, e.g. car-cars, boy-boys, fly-flies, bush-bushes
  --    = \n -> lin N (smartNoun n) ;
  --  mkN : Str -> Str -> Noun                                          -- irregular noun, e.g. man-men
  --    = \sg,pl -> lin N (mkNoun sg pl) ;
  --  } ;

  -- herbert italian
   --mkN =  overload {
   -- mkN : Str -> N                                                -- predictable noun
   --   = \n -> lin N (smartNoun n) ;
   -- mkN : Str -> Str -> Gender -> N                                   -- irregular noun
   --   = \sg,pl,g -> lin N (mkNoun sg pl g) ;
   -- } ;

  mkN = overload {
    mkN : Str ->  N 
      = \n -> lin N (smartNoun n) ;
    mkN : Str -> Str -> Str -> Gender -> N 
      = \sg,genSg,pl,g -> lin N (mkNoun sg genSg pl g) ;
   };


  -- engelska:
  -- mkA : Str -> A
  --  = \s -> lin A {s = s} ; 

  mkA = overload {
  mkA : Str -> A = \a -> lin A (mkAdjective a) ;
  --mkA : Str -> Bool -> A = \a,p -> lin A (smartAdjective a ** { isPre = p }) ;
  } ;

   mkV = overload {
    mkV : (inf : Str) -> V  -- predictable verbs, e.g. "sehen"
      = \s -> lin V (smartVerb s) ;
    mkV : (inf,sg1,sg2,sg3,pl1,pl2,pl3: Str) -> V  -- irregular verbs, or verbs with umlaut, or verbs with sein as auxiliary verb 
      = \inf,sg1,sg2,sg3,pl1,pl2,pl3 -> lin V (irregVerb inf sg1 sg2 sg3 pl1 pl2 pl3) ;
    } ;



   mkV2 = overload {
    mkV2 : Str -> V2                                              -- predictable verb with direct object, e.g. "wash"
      = \s   -> lin V2 (smartVerb s ** {c = \\g, n => []}) ;
    mkV2 : Str -> Str -> V2                                      -- predictable verb with preposition, e.g. "wait - for"
      = \s,p -> lin V2 (smartVerb s ** {c = \\g, n => p}) ;
    mkV2 : V -> V2                                                -- any verb with direct object, e.g. "drink"
      = \v   -> lin V2 (v ** {c = \\g, n => []}) ;
    mkV2 : V -> Str -> V2                                         -- any verb with preposition
      = \v,p -> lin V2 ( v ** {c = \\g, n => p}) ;
    } ;

-- HERBERT:  
 -- mkV2 = overload {
 --   mkV2 : Str -> V2 =
 --     \v -> lin V2 (smartVerb v) ** { c = emptyPreposition } ;
 --   mkV2 : V -> V2 =
 --     \v -> lin V2 (v ** { c = emptyPreposition });
 --   } ;

-- ENGELSKA:
--mkV2 = overload {
--mkV2 : Str -> V2          -- predictable verb with direct object, e.g. "wash"
--  = \s   -> lin V2 (smartVerb s ** {c = []}) ;
--mkV2 : Str  -> Str -> V2  -- predictable verb with preposition, e.g. "wait - for"
--  = \s,p -> lin V2 (smartVerb s ** {c = p}) ;
--mkV2 : V -> V2            -- any verb with direct object, e.g. "drink"-
--  = \v   -> lin V2 (v ** {c = []}) ;
--mkV2 : V -> Str -> V2     -- any verb with preposition
--  = \v,p -> lin V2 (v ** {c = p}) ;
-- } ;


-- --------------------------------------------------

  mkAdv : Str -> Adv
    = \s -> lin Adv {s = s} ;
  
  
  mkPrep : Str -> Prep
    = \s -> lin Prep {s = s} ;

}