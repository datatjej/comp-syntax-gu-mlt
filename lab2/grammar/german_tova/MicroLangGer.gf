--# -path=.:../abstract
concrete MicroLangGer of MicroLang = open MicroResGer, Prelude in {

-----------------------------------------------------
---------------- Grammar part -----------------------
-----------------------------------------------------

  lincat
    Utt = {s : Str} ;
    
    S  = {s : Str} ;
    VP = {verb : Verb ; compl : Gender => Number => Str ; isPron : Bool } ; --hl
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

  -- PredVPS : NP -> VPS -> S ; 

  --PredVPS np vp = {
  --  s = np.s ! Nom ++ vp.verb.s ! agr2vform np.a ++ vp.compl
  --  } ;

  PredVPS np vp = { -- Saga
  s = np.s ! Nom ++ vp.verb.s ++ vp.compl ! np.a  
   } ;

      
    --UseV v = {
    --  verb = v ;
    --  compl = [] ;
    --  } ;

    UseV v = {
    verb = v ;
    compl = \\_,_ => [] ;
    isPron = False
    } ;
      
    --ComplV2 v2 np = {
    --  verb = v2 ;
    --  compl = v2.c ++ np.s ! Acc  -- NP object in the accusative, preposition first
    --  } ;

    ComplV2 v2 np = {
    verb = v2 ;
    compl = \\_,_ => v2.c ! np.g ! np.n ++ np.det ++ np.s ! Acc ;
    isPron = np.isPron ;
	  adv = []
	  } ;

    --UseComp comp = {
    --  verb = be_Verb ;     -- the verb is the copula "be"
    --  compl = comp.s
    --  } ;

    UseComp comp = {
    verb = be_Verb ;     -- the verb is the copula "be"
    compl = comp.s ;
    isPron = False ;
    } ;
    
    CompAP ap = ap ;
    --AdvVP vp adv =
    --  vp ** {compl = vp.compl ++ adv.s} ;

    AdvVP vp adv =
    vp ** {compl = \\g,n => vp.compl ! g ! n ++ adv.s} ;  
      

    DetCN det cn = {
    s = \\c => det.s ! cn.g ! c ++ cn.s ! det.n ! c ;
    n = det.n;
    g = cn.g;
    isPron = False
    } ;

    -- DetForm = DF Gender Case + Number;
    -- NounForm = NF Number Case + Gender;
   
   UsePron p = p ** { det = "" ; isPron = True } ;
            
            
    -- a_Det = {s = pre {"a"|"e"|"i"|"o" => "an" ; _ => "a"} ; n = Sg ; g = Masc} ;   --- a/an can get wrong

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

    -- AdjCommonNoun : Adjective -> CommonNoun -> CommonNoun = \adj, noun -> {
    --  noun = \\nf => adj.s ! case nf of {
    --    NF Sg Gen => AF Nom GPl;
    --    NF n c => AF c (gennum noun.g n)
    --  } ++ noun.noun ! nf;
    --  g = noun.g
    -- };

  UseN n = n ;

    AdjCN ap cn = {
      s = table {n => ap.s ++ cn.s ! n}
      } ;

    -- DetForm = DF Gender Case Number;
    -- NounForm = NF Number Case ;

    PositA a = a ;

    PrepNP prep np = {s = prep.s ++ np.s ! Acc} ;

    in_Prep = {s = "in"} ;
    on_Prep = {s = "on"} ;
    with_Prep = {s = "with"} ;

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
lin animal_N = mkN "Tier" "Tiere" Masc Nom;
lin apple_N = mkN "Apfel" "Äpfel" Masc Nom;
lin baby_N = mkN "Baby" "Babys" Neut Nom;
lin bad_A = mkA "schlecht" ;
lin beer_N = mkN "Bier" "Biere" Neut Nom;
lin big_A = mkA "gross" ;
lin bike_N = mkN "Fahrrad" "Fahrräder" Masc Nom;
lin bird_N = mkN "Vogel" "Vögel" Masc Nom;
lin black_A = mkA "schwarz" ;
lin blood_N = mkN "Blut" "Blut" Neut Nom ;
lin blue_A = mkA "blau" ;
lin boat_N = mkN "Boot" "Boote" Neut Nom ;
lin book_N = mkN "Buch" "Bücher" Neut Nom;
lin boy_N = mkN "Junge" "Jungen" Masc  Nom;
lin bread_N = mkN "Brot" "Brote" Neut Nom ;
lin break_V2 = mkV2 (mkV "brechen") ; 
lin buy_V2 = mkV2 (mkV "kaufen") ;
lin car_N = mkN "Auto" ;
lin cat_N = mkN "Katze" ;
lin child_N = mkN "Kind" "Kinder" Neut Nom;
lin city_N = mkN "Stadt" "Städte" Fem Nom;
lin clean_A = mkA "sauber" ;
lin clever_A = mkA "schlau" ;
lin cloud_N = mkN "Wolke" ;
lin cold_A = mkA "kalt" ;
lin come_V = mkV "kommen";
lin computer_N = mkN "Computer" ;
lin cow_N = mkN "Kuh" "Kühe" Fem Nom;
lin dirty_A = mkA "schmutzig" ;
lin dog_N = mkN "Hund" "Hunde" Masc Nom;
lin drink_V2 = mkV2 (mkV "trinken" "trinke" "trinkst" "trint" "trinken" "trinkst" "trinken") ;
lin eat_V2 = mkV2 (mkV "essen" "esse" "isst" "isst" "essen" "isst" "essen") ;
lin find_V2 = mkV2 (mkV "finden" "finde" "findest" "findet" "finden" "findet" "finden") ;
lin fire_N = mkN "Feuer" ;
lin fish_N = mkN "Fisch" "Fische" Masc Nom;
lin flower_N = mkN "Blume" ;
lin friend_N = mkN "Freund" "Freunde" Masc Nom;
lin girl_N = mkN "Mädchen" ;
lin good_A = mkA "gut" ;
lin go_V = mkV "gehen";
lin grammar_N = mkN "Grammatik" ;
lin green_A = mkA "grün" ;
lin heavy_A = mkA "schwer" ;
lin horse_N = mkN "Pferd" "Pferde" Masc Nom;
lin hot_A = mkA "warm" ;
lin house_N = mkN "Haus" "Hause" Neut Nom ;
-- lin john_PN = mkPN "John" ;
lin jump_V = mkV "springen";
lin kill_V2 = mkV2 "töten" ;
-- lin know_VS = mkVS (mkV "wissen" "wusste" "gewusst") ;
lin language_N = mkN "Sprache" ;
lin live_V = mkV "leben" ;
lin love_V2 = mkV2 (mkV "lieben") ;
lin man_N = mkN "Mann" "Männer" Masc Nom;
lin milk_N = mkN "Milch" "Milche" Masc Nom ;
lin music_N = mkN "Musik" ;
lin new_A = mkA "neu" ;
lin now_Adv = mkAdv "jetzt" ;
lin old_A = mkA "alt" ;
-- lin paris_PN = mkPN "Paris" ;
lin play_V = mkV "spielen" ;
lin read_V2 = mkV2 (mkV "lesen" "lese" "liesst" "liesst" "lesen" "lest" "lesen") ;
lin ready_A = mkA "bereit" ;
lin red_A = mkA "rot" ;
lin river_N = mkN "Fluss" "Flüsse" Masc Nom;
lin run_V = mkV "laufen";
lin sea_N = mkN "See" "Seen" Fem Nom;
lin see_V2 = mkV2 (mkV "sehen" "sehe" "siehst" "sieht" "sehen" "seht" "sehen") ;
lin ship_N = mkN "Schiff" "Schiffe" Masc Nom ;
lin sleep_V = mkV "schlafen";
lin small_A = mkA "klein" ;
lin star_N = mkN "Stern" "Sterne" Masc Nom;
lin swim_V = mkV "schwimmen";
lin teach_V2 = mkV2 (mkV "lehren" "lehre" "lehrst" "lehrt" "lehren" "lehrt" "lehren") ;
lin train_N = mkN "Zug" "Züge" Masc Nom;
lin travel_V = mkV "reisen" ;
lin tree_N = mkN "Baum" "Bäume" Masc Nom;
lin understand_V2 = mkV2 (mkV "verstehen" "verstehe" "verstehst" "versteht" "verstehen" "versteht" "verstehen") ;
lin wait_V2 = mkV2 "warten" "auf" ;
lin walk_V = mkV "gehen";
lin warm_A = mkA "warm" ;
lin water_N = mkN "Wasser" "Wasser" Neut Nom ;
lin white_A = mkA "weiss" ;
lin wine_N = mkN "Wein" "Weine" Masc Nom ;
lin woman_N = mkN "Frau" "Frauen" Fem Nom;
lin yellow_A = mkA "gelb" ;
lin young_A = mkA "jung" ;

---------------------------
-- Paradigms part ---------
---------------------------

oper

  --mkN = overload {
  --  mkN : Str -> Noun   -- predictable noun, e.g. car-cars, boy-boys, fly-flies, bush-bushes
  --    = \n -> lin N (smartNoun n) ;
  --  mkN : Str -> Str -> Noun  -- irregular noun, e.g. man-men
  --    = \sg,pl -> lin N (mkNoun sg pl) ;
  --  } ;

   -- mkN =  overload {
   -- mkN : Str -> N   -- predictable noun
   --   = \n -> lin N (smartNoun n) ;
   -- mkN : Str -> Str -> Gender -> Case -> N  -- irregular noun
   --   = \sg,pl,g,c -> lin N (mkNoun sg pl g c) ;
  --  } ;

  -- put in MicroLang:
  mkN = overload {
  mkN : (word : Str) -> Noun = makeNoun;
  mkN : (nomSg, accSg, datSg, genSg, nomPl, accPl, datPl, genPl : Str) -> Gender -> Noun = mkWorstN
  };

  -- mkA : Str -> A
  --  = \s -> lin A {s = s} ;

  mkA = overload {
  mkA : Str -> A = \a -> lin A (smartAdjective a) ;
  mkA : Str -> Bool -> A = \a,p -> lin A (smartAdjective a ** { isPre = p }) ;
  } ;

   mkV = overload {
    mkV : (inf : Str) -> V  -- predictable verb, e.g. play-plays, cry-cries, wash-washes
      = \s -> lin V (smartVerb s) ;
    mkV : (inf,sg1,sg2,sg3,pl1,pl2,pl3 : Str) -> V  -- irregular verb, e.g. drink-drank-drunk
      = \inf,sg1,sg2,sg3,pl1,pl2,pl3 -> lin V (irregVerb inf sg1 sg2 sg3 pl1 pl2 pl3) ;
    } ;

  -- mkV2 = overload {
  --  mkV2 : Str -> V2          -- predictable verb with direct object, e.g. "wash"
  --    = \s   -> lin V2 (smartVerb s ** {c =  --[]}) ;
  -- mkV2 : Str  -> Str -> V2  -- predictable verb with preposition, e.g. "wait - for"
  --  = \s,p -> lin V2 (smartVerb s ** {c = p}) ;
  -- mkV2 : V -> V2            -- any verb with direct object, e.g. "drink"
  --  = \v   -> lin V2 (v ** {c = []}) ;
  -- mkV2 : V -> Str -> V2     -- any verb with preposition
  --  = \v,p -> lin V2 (v ** {c = p}) ;
  -- } ;

  mkV2 = overload {
    mkV2 : Str -> V2                                              -- predictable verb with direct object, e.g. "wash"
      = \s   -> lin V2 (smartVerb s ** {c = \\g, n => []}) ;
    mkV2 : Str  -> Str -> V2                                      -- predictable verb with preposition, e.g. "wait - for"
      = \s,p -> lin V2 (smartVerb s ** {c = \\g, n => p}) ;
    mkV2 : V -> V2                                                -- any verb with direct object, e.g. "drink"
      = \v   -> lin V2 (v ** {c = \\g, n => []}) ;
    mkV2 : V -> Str -> V2                                         -- any verb with preposition
      = \v,p -> lin V2 (v ** {c = \\g, n => p}) ;
    } ;

  mkAdv : Str -> Adv
    = \s -> lin Adv {s = s} ;
  
  mkPrep : Str -> Prep
    = \s -> lin Prep {s = s} ;

}
