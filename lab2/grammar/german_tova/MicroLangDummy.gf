--# -path=.:../abstract

concrete MicroLangDummy of MicroLang = open ResDummy, Prelude in {

-- a very minimal concrete syntax: everything is just {s : Str}

-----------------------------------------------------
---------------- Grammar part -----------------------
-----------------------------------------------------

  lincat
-- Common
    Utt,    -- sentence, question, word...         e.g. "be quiet"

-- Cat
    S,      -- declarative sentence                e.g. "she lives here"
    VP,     -- verb phrase                         e.g. "lives here"
    Comp,   -- complement of copula                e.g. "warm"
    AP,     -- adjectival phrase                   e.g. "warm"
    NP,     -- noun phrase (subject or object)     e.g. "the red house"
    Det,    -- determiner phrase                   e.g. "those"
    Prep,   -- preposition, or just case           e.g. "in", dative
    V,      -- one-place verb                      e.g. "sleep" 
    V2,     -- two-place verb                      e.g. "love"
    A,      -- one-place adjective                 e.g. "warm"
    Pron,   -- personal pronoun                    e.g. "she"
    Adv     -- adverbial phrase                    e.g. "in the house"
     = {s : Str} ;

    N,      -- common noun                         e.g. "house"
    CN       -- common noun (without determiner)    e.g. "red house"
    = Noun;    

  lin
-- Phrase
    -- : S  -> Utt ;         -- he walks
    UttS s = s ;

    -- : NP -> Utt ;         -- he
    UttNP np = np ;

-- Sentence
    -- : NP -> VP -> S ;             -- John walks
    PredVPS np vp = {s = np.s ++ vp.s} ;

-- Verb
    -- : V   -> VP ;             -- sleep
    UseV v = v ;

    -- : V2  -> NP -> VP ;       -- love it
    ComplV2 v2 np = {s = v2.s ++ np.s} ;

    -- : Comp  -> VP ;           -- be small
    UseComp comp = {s = "be" ++ comp.s} ;

    -- : AP  -> Comp ;           -- small
    CompAP ap = ap ;

    -- : VP -> Adv -> VP ;       -- sleep here
    AdvVP vp adv = {s = vp.s ++ adv.s} ;

-- Noun
    -- : Det -> CN -> NP ;       -- the man
    -- DetCN det cn = {
    --   s = det.s ++ cn.s ;
    -- } ;

    -- : Pron -> NP ;            -- she
    UsePron pron = pron ;

    -- : Det ;                   -- indefinite singular
    a_Det = {s = "a"} ;

    -- : Det ;                   -- indefinite plural
    aPl_Det = {s = ""} ;

    -- : Det ;                   -- definite singular   ---s
    the_Det = {s = "the"} ;

    -- : Det ;                   -- definite plural     ---s
    thePl_Det = {s = "the"} ;

    -- : N -> CN ;               -- house
    UseN n = n ;

    -- : AP -> CN -> CN ;        -- big house
    -- AdjCN ap cn = {
    --   s = ap.s ++ cn.s ;
    -- } ;

-- Adjective
    -- : A  -> AP ;              -- warm
    PositA a = a ;

-- Adverb
    -- : Prep -> NP -> Adv ;     -- in the house
    PrepNP prep np = {s = prep.s ++ np.s} ;

-- Structural
    -- : Prep ;
    in_Prep = mkPrep "in" ;
    on_Prep = mkPrep "on" ;
    with_Prep = mkPrep "with" ;

    he_Pron   = mkPron "he"   ;
    she_Pron  = mkPron "she"  ;
    they_Pron = mkPron "they" ;

-- oper

--   mkPrep : Str -> {s : Str}  ;
--   mkPrep str = {s = str} ;

--   mkPron : Str -> {s : Str}  ;
--   mkPron str = {s = str} ;

-----------------------------------------------------
---------------- Lexicon part -----------------------
-----------------------------------------------------

lin
  already_Adv = mkAdv "schon" ;
  animal_N = mkN "tier" ;
  apple_N = mkN "Apfel";
  baby_N = mkN "Baby" ;
  bad_A = mkA "schlecht" ;
  beer_N = mkN "Bier" ;
  big_A = mkA "gross" ;
  bike_N = mkN "Fahrrad";
  bird_N = mkN "Vogel";
  black_A = mkA "schwarz" ;
  blood_N = mkN "Blut" ;
  blue_A = mkA "blau" ;
  boat_N = mkN "Boot" ;
  book_N = mkN "Buch" "Bücher" ;
  boy_N = mkN "Junge" ;
  bread_N = mkN "Brot" ;
  break_V2 = mkV2 "brechen"; 
  buy_V2 = mkV2 "kaufen";
  car_N = mkN "Auto" ;
  cat_N = mkN "Katze" ;
  child_N = mkN "Kind" "Kinder";
  city_N = mkN "Stadt";
  clean_A = mkA "sauber" ;
  clever_A = mkA "schlau" ;
  cloud_N = mkN "Wolke" ;
  cold_A = mkA "kalt" ;
  come_V = mkV "kommen";
  computer_N = mkN "Computer" ;
  cow_N = mkN "Kuh";
  dirty_A = mkA "schmutzig" ;
  dog_N = mkN "Hund" ;
  drink_V2 = mkV2 "trinken";
  eat_V2 = mkV2 "essen";
  find_V2 = mkV2 "finden";
  fire_N = mkN "Feuer" ;
  fish_N = mkN "Fisch";
  flower_N = mkN "Blume" ;
  friend_N = mkN "Freund" ;
  girl_N = mkN "Mädchen" ;
  good_A = mkA "gut" ;
  go_V = mkV "gehen";
  grammar_N = mkN "Grammatik" ;
  green_A = mkA "grün" ;
  heavy_A = mkA "schwer" ;
  horse_N = mkN "Pferd" ;
  hot_A = mkA "warm" ;
  house_N = mkN "Haus" ;
  -- lin john_PN = mkPN "John" ;
  jump_V = mkV "springen";
  kill_V2 = mkV2 "töten" ;
  -- lin know_VS = mkVS (mkV "wissen" "wusste" "gewusst") ;
  language_N = mkN "Sprache" ;
  live_V = mkV "leben" ;
  love_V2 = mkV2 "lieben";
  man_N = mkN "Mann" "Männer";
  milk_N = mkN "Milch" ;
  music_N = mkN "Musik" ;
  new_A = mkA "neu" ;
  now_Adv = mkAdv "jetzt" ;
  old_A = mkA "alt" ;
  -- lin paris_PN = mkPN "Paris" ;
  play_V = mkV "spielen" ;
  read_V2 = mkV2 "lesen";
  ready_A = mkA "bereit" ;
  red_A = mkA "rot" ;
  river_N = mkN "Fluss" "Flüsse";
  run_V = mkV "laufen";
  sea_N = mkN "See" ;
  see_V2 = mkV2 "sehen";
  ship_N = mkN "Schiff" ;
  sleep_V = mkV "schlafen";
  small_A = mkA "klein" ;
  star_N = mkN "Stern" ;
  swim_V = mkV "schwimmen";
  teach_V2 = mkV2 "lehren";
  train_N = mkN "Zug" ;
  travel_V = mkV "reisen" ;
  tree_N = mkN "Baum" "Bäume";
  understand_V2 = mkV2 "verstehen";
  wait_V2 = mkV2 "warten"; --"auf" ;
  walk_V = mkV "gehen";
  warm_A = mkA "warm" ;
  water_N = mkN "Wasser" ;
  white_A = mkA "weiss" ;
  wine_N = mkN "Wein" ;
  woman_N = mkN "Frau" ;
  yellow_A = mkA "gelb" ;
  young_A = mkA "jung" ;

}