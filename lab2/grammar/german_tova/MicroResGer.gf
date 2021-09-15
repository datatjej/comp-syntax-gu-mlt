resource MicroResGer = open Prelude, Predef in {

param
  Number = Sg | Pl ;
  Case = Nom | Acc | Dat | Gen ;
  Person = P1 | P2 | P3 ; 
  Gender = Fem | Masc | Neut ;
  VForm = Inf | Pres Number Person ;
  -- VType = Weak | Strong ; --needed? not sure
  Aux = haben | sein ;
  
  UseAP = Attr FormA | Pred ;
  AForm = Strong | Weak | Mixed ;
  FormA = sgA AForm Gender Case | plA AForm Case ;


oper
  --  define types:

  -- FormA = sgA (_ : {Adekl} ) (_ : {Gender} ) (_ : {Case} ) | plA (_ : {Adekl} ) (_ : {Case} ) ;
  --- AFORM = ADEKL

  Noun: Type = {s : Number => Case => Str ; g : Gender};
  -- Noun: Type = {s : AForm => Case => Str ; g : Gender ; n : Number}; -- ÄNDRAD ENLIGT TY

  -- Determiner: Type = {s: Gender => Number => Case => Str ; d : AForm}; -- ÄNDRAD ENLIGT TY
  Determiner : Type = {s : Gender => Case => Str ; n : Number ; d : AForm};

  mkNoun : (sg, genSg, pl : Str) -> Gender -> Noun = 
    \sg,genSg,pl,g -> {
    s = table {
      Sg => table {
        Nom => sg ;
        Acc => sg ;
        Dat => sg ;
        Gen => genSg
        } ;
      Pl => table {
        Nom => pl ;
        Acc => pl ;
        Dat => pl + "n" ;
        Gen => pl
        }
      } ;
    g = g
    } ;

      
 smartNoun : Str -> Noun = \sg -> case sg of {
    _ + ("ik"|"au"|"ilch"|"e")    => regNounFem sg;
    _ + ("ier"|"erd"|"und"|"iff"|"ern"|"ein")					    => regNounMasc sg;
    _ + ("o"|"y")   			=> regNounNeut sg
    -- _ => error ("No smarts for nouns here: " ++ sg)
   } ;


  regNounMasc : Str -> Noun = \tier ->
    let
      tier = tier;
      tiere = tier + "e";
      tieres = tier + "es";
    in
    mkNoun 
      tier tieres tiere
      Masc ;

    regNounFem : Str -> Noun = \frau ->
    let
      frau = frau;
      frauen = frau + "en";
    in
    mkNoun 
      frau frau frauen
      Fem ;

  regNounNeut : Str -> Noun = \baby ->
    let
      baby = baby;
      babys = baby + "s";
    in
    mkNoun 
      baby babys babys
      Neut ;
  

  -- herbert: Adjective : Type = {s : Gender => Number => Str ; isPre : Bool } ;

  -- OLD: Adjective : Type = {s : Gender => Number => Str} ;

  Adjective : Type = {s : UseAP => Str} ;

  -- NOTES: AdjForms : Type = { pred, mnom, macc, mdat, fnom, nnom : Str} 

  mkAdjective : Str -> Adjective =         -- mkFormsA : {Comp} -> Str -> {UseAP} => Str = \c_0 -> \str_1 -> table ({UseAP} ) {
        \str_1 -> { s = table {
    Attr (sgA Strong Masc Nom) => str_1 + "er" ;
    Attr (sgA Strong Masc Gen)=> str_1 + "es" ;
    Attr (sgA Strong Masc Dat)=> str_1 + "em" ;
    Attr (sgA Strong Masc Acc) => str_1 + "en" ;
    Attr (sgA Strong Fem Nom) => str_1 + "e" ;
    Attr (sgA Strong Fem Gen) => str_1 + "er" ;
    Attr (sgA Strong Fem Dat) => str_1 + "er" ;
    Attr (sgA Strong Fem Acc) => str_1 + "e" ;
    Attr (sgA Strong Neut Nom) => str_1 + "es" ;
    Attr (sgA Strong Neut Gen) => str_1 + "es" ;
    Attr (sgA Strong Neut Dat) => str_1 + "em" ;
    Attr (sgA Strong Neut Acc) => str_1 + "es" ;
    Attr (sgA Weak Masc Nom) => str_1 + "e" ;
    Attr (sgA Weak Masc Gen)=> str_1 + "en" ;
    Attr (sgA Weak Masc Dat)=> str_1 + "en" ;
    Attr (sgA Weak Masc Acc) => str_1 + "en" ;
    Attr (sgA Weak Fem Nom) => str_1 + "e" ;
    Attr (sgA Weak Fem Gen) => str_1 + "en" ;
    Attr (sgA Weak Fem Dat) => str_1 + "en" ;
    Attr (sgA Weak Fem Acc) => str_1 + "e" ;
    Attr (sgA Weak Neut Nom) => str_1 + "e" ;
    Attr (sgA Weak Neut Gen) => str_1 + "en" ;
    Attr (sgA Weak Neut Dat) => str_1 + "en" ;
    Attr (sgA Weak Neut Acc) => str_1 + "e" ;
    -- < MIXED sgA >
    Attr (sgA Mixed Masc Nom) => str_1 + "er" ;
    Attr (sgA Mixed Masc Gen)=> str_1 + "en" ;
    Attr (sgA Mixed Masc Dat)=> str_1 + "en" ;
    Attr (sgA Mixed Masc Acc) => str_1 + "en" ;
    Attr (sgA Mixed Fem Nom) => str_1 + "e" ;
    Attr (sgA Mixed Fem Gen) => str_1 + "en" ;
    Attr (sgA Mixed Fem Dat) => str_1 + "en" ;
    Attr (sgA Mixed Fem Acc) => str_1 + "e" ;
    Attr (sgA Mixed Neut Nom) => str_1 + "es" ;
    Attr (sgA Mixed Neut Gen) => str_1 + "en" ;
    Attr (sgA Mixed Neut Dat) => str_1 + "en" ;
    Attr (sgA Mixed Neut Acc) => str_1 + "es" ;
    --------------------------------
    Attr (plA Strong Nom) => str_1 + "e" ;
    Attr (plA Strong Gen) => str_1 + "er" ;
    Attr (plA Strong Dat) => str_1 + "en" ;
    Attr (plA Strong Acc) => str_1 + "e" ;
    Attr (plA Weak Nom) => str_1 + "en" ;
    Attr (plA Weak Gen) => str_1 + "en" ;
    Attr (plA Weak Dat) => str_1 + "en" ;
    Attr (plA Weak Acc) => str_1 + "en" ;
    -- < MIXED PlA>
    Attr (plA Mixed Nom) => str_1 + "e" ;
    Attr (plA Mixed Gen) => str_1 + "er" ;
    Attr (plA Mixed Dat) => str_1 + "en" ;
    Attr (plA Mixed Acc) => str_1 + "e" ;
    --------------------------
    Pred => str_1
  }} ;


 --  kleine, kleine, klein, kleine, kleiner, kleine
  -- isPre --> isIndef 

  Verb : Type = {s : VForm => Str} ;

   mkVerb : (inf,sg1,sg2,sg3,pl1,pl2,pl3 : Str) -> Verb
    = \inf,sg1,sg2,sg3,pl1,pl2,pl3 -> {
    s = table {
      Inf => inf ;
      Pres Sg P1 => sg1;
      Pres Sg P2 => sg2;
      Pres Sg P3 => sg3;
      Pres Pl P1 => pl1;
      Pres Pl P2 => pl2;
      Pres Pl P3 => pl3
       }
    } ;

    regVerb : (inf,stem : Str) -> Verb
      = \inf,stem -> {
	    s = table {
	    -- sehen
	    Inf => inf ;
	    -- seh + e
	    Pres Sg P1 => stem + "e"; -- Sg P1? 
      Pres Sg P2 => stem + "st";
      Pres Sg P3 => stem + "t";
      Pres Pl P1 => inf;
      Pres Pl P2 => stem + "t";
      Pres Pl P3 => inf
	    } 
    } ;

      regVerb2 : (inf,stem : Str) -> Verb
      = \inf,stem -> {
	    s = table {
	    -- warten
	    Inf => inf ;
	    -- wart + e
	    Pres Sg P1 => stem + "te"; -- Sg P1? 
      Pres Sg P2 => stem + "test";
      Pres Sg P3 => stem + "tet";
      Pres Pl P1 => inf;
      Pres Pl P2 => stem + "tet";
      Pres Pl P3 => inf
	    } 
    } ;

    --TODO: stems ending in -t --> tött, warten --> tötet, wartet

--   -- regular verbs with predictable variations
    smartVerb : Str -> Verb = \inf -> case inf of {
      seh + "en" => regVerb inf seh ;
      war + "ten" => regVerb2 inf war ;
      -- dorm + "ire" => verb3ire inf dorm dorm ;
      _ => error ("No smarts for verbs here: " ++ inf)
      } ;

  irregVerb : (inf,sg1,sg2,sg3,pl1,pl2,pl3 : Str) -> Verb =
    \inf,sg1,sg2,sg3,pl1,pl2,pl3 ->
      let verb = smartVerb inf 
	  in mkVerb inf sg1 sg2 sg3 pl1 pl2 pl3;

  
  -- two-place verb with "case" as preposition; for transitive verbs, c=[]

  -- Verb2 : Type = Verb ** {c : Str} ; -- engelska
  Verb2 : Type = Verb ** {c : Gender => Number => Str} ; 

  be_Verb : Verb = mkVerb "sein" "bin" "bist" "ist" "sind" "seid" "sind";

-- Preposition : Type = {s : Str ; con : Gender => Number => Str} ;

 agr2vform : Number -> VForm = \a -> case a of {
    Sg => Pres Sg P3 ;
    Pl => Pres Pl P3 
  } ;

--}

}