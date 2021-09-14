resource MicroResGer = open Prelude, Predef in {

param
  Number = Sg | Pl ;
  Case = Nom | Acc | Dat | Gen ;
  Person = P1 | P2 | P3 ; 
  Gender = Fem | Masc | Neut ;
  VForm = Inf | Pres Number Person ;
  VType = Weak | Strong ; --needed? not sure
  Aux = haben | sein ;
  -- AdjForm = Strong | Weak ;

oper
  --  define types:
  Noun: Type = {s : Number => Case => Str ; g : Gender};
  Determiner: Type = {s : Gender => Case => Str ; n : Number};

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

  Adjective : Type = {s : Gender => Number => Str} ;

  -- NOTES: AdjForms : Type = { pred, mnom, macc, mdat, fnom, nnom : Str} 

  mkAdjective : Str -> Adjective =
      \a ->
      {
	s = table {
	  Fem => 
	    table {
	      Sg => a + "e";
	      Pl => a + "e" 
	    } ; 
      Neut =>
      table {
        Sg => a + "es" ;
        Pl => a + "e"
      } ;
	    Masc =>
	    table {
	      Sg => a + "er" ;
	      Pl => a + "e"
	    }
	  } 
      } ;

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
  -- Verb2 : Type = Verb ** {c : Gender => Number => Str} ; 

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