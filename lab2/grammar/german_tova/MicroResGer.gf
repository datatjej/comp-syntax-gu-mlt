resource MicroResGer = open Prelude, Predef in {

param
  Number = Sg | Pl ;
  Case = Nom | Acc | Dat | Gen ;
  Person = P1 | P2 | P3 ; 
  Gender = Fem | Masc | Neut ;
  VForm = Inf | Pres Number Person ;
  Aux = haben | sein ;
  
  UseAP = Attr FormA | Pred ;
  AForm = Strong | Weak | Mixed ;
  FormA = sgA AForm Gender Case | plA AForm Case ;


oper
  --  define types:

  Noun: Type = {s : Number => Case => Str ; g : Gender};

  Determiner : Type = {s : Gender => Case => Str ; n : Number ; d : AForm};

  mkNoun : (sgNom,sgAcc,sgDat,sgGen,plNom,plAcc,plDat,plGen : Str) -> Gender -> Noun = 
    \sgNom,sgAcc,sgDat,sgGen,plNom,plAcc,plDat,plGen,g -> {
    s = table {
      Sg => table {
        Nom => sgNom ;
        Acc => sgAcc ;
        Dat => sgDat ;
        Gen => sgGen
        } ;
      Pl => table {
        Nom => plNom ;
        Acc => plAcc ;
        Dat => plDat ;
        Gen => plGen
        }
      } ;
    g = g
    } ;
      
 smartNoun : Str -> Noun = \sg -> case sg of {
    _ + ("ik"|"au"|"ilch"|"e"|"ur")                => regNounFem sg;
    _ + ("ier"|"erd"|"und"|"iff"|"ern"|"ein"|"hol")	=> regNounMasc sg;
    _ + ("o"|"y")   			                    => regNounNeut sg;
    _ => error ("No smarts for nouns here: " ++ sg)
   } ;

  -- Katzeen, Blumeen

  regNounMasc : Str -> Noun = \tier ->
    let
      tier = tier;
      tiere = tier + "e";
      tieren = tier + "en";
      tieres = tier + "es";
    in
    mkNoun 
      tier tier tier tieres 
      tiere tiere tieren tiere
      Masc ;

    regNounFem : Str -> Noun = \frau ->
    let
      frau = frau;
      frauen = frau + "en";
    in
    mkNoun 
      frau frau frau frau
      frauen frauen frauen frauen
      Fem ;

  regNounNeut : Str -> Noun = \baby ->
    let
      baby = baby;
      babys = baby + "s";
    in
    mkNoun 
      baby baby baby babys 
      babys babys babys babys
      Neut ;

  Adjective : Type = {s : UseAP => Str} ;

  mkAdjective : Str -> Adjective =
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
	    Pres Sg P1 => stem + "e";
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
	    Pres Sg P1 => stem + "te"; 
      Pres Sg P2 => stem + "test";
      Pres Sg P3 => stem + "tet";
      Pres Pl P1 => inf;
      Pres Pl P2 => stem + "tet";
      Pres Pl P3 => inf
	    } 
    } ;

      regVerb3 : (inf,stem : Str) -> Verb
      = \inf,stem -> {
	    s = table {
	    -- finden
	    Inf => inf ;
	    -- find + e
	    Pres Sg P1 => stem + "de";
      Pres Sg P2 => stem + "dest";
      Pres Sg P3 => stem + "det";
      Pres Pl P1 => inf;
      Pres Pl P2 => stem + "det";
      Pres Pl P3 => inf
	    } 
    } ;

    -- regular verbs with predictable variations
    smartVerb : Str -> Verb = \inf -> case inf of {
      fin + "den" => regVerb3 inf fin ;     -- finden
      war + "ten" => regVerb2 inf war ;     -- warten, töten
      seh + "en" => regVerb inf seh ;
      _ => error ("No smarts for verbs here: " ++ inf)
      } ;

  irregVerb : (inf,sg1,sg2,sg3,pl1,pl2,pl3 : Str) -> Verb =
    \inf,sg1,sg2,sg3,pl1,pl2,pl3 ->
      let verb = smartVerb inf 
	  in mkVerb inf sg1 sg2 sg3 pl1 pl2 pl3;

  -- two-place verb with "case" as preposition; for transitive verbs, c=[]

  Verb2 : Type = Verb ** {c : Str};  --tidigare: {c : Gender => Number => Str} ; 

  be_Verb : Verb = mkVerb "sein" "bin" "bist" "ist" "sind" "seid" "sind";

-- Preposition : Type = {s : Str ; con : Gender => Number => Str} ;


--}

}