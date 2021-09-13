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
  
   -- "Apfel" "Apfel" "Apfel" "Apfels" "Äpfel" "Äpfel" "Äpfeln" "Äpfel" Masc;
   -- "Kind" "Kind" "Kind" "Kindes" "Kinder" "Kinder" "Kindern "Kinder" Neut;
   -- "Bier" "Bier" "Bier" "Bieres" "Biere" "Biere" "Bieren" 
   -- "Stadt" "Stadt" "Stadt" "Stadt" "Städte" "Städte" "Städten" "Städte" Fem;

  -- Adjective : Type = {s : Str} ;
  -- Adjective : Type = {s : Gender => Number => Str ; isPre : Bool } ;

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
  -- two-place verb with "case" as preposition; for transitive verbs, c=[]
  Verb2 : Type = Verb ** {c : Gender => Number => Str} ;

  mkVerb : (inf, sg1, sg2, sg3, pl1, pl2, pl3: Str) -> Verb
    = \inf,sg1,sg2,sg3,pl1, pl2, pl3 -> {
    s = table {
      Inf => inf ;
      Pres Sg P1 => sg1 ;
      Pres Sg P2 => sg2 ;
      Pres Sg P3 => sg3 ;
      Pres Pl P1 => pl1 ;
      Pres Pl P2 => pl2 ;
      Pres Pl P3 => pl3
      }
    } ;

  regVerb : (inf: Str) -> Verb = \inf ->
    mkVerb inf (inf + "s") (inf + "ed") (inf + "ed") (inf + "ing") (inf + "ing") (inf + "ing")  ;

  -- regular verbs with predictable variations
  smartVerb : Str -> Verb = \inf -> case inf of {
    pl +  ("a"|"e"|"i"|"o"|"u") + "y" => regVerb inf ;
    cr +  "y" =>  mkVerb inf (cr + "ies") (cr + "ied") (cr + "ied") (inf + "ing") (inf + "ing") (inf + "ing")  ;
    lov + "e"  => mkVerb inf (inf + "s") (lov + "ed") (lov + "ed") (lov + "ing") (inf + "ing") (inf + "ing") ;
    kis + ("s"|"sh"|"x"|"o") => mkVerb inf (inf + "es") (inf + "ed") (inf + "ed") (inf + "ing") (inf + "ing") (inf + "ing") ;
    _ => regVerb inf
  } ;

  -- normal irregular verbs e.g. drink,drank,drunk
  -- irregVerb : (inf,past,pastpart : Str) -> Verb =
  --  \inf,past,pastpart ->
  --    let verb = smartVerb inf
  --    in mkVerb inf (verb.s ! PresSg3) past pastpart (verb.s ! PresPart) ;   

  irregVerb : (inf,sg1,sg2,sg3,pl1,pl2,pl3 : Str) -> Verb =
    \inf,sg1,sg2,sg3,pl1,pl2,pl3 ->
      let verb = smartVerb inf 
	  in mkVerb inf sg1 sg2 sg3 pl1 pl2 pl3 ;

  -- be_Verb : Verb = mkVerb "are" "is" "was" "been" "being" ; ---s to be generalized
  be_Verb : Verb = mkVerb "sein" "bin" "sind" "bist" "seid" "ist" "sind" ; ---s to be generalized


  agr2vform : Number -> VForm = \a -> case a of {
    Sg => Pres Sg P3 ;
    Pl => Pres Pl P3 
  } ;

--}

}