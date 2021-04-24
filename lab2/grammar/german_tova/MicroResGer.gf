resource MicroResGer = open Prelude in {

param
  Number = Sg | Pl ;
  Case = Nom | Acc | Dat | Gen ;
  Person = P1 | P2 | P3 ; 
  Gender = Fem | Masc | Neut ;
  Article = Der | Die | Das | Den | Dem | Des ;

  -- Agreement = Agr Number ; ---s Person to be added
  Agreement = Agr Gender Number Person ;

  -- all forms of normal Eng verbs, although not yet used in MiniGrammar
  -- VForm = Inf | PresSg3 | Past | PastPart | PresPart ; 
  
  VForm = Inf | Pres Number Person ;
  VType = Weak | Strong ; --needed? not sure
  Aux = haben | sein ;
  
oper
  -- define types:
  Noun : Type = {s : Number => Str ; g : Gender ; c : Case} ;
  -- Noun : Type = {s : Number => Str ; g : Gender } ;

  mkNoun : (sg, pl : Str) -> Gender -> Case -> Noun = \sg, pl, gender, cas -> {
     s = table { Sg => sg ; Pl => pl } ;
     g = gender ;
	   c = cas
     } ;

  regNoun : Str -> Noun = 
    \sg -> mkNoun sg (sg + "e") Fem Nom;

  smartNoun : Str -> Noun = \sg -> case sg of {
    _ + ("ik"|"au")         => mkNoun sg (sg + "en") Fem Nom;
    _ + ("ik"|"au")         => mkNoun sg (sg + "en") Fem Dat;
    _ + ("ik"|"au")         => mkNoun sg (sg + "en") Fem Gen;
	  _ + "e"					        => mkNoun sg (sg + "n") Masc Nom;
    _ + "o"   				      => mkNoun sg (sg + "s") Neut Nom;
	   _ + ("chen"|"er")      => mkNoun sg sg Fem Nom -- Don't add anything to these nouns, because Pl = Sg
  } ;
	
  -- smart paradigm
  --smartNoun : Str -> Noun = \sg -> case sg of {
  --  _ + ("ay"|"ey"|"oy"|"uy") => regNoun sg ;
  -- x + "y"                   => mkNoun sg (x + "ies") ;
  --  _ + ("ch"|"sh"|"s"|"o")   => mkNoun sg (sg + "es") ;
  --  _                         => regNoun sg
  --  } ;
	
	-- +e: Blut(e), Tier(e), Bier(e), Boot(e), Brot(e), Hund(e), Fisch(e), Pferd(e), Milch(e), Schiff(e), Stern(e), Wein(e)
	-- +n: Junge(n), Katze(n), Wolke(n), Blume(n), Sprache(n), See(n)
	-- +s: Auto(s) OBS: inget -n i dativ
	-- +en: Grammatik(en), Musik(en), Frau(en)
	--vokaländring: Apfel, Vogel: Apfel --> Äpfel, Vogel -> Vögel
	--vokaländring + er: Buch --> Bücher, Haus --> Häuser, Baum --> Bäume
	--vokaländring + e: Stadt --> Städte, Kuh --> Kühe, Zug --> Züge
	--oregelbundna: Kind(er)
	--ingen ändring: Computer, Feuer, Mädchen, Wasser

  Adjective : Type = {s : Str} ;

  Verb : Type = {s : VForm => Str} ;

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

  -- regVerb : (inf: Str) -> Verb = \inf ->
  --  mkVerb inf (inf + "s") (inf + "ed") (inf + "ed") (inf + "ing") ;

  -- regular verbs with predictable variations
  --smartVerb : Str -> Verb = \inf -> case inf of {
  --   pl  +  ("a"|"e"|"i"|"o"|"u") + "y" => regVerb inf ;
  --   cr  +  "y" =>  mkVerb inf (cr + "ies") (cr + "ied") (cr + "ied") (inf + "ing") ;
  --   lov + "e"  => mkVerb inf (inf + "s") (lov + "ed") (lov + "ed") (lov + "ing") ;
  --   kis + ("s"|"sh"|"x"|"o") => mkVerb inf (inf + "es") (inf + "ed") (inf + "ed") (inf + "ing") ;
  --   _ => regVerb inf
  --   } ;



  -- normal irregular verbs e.g. drink,drank,drunk
  --irregVerb : (inf,past,pastpart : Str) -> Verb =
  --  \inf,past,pastpart ->
  --    let verb = smartVerb inf
  --    in mkVerb inf (verb.s ! PresSg3) past pastpart (verb.s ! PresPart) ;   

  -- two-place verb with "case" as preposition; for transitive verbs, c=[]
  Verb2 : Type = Verb ** {c : Str} ;

  -- be_Verb : Verb = mkVerb "are" "is" "was" "been" "being" ; ---s to be generalized
  be_Verb : Verb = mkVerb "sein" "bin" "sind" "bist" "seid" "ist" "sind" ; ---s to be generalized


---s a very simplified verb agreement function for Micro
--  agr2vform : Agreement -> VForm = \a -> case a of {
--    Agr Sg => PresSg3 ;
--    Agr Pl => Inf
--    } ;

}
