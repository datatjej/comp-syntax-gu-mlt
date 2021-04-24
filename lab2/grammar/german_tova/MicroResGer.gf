resource MicroResGer = open Prelude in {

param
  Number = Sg | Pl ;
  Case = Nom | Acc | Dat | Gen ;
  Gender = Fem | Masc | Neut ;


  -- Agreement = Agr Number ; ---s Person to be added
  Agreement = Agr Gender Number Person ;

  -- all forms of normal Eng verbs, although not yet used in MiniGrammar
  -- VForm = Inf | PresSg3 | Past | PastPart | PresPart ; 
  
  -- all forms of normal Ger verbs
  Person = P1 | P2 | P3 ; 
  -- herbert: VForm = Inf | Cond Number Person | Ind Tense Number Person | SubjPres Number Person | SubjImpf Number Person | Imp Number Person ; 
  -- paper: present tense/indicative, present tense/conjunctive, past tense/ indicative and past tense/ conjunctive.
  VForm = Inf | Ind | Conj | Imp | Part ;
  VType = Weak | Strong ; --needed? not sure
  Tense = Pres | Past ;
  Aux = haben | sein ;
  
  Article = Der | Die | Das | Den | Dem | Des ;
  
oper

  Noun : Type = {s : Number => Str ; g : Gender ; c : Case} ;
  -- Noun : Type = {s : Number => Str ; g : Gender } ;

  mkNoun : (sg, pl : Str) -> Gender -> Case -> Noun = \sg, pl, gender, cas -> {
     s = table { Sg => sg ; Pl => pl } ;
     g = gender ;
	 c = cas
     } ;

  -- regNoun : Str -> Noun = \sg -> mkNoun sg (sg + "s") ;
  regNoun : Str -> Noun = \sg -> mkNoun sg (sg + "e") ;

  -- smart paradigm
  --smartNoun : Str -> Noun = \sg -> case sg of {
  --  _ + ("ay"|"ey"|"oy"|"uy") => regNoun sg ;
  -- x + "y"                   => mkNoun sg (x + "ies") ;
  --  _ + ("ch"|"sh"|"s"|"o")   => mkNoun sg (sg + "es") ;
  --  _                         => regNoun sg
  --  } ;
	
  smartNoun : Str -> Noun = \sg -> case sg of {
    _ + ("ik"|"au")         => mkNoun sg (sg + "en") ;
	_ + "e"					=> mkNoun sg (sg + "n") ;
    _ + "o"   				=> mkNoun sg (sg + "s") ;
	_ + ("chen"|"er")		=> sg
    } ;
	
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

  mkVerb : (inf,pres,past,pastpart,prespart : Str) -> Verb
    = \inf,pres,past,pastpart,prespart -> {
    s = table {
      Inf => inf ;
      PresSg3 => pres ;
      Past => past ;
      PastPart => pastpart ;
      PresPart => prespart
      }
    } ;

  regVerb : (inf : Str) -> Verb = \inf ->
    mkVerb inf (inf + "s") (inf + "ed") (inf + "ed") (inf + "ing") ;

  -- regular verbs with predictable variations
  --smartVerb : Str -> Verb = \inf -> case inf of {
  --   pl  +  ("a"|"e"|"i"|"o"|"u") + "y" => regVerb inf ;
  --   cr  +  "y" =>  mkVerb inf (cr + "ies") (cr + "ied") (cr + "ied") (inf + "ing") ;
  --   lov + "e"  => mkVerb inf (inf + "s") (lov + "ed") (lov + "ed") (lov + "ing") ;
  --   kis + ("s"|"sh"|"x"|"o") => mkVerb inf (inf + "es") (inf + "ed") (inf + "ed") (inf + "ing") ;
  --   _ => regVerb inf
  --   } ;

  smartVerb : Str -> Verb = \inf -> case inf of {
     pl  +  ("a"|"e"|"i"|"o"|"u") + "y" => regVerb inf ;
     cr  +  "y" =>  mkVerb inf (cr + "ies") (cr + "ied") (cr + "ied") (inf + "ing") ;
     lov + "e"  => mkVerb inf (inf + "s") (lov + "ed") (lov + "ed") (lov + "ing") ;
     kis + ("s"|"sh"|"x"|"o") => mkVerb inf (inf + "es") (inf + "ed") (inf + "ed") (inf + "ing") ;
     _ => regVerb inf
     } ;

  -- normal irregular verbs e.g. drink,drank,drunk
  irregVerb : (inf,past,pastpart : Str) -> Verb =
    \inf,past,pastpart ->
      let verb = smartVerb inf
      in mkVerb inf (verb.s ! PresSg3) past pastpart (verb.s ! PresPart) ;   

  -- two-place verb with "case" as preposition; for transitive verbs, c=[]
  Verb2 : Type = Verb ** {c : Str} ;

  -- be_Verb : Verb = mkVerb "are" "is" "was" "been" "being" ; ---s to be generalized
  be_Verb : Verb = mkVerb "sein" "bin" "sind" "bist" "seid" "ist" "sind" ; ---s to be generalized


---s a very simplified verb agreement function for Micro
  agr2vform : Agreement -> VForm = \a -> case a of {
    Agr Sg => PresSg3 ;
    Agr Pl => Inf
    } ;

}
