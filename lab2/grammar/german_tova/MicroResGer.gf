resource MicroResGer = open Prelude in {

param
  Number = Sg | Pl ;
  Case = Nom | Acc | Dat | Gen ;
  Person = P1 | P2 | P3 ; 
  Gender = Fem | Masc | Neut ;
  --NounForm = NF Number Case ; -- p. 94 course notes
  -- DetForm = DF Gender Case;
  VForm = Inf | Pres Number Person ;
  VType = Weak | Strong ; --needed? not sure
  Aux = haben | sein ;
  
  
oper
  --  define types:
  --Noun: Type = {s : NounForm => Str; g : Gender};
  --Determiner : Type = {s : DetForm => Str; n : Number};
  Noun: Type = {s : Number => Case => Str ; g : Gender};
  Determiner: Type = {s : Gender => Case => Str ; n : Number};

-- TODO: make gender the deciding factor for which function to use
makeNoun : Str -> Noun = \word ->
  case word of {
    stem + "а"         => mkFemN stem;
    stem + ("о" | "е") => mkNeutN word;
      _                 => mkMascN word };

mkMascN : Str -> Noun = \mann ->
      { s = table {
        Sg => table {
           Nom => mann ; Acc => mann ; Dat => mann ; Gen => mann + "es"
           } ;
        Pl => table {
          Nom => mann + "er" ; Acc => mann + "er" ; Dat => mann + "ern"; Gen => mann + "er"
        }
      }; 
      g = Masc 
      };


mkFemN : Str -> Noun = \frau ->
      { s = table {
        Sg => table {
           Nom => frau ; Acc => frau ; Dat => frau ; Gen => frau
           } ;
        Pl => table {
          Nom => frau + "en" ; Acc => frau + "en" ; Dat => frau + "en"; Gen => frau + "en"
        }
      }; 
      g = Fem 
      };
  
mkNeutN : Str -> Noun = \kind ->
      { s = table {
        Sg => table {
           Nom => kind ; Acc => kind ; Dat => kind ; Gen => kind + "es"
           } ;
        Pl => table {
          Nom => kind + "er" ; Acc => kind + "er" ; Dat => kind + "ern"; Gen => kind + "er"
        }
      }; 
      g = Neut
      };
  
  
  mkWorstN  : (nomSg, accSg, datSg, genSg, nomPl, accPl, datPl, genPl : Str) -> Gender -> Noun
    =  \nomSg, accSg, datSg, genSg, nomPl, accPl, datPl, genPl, g ->
   {
     s = table {
       Sg => table {
           Nom => nomSg ; Acc => accSg ; Dat => datSg ; Gen => genSg
           } ;
      Pl => table {
           Nom => nomPl ; Acc => accPl ; Dat => datPl ; Gen => genPl
           }
     };
     g = g 
     };

  -- mkNoun : (sg, pl : Str) -> Gender -> Case -> Noun = \sg, pl, gender, cas -> {
  --   s = table { Sg => sg ; Pl => pl } ;
  --   g = gender ;
	--   c = cas
  --   } ;


  -- regNoun : Str -> Noun = 
  --  \sg -> mkNoun sg (sg + "e") Fem Nom;

  -- smartNoun : Str -> Noun = \sg -> case sg of {
  --  _ + ("ik"|"au")         => mkNoun sg (sg + "en") Fem Nom;
  --  _ + ("ik"|"au")         => mkNoun sg (sg + "en") Fem Dat;
  --  _ + ("ik"|"au")         => mkNoun sg (sg + "en") Fem Gen;
	--  _ + "e"					        => mkNoun sg (sg + "n") Masc Nom;
  --  _ + "o"   				      => mkNoun sg (sg + "s") Neut Nom;
	--   _ + ("chen"|"er")      => mkNoun sg sg Fem Nom -- Don't add anything to these nouns, because Pl = Sg
  -- } ;
	
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

  -- Adjective : Type = {s : Str} ;
  -- Adjective : Type = {s : Gender => Number => Str ; isPre : Bool } ;

  Adjective : Type = {s : Gender => Number => Str} ;


  -- mkAdjective : (femSg, femPl, neutSg, neutPl, mascSg, mascPl : Str) -> Adjective =
  mkAdjective : (_, _, _, _, _, _ : Str) -> Adjective =
      \femSg, femPl, neutSg, neutPl, mascSg, mascPl ->
      {
	s = table {
	  Fem => 
	    table {
	      Sg => femSg ;
	      Pl => femPl 
	    } ;
      Neut =>
      table {
        Sg => neutSg ;
        Pl => neutPl
      } ;
	  Masc =>
	    table {
	      Sg => mascSg ;
	      Pl => mascPl
	    }
	  } 
      } ;

    -- herbert
    -- smartAdjective : Str -> Adjective =
    -- \sg ->
    -- case sg of {
	  -- italian + "o" => mkAdjective (italian + "a") (italian + "e") sg (italian + "i") False ;
	  -- grand + "e" => mkAdjective sg (grand + "i") sg sg False
	  --_ =>  error ("No smarts for adjectives here: " ++ sg)
    --} ;

  regAdj : Str -> Adjective = \mascSg -> mkAdjective mascSg (mascSg + "e") (mascSg + "s") (mascSg + "es") (mascSg + "es") (mascSg + "es") ;
   smartAdjective : Str -> Adjective = \mascSg -> case mascSg of {
  	gran + "d"					=> regAdj mascSg ;
	  roug + "e"					=> mkAdjective mascSg (roug + "es") (roug + "es") (roug + "es") (roug + "es") (roug + "es");
	  mauvai + "s"				=> mkAdjective mascSg (mauvai + "se") (mauvai + "ses") (mauvai + "ses") (mauvai + "ses") (mauvai + "ses") ;
	  b + "on"					  => mkAdjective mascSg (b + "onne") (b + "ons") (b + "onnes") (b + "onnes") (b + "onnes");
	  anci + "en"					=> mkAdjective mascSg (anci + "enne") (anci + "ens") (anci + "ennes") (anci + "ens") (anci + "ennes");
	  nouv + "eau"				=> mkAdjective mascSg (nouv + "elle") (nouv + "eaux") (nouv + "elles") (nouv + "eaux") (nouv + "elles");
	  _	                        => regAdj mascSg
  } ;
	

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

}