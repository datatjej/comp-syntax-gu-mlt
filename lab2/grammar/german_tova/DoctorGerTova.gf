concrete DoctorGerTova of Doctor =
  open
    SyntaxGer,
    ParadigmsGer,
    Prelude
  in {

-- application using standard RGL

lincat
  Phrase = Utt ;
  Fact = Cl ;
  Action = VP ;
  Property = VP ;
  Profession = CN ;
  Person = NP ;
  Place = {at,to : Adv} ;
  Substance = NP ;
  Illness = NP ;

lin
  presPosPhrase fact = mkUtt (mkS fact) ;
  presNegPhrase fact = mkUtt (mkS negativePol fact) ;
  pastPosPhrase fact = mkUtt (mkS anteriorAnt fact) ;
  pastNegPhrase fact = mkUtt (mkS anteriorAnt negativePol fact) ;
  presQuestionPhrase fact = mkUtt (mkQS (mkQCl fact)) ;
  pastQuestionPhrase fact = mkUtt (mkQS anteriorAnt (mkQCl fact)) ;

  impPosPhrase action = mkUtt (mkImp action) ;
  impNegPhrase action = mkUtt negativePol (mkImp action) ;

  actionFact person action = mkCl person action ;
  propertyFact person property = mkCl person property ;

  isProfessionProperty profession = mkVP (mkNP a_Det profession) ;
  needProfessionProperty profession = mkVP need_V2 (mkNP a_Det profession) ;
  isAtPlaceProperty place = mkVP place.at ;
  haveIllnessProperty illness = mkVP have_V2 illness ;

  theProfessionPerson profession = mkNP the_Det profession ;

  iMascPerson = i_NP ;
  iFemPerson = i_NP ;
  youMascPerson = you_NP ;
  youFemPerson = you_NP ;
  hePerson = he_NP ;
  shePerson = she_NP ;

  goToAction place = mkVP (mkVP go_V) place.to ;
  stayAtAction place = mkVP (mkVP stay_V) place.at ;
  vaccinateAction person = mkVP vaccinate_V2 person ;
  examineAction person = mkVP examine_V2 person ;
  takeSubstanceAction substance = mkVP take_V2 substance ;

-- end of what could be a functor
--------------------------------

  coughAction = mkVP (mkV "husten") ;
  breatheAction = mkVP (mkV "atmen") ;
  vomitAction = mkVP (mkV "kotzen") ;
  sleepAction = mkVP (mkV "schlafen" "schläft" "schlief" "schliefe" "geschlafen") ;
  dressAction = mkVP (reflV (mkV "an" (mkV "ziehen" "zieht" "zog" "zöge" "gezogen")) accusative) ;
  undressAction = mkVP (reflV (mkV "aus" (mkV "ziehen" "zieht" "zog" "zöge" "gezogen")) accusative) ;

  eatAction = mkVP (mkV "essen" "isst" "aß" "äße" "gegessen") ; 
  drinkAction = mkVP (mkV "trinken" "trinkt" "trank" "tränke" "getrunken") ;
  smokeAction = mkVP (mkV "rauchen") ;
  measureTemperatureAction = mkVP (mkV2 (mkV "messen" "misst" "maß" "mäße" "gemessen")) (mkNP the_Det (mkN "Körpertemperatur" feminine)) ;
  measureBloodPressureAction = mkVP (mkV2 (mkV "messen" "misst" "maß" "mäße" "gemessen")) (mkNP the_Det (mkN "Blutdruck")) ;
  hospitalPlace = {at = pAdv "im Krankenhaus" ; to = pAdv "zum Krankenhaus"} ;
  homePlace = {at = pAdv "zu Hause" ; to = pAdv "nach Hause"} ;
  schoolPlace = {at = pAdv "in der Schule" ; to = pAdv "zur Schule"} ;
  workPlace = {at = pAdv "auf der Arbeit" ; to = pAdv "zur Arbeit"} ;

  doctorProfession = mkCN (mkN "Doktor") ;
  nurseProfession = mkCN (mkN "Krankenschwester" feminine) ;
  interpreterProfession = mkCN (mkN "Übersetzer") ;

  bePregnantProperty = mkVP (mkA "schwanger") ;
  beIllProperty = mkVP (mkA "krank") ;
  beWellProperty = mkVP (mkA "gesund") ;
  beDeadProperty = mkVP (mkA "tot") ;
  haveAllergiesProperty = mkVP have_V2 (mkNP aPl_Det (mkN "Allergie")) ;
  havePainsProperty = mkVP have_V2 (mkNP aPl_Det (mkN "Schmerz")) ;
  haveChildrenProperty = mkVP have_V2 (mkNP aPl_Det (mkN "Kind" "Kind" "Kind" "Kindes" "Kinder" "Kindern")) ;


  feverIllness = mkNP a_Det (mkN "Fieber") ;
  fluIllness = mkNP a_Det (mkN "Grippe") ;
  headacheIllness = mkNP a_Det (mkN "Kopfweh") ;
  diarrheaIllness = mkNP a_Det (mkN "Durchfall") ;
  heartDiseaseIllness = mkNP a_Det (mkN "Herzkrankheit") ;
  lungDiseaseIllness = mkNP a_Det (mkN "Lungkrankheit") ;
  hypertensionIllness = mkNP (mkN "Hypertonie") ;

  alcoholSubstance = mkNP (mkN "Alkohol") ;
  medicineSubstance = mkNP a_Det (mkN "Medizine") ;
  drugsSubstance = mkNP aPl_Det (mkN "Droge") ;

oper
  pAdv : Str -> Adv = ParadigmsGer.mkAdv ;

  go_V = seinV (mkV "gehen" "geht" "ging" "ginge" "gegagen") ;
  stay_V = seinV (mkV "bleiben" "bleibt" "blieb" "bliebe" "geblieben") ;
  need_V2 = mkV2 (mkV "brauchen") ; 
  take_V2 = mkV2 (mkV "nehmen" "nimmt" "nimm" "nahm" "nähme" "genommen") ;
  put_V2 = mkV2 (mkV "legen") ;
  undress_V = reflV (mkV "ausziehen") ;
  vaccinate_V2 = mkV2 (mkV "impfen") ;
  examine_V2 = mkV2 (mkV "untersuchen" "untersucht" "untersuche" "untersuchte" "untersucht") ;
}
