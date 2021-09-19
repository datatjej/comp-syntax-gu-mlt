concrete DoctorEng of Doctor =
  open
    SyntaxEng,
    ParadigmsEng,
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
  breatheAction = mkVP (mkV "atmen" "atme" "atmest" "atmet" "atmen" "atmet" "atmen") ;
  vomitAction = mkVP (mkV "kotzen") ;
  sleepAction = mkVP (mkV "schlafen") ;
  undressAction = mkVP (mkVP take_V2 (mkNP thePl_Det (mkN "Kleider" "Kleider" "Kleidern" "Kleider" "Kleider" "Kleider" "Kleidern" "Kleider" Fem))) (pAdv "auf") ;
  dressAction = mkVP (mkVP put_V2 (mkNP thePl_Det (mkN "Kleider" "Kleider" "Kleidern" "Kleider" "Kleider" "Kleider" "Kleidern" "Kleider" Fem))) (pAdv "an") ;
  eatAction = mkVP (mkV "essen" "esse" "isst" "isst" "essen" "isst" "essen") ;
  drinkAction = mkVP (mkV "trinken") ;
  smokeAction = mkVP (mkV "rauchen") ;
  measureTemperatureAction = mkVP (mkV2 (mkV "messen")) (mkNP the_Det (mkN "Körpertemperatur")) ;
  measureBloodPressureAction = mkVP (mkV2 (mkV "messen")) (mkNP the_Det (mkN "Blutdruck")) ;

  hospitalPlace = {at = pAdv "im Krankenhaus" ; to = pAdv "zum Krankenhaus"} ;
  homePlace = {at = pAdv "zu Hause" ; to = pAdv "nach Hause"} ;
  schoolPlace = {at = pAdv "in der Schule" ; to = pAdv "zur Schule"} ;
  workPlace = {at = pAdv "auf der Arbeit" ; to = pAdv "zur Arbeit"} ;

  doctorProfession = mkCN (mkN "Doktor") ;
  nurseProfession = mkCN (mkN "Krankenschwester") ;
  interpreterProfession = mkCN (mkN "Übersetzer") ;

  bePregnantProperty = mkVP (mkA "schwanger") ;
  beIllProperty = mkVP (mkA "krank") ;
  beWellProperty = mkVP (mkA "gesund") ;
  beDeadProperty = mkVP (mkA "tot") ;
  haveAllergiesProperty = mkVP have_V2 (mkNP aPl_Det (mkN "Allergie")) ;
  havePainsProperty = mkVP have_V2 (mkNP aPl_Det (mkN "Schmerz")) ;
  haveChildrenProperty = mkVP have_V2 (mkNP aPl_Det (mkN "Kind" "Kinder")) ;

  feverIllness = mkNP a_Det (mkN "Fieber") ;
  fluIllness = mkNP a_Det (mkN "Grippe") ;
  headacheIllness = mkNP a_Det (mkN "Kopfweh") ;
  diarrheaIllness = mkNP a_Det (mkN "Durchfall") ;
  heartDiseaseIllness = mkNP a_Det (mkN "Herzbeschwerden") ;
  lungDiseaseIllness = mkNP a_Det (mkN "Lungbeschwerden") ;
  hypertensionIllness = mkNP (mkN "Hypertension") ;

  alcoholSubstance = mkNP (mkN "Alkohol") ; --der
  medicineSubstance = mkNP a_Det (mkN "Medikamente") ;
  drugsSubstance = mkNP aPl_Det (mkN "Drogen") ;

oper
  pAdv : Str -> Adv = ParadigmsEng.mkAdv ;

  go_V = mkV "gehen" ;
  stay_V = mkV "bleiben" ;
  need_V2 = mkV2 (mkV "brauchen") ;
  take_V2 = mkV2 (mkV "nehmen" "nehme" "nimmst" "nimmt" "nehmen" "nehmt" "nehmen") ;
  put_V2 = mkV2 (mkV "legen") ;
  vaccinate_V2 = mkV2 (mkV "impfen") ;
  examine_V2 = mkV2 (mkV "untersuchen") ;

}
