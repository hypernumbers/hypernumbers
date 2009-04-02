// Define the language string object
function LangStr(){
  this.langs = new Array();
  this.frags = {};
};
//
// Now define some functions of the object
//

// what languages are loaded into the language string object
LangStr.prototype.get_langs = function () {
  var langs = new Array();
  var i =0;
  for (x in this.frags)
    {
      langs[i]=x;
      i++;
    }
  return langs
};

LangStr.prototype.add_frag = function(fragname,frag,lang) {
  // check if the language is already in the object
  if (!this.frags[lang]) {
    this.frags[lang] = {};
  }
  // check if this fragname is already defined for the language
  if (!this.frags[lang]) {
    this.frags[lang][fragname]={};
  }
  // now stick the value in/overwrite the existing value
  this.frags[lang][fragname] = frag;
};
// get all the fragments in a particular language
LangStr.prototype.get_frags = function(lang) {
  return this.frags[lang];
};

//
// This is a spoofer for the moment
//
function load_language_strings(oLangStr) {
  
  // English
  oLangStr.add_frag("login","login","gb");
  oLangStr.add_frag("logout","logout","gb");
  oLangStr.add_frag("2","username","gb");
  oLangStr.add_frag("3","Password","gb");
  oLangStr.add_frag("file","File","gb");
  oLangStr.add_frag("browse","browse","gb");
  oLangStr.add_frag("save","save","gb");
  oLangStr.add_frag("import spreadsheet","open Microsoft Excel 97/2000/2003 (*.xls)","gb");
  oLangStr.add_frag("edit","Edit","gb");
  oLangStr.add_frag("paste values","paste values","gb");
  oLangStr.add_frag("paste formulas","paste formulas","gb");
  oLangStr.add_frag("clear formats","clear formats","gb");
  oLangStr.add_frag("clear values","clear values","gb");
  oLangStr.add_frag("clear both","clear both","gb");
  oLangStr.add_frag("font","Font","gb");
  oLangStr.add_frag("format","Format","gb");
  oLangStr.add_frag("15","plain","gb");
  oLangStr.add_frag("16","rounded","gb");
  oLangStr.add_frag("17","2 decimals","gb");
  oLangStr.add_frag("18","financial","gb");
  oLangStr.add_frag("19","currency","gb");
  oLangStr.add_frag("20","percentage","gb");
  oLangStr.add_frag("21","date","gb");
  oLangStr.add_frag("22","time","gb");
  oLangStr.add_frag("23","date and time","gb");

  // French
  oLangStr.add_frag("login","ouvrir une session","fr");
  oLangStr.add_frag("logout","fermer unse session","fr");
  oLangStr.add_frag("2","username","fr");
  oLangStr.add_frag("3","mot de passé","fr");
  oLangStr.add_frag("file","Fichier","fr");
  oLangStr.add_frag("browse","naviguer","fr");
  oLangStr.add_frag("save","enregistrer","fr");
  oLangStr.add_frag("import spreadsheet","ouvrir Microsoft Excel 97/2000/2003 (*.xls)","fr");
  oLangStr.add_frag("edit","Édition","fr");
  oLangStr.add_frag("paste values","coller (valeurs)","fr");
  oLangStr.add_frag("paste formulas","coller (formules)","fr");
  oLangStr.add_frag("clear formats","gommer (formats)","fr");
  oLangStr.add_frag("clear values","gommer (valeurs)","fr");
  oLangStr.add_frag("clear both","gommer (formats et valeurs)","fr");
  oLangStr.add_frag("font","Police","fr");
  oLangStr.add_frag("format","Format","fr");
  oLangStr.add_frag("15","plaine","fr");
  oLangStr.add_frag("16","arrondir","fr");
  oLangStr.add_frag("17","2 décimales","fr");
  oLangStr.add_frag("18","financier","fr");
  oLangStr.add_frag("19","monnaie","fr");
  oLangStr.add_frag("20","pourcentage","fr");
  oLangStr.add_frag("21","date","fr");
  oLangStr.add_frag("22","temps","fr");
  oLangStr.add_frag("23","date et temps","fr");

  // German
  oLangStr.add_frag("login","Logon","de");
  oLangStr.add_frag("logout","Logout","de");
  oLangStr.add_frag("2","Username","de");
  oLangStr.add_frag("3","Kennwort","de");
  oLangStr.add_frag("file","Datei","de");
  oLangStr.add_frag("browse","grasen","de");
  oLangStr.add_frag("save","speichern","de");
  oLangStr.add_frag("import spreadsheet","Öffnen Microsoft Excel 97/2000/2003 (*.xls)","de");
  oLangStr.add_frag("edit","Bearbeiten","de");
  oLangStr.add_frag("paste values","einfügen (werte)","de");
  oLangStr.add_frag("paste formulas","einfügen (formeln)","de");
  oLangStr.add_frag("clear formats","freie Formate","de");
  oLangStr.add_frag("clear values","freie Werte","de");
  oLangStr.add_frag("clear both","freie Werte und Formate","de");
  oLangStr.add_frag("font","Schriftname","de");
  oLangStr.add_frag("format","Format","de");
  oLangStr.add_frag("15","Ebene ","de");
  oLangStr.add_frag("16","gerundet ","de");
  oLangStr.add_frag("17","2 Dezimalstriche ","de");
  oLangStr.add_frag("18","finanziell","de");
  oLangStr.add_frag("19","Währung","de");
  oLangStr.add_frag("20","Prozentsatz","de");
  oLangStr.add_frag("21","Datum","de");
  oLangStr.add_frag("22","Zeit","de");
  oLangStr.add_frag("23","Datum und Zeit","de");

  // Italian
  oLangStr.add_frag("login","Inizio Attività","it");
  oLangStr.add_frag("logout","Termine Attività","it");
  oLangStr.add_frag("2","Username","it");
  oLangStr.add_frag("3","Parola d'accesso ","it");
  oLangStr.add_frag("file","File","it");
  oLangStr.add_frag("browse","rassegna ","it");
  oLangStr.add_frag("save","salva","it");
  oLangStr.add_frag("import spreadsheet","Apri Microsoft Excel 97/2000/2003 (*.xls)","it");
  oLangStr.add_frag("edit","modifica","it");
  oLangStr.add_frag("paste values","incolla (valori)","it");
  oLangStr.add_frag("paste formulas","incolla (formule)","it");
  oLangStr.add_frag("clear formats","formato libere ","it");
  oLangStr.add_frag("clear values","valori liberi ","it");
  oLangStr.add_frag("clear both","formato e valori liberi","it");
  oLangStr.add_frag("font","Nomme del carattere","it");
  oLangStr.add_frag("format","Formato","it");
  oLangStr.add_frag("15","pianura ","it");
  oLangStr.add_frag("16","arrotondato ","it");
  oLangStr.add_frag("17","2 decimali ","it");
  oLangStr.add_frag("18","finanziario ","it");
  oLangStr.add_frag("19","valuta ","it");
  oLangStr.add_frag("20","percentuale ","it");
  oLangStr.add_frag("21","data ","it");
  oLangStr.add_frag("22","tempo ","it");
  oLangStr.add_frag("23","data ed tempo","it");

  // Portuguese
  oLangStr.add_frag("login","início de uma sessão","pt");
  oLangStr.add_frag("logout","saída","pt");
  oLangStr.add_frag("2","username","pt");
  oLangStr.add_frag("3","Senha","pt");
  oLangStr.add_frag("file","Lima","pt");
  oLangStr.add_frag("browse","consulte","pt");
  oLangStr.add_frag("save","Guardar","pt");
  oLangStr.add_frag("import spreadsheet","Abrir Microsoft Excel 97/2000/2003 (*.xls)","pt");
  oLangStr.add_frag("edit","Editar","pt");
  oLangStr.add_frag("paste values","Colar (valores)","pt");
  oLangStr.add_frag("paste formulas","Colar (fórmulas)","pt");
  oLangStr.add_frag("clear formats","formatos desobstruídos","pt");
  oLangStr.add_frag("clear values","valores desobstruídos","pt");
  oLangStr.add_frag("clear both","formatos et valores desobstruídos","pt");
  oLangStr.add_frag("font","Nomo do tipo de letra","pt");
  oLangStr.add_frag("format","Formatar","pt");
  oLangStr.add_frag("15","planície","pt");
  oLangStr.add_frag("16","arredondado","pt");
  oLangStr.add_frag("17","2 decimais","pt");
  oLangStr.add_frag("18","financeiro","pt");
  oLangStr.add_frag("19","moeda","pt");
  oLangStr.add_frag("20","porcentagem","pt");
  oLangStr.add_frag("21","data","pt");
  oLangStr.add_frag("22","tempo","pt");
  oLangStr.add_frag("23","data e temp","pt");

  // Russian

  // Spanish
  oLangStr.add_frag("login","Connexion","es");
  oLangStr.add_frag("logout","registro de estado de la máquina","es");
  oLangStr.add_frag("2","username","es");
  oLangStr.add_frag("3","Contraseña","es");
  oLangStr.add_frag("file","Archivo","es");
  oLangStr.add_frag("browse","Hojee","es");
  oLangStr.add_frag("save","Guardar","es");
  oLangStr.add_frag("import spreadsheet","abrir Microsoft Excel 97/2000/2003 (*.xls)","es");
  oLangStr.add_frag("edit","Editar","es");
  oLangStr.add_frag("paste values","pegar (valores)","es");
  oLangStr.add_frag("paste formulas","pegar (fórmulas)","es");
  oLangStr.add_frag("clear formats","formatos claros ","es");
  oLangStr.add_frag("clear values","valores claros ","es");
  oLangStr.add_frag("clear both","formatos y valores claros ","es");
  oLangStr.add_frag("font","Fuente","es");
  oLangStr.add_frag("format","Formato","es");
  oLangStr.add_frag("15","Llano","es");
  oLangStr.add_frag("16","Redondeado","es");
  oLangStr.add_frag("17","2 decimales","es");
  oLangStr.add_frag("18","Financier","es");
  oLangStr.add_frag("19","Moneda","es");
  oLangStr.add_frag("20","Porcentaje","es");
  oLangStr.add_frag("21","fecha","es");
  oLangStr.add_frag("22","tiempo","es");
  oLangStr.add_frag("23","fecha y tiempo","es");
 
};

// Focus object
function Focus(){
  this.focuspoint="";
};
// Now add a function to the object
Focus.prototype.changeFocus = function(val) {
  var newElem=document.getElementById(val);
  if (this.focuspoint != "") {
    var oldElem=document.getElementById(this.focuspoint);
    oldElem.style.border="0px";
  };
  newElem.style.border="3px solid red";
  this.focuspoint=val;
};

// Sets the current form builder language
function setFormBuilderLang(e, objId) {
  writeLang(objId);
  oLangFocus.changeFocus(objId)
    };

function writeLang(language) {
  var lang = language.replace("language_","");
  var strings=oLangStrFrmBuilder.get_frags(lang);
  for (x in strings) {
    var elem = document.getElementById(x);
    if (elem != undefined) {
      elem.textContent=strings[x];
    }
  }
};

function setup_langs(){
  load_language_strings(oLangStrFrmBuilder);
  var langs=oLangStrFrmBuilder.get_langs(); 
  var i=0;
  var html="&nbsp;";
  var len = langs.length;
  while (i < len) {
    html += "<img id=\"language_"+langs[i]+"\" src=\"/img/flags/"+langs[i]+".png\">&nbsp;";
    i++;
  }
  // Now insert the flag fragments
  var insertPoint = document.getElementById("formbuilder_lang");
  var newNode = document.createElement("div");
  newNode.innerHTML=html;
  insertPoint.appendChild(newNode);
  // now add event listeners to the various flags
  i=0;
  while (i < len) {
    var id="language_"+langs[i];
    // need to add a Javascript event...
    HN.Util.addEvent(HN.Util.id(id), "click", function (e) { setFormBuilderLang(e, id) })
      // Event.addListener(id,'click',setFormBuilderLang,id,false);
      i++;
  }
  // now set up the default language
  writeLang("language_"+langs[0]);
  // now mark the default language
  oLangFocus.changeFocus("language_"+langs[0])
    };
