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
  oLangStr.add_frag("multi_1","Debugging","gb");
  oLangStr.add_frag("multi_2","Current Object","gb");
  oLangStr.add_frag("multi_3","Common","gb");
  oLangStr.add_frag("multi_6","Properties","gb");
  oLangStr.add_frag("multi_7","Text:","gb");
  oLangStr.add_frag("multi_8","Right-click on the panel to build a form","gb");
  oLangStr.add_frag("multi_9","Double-click on form elements to configure them","gb");
  // French
  oLangStr.add_frag("multi_1","Le Debugging","fr");
  oLangStr.add_frag("multi_2","L'Object Courrant","fr");
  oLangStr.add_frag("multi_3","Les Commons","fr");
  oLangStr.add_frag("multi_6","Les Properties","fr");
  oLangStr.add_frag("multi_7","La Texte:","fr");
  oLangStr.add_frag("multi_8","Right-click on the panel to build a form, s'il vous plais!","fr");
  oLangStr.add_frag("multi_9","Double-click on form elements to configure them, s'il vous plais!","fr");
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
    elem.textContent=strings[x];
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
    Event.addListener(id,'click',setFormBuilderLang,id,false);
    i++;
  }
  // now set up the default language
  writeLang("language_"+langs[0]);
  // now mark the default language
  oLangFocus.changeFocus("language_"+langs[0])
};
