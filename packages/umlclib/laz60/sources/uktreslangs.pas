(**
 **************************************************************************
 *                                                                        *
 *  This file is part of the uktat Developer's Component Library.        *
 *                                                                        *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution, *
 *  for details about the copyright.                                      *
 *                                                                        *
 *  This program is distributed in the hope that it will be useful,       *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *
 *                                                                        *
 **************************************************************************
**)

unit uktreslangs;

interface
uses
  dummy;

{$INCLUDE 'uktlib_language.inc'}

 (* ISO 639 language codes *)

 (* Note: *)
 (* 1. "SpanishEurope"  added .*)
 (* 2. "PortugueseEurope" added .*)
 (* 3. "SpanishAmerica" added .*)
 (* 4. "PortugueseBrasil" added .*)
 (* 5. "JapaneseAynu" added .*)
 (* 6. "None" added .*)

//resourcestring

const
  strlgNeutral       = 'lgNeutral';
//  strlgNone          = 'lgNone';

  strlgAfar          = 'lgAfar';
  strlgAbkhazian     = 'lgAbkhazian';
  strlgAvestan       = 'lgAvestan';
  strlgAfrikaans     = 'lgAfrikaans';
  strlgAkan          = 'lgAkan';
  strlgAmharic       = 'lgAmharic';
  strlgAragonese     = 'lgAragonese';
  strlgArabic        = 'lgArabic';
  strlgAssamese      = 'lgAssamese';
  strlgAvaric        = 'lgAvaric';
  strlgAymara        = 'lgAymara';
  strlgAzerbaijani   = 'lgAzerbaijani';
  strlgBashkir       = 'lgBashkir';
  strlgBelarusian    = 'lgBelarusian';
  strlgBulgarian     = 'lgBulgarian';
  strlgBihari        = 'lgBihari';
  strlgBislama       = 'lgBislama';
  strlgBambara       = 'lgBambara';
  strlgBengali       = 'lgBengali';
  strlgTibetan       = 'lgTibetan';
  strlgBreton        = 'lgBreton';
  strlgBosnian       = 'lgBosnian';
  strlgCatalan       = 'lgCatalan';
  strlgChechen       = 'lgChechen';
  strlgChamorro      = 'lgChamorro';
  strlgCorsican      = 'lgCorsican';
  strlgCree          = 'lgCree';
  strlgCzech         = 'lgCzech';
  strlgChuvash       = 'lgChuvash';
  strlgWelsh         = 'lgWelsh';
  strlgDanish        = 'lgDanish';
  strlgGerman        = 'lgGerman';
  strlgDivehi        = 'lgDivehi';
  strlgDzongkha      = 'lgDzongkha';
  strlgEwe           = 'lgEwe';
  strlgGreek         = 'lgGreek';
  strlgEnglish       = 'lgEnglish';
  strlgEnglishAmerican   = 'lgAmericanEnglish';
  strlgEnglishBritish    = 'lgBritishEnglish';
  strlgEnglishAustralian = 'lgAustralianEnglish';
  strlgEsperanto         = 'lgEsperanto';
  strlgSpanish           = 'lgSpanish';
  strlgEstonian          = 'lgEstonian';
  strlgBasque            = 'lgBasque';
  strlgPersian           = 'lgPersian';
  strlgFulah             = 'lgFulah';
  strlgFinnish           = 'lgFinnish';
  strlgFijian            = 'lgFijian';
  strlgFaroese           = 'lgFaroese';
  strlgFrench            = 'lgFrench';
  strlgWalloonBelgique   = 'lgWalloonBelgique';
  strlgFrisian           = 'lgFrisian';
  strlgIrish             = 'lgIrish';
  strlgGaelic            = 'lgGaelic';
  strlgGallegan          = 'lgGallegan';
  strlgGuarani           = 'lgGuarani';
  strlgGujarati          = 'lgGujarati';
  strlgManx              = 'lgManx';
  strlgHausa             = 'lgHausa';
  strlgHebrew            = 'lgHebrew';
  strlgHindi             = 'lgHindi';
  strlgHiriMotu          = 'lgHiriMotu';
  strlgCroatian          = 'lgCroatian';
  strlgHaitian           = 'lgHaitian';
  strlgHungarian         = 'lgHungarian';
  strlgArmenian          = 'lgArmenian';
  strlgHerero            = 'lgHerero';
  strlgInterlingua       = 'lgInterlingua';
  strlgIndonesian        = 'lgIndonesian';
  strlgInterlingue       = 'lgInterlingue';
  strlgIgbo              = 'lgIgbo';
  strlgSichuanYi         = 'lgSichuanYi';
  strlgInupiaq           = 'lgInupiaq';
  strlgIdo               = 'lgIdo';
  strlgIcelandic         = 'lgIcelandic';
  strlgItalian           = 'lgItalian';
  strlgInuktitut         = 'lgInuktitut';
  strlgJapanese          = 'lgJapanese';
  strlgJavanese          = 'lgJavanese';
  strlgGeorgian          = 'lgGeorgian';
  strlgKongo             = 'lgKongo';
  strlgKikuyu            = 'lgKikuyu';
  strlgKuanyama          = 'lgKuanyama';
  strlgKazakh            = 'lgKazakh';
  strlgGreenlandic       = 'lgGreenlandic';
  strlgKhmer             = 'lgKhmer';
  strlgKannada           = 'lgKannada';
  strlgKorean            = 'lgKorean';
  strlgKanuri            = 'lgKanuri';
  strlgKashmiri          = 'lgKashmiri';
  strlgKurdish           = 'lgKurdish';
  strlgCornish           = 'lgCornish';
  strlgKomi              = 'lgKomi';
  strlgKirghiz           = 'lgKirghiz';
  strlgLatin             = 'lgLatin';
  strlgLuxembourgish     = 'lgLuxembourgish';
  strlgGanda             = 'lgGanda';
  strlgLimburgan         = 'lgLimburgan';
  strlgLingala           = 'lgLingala';
  strlgLao               = 'lgLao';
  strlgLithuanian        = 'lgLithuanian';
  strlgLubaKatanga       = 'lgLuba Katanga';
  strlgLatvian           = 'lgLatvian';
  strlgMalagasy          = 'lgMalagasy';
  strlgMarshallese       = 'lgMarshallese';
  strlgMaori             = 'lgMaori';
  strlgMacedonian        = 'lgMacedonian';
  strlgMalayalam         = 'lgMalayalam';
  strlgMongolian         = 'lgMongolian';
  strlgMoldavian         = 'lgMoldavian';
  strlgMarathi           = 'lgMarathi';
  strlgMalay             = 'lgMalay';
  strlgMaltese           = 'lgMaltese';
  strlgBurmese           = 'lgBurmese';
  strlgNauru             = 'lgNauru';
  strlgNorwegianBokmaal  = 'lgNorwegianBokmaal';
  strlgNdebeleNorth      = 'lgNdebele North';
  strlgNepali            = 'lgNepali';
  strlgNdonga            = 'lgNdonga';
  strlgDutch             = 'lgDutch';
  strlgFlemish           = 'lgFlemish';
  strlgNorwegianNynorsk  = 'lgNorwegianNynorsk';
  strlgNorwegian         = 'lgNorwegian';
  strlgNdebeleSouth      = 'lgNdebeleSouth';
  strlgNavajo            = 'lgNavajo';
  strlgChichewa          = 'lgChichewa';
  strlgOccitan           = 'lgOccitan';
  strlgOjibwa            = 'lgOjibwa';
  strlgOromo             = 'lgOromo';
  strlgOriya             = 'lgOriya';
  strlgOssetian          = 'lgOssetian';
  strlgPanjabi           = 'lgPanjabi';
  strlgPali              = 'lgPali';
  strlgPolish            = 'lgPolish';
  strlgPushto            = 'lgPushto';
  strlgPortuguese        = 'lgPortuguese';
  strlgQuechua           = 'lgQuechua';
  strlgRaetoRomance      = 'lgRaetoRomance';
  strlgRundi             = 'lgRundi';
  strlgRomanian          = 'lgRomanian';
  strlgRussian           = 'lgRussian';
  strlgKinyarwanda       = 'lgKinyarwanda';
  strlgSanskrit          = 'lgSanskrit';
  strlgSardinian         = 'lgSardinian';
  strlgSindhi            = 'lgSindhi';
  strlgNorthernSami      = 'lgNorthernSami';
  strlgSango             = 'lgSango';
  strlgSinhalese         = 'lgSinhalese';
  strlgSlovak            = 'lgSlovak';
  strlgSlovenian         = 'lgSlovenian';
  strlgSamoan            = 'lgSamoan';
  strlgShona             = 'lgShona';
  strlgSomali            = 'lgSomali';
  strlgAlbanian          = 'lgAlbanian';
  strlgSerbian           = 'lgSerbian';
  strlgSwati             = 'lgSwati';
  strlgSothoSouthern     = 'lgSothoSouthern';
  strlgSundanese         = 'lgSundanese';
  strlgSwedish           = 'lgSwedish';
  strlgSwahili           = 'lgSwahili';
  strlgTamil             = 'lgTamil';
  strlgTelugu            = 'lgTelugu';
  strlgTajik             = 'lgTajik';
  strlgThai              = 'lgThai';
  strlgTigrinya          = 'lgTigrinya';
  strlgTurkmen           = 'lgTurkmen';
  strlgTagalog           = 'lgTagalog';
  strlgTswana            = 'lgTswana';
  strlgTonga             = 'lgTonga';
  strlgTurkish           = 'lgTurkish';
  strlgTsonga            = 'lgTsonga';
  strlgTatar             = 'lgTatar';
  strlgTwi               = 'lgTwi';
  strlgTahitian          = 'lgTahitian';
  strlgUighur            = 'lgUighur';
  strlgUkrainian         = 'lgUkrainian';
  strlgUrdu              = 'lgUrdu';
  strlgUzbek             = 'lgUzbek';
  strlgVenda             = 'lgVenda';
  strlgVietnamese        = 'lgVietnamese';
  strlgVolapuk           = 'lgVolapuk';
  strlgWalloon           = 'lgWalloon';
  strlgWolof             = 'lgWolof';
  strlgXhosa             = 'lgXhosa';
  strlgYiddish           = 'lgYiddish';
  strlgYoruba            = 'lgYoruba';
  strlgZhuang            = 'lgZhuang';
  strlgChinese           = 'lgChinese';
  strlgZulu              = 'lgZulu';

  strlgSpanishEurope     = 'lgSpanishEurope';
  strlgSpanishAmerica    = 'lgSpanishAmerica';
  strlgPortugueseEurope  = 'lgPortugueseEurope';
  strlgPortugueseBrasil  = 'lgPortugueseBrasil';
  strlgJapaneseAynu      = 'lgJapaneseAynu';

//  textlgNone          = 'None';
  textlgNeutral       = 'Neutral';

  textlgAfar          = 'Afar';
  textlgAbkhazian     = 'Abkhazian';
  textlgAvestan       = 'Avestan';
  textlgAfrikaans     = 'Afrikaans';
  textlgAkan          = 'Akan';
  textlgAmharic       = 'Amharic';
  textlgAragonese     = 'Aragonese';
  textlgArabic        = 'Arabic';
  textlgAssamese      = 'Assamese';
  textlgAvaric        = 'Avaric';
  textlgAymara        = 'Aymara';
  textlgAzerbaijani   = 'Azerbaijani';
  textlgBashkir       = 'Bashkir';
  textlgBelarusian    = 'Belarusian';
  textlgBulgarian     = 'Bulgarian';
  textlgBihari        = 'Bihari';
  textlgBislama       = 'Bislama';
  textlgBambara       = 'Bambara';
  textlgBengali       = 'Bengali';
  textlgTibetan       = 'Tibetan';
  textlgBreton        = 'Breton';
  textlgBosnian       = 'Bosnian';
  textlgCatalan       = 'Catalan';
  textlgChechen       = 'Chechen';
  textlgChamorro      = 'Chamorro';
  textlgCorsican      = 'Corsican';
  textlgCree          = 'Cree';
  textlgCzech         = 'Czech';
  textlgChuvash       = 'Chuvash';
  textlgWelsh         = 'Welsh';
  textlgDanish        = 'Danish';
  textlgGerman        = 'German';
  textlgDivehi        = 'Divehi';
  textlgDzongkha      = 'Dzongkha';
  textlgEwe           = 'Ewe';
  textlgGreek         = 'Greek';
  textlgEnglish       = 'English';
  textlgEnglishAmerican   = 'American English';
  textlgEnglishBritish    = 'British English';
  textlgEnglishAustralian = 'Australian English';
  textlgEsperanto         = 'Esperanto';
  textlgSpanish           = 'Spanish';
  textlgEstonian          = 'Estonian';
  textlgBasque            = 'Basque';
  textlgPersian           = 'Persian';
  textlgFulah             = 'Fulah';
  textlgFinnish           = 'Finnish';
  textlgFijian            = 'Fijian';
  textlgFaroese           = 'Faroese';
  textlgFrench            = 'French';
  textlgWalloonBelgique   = 'Walloon Belgique';
  textlgFrisian           = 'Frisian';
  textlgIrish             = 'Irish';
  textlgGaelic            = 'Gaelic';
  textlgGallegan          = 'Gallegan';
  textlgGuarani           = 'Guarani';
  textlgGujarati          = 'Gujarati';
  textlgManx              = 'Manx';
  textlgHausa             = 'Hausa';
  textlgHebrew            = 'Hebrew';
  textlgHindi             = 'Hindi';
  textlgHiriMotu          = 'HiriMotu';
  textlgCroatian          = 'Croatian';
  textlgHaitian           = 'Haitian';
  textlgHungarian         = 'Hungarian';
  textlgArmenian          = 'Armenian';
  textlgHerero            = 'Herero';
  textlgInterlingua       = 'Interlingua';
  textlgIndonesian        = 'Indonesian';
  textlgInterlingue       = 'Interlingue';
  textlgIgbo              = 'Igbo';
  textlgSichuanYi         = 'Sichuan Yi';
  textlgInupiaq           = 'Inupiaq';
  textlgIdo               = 'Ido';
  textlgIcelandic         = 'Icelandic';
  textlgItalian           = 'Italian';
  textlgInuktitut         = 'Inuktitut';
  textlgJapanese          = 'Japanese';
  textlgJavanese          = 'Javanese';
  textlgGeorgian          = 'Georgian';
  textlgKongo             = 'Kongo';
  textlgKikuyu            = 'Kikuyu';
  textlgKuanyama          = 'Kuanyama';
  textlgKazakh            = 'Kazakh';
  textlgGreenlandic       = 'Greenlandic';
  textlgKhmer             = 'Khmer';
  textlgKannada           = 'Kannada';
  textlgKorean            = 'Korean';
  textlgKanuri            = 'Kanuri';
  textlgKashmiri          = 'Kashmiri';
  textlgKurdish           = 'Kurdish';
  textlgCornish           = 'Cornish';
  textlgKomi              = 'Komi';
  textlgKirghiz           = 'Kirghiz';
  textlgLatin             = 'Latin';
  textlgLuxembourgish     = 'Luxembourgish';
  textlgGanda             = 'Ganda';
  textlgLimburgan         = 'Limburgan';
  textlgLingala           = 'Lingala';
  textlgLao               = 'Lao';
  textlgLithuanian        = 'Lithuanian';
  textlgLubaKatanga       = 'Luba Katanga';
  textlgLatvian           = 'Latvian';
  textlgMalagasy          = 'Malagasy';
  textlgMarshallese       = 'Marshallese';
  textlgMaori             = 'Maori';
  textlgMacedonian        = 'Macedonian';
  textlgMalayalam         = 'Malayalam';
  textlgMongolian         = 'Mongolian';
  textlgMoldavian         = 'Moldavian';
  textlgMarathi           = 'Marathi';
  textlgMalay             = 'Malay';
  textlgMaltese           = 'Maltese';
  textlgBurmese           = 'Burmese';
  textlgNauru             = 'Nauru';
  textlgNorwegianBokmaal  = 'Norwegian Bokmaal';
  textlgNdebeleNorth      = 'Ndebele North';
  textlgNepali            = 'Nepali';
  textlgNdonga            = 'Ndonga';
  textlgDutch             = 'Dutch';
  textlgFlemish           = 'Flemish';
  textlgNorwegianNynorsk  = 'Norwegian Nynorsk';
  textlgNorwegian         = 'Norwegian';
  textlgNdebeleSouth      = 'Ndebele South';
  textlgNavajo            = 'Navajo';
  textlgChichewa          = 'Chichewa';
  textlgOccitan           = 'Occitan';
  textlgOjibwa            = 'Ojibwa';
  textlgOromo             = 'Oromo';
  textlgOriya             = 'Oriya';
  textlgOssetian          = 'Ossetian';
  textlgPanjabi           = 'Panjabi';
  textlgPali              = 'Pali';
  textlgPolish            = 'Polish';
  textlgPushto            = 'Pushto';
  textlgPortuguese        = 'Portuguese';
  textlgQuechua           = 'Quechua';
  textlgRaetoRomance      = 'RaetoRomance';
  textlgRundi             = 'Rundi';
  textlgRomanian          = 'Romanian';
  textlgRussian           = 'Russian';
  textlgKinyarwanda       = 'Kinyarwanda';
  textlgSanskrit          = 'Sanskrit';
  textlgSardinian         = 'Sardinian';
  textlgSindhi            = 'Sindhi';
  textlgNorthernSami      = 'Northern Sami';
  textlgSango             = 'Sango';
  textlgSinhalese         = 'Sinhalese';
  textlgSlovak            = 'Slovak';
  textlgSlovenian         = 'Slovenian';
  textlgSamoan            = 'Samoan';
  textlgShona             = 'Shona';
  textlgSomali            = 'Somali';
  textlgAlbanian          = 'Albanian';
  textlgSerbian           = 'Serbian';
  textlgSwati             = 'Swati';
  textlgSothoSouthern     = 'Sotho Southern';
  textlgSundanese         = 'Sundanese';
  textlgSwedish           = 'Swedish';
  textlgSwahili           = 'Swahili';
  textlgTamil             = 'Tamil';
  textlgTelugu            = 'Telugu';
  textlgTajik             = 'Tajik';
  textlgThai              = 'Thai';
  textlgTigrinya          = 'Tigrinya';
  textlgTurkmen           = 'Turkmen';
  textlgTagalog           = 'Tagalog';
  textlgTswana            = 'Tswana';
  textlgTonga             = 'Tonga';
  textlgTurkish           = 'Turkish';
  textlgTsonga            = 'Tsonga';
  textlgTatar             = 'Tatar';
  textlgTwi               = 'Twi';
  textlgTahitian          = 'Tahitian';
  textlgUighur            = 'Uighur';
  textlgUkrainian         = 'Ukrainian';
  textlgUrdu              = 'Urdu';
  textlgUzbek             = 'Uzbek';
  textlgVenda             = 'Venda';
  textlgVietnamese        = 'Vietnamese';
  textlgVolapuk           = 'Volapuk';
  textlgWalloon           = 'Walloon';
  textlgWolof             = 'Wolof';
  textlgXhosa             = 'Xhosa';
  textlgYiddish           = 'Yiddish';
  textlgYoruba            = 'Yoruba';
  textlgZhuang            = 'Zhuang';
  textlgChinese           = 'Chinese';
  textlgZulu              = 'Zulu';

  textlgSpanishEurope     = 'Spanish Europe';
  textlgSpanishAmerica    = 'Spanish America';
  textlgPortugueseEurope  = 'Portuguese Europe';
  textlgPortugueseBrasil  = 'PortugueseBrasil';
  textlgJapaneseAynu      = 'JapaneseAynu';

(*
aa	Afar
ab	Abkhazian
ae	Avestan
af	Afrikaans
ak	Akan
am	Amharic
an	Aragonese
ar	Arabic
as	Assamese
av	Avaric
ay	Aymara
az	Azerbaijani
ba	Bashkir
be	Belarusian
bg	Bulgarian
bh	Bihari
bi	Bislama
bm	Bambara
bn	Bengali
bo	Tibetan
br	Breton
bs	Bosnian
ca	Catalan
ce	Chechen 
ch	Chamorro
co	Corsican
cr	Cree
cs	Czech
cv	Chuvash
cy	Welsh
da	Danish 
de	German
dv	Divehi
dz	Dzongkha
ee	Ewe
el	Greek
en	English
en_US	American English
en_GB	British English
en_AU	Australian English
eo	Esperanto
es	Spanish
et	Estonian
eu	Basque
fa	Persian
ff	Fulah
fi	Finnish
fj	Fijian
fo	Faroese
fr	French
fr_BE	WalloonBelgique
fy	Frisian
ga	Irish
gd	Gaelic
gl	Gallegan
gn	Guarani
gu	Gujarati
gv	Manx
ha	Hausa
he	Hebrew
hi	Hindi
ho	Hiri Motu
hr	Croatian
ht	Haitian
hu	Hungarian
hy	Armenian
hz	Herero
ia	Interlingua
id	Indonesian
ie	Interlingue
ig	Igbo
ii	Sichuan Yi
ik	Inupiaq
io	Ido
is	Icelandic
it	Italian
iu	Inuktitut
ja	Japanese
jv	Javanese
ka	Georgian
kg	Kongo
ki	Kikuyu
kj	Kuanyama
kk	Kazakh
kl	Greenlandic
km	Khmer
kn	Kannada
ko	Korean
kr	Kanuri
ks	Kashmiri
ku	Kurdish
kw	Cornish
kv	Komi
ky	Kirghiz
la	Latin
lb	Luxembourgish
lg	Ganda
li	Limburgan
ln	Lingala
lo	Lao
lt	Lithuanian
lu	Luba-Katanga
lv	Latvian
mg	Malagasy
mh	Marshallese
mi	Maori
mk	Macedonian
ml	Malayalam
mn	Mongolian
mo	Moldavian
mr	Marathi
ms	Malay
mt	Maltese
my	Burmese
na	Nauru
nb	Norwegian Bokmaal
nd	Ndebele, North
ne	Nepali
ng	Ndonga
nl	Dutch
nl_BE	Flemish
nn	Norwegian Nynorsk
no	Norwegian
nr	Ndebele, South
nv	Navajo
ny	Chichewa
oc	Occitan
oj	Ojibwa
om	Oromo
or	Oriya
os	Ossetian
pa	Panjabi
pi	Pali
pl	Polish 
ps	Pushto
pt	Portuguese 
qu	Quechua
rm	Raeto-Romance
rn	Rundi
ro	Romanian
ru	Russian
rw	Kinyarwanda
sa	Sanskrit
sc	Sardinian
sd	Sindhi
se	Northern Sami
sg	Sango
si	Sinhalese
sk	Slovak
sl	Slovenian
sm	Samoan 
sn	Shona
so	Somali
sq	Albanian
sr	Serbian
ss	Swati
st	Sotho, Southern 
su	Sundanese 
sv	Swedish
sw	Swahili
ta	Tamil
te	Telugu
tg	Tajik
th	Thai
ti	Tigrinya
tk	Turkmen
tl	Tagalog
tn	Tswana
to	Tonga
tr	Turkish
ts	Tsonga
tt	Tatar 
tw	Twi
ty	Tahitian
ug	Uighur
uk	Ukrainian
ur	Urdu
uz	Uzbek
ve	Venda
vi	Vietnamese
vo	Volapuk
wa	Walloon
wo	Wolof
xh	Xhosa
yi	Yiddish
yo	Yoruba
za	Zhuang
zh	Chinese
zu	Zulu
*)

implementation

end.
