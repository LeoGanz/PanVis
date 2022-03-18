{-# LANGUAGE OverloadedStrings #-}

module DistrictPaths where

import qualified Data.ByteString.Char8 as C
import qualified Data.Map as Map
import qualified Text.XML as XML
import System.IO.Temp

import XMLTemplate


test = writeSVGFile exampleList "images/germany_counties_test.svg"


exampleHead = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<svg version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\" x=\"0px\" y=\"0px\" width=\"506.3px\" height=\"600px\" viewBox=\"0 0 732.4 1023\">\n<title>panvis</title>\n<defs>\n <linearGradient x1=\"402\" y1=\"970\" x2=\"702\" y2=\"970\" gradientUnits=\"userSpaceOnUse\" id=\"lGr\">\n  <stop offset=\"0\" style=\"stop-color:#fff9f3\"/>\n  <stop offset=\"0.166\" style=\"stop-color:#feebe2\"/>\n  <stop offset=\"0.333\" style=\"stop-color:#fa9fb5\"/>\n  <stop offset=\"0.5\" style=\"stop-color:#dd3497\"/>\n  <stop offset=\"0.666\" style=\"stop-color:#ae017e\"/>\n  <stop offset=\"0.833\" style=\"stop-color:#7a0177\"/>\n  <stop offset=\"1\" style=\"stop-color:#000\"/>\n </linearGradient>\n</defs>\n<style type=\"text/css\">\ntext {font-family:monospace,Courier New,Liberation Mono;font-size:14px}\npath {stroke:#fff;stroke-width:0.3}\npath:hover {stroke-width:1.5}\n</style>\n<g id=\"legend\">\n <text x=\"611\" y=\"960\">1 / 100 000</text>\n <rect style=\"fill:url(#lGr);fill-opacity:1\" height=\"20\" width=\"300\" x=\"402\" y=\"970\"/>\n <path d=\"m402,990v6h2v-4h47.7v4h2v-4h47.6v4h2v-4H551v4h2v-4h47.6v4h2v-4h47.7v4h2v-4H700v4h2v-6z\" style=\"fill:#000000;stroke-width:0\"/>\n <text x=\"390\" y=\"1010\">>0</text>\n <text x=\"435.2\" y=\"1010\">4000</text>\n <text x=\"485\" y=\"1010\">6000</text>\n <text x=\"531\" y=\"1010\">10000</text>\n <text x=\"581\" y=\"1010\">15000</text>\n <text x=\"629\" y=\"1010\">22000</text>\n <text x=\"678.5\" y=\"1010\">≥30000</text>\n</g>\n"
exampleTail = "</svg>"
exampleList :: [(C.ByteString, Int)]
exampleList = [
     ("LK Stadtverband Saarbrücken", 1)
    ,("LK Merzig-Wadern", 2)
    ,("LK Neunkirchen", 3)
    ,("LK Saarlouis", 4)
    ,("LK Saarpfalz-Kreis", 5)
    ,("LK Sankt Wendel", 6)
    ,("SK Berlin Mitte", 7)
    ,("SK Berlin Friedrichshain-Kreuzberg", 8)
    ,("SK Berlin Pankow", 9)
    ,("SK Berlin Charlottenburg-Wilmersdorf", 10)
    ,("SK Berlin Spandau", 11)
    ,("SK Berlin Steglitz-Zehlendorf", 12)
    ,("SK Berlin Tempelhof-Schöneberg", 13)
    ,("SK Berlin Neukölln", 14)
    ,("SK Berlin Treptow-Köpenick", 15)
    ,("SK Berlin Marzahn-Hellersdorf", 16)
    ,("SK Berlin Lichtenberg", 17)
    ,("SK Berlin Reinickendorf", 18)
    ,("SK Brandenburg a.d.Havel", 19)
    ,("SK Cottbus", 20)
    ,("SK Frankfurt (Oder)", 21)
    ,("SK Potsdam", 22)
    ,("LK Barnim", 23)
    ,("LK Dahme-Spreewald", 24)
    ,("LK Elbe-Elster", 25)
    ,("LK Havelland", 26)
    ,("LK Märkisch-Oderland", 27)
    ,("LK Oberhavel", 28)
    ,("LK Oberspreewald-Lausitz", 29)
    ,("LK Oder-Spree", 30)
    ,("LK Ostprignitz-Ruppin", 31)
    ,("LK Potsdam-Mittelmark", 32)
    ,("LK Prignitz", 33)
    ,("LK Spree-Neiße", 34)
    ,("LK Teltow-Fläming", 35)
    ,("LK Uckermark", 36)
    ,("SK Rostock", 37)
    ,("SK Schwerin", 38)
    ,("LK Mecklenburgische Seenplatte", 39)
    ,("LK Rostock", 40)
    ,("LK Vorpommern-Rügen", 41)
    ,("LK Nordwestmecklenburg", 42)
    ,("LK Vorpommern-Greifswald", 43)
    ,("LK Ludwigslust-Parchim", 44)
    ,("SK Chemnitz", 45)
    ,("LK Erzgebirgskreis", 46)
    ,("LK Mittelsachsen", 47)
    ,("LK Vogtlandkreis", 48)
    ,("LK Zwickau", 49)
    ,("SK Dresden", 50)
    ,("LK Bautzen", 51)
    ,("LK Görlitz", 52)
    ,("LK Meißen", 53)
    ,("LK Sächsische Schweiz-Osterzgebirge", 54)
    ,("SK Leipzig", 55)
    ,("LK Leipzig", 56)
    ,("LK Nordsachsen", 57)
    ,("SK Dessau-Roßlau", 58)
    ,("SK Halle", 59)
    ,("SK Magdeburg", 60)
    ,("LK Altmarkkreis Salzwedel", 61)
    ,("LK Anhalt-Bitterfeld", 62)
    ,("LK Börde", 63)
    ,("LK Burgenlandkreis", 64)
    ,("LK Harz", 65)
    ,("LK Jerichower Land", 66)
    ,("LK Mansfeld-Südharz", 67)
    ,("LK Saalekreis", 68)
    ,("LK Salzlandkreis", 69)
    ,("LK Stendal", 70)
    ,("LK Wittenberg", 71)
    ,("SK Erfurt", 72)
    ,("SK Gera", 73)
    ,("SK Jena", 74)
    ,("SK Suhl", 75)
    ,("SK Weimar", 76)
    ,("LK Eichsfeld", 77)
    ,("LK Nordhausen", 78)
    ,("LK Wartburgkreis", 79)
    ,("LK Unstrut-Hainich-Kreis", 80)
    ,("LK Kyffhäuserkreis", 81)
    ,("LK Schmalkalden-Meiningen", 82)
    ,("LK Gotha", 83)
    ,("LK Sömmerda", 84)
    ,("LK Hildburghausen", 85)
    ,("LK Ilm-Kreis", 86)
    ,("LK Weimarer Land", 87)
    ,("LK Sonneberg", 88)
    ,("LK Saalfeld-Rudolstadt", 89)
    ,("LK Saale-Holzland-Kreis", 90)
    ,("LK Saale-Orla-Kreis", 91)
    ,("LK Greiz", 92)
    ,("LK Altenburger Land", 93)
    ,("SK Flensburg", 94)
    ,("SK Kiel", 95)
    ,("SK Lübeck", 96)
    ,("SK Neumünster", 97)
    ,("LK Dithmarschen", 98)
    ,("LK Herzogtum Lauenburg", 99)
    ,("LK Nordfriesland", 100)
    ,("LK Ostholstein", 101)
    ,("LK Pinneberg", 102)
    ,("LK Plön", 103)
    ,("LK Rendsburg-Eckernförde", 104)
    ,("LK Schleswig-Flensburg", 105)
    ,("LK Segeberg", 106)
    ,("LK Steinburg", 107)
    ,("LK Stormarn", 108)
    ,("SK Hamburg", 109)
    ,("SK Braunschweig", 110)
    ,("SK Salzgitter", 111)
    ,("SK Wolfsburg", 112)
    ,("LK Gifhorn", 113)
    ,("LK Goslar", 114)
    ,("LK Helmstedt", 115)
    ,("LK Northeim", 116)
    ,("LK Peine", 117)
    ,("LK Wolfenbüttel", 118)
    ,("LK Göttingen", 119)
    ,("Region Hannover", 120)
    ,("LK Diepholz", 121)
    ,("LK Hameln-Pyrmont", 122)
    ,("LK Hildesheim", 123)
    ,("LK Holzminden", 124)
    ,("LK Nienburg (Weser)", 125)
    ,("LK Schaumburg", 126)
    ,("LK Celle", 127)
    ,("LK Cuxhaven", 128)
    ,("LK Harburg", 129)
    ,("LK Lüchow-Dannenberg", 130)
    ,("LK Lüneburg", 131)
    ,("LK Osterholz", 132)
    ,("LK Rotenburg (Wümme)", 133)
    ,("LK Heidekreis", 134)
    ,("LK Stade", 135)
    ,("LK Uelzen", 136)
    ,("LK Verden", 137)
    ,("SK Delmenhorst", 138)
    ,("SK Emden", 139)
    ,("SK Oldenburg", 140)
    ,("SK Osnabrück", 141)
    ,("SK Wilhelmshaven", 142)
    ,("LK Ammerland", 143)
    ,("LK Aurich", 144)
    ,("LK Cloppenburg", 145)
    ,("LK Emsland", 146)
    ,("LK Friesland", 147)
    ,("LK Grafschaft Bentheim", 148)
    ,("LK Leer", 149)
    ,("LK Oldenburg", 150)
    ,("LK Osnabrück", 151)
    ,("LK Vechta", 152)
    ,("LK Wesermarsch", 153)
    ,("LK Wittmund", 154)
    ,("SK Bremen", 155)
    ,("SK Bremerhaven", 156)
    ,("SK Düsseldorf", 157)
    ,("SK Duisburg", 158)
    ,("SK Essen", 159)
    ,("SK Krefeld", 160)
    ,("SK Mönchengladbach", 161)
    ,("SK Mülheim a.d.Ruhr", 162)
    ,("SK Oberhausen", 163)
    ,("SK Remscheid", 164)
    ,("SK Solingen", 165)
    ,("SK Wuppertal", 166)
    ,("LK Kleve", 167)
    ,("LK Mettmann", 168)
    ,("LK Rhein-Kreis Neuss", 169)
    ,("LK Viersen", 170)
    ,("LK Wesel", 171)
    ,("SK Bonn", 172)
    ,("SK Köln", 173)
    ,("SK Leverkusen", 174)
    ,("StädteRegion Aachen", 175)
    ,("LK Düren", 176)
    ,("LK Rhein-Erft-Kreis", 177)
    ,("LK Euskirchen", 178)
    ,("LK Heinsberg", 179)
    ,("LK Oberbergischer Kreis", 180)
    ,("LK Rheinisch-Bergischer Kreis", 181)
    ,("LK Rhein-Sieg-Kreis", 182)
    ,("SK Bottrop", 183)
    ,("SK Gelsenkirchen", 184)
    ,("SK Münster", 185)
    ,("LK Borken", 186)
    ,("LK Coesfeld", 187)
    ,("LK Recklinghausen", 188)
    ,("LK Steinfurt", 189)
    ,("LK Warendorf", 190)
    ,("SK Bielefeld", 191)
    ,("LK Gütersloh", 192)
    ,("LK Herford", 193)
    ,("LK Höxter", 194)
    ,("LK Lippe", 195)
    ,("LK Minden-Lübbecke", 196)
    ,("LK Paderborn", 197)
    ,("SK Bochum", 198)
    ,("SK Dortmund", 199)
    ,("SK Hagen", 200)
    ,("SK Hamm", 201)
    ,("SK Herne", 202)
    ,("LK Ennepe-Ruhr-Kreis", 203)
    ,("LK Hochsauerlandkreis", 204)
    ,("LK Märkischer Kreis", 205)
    ,("LK Olpe", 206)
    ,("LK Siegen-Wittgenstein", 207)
    ,("LK Soest", 208)
    ,("LK Unna", 209)
    ,("SK Darmstadt", 210)
    ,("SK Frankfurt am Main", 211)
    ,("SK Offenbach", 212)
    ,("SK Wiesbaden", 213)
    ,("LK Bergstraße", 214)
    ,("LK Darmstadt-Dieburg", 215)
    ,("LK Groß-Gerau", 216)
    ,("LK Hochtaunuskreis", 217)
    ,("LK Main-Kinzig-Kreis", 218)
    ,("LK Main-Taunus-Kreis", 219)
    ,("LK Odenwaldkreis", 220)
    ,("LK Offenbach", 221)
    ,("LK Rheingau-Taunus-Kreis", 222)
    ,("LK Wetteraukreis", 223)
    ,("LK Gießen", 224)
    ,("LK Lahn-Dill-Kreis", 225)
    ,("LK Limburg-Weilburg", 226)
    ,("LK Marburg-Biedenkopf", 227)
    ,("LK Vogelsbergkreis", 228)
    ,("SK Kassel", 229)
    ,("LK Fulda", 230)
    ,("LK Hersfeld-Rotenburg", 231)
    ,("LK Kassel", 232)
    ,("LK Schwalm-Eder-Kreis", 233)
    ,("LK Waldeck-Frankenberg", 234)
    ,("LK Werra-Meißner-Kreis", 235)
    ,("SK Koblenz", 236)
    ,("LK Ahrweiler", 237)
    ,("LK Altenkirchen", 238)
    ,("LK Bad Kreuznach", 239)
    ,("LK Birkenfeld", 240)
    ,("LK Cochem-Zell", 241)
    ,("LK Mayen-Koblenz", 242)
    ,("LK Neuwied", 243)
    ,("LK Rhein-Hunsrück-Kreis", 244)
    ,("LK Rhein-Lahn-Kreis", 245)
    ,("LK Westerwaldkreis", 246)
    ,("SK Trier", 247)
    ,("LK Bernkastel-Wittlich", 248)
    ,("LK Bitburg-Prüm", 249)
    ,("LK Vulkaneifel", 250)
    ,("LK Trier-Saarburg", 251)
    ,("SK Frankenthal", 252)
    ,("SK Kaiserslautern", 253)
    ,("SK Landau i.d.Pfalz", 254)
    ,("SK Ludwigshafen", 255)
    ,("SK Mainz", 256)
    ,("SK Neustadt a.d.Weinstraße", 257)
    ,("SK Pirmasens", 258)
    ,("SK Speyer", 259)
    ,("SK Worms", 260)
    ,("SK Zweibrücken", 261)
    ,("LK Alzey-Worms", 262)
    ,("LK Bad Dürkheim", 263)
    ,("LK Donnersbergkreis", 264)
    ,("LK Germersheim", 265)
    ,("LK Kaiserslautern", 266)
    ,("LK Kusel", 267)
    ,("LK Südliche Weinstraße", 268)
    ,("LK Rhein-Pfalz-Kreis", 269)
    ,("LK Mainz-Bingen", 270)
    ,("LK Südwestpfalz", 271)
    ,("SK Stuttgart", 272)
    ,("LK Böblingen", 273)
    ,("LK Esslingen", 274)
    ,("LK Göppingen", 275)
    ,("LK Ludwigsburg", 276)
    ,("LK Rems-Murr-Kreis", 277)
    ,("SK Heilbronn", 278)
    ,("LK Heilbronn", 279)
    ,("LK Hohenlohekreis", 280)
    ,("LK Schwäbisch Hall", 281)
    ,("LK Main-Tauber-Kreis", 282)
    ,("LK Heidenheim", 283)
    ,("LK Ostalbkreis", 284)
    ,("SK Baden-Baden", 285)
    ,("SK Karlsruhe", 286)
    ,("LK Karlsruhe", 287)
    ,("LK Rastatt", 288)
    ,("SK Heidelberg", 289)
    ,("SK Mannheim", 290)
    ,("LK Neckar-Odenwald-Kreis", 291)
    ,("LK Rhein-Neckar-Kreis", 292)
    ,("SK Pforzheim", 293)
    ,("LK Calw", 294)
    ,("LK Enzkreis", 295)
    ,("LK Freudenstadt", 296)
    ,("SK Freiburg i.Breisgau", 297)
    ,("LK Breisgau-Hochschwarzwald", 298)
    ,("LK Emmendingen", 299)
    ,("LK Ortenaukreis", 300)
    ,("LK Rottweil", 301)
    ,("LK Schwarzwald-Baar-Kreis", 302)
    ,("LK Tuttlingen", 303)
    ,("LK Konstanz", 304)
    ,("LK Lörrach", 305)
    ,("LK Waldshut", 306)
    ,("LK Reutlingen", 307)
    ,("LK Tübingen", 308)
    ,("LK Zollernalbkreis", 309)
    ,("SK Ulm", 310)
    ,("LK Alb-Donau-Kreis", 311)
    ,("LK Biberach", 312)
    ,("LK Bodenseekreis", 313)
    ,("LK Ravensburg", 314)
    ,("LK Sigmaringen", 315)
    ,("SK Ingolstadt", 316)
    ,("SK München", 317)
    ,("SK Rosenheim", 318)
    ,("LK Altötting", 319)
    ,("LK Berchtesgadener Land", 320)
    ,("LK Bad Tölz-Wolfratshausen", 321)
    ,("LK Dachau", 322)
    ,("LK Ebersberg", 323)
    ,("LK Eichstätt", 324)
    ,("LK Erding", 325)
    ,("LK Freising", 326)
    ,("LK Fürstenfeldbruck", 327)
    ,("LK Garmisch-Partenkirchen", 328)
    ,("LK Landsberg a.Lech", 329)
    ,("LK Miesbach", 330)
    ,("LK Mühldorf a.Inn", 331)
    ,("LK München", 332)
    ,("LK Neuburg-Schrobenhausen", 333)
    ,("LK Pfaffenhofen a.d.Ilm", 334)
    ,("LK Rosenheim", 335)
    ,("LK Starnberg", 336)
    ,("LK Traunstein", 337)
    ,("LK Weilheim-Schongau", 338)
    ,("SK Landshut", 339)
    ,("SK Passau", 340)
    ,("SK Straubing", 341)
    ,("LK Deggendorf", 342)
    ,("LK Freyung-Grafenau", 343)
    ,("LK Kelheim", 344)
    ,("LK Landshut", 345)
    ,("LK Passau", 346)
    ,("LK Regen", 347)
    ,("LK Rottal-Inn", 348)
    ,("LK Straubing-Bogen", 349)
    ,("LK Dingolfing-Landau", 350)
    ,("SK Amberg", 351)
    ,("SK Regensburg", 352)
    ,("SK Weiden i.d.OPf.", 353)
    ,("LK Amberg-Sulzbach", 354)
    ,("LK Cham", 355)
    ,("LK Neumarkt i.d.OPf.", 356)
    ,("LK Neustadt a.d.Waldnaab", 357)
    ,("LK Regensburg", 358)
    ,("LK Schwandorf", 359)
    ,("LK Tirschenreuth", 360)
    ,("SK Bamberg", 361)
    ,("SK Bayreuth", 362)
    ,("SK Coburg", 363)
    ,("SK Hof", 364)
    ,("LK Bamberg", 365)
    ,("LK Bayreuth", 366)
    ,("LK Coburg", 367)
    ,("LK Forchheim", 368)
    ,("LK Hof", 369)
    ,("LK Kronach", 370)
    ,("LK Kulmbach", 371)
    ,("LK Lichtenfels", 372)
    ,("LK Wunsiedel i.Fichtelgebirge", 373)
    ,("SK Ansbach", 374)
    ,("SK Erlangen", 375)
    ,("SK Fürth", 376)
    ,("SK Nürnberg", 377)
    ,("SK Schwabach", 378)
    ,("LK Ansbach", 379)
    ,("LK Erlangen-Höchstadt", 380)
    ,("LK Fürth", 381)
    ,("LK Nürnberger Land", 382)
    ,("LK Neustadt a.d.Aisch-Bad Windsheim", 383)
    ,("LK Roth", 384)
    ,("LK Weißenburg-Gunzenhausen", 385)
    ,("SK Aschaffenburg", 386)
    ,("SK Schweinfurt", 387)
    ,("SK Würzburg", 388)
    ,("LK Aschaffenburg", 389)
    ,("LK Bad Kissingen", 390)
    ,("LK Rhön-Grabfeld", 391)
    ,("LK Haßberge", 392)
    ,("LK Kitzingen", 393)
    ,("LK Miltenberg", 394)
    ,("LK Main-Spessart", 395)
    ,("LK Schweinfurt", 396)
    ,("LK Würzburg", 397)
    ,("SK Augsburg", 398)
    ,("SK Kaufbeuren", 399)
    ,("SK Kempten", 400)
    ,("SK Memmingen", 401)
    ,("LK Aichach-Friedberg", 402)
    ,("LK Augsburg", 403)
    ,("LK Dillingen a.d.Donau", 404)
    ,("LK Günzburg", 405)
    ,("LK Neu-Ulm", 406)
    ,("LK Lindau", 407)
    ,("LK Ostallgäu", 408)
    ,("LK Unterallgäu", 409)
    ,("LK Donau-Ries", 410)
    ,("LK Oberallgäu", 411)]


writeSVGFile :: [(C.ByteString, Int)] -> FilePath -> IO ()
writeSVGFile districtValList filePath =
    XML.writeFile XML.def filePath $ XML.Document (XML.Prologue [] Nothing []) root []
    where
        root = XML.Element "svg" (Map.fromList svgAttributes) $ generateXMLCode districtValList colorGradient
        svgAttributes = [
            ("version","1.1")
            ,("xmlns", "http://www.w3.org/2000/svg")
            ,("x", "0px")
            ,("y", "0px")
            ,("width", "506.3px")
            ,("height", "600px")
            ,("viewBox", "0 0 732.4 1023")]