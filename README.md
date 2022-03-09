# PanVis - Pandemie Visualisierung
## Gruppe
Korbinian Meier
Leonard Ganz
Pierre Franke

## Beschreibung
In diesem Projekt sollen Daten zur andauernden Pandemie visualisiert werden. Auf einer
Deutschlandkarte werden Regionen in verschiedenen Hierarchiestufen (Bundesländer, Landkreise)
dargestellt. Für diese werden Inzidenzwerte z.B. durch Einfärbung im Heatmap-Stil
veranschaulicht. Zur Inspiration der Darstellung dienen die [Übersichtskarten von 23 degrees
GmbH](https://app.23degrees.io/indicator/covid?folder9hfAcvpFw0IxDKRZ-collection-deutschland=QixrVWHzvvuvLN03-collection-statistiken-zu-neuinfektionen~hnO48q9rT7hQwl5F-choro-7-tage-inzidenz-neuinfektionen), insbesondere [diese](https://app.23degrees.io/embed/3T3vqcZBX8hrLeYT-choro-sars-cov-2-infektionen-in), welche auch in der Tagesschau verwendet werden, und das [RKI Dashboard](https://experience.arcgis.com/experience/478220a4c454480e823b17327b2bf1d4).

Neben den aktuellen Inzidenzen soll die historische Entwicklung der Fallzahlen sichtbar sein.
Weitere Statistiken sind denkbar.

## Umsetzung
Die Daten sollen aus den öffentlich zur Verfügung stehenden [REST-API](https://arcgis.esri.de/nutzung-der-api-des-rki-covid-19-dashboard/)s entnommen werden,
welche von Esri Deutschland im Auftrag des Robert Koch-Instituts betrieben werden. Zur
Verarbeitung der Datensätze in einem bestimmten JSON-Format und Überführung in eigene
Datenstrukturen soll **Template Haskell** eingesetzt werden. Für die Verwaltung und den Zugriff auf
die Datenstruktur soll mit **funktionalen Referenzen / Linsen** gearbeitet werden. Die Entwicklung
der Datenvisualisierung in einer grafischen Benutzeroberfläche deckt das Thema
**Anwendungsprogrammierung** ab. Dieser Bereich wird durch die eingangs genannte
**Web**komponente ergänzt.

## Setup

install hpack with "stack install hpack"

use "hpack" to update panvis.cabal based on changes in package.yaml (executed automatically by stack build)

if missing depepndencies: "stack install"

build with "stack build"

run with "stack exec panvis-exe"