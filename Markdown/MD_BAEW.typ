// Some definitions presupposed by pandoc's typst output.
#let blockquote(body) = [
  #set text( size: 0.92em )
  #block(inset: (left: 1.5em, top: 0.2em, bottom: 0.2em))[#body]
]

#let horizontalrule = [
  #line(start: (25%,0%), end: (75%,0%))
]

#let endnote(num, contents) = [
  #stack(dir: ltr, spacing: 3pt, super[#num], contents)
]

#show terms: it => {
  it.children
    .map(child => [
      #strong[#child.term]
      #block(inset: (left: 1.5em, top: -0.4em))[#child.description]
      ])
    .join()
}

// Some quarto-specific definitions.

#show raw.where(block: true): set block(
    fill: luma(230),
    width: 100%,
    inset: 8pt,
    radius: 2pt
  )

#let block_with_new_content(old_block, new_content) = {
  let d = (:)
  let fields = old_block.fields()
  fields.remove("body")
  if fields.at("below", default: none) != none {
    // TODO: this is a hack because below is a "synthesized element"
    // according to the experts in the typst discord...
    fields.below = fields.below.amount
  }
  return block.with(..fields)(new_content)
}

#let empty(v) = {
  if type(v) == "string" {
    // two dollar signs here because we're technically inside
    // a Pandoc template :grimace:
    v.matches(regex("^\\s*$")).at(0, default: none) != none
  } else if type(v) == "content" {
    if v.at("text", default: none) != none {
      return empty(v.text)
    }
    for child in v.at("children", default: ()) {
      if not empty(child) {
        return false
      }
    }
    return true
  }

}

// Subfloats
// This is a technique that we adapted from https://github.com/tingerrr/subpar/
#let quartosubfloatcounter = counter("quartosubfloatcounter")

#let quarto_super(
  kind: str,
  caption: none,
  label: none,
  supplement: str,
  position: none,
  subrefnumbering: "1a",
  subcapnumbering: "(a)",
  body,
) = {
  context {
    let figcounter = counter(figure.where(kind: kind))
    let n-super = figcounter.get().first() + 1
    set figure.caption(position: position)
    [#figure(
      kind: kind,
      supplement: supplement,
      caption: caption,
      {
        show figure.where(kind: kind): set figure(numbering: _ => numbering(subrefnumbering, n-super, quartosubfloatcounter.get().first() + 1))
        show figure.where(kind: kind): set figure.caption(position: position)

        show figure: it => {
          let num = numbering(subcapnumbering, n-super, quartosubfloatcounter.get().first() + 1)
          show figure.caption: it => {
            num.slice(2) // I don't understand why the numbering contains output that it really shouldn't, but this fixes it shrug?
            [ ]
            it.body
          }

          quartosubfloatcounter.step()
          it
          counter(figure.where(kind: it.kind)).update(n => n - 1)
        }

        quartosubfloatcounter.update(0)
        body
      }
    )#label]
  }
}

// callout rendering
// this is a figure show rule because callouts are crossreferenceable
#show figure: it => {
  if type(it.kind) != "string" {
    return it
  }
  let kind_match = it.kind.matches(regex("^quarto-callout-(.*)")).at(0, default: none)
  if kind_match == none {
    return it
  }
  let kind = kind_match.captures.at(0, default: "other")
  kind = upper(kind.first()) + kind.slice(1)
  // now we pull apart the callout and reassemble it with the crossref name and counter

  // when we cleanup pandoc's emitted code to avoid spaces this will have to change
  let old_callout = it.body.children.at(1).body.children.at(1)
  let old_title_block = old_callout.body.children.at(0)
  let old_title = old_title_block.body.body.children.at(2)

  // TODO use custom separator if available
  let new_title = if empty(old_title) {
    [#kind #it.counter.display()]
  } else {
    [#kind #it.counter.display(): #old_title]
  }

  let new_title_block = block_with_new_content(
    old_title_block, 
    block_with_new_content(
      old_title_block.body, 
      old_title_block.body.body.children.at(0) +
      old_title_block.body.body.children.at(1) +
      new_title))

  block_with_new_content(old_callout,
    block(below: 0pt, new_title_block) +
    old_callout.body.children.at(1))
}

// 2023-10-09: #fa-icon("fa-info") is not working, so we'll eval "#fa-info()" instead
#let callout(body: [], title: "Callout", background_color: rgb("#dddddd"), icon: none, icon_color: black) = {
  block(
    breakable: false, 
    fill: background_color, 
    stroke: (paint: icon_color, thickness: 0.5pt, cap: "round"), 
    width: 100%, 
    radius: 2pt,
    block(
      inset: 1pt,
      width: 100%, 
      below: 0pt, 
      block(
        fill: background_color, 
        width: 100%, 
        inset: 8pt)[#text(icon_color, weight: 900)[#icon] #title]) +
      if(body != []){
        block(
          inset: 1pt, 
          width: 100%, 
          block(fill: white, width: 100%, inset: 8pt, body))
      }
    )
}

//#assert(sys.version.at(1) >= 11 or sys.version.at(0) > 0, message: "This template requires Typst Version 0.11.0 or higher. The version of Quarto you are using uses Typst version is " + str(sys.version.at(0)) + "." + str(sys.version.at(1)) + "." + str(sys.version.at(2)) + ". You will need to upgrade to Quarto 1.5 or higher to use apaquarto-typst.")

// counts how many appendixes there are
#let appendixcounter = counter("appendix")
// make latex logo
// https://github.com/typst/typst/discussions/1732#discussioncomment-6566999
#let TeX = style(styles => {
  set text(font: ("New Computer Modern", "Times", "Times New Roman"))
  let e = measure("E", styles)
  let T = "T"
  let E = text(1em, baseline: e.height * 0.31, "E")
  let X = "X"
  box(T + h(-0.15em) + E + h(-0.125em) + X)
})
#let LaTeX = style(styles => {
  set text(font: ("New Computer Modern", "Times", "Times New Roman"))
  let a-size = 0.66em
  let l = measure("L", styles)
  let a = measure(text(a-size, "A"), styles)
  let L = "L"
  let A = box(scale(x: 105%, text(a-size, baseline: a.height - l.height, "A")))
  box(L + h(-a.width * 0.67) + A + h(-a.width * 0.25) + TeX)
})

#let firstlineindent=0.5in

// documentmode: man
#let man(
  title: none,
  runninghead: none,
  margin: (x: 1in, y: 1in),
  paper: "us-letter",
  font: ("Times", "Times New Roman"),
  fontsize: 12pt,
  leading: 18pt,
  spacing: 18pt,
  firstlineindent: 0.5in,
  toc: false,
  lang: "en",
  cols: 1,
  doc,
) = {

  set page(
    paper: paper,
    margin: margin,
    header-ascent: 50%,
    header: grid(
      columns: (9fr, 1fr),
      align(left)[#upper[#runninghead]],
      align(right)[#counter(page).display()]
    )
  )


 
if sys.version.at(1) >= 11 or sys.version.at(0) > 0 {
  set table(    
    stroke: (x, y) => (
        top: if y <= 1 { 0.5pt } else { 0pt },
        bottom: .5pt,
      )
  )
}
  set par(
    justify: false, 
    leading: leading,
    first-line-indent: firstlineindent
  )

  // Also "leading" space between paragraphs
  set block(spacing: spacing, above: spacing, below: spacing)

  set text(
    font: font,
    size: fontsize,
    lang: lang
  )

  show link: set text(blue)

  show quote: set pad(x: 0.5in)
  show quote: set par(leading: leading)
  show quote: set block(spacing: spacing, above: spacing, below: spacing)
  // show LaTeX
  show "TeX": TeX
  show "LaTeX": LaTeX

  // format figure captions
  show figure.where(kind: "quarto-float-fig"): it => [
    #if int(appendixcounter.display().at(0)) > 0 [
      #heading(level: 2, outlined: false)[#it.supplement #appendixcounter.display("A")#it.counter.display()]
    ] else [
      #heading(level: 2, outlined: false)[#it.supplement #it.counter.display()]
    ]
    #par[#emph[#it.caption.body]]
    #align(center)[#it.body]
  ]
  
  // format table captions
  show figure.where(kind: "quarto-float-tbl"): it => [
    #if int(appendixcounter.display().at(0)) > 0 [
      #heading(level: 2, outlined: false)[#it.supplement #appendixcounter.display("A")#it.counter.display()]
    ] else [
      #heading(level: 2, outlined: false)[#it.supplement #it.counter.display()]
    ]
    #par[#emph[#it.caption.body]]
    #block[#it.body]
  ]

 // Redefine headings up to level 5 
  show heading.where(
    level: 1
  ): it => block(width: 100%, below: leading, above: leading)[
    #set align(center)
    #set text(size: fontsize)
    #it.body
  ]
  
  show heading.where(
    level: 2
  ): it => block(width: 100%, below: leading, above: leading)[
    #set align(left)
    #set text(size: fontsize)
    #it.body
  ]
  
  show heading.where(
    level: 3
  ): it => block(width: 100%, below: leading, above: leading)[
    #set align(left)
    #set text(size: fontsize, style: "italic")
    #it.body
  ]

  show heading.where(
    level: 4
  ): it => text(
    size: 1em,
    weight: "bold",
    it.body
  )

  show heading.where(
    level: 5
  ): it => text(
    size: 1em,
    weight: "bold",
    style: "italic",
    it.body
  )

  if cols == 1 {
    doc
  } else {
    columns(cols, gutter: 4%, doc)
  }


}


#show: document => man(
  runninghead: "AFFEKT, ATTRIBUTION UND BEWEGUNGSADHÄRENZ",
  lang: "de",
  document,
)

\
\
#block[
#heading(
level: 
1
, 
numbering: 
none
, 
outlined: 
false
, 
[
Was uns am Laufen hält: Vorhersagen von Bewegungsadhärenz durch Affekt und Attributionsstile
]
)
]
#set align(center)
#block[
\
Enno Winkler

Lehrgebiet Gesundheitspsychologie, Externer Betreuer Dr.~Sascha Leisterer (Humboldt-Universität zu Berlin), Fernuniversität Hagen

]
#set align(left)
\
\
#block[
#heading(
level: 
1
, 
numbering: 
none
, 
outlined: 
false
, 
[
Author Note
]
)
]
#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
Enno Winkler #box(image("_extensions/apaquarto/ORCID-iD_icon-vector.svg", width: 4.23mm)) https:\/\/orcid.org/0000-0000-0000-0001

#pagebreak()

#block[
#heading(
level: 
1
, 
numbering: 
none
, 
outlined: 
false
, 
[
Zusammenfassung
]
)
]
#block[
This document is a template.

]
#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
#emph[Schlüsselwörter];: keyword1, keyword2, keyword3

#pagebreak()

#block[
#heading(
level: 
1
, 
numbering: 
none
, 
outlined: 
false
, 
[
Was uns am Laufen hält: Vorhersagen von Bewegungsadhärenz durch Affekt und Attributionsstile
]
)
]
#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
Die Empfehlungen zu regelmäßiger physischer Aktivität (PA, #link(<ref-caspersen1985physical>)[Caspersen et al., 1985];) der WHO (#link(<ref-WHO2010>)[World Health Organization, 2010];) werden von nur etwa 26% der erwachsenen Bevölkerung in Deutschland in Bezug auf Muskel- und Ausdauertraining erreicht (#link(<ref-RKI_2022>)[Robert Koch-Institut, 2022];). Das hat großen gesundheitlichen und finanziellen Schaden zur Folge. Ding et al. (#link(<ref-Ding2016>)[2016];) gingen der Frage nach, welche Kosten sich jährlich und weltweit durch Produktivitätsausfälle und Behandlungskosten als Folge von Inaktivität ergeben. Als Endergebnis nennen die Autor:innen, dass der Gesamtbetrag einer konservativen Schätzung nach zwischen 19 und 182 Milliarden, einer weniger konservativen Schätzung nach allerdings zwischen 47 und 339 Milliarden Dollar liegt. Dabei sind die gesundheitlichen Vorteile von PA ermutigend. Menschen, die ein relativ hohes Niveau an PA berichten, zeigen eine deutlich reduzierte Mortalität (#link(<ref-Warburton2006>)[Warburton, 2006];). Auch Studien mit direkten Messmethoden kommen zu diesem Ergebnis. So fanden Myers et al. (#link(<ref-Myers2004>)[2004];) in einer Analyse der Trainingskapazität von Männern im fortgeschrittenen Erwachsenenalter eine um etwa 20% reduzierte Mortalität pro Zuwachs des Aktivitätsniveaus um ein metabolisches Äquivalent. Eine Erklärung dafür ist, dass aktivere Individuen seltener an chronischen Krankheiten erkranken. Lee et al. (#link(<ref-Lee2012>)[2012];) berichten in einer Meta-Analyse, dass die Krankheitslast von Herz-Kreislaufkrankheiten, Krebs und Diabetes bei Inaktivität um 6-10% erhöht ist und dass damit eine um mehr als ein halbes Jahr verkürzte durchschnittliche Lebenserwartung einhergeht. Körperliche Aktivität bringt auch Vorteile für die psychische Gesundheit. Das geschieht sowohl durch die Veränderung physiologischer als auch psychologischer Zustände im Körper (#link(<ref-Mikkelsen2017>)[Mikkelsen et al., 2017];).

Die Wirkung von PA gegen Depression ist eine vieldiskutierte Forschungsfrage, die noch nicht abschließend geklärt ist. Ledochowski et al. (#link(<ref-Ledochowski2016>)[2016];) kommen in einer systematischen Literaturrecherche zu dem Ergebnis, dass depressive Symptomatik sich durch moderate, zum Trainingszustand des:der Patientin passende PA reduzieren lässt, und dass sich dadurch affektive Zustände verbessern. Die Autor:innen 😇 sprechen die Empfehlung aus, Bewegung als Teil der Behandlung von Depression zu nutzen. In einer systematischen Literaturrecherche der Cochrane-Datenbank (#link(<ref-Cooney08>)[Cooney & Mead, 2013];) äußern sich die Autor:innen eher zurückhaltend. Die Effekte für Depression sind allenfalls moderat, und bei alleinigem Einbezug von rigoros kontrollierten Studien entziehen sich die Effekte statistischer Signifikanz. Es gibt außerdem Befunde, die zeigen, dass PA zwar effektiv in der Behandlung, nicht aber in der Prävention von Depression (#link(<ref-Carter2016>)[Carter et al., 2016];; #link(<ref-Paluska2000>)[Paluska & Schwenk, 2000];) ist. Die Forschungsgruppe um Ioannis D. Morres berichtet ermutigendere Ergebnisse. In einer Meta-Analyse zu aerobischer Bewegung finden Morres et al. (#link(<ref-Morres2018>)[2018];) mittlere bis große Effekte in der Behandlung von Depression. Hier zeigen sich Hinweise, dass z.B. eine Berücksichtigung der Vorlieben für Sport wichtig ist. Die Art der Bewegungs-Intervention spielt eine Rolle und bedarf weiterer Forschung.

Der Fokus von Studien zur Bewegungsförderung lag bisher auf Interventionen, die inaktive Personen dazu ermutigen, sich mehr zu bewegen und auf der Adhärenz zu solchen Interventionen (siehe z.B. #link(<ref-Gillison2009>)[Gillison et al., 2009];). Weniger Studien befassten sich mit der Frage, welche Faktoren die Aufrechterhaltung einer Bewegungsgewohnheit von Personen, die bereits gewohnheitsmäßig aktiv sind, beeinflussen (z.B. #link(<ref-Stetson2005>)[Stetson et al., 2005];). Das Rückfallpräventionsmodell (Relapse Prevention Model, Marlatt and George (#link(<ref-Marlatt1984>)[1984];)) könnte einen geeigneten Rahmen zur Untersuchung dieser Fragestellung darstellen.

= Theorie
<theorie>
== Physische Aktivität und Bewegung
<physische-aktivität-und-bewegung>
#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
Physische Aktivität (physical activity \[PA\]), auch #emph[körperliche Aktivität] (z.B. #link(<ref-Krug2013>)[Krug et al., 2013];) wird in vielen Definitionen als mechanistisch verstanden. So definiert Caspersen et al. (#link(<ref-caspersen1985physical>)[1985];) physische Aktivität als jedwede Aktivität der Skelettmuskeln, die einen Energieumsatz zur Folge hat. Andere Autor:innen unterscheiden sich in der Definition von physischer Aktivät in den Bedeutungsnuancen. So spezifizieren beispielsweise Hollmann and Strüder (#link(<ref-hollmann2009sportmedizin>)[2009];), dass die Aktivität in einer #emph[Steigerung] des Energieumsatzes resultieren muss, um als PA zu gelten. Zweifelsohne sind klare und einfache Definitionen essenziell für den wissenschaftlichen Zugang zu diesem Phänomen. Piggin (#link(<ref-Piggin2020>)[2020];) argumentiert jedoch, dass diese reduktionistische Auffassung von PA wird dem komplexen Erleben und Verhalten im Zusammenhang mit physicher Aktivität nicht gerecht wird und steht einer ganzheitlichen Betrachtung im Wege steht. Das ist auch im Einklang mit dem Biopsychosozialen Ansatz (#link(<ref-Engel1977>)[Engel, 1977];), der auf dem biomedizinischen Ansatz aufbaut und ihn erweitert. Im Gegensatz zum krankheits- und defizitorientierten Biomedizinischen Ansatz berücksichtigt der Biopsychosoziale Ansatz auch psychische und soziale Faktoren, zusätzliche zu den biologischen Faktoren. Um den Zusammenhang psychischer Phänomene (Affekt, Attribution) mit PA in dem Kontext der Gesundheitsförderung zu diskutieren, übernehme ich daher die Definition von Piggin (#link(<ref-Piggin2020>)[2020];): "Physical activity involves people moving, acting and performing within culturally specific spaces and contexts, and influenced by a unique array of interests, emotions, ideas, instructions and relationships." (S. 5).

Bewegungsadhärenz (exercise adherence, z.B. (#link(<ref-Mcauley1994>)[Mcauley et al., 1994];)) beinhaltet das Einhalten von selbst-oder fremdgesetzten Zielen im Zusammenhang mit PA (#link(<ref-Buckworth2007>)[Buckworth & Dishman, 2007];) und hat in der Regel das Ziel der Bewegungsförderung in einem gesundheitlichen Kontext (#link(<ref-Ainsworth2020>)[Ainsworth & Der Ananian, 2020];).

- Empfohlene PA

=== Messung von Physischer Aktivität
<messung-von-physischer-aktivität>
#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
Es gibt drei grobe Kategorien, in die Messmethoden für physische Aktivität fallen, und zwar direkte, indirekte und Fragebogen.

Die Messung per Fragebogen unterliegt typischen Problemen des Selbstberichts. Da beim Beantworten an Vergangenes erinnert wird, und bei der Konstruktion der Erinnerung abweichen auftreten können, ergibt sich das Problem der Retrospektivität. Außerdem kann sich das Antwortverhalten von Versuchspersonen verändern, wenn sie sich Bewusst sind, dass ihre Antworten analysiert werden, also Reaktivität herrscht. Zum Beispiel könnten Personen so antworten, wie es sozial erwünscht ist. Wenn eine Versuchsperson sich bewusst oder unbewusst als besonders sportliche Person darstellen möchte, könnte sie eine erhöhte Trainingsintensität berichten. Zur Messung physischer Aktivität gibt es Fragebögen, die das durchschnittliche Niveau physischer Aktivität im Alltag zu erfassen suchen, und Fragebögen, die sich spezifisch auf einzelne Trainingseinheiten beziehen.

Direkte Messmethoden beziehen sich auf die Beobachtung von physischer Aktivität oder Messmethoden von physischen Parametern, die direkt mit physischer Aktivität zusammenhängen. Dazu zählen Herzschlag oder der Laktat-Level im Blut, oder die strukturierte Beobachtung (SOCARP), wozu auch das Stoppen der Zeit bei dem Rundenlauf (e.g.~Cooper-Test) beinahltet.

== Affekt
<affekt>
#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
Affekt wird auf zwei Dimensionen konzipiert, und zwar Valenz und Aktivierung. Positiver affekt fühlt sich gut an und zieht uns dazu

Aktivierender Affekt geht mit erhöhtem Puls, Aufgeregtheit, gehobener Stimmung etc. einher. Niedrige Ausprägungen auf dieser Dimension des Affekts gehen mit Entspannung oder Rückzug einher. Im Circumplex Model of Affect \[LIT\] wird diese Konzeption vereint.

== Attribution
<attribution>
#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
Menschen ziehen für ihre Erfolge und Misserfolge unterschiedliche Erklärungen heran. Diese Erklärungen werden als Attribution bezeichnet, und unterscheiden sich auf den Dimensionen des Lokus, der Variabilität und der Globalität. Das Nichteinhalten eines Trainingsregimens könnte beispielsweise auf externe Faktoren oder interne, in der Person befindliche Faktoren attribuiert werden (Lokus). Ebenso könnte die variable Erklärung des "einmaligen Ausrutschers" oder die stabile Erklärung der "Fehlenden Sportlichkeit" herangezogen werden. Zuletzt könnten Personen sich den Misserfolg durch Faktoren erklären, die sich nur auf diese Situation beziehen ("ich kann mich zwar nicht an meine Bewegungsziele halten, aber meine Studienziele schaffe ich"), oder auf andere Situationen generalisieren ("Ich bin generell ein wenig Zielstrebiger Mensch").

== Das Rückfallmodell
<das-rückfallmodell>
#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
Das Rückfallmodell von Marlatt and George (#link(<ref-Marlatt1984>)[1984];) beleuchtet die Mechanismen der Aufrechterhaltung von Gesundheitsverhalten. Es basiert auf der Theorie des Sozialen Lernens (#link(<ref-bandura1977social>)[Bandura, 1977];) und beinhaltet sowohl kognitive als auch behaviorale Komponenten. Marlatt and George (#link(<ref-Marlatt1984>)[1984];) erklären, dass in diesem Modell die Initiierung von Gesundheitsverhalten und die Aufrechterhaltung dessen als zwei komplett verschiedene Prozesse verstanden werden. Somit ist also in diesem theoretischen Rahmen irrelevant, wie ein Individuum ein Gesundheitsverhalten, wie z.B. das Nichtrauchen oder regelmäßige PA, aufgenommen hat. Es wird allein die Frage betrachtet, wie Individuen auf Ausfälle bzw. Rückfälle vorbereitet werden können. Rückfälle werden hier nicht als Versagen behandelt, sondern als Teil des Prozesses zu einem stabilen Gesundheitsverhalten. Das Modell ist ist auch auf den Kontext von Sport und Bewegung anwendbar (z.B. #link(<ref-Marcus1997>)[Marcus et al., 1997];). Es wurde in Bezug auf Populationen mit bewegungsarmer Lebensweise (#link(<ref-Marcus1993>)[Marcus & Stanton, 1993];; #link(<ref-Martin1984>)[Martin et al., 1984];); und bereits aktive Populationen (#link(<ref-Stetson2005>)[Stetson et al., 2005];) angewendet.

Zentrale Konstrukte in diesem Modell sind nach Marlatt and George (#link(<ref-Marlatt1984>)[1984];) Hochrisikosituationen (high risk situations), wahrgenommene Kontrolle (perceived control), Coping-Reaktion (coping response), Positive Erwartungen (positive outcome expectancies), und der Verstoß gegen Abstinenz (abstinence violation effect).

Hochrisikosituationen sind Situationen, in denen die Wahrscheinlichkeit eines Rückfalls erhöht ist. Die drei im Sample von Marlatt and George (#link(<ref-Marlatt1984>)[1984];) am häufigsten berichteten Situationen sind negative emotionale Zustände (55%), sozialer Druck (20%),und zwischenmenschliche Konflikte (16%). Im Kontext von PA könnte ein solcher Zustand von negativen Gefühlen nach dem Sport herrühren.

Individuen erleben wahrgenommene Kontrolle, wenn sie erfolgreich Abstinenz ausüben oder, im Kontext von PA also von sedentärem Verhalten absehen. Wahrgenommene Kontrolle steigt, je länger die Abstinenz andauert und sinkt in einer Hochrisikosituation.

Coping-Reaktionen verbessern die wahrgenommene Kontrolle in einer Hochrisikosituation allerdings erheblich. Hierbei handelt es sich um ein Repertoire von Verhaltensweisen, um mit Hochrisikosituationen umzugehen. Als Beispiel in Bezug auf Sport und Bewegung sei die Angewohnheit genannt, trotz schlechten Wetters mit entsprechender Kleidung laufen zu gehen.

Positive Erwartungen stammen von der erwarteten sofortigen Belohnung durch das Unterlassen von Gesundheitsverhalten, wie z.B. wenn das Unterlassen einer Lauf-Einheit kurzfristig belohnend erscheint.

Verstöße gegen die Abstinenz werden von persönlicher Attribution begleitet. Das Rückfallmodell postuliert, dass eine internal-variable Attribution dienlich ist. Nach einem Rückfall zu sedentärem Verhalten wäre es demnach dienlich, die Ursache bei sich selbst zu sehen und als veränderbar zu betrachten.

Kritik am Rückfallmodell Innerhalb dieses Modells ist die wichtige Rolle der Selbstwirksamkeit bereits gut untersucht, in Bezug auf andere Variablen ist die Studienlage allerdings noch dünn (#link(<ref-Amireault2013>)[Amireault et al., 2013];).

== Hypothesen und Forschungsfrage
<hypothesen-und-forschungsfrage>
#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
Es ergibt sich die Forschungsfrage, inwiefern Affekt oder der Attributionsstil den Trainingsausfall bzw. die wahrgenommene Zielerreichung vorhersagen. Hierbei bezieht sich Affekt auf den #emph[State]

Wenn negatives Gefühlserleben und eine undienliche Attribution nach einem Rückfall häufiger zum schlussenlichen Unterlassen des Gesundheitsverhaltens fühlen, dann sollten ein tendenziell negatives Affekterleben und ein external - stabiler Attributionsstil weniger Trainingseinheiten und eine schwächer ausgeprägte Erreichung des gesetzen Ziels erreichen.

H1: Ein internal - variabler Attributionsstil sagt weniger Trainingsausfälle (H1.1), sowie eine geringere wahrgenommene Zielerreichung vorher (H1.2). H2: Ein negativeres Affekterleben sagt mehr Trainingsausfälle (H2.1) und eine geringere wahrgenommene Zielerreichung vorher (H2.2).

= Methode
<methode>
== Instrumente
<instrumente>
#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
Da es sich um ein Sample mit internationalen studierenden handelte, wurde bei allen Fragebögen die englische Version verwendet.

#emph[Demografische Variablen:] In der Baseline-Erhebung wurden das Alter, das Geschlecht, die Sportart und das Sportziel (z.B. "Marathon") abgefragt.

#emph[Session RPE:] Die Session Rate of Perceived Exhaustion (Sessionn-RPE, #link(<ref-Foster2001>)[Foster et al., 2001];) misst die wahrgenommene körperliche Belastung einer Trainingseinheit auf einer Skala von 0-10. Das Item lautet "How was your workout?" und die Skala soll etwa 30 Minuten nach Beenden des Trainings ausgefüllt werden. Auf den Stufen ß-5 ist jeder Wert beschriftet (0 = Rest, 1 = Very, Very Easy; 2 = Easy; 3 = Moderate; 4 = Somewhat hard; 5 = Hard). Auf den Stufen 6-10 sind nur Stufe 7 (Very Hard) und der Maximalwert (10 = Maximum) beschriftet. Diese Methode der Messung der Trainingsintensität wurde vielfach validiert und die Reliabilität der Skala ist gut (#link(<ref-Foster2021>)[Foster et al., 2021];). Die Übereinstimmung mit Messungen der Herzfrequenz bei vielen verschiedenen Arten von Sport ist hoch (#link(<ref-Day2004>)[Day et al., 2004];; #link(<ref-Foster2001>)[Foster et al., 2001];).

#emph[Attributionsstil:] Zur Erfassung des Attributionsstils wurde das Attribution Style Questionnaire (#link(<ref-Peterson1982>)[Peterson et al., 1982];) eingesetzt. Den Versuchspersonen wurden 16 verschiedene Szenarien präsentiert, die einen Erfolg oder Misserfolg schilderten, z.B. "You meet a friend who compliments you on your appearance." Es wurde dann die Ursache für den Erfolg oder Misserfolg in einer offenen Frage abgefragt, damit die Teilnehmenden sich beim Antworten auf die darauffolgenden Fragen nur auf diese eine Ursache beziehen. Danach wurden auf einem semantischen Differenzial mit Werten von 1-7, z.B. mit den Polen "Totally due to other people or circumstances" und "totally due to me" die drei Dimensionen Internalität, Stabilität und Globalität erfasst. Der Mittelwert aller 16 Situationen wurde für jede Dimension gebildet. Die Reliablität und Validität der Daten aus diesem Fragebogen wird u.a. von (#link(<ref-Corr1996>)[Corr & Gray, 1996];) unterstützt.

#emph[Positive and Negative Affect Scale (PANAS):] Die Positive and Negative Affect Scale (Panas, (#link(<ref-Watson1988>)[Watson et al., 1988];)) erfasst auf einer Skala von 1 ("not at all") bis 5 ("extremely") den positiven und negativen Affekt, mit jeweils 10 Items für positiven und 10 Items für negativen Affekt. Jedes Item ist nur ein einzelnes Wort, z.B. "interested" für positiven Affekt, oder "distressed" für negativen Affekt. Da es keine negativ gepolten Items gibt, werden die einzelnen Werte zu einem Gesamtwert für positiven und negativen Affekt gemittelt. Neben der Originalstudie von Watson et al. (#link(<ref-Watson1988>)[1988];) wurde die die Skala auch in neueren Studien als reliabel und valide erklärt (#link(<ref-Crawford2004>)[Crawford & Henry, 2004];).

#emph[Trainingsbezogene Variablen:] Im Baseline-Fragebogen wruden die wöchentliche Trainingszeit in Stunden und die Lauf-Kilometerzahl abgefragt. Zu jeder Trainingseinheit wurden ebenso die Trainingszeit und Kilometerzahl, bezogen auf das einzelne Training, abgefragt. Dazu wurden die Versuchspersonen gefragt, wie sehr die Intention, das Trainingsziel zu erreichen, ausgeprägt war (Commitment, visuelle Analogskala von 1-100), und zu wieviel Prozent sie ihr Trainingsziel erreicht war (Goal Attainment)

#emph[Weitere Instrumente:] In diesem Forschungsprojekt wurden außerdem authentischer und überheblicher Stolz (authentic and hubristic pride, Tracy and Robins (#link(<ref-Tracy2007>)[2007];)), und implizite Motive (#link(<ref-Sokolowski2000>)[Sokolowski et al., 2000];; #link(<ref-winter1994manual>)[Winter, 1994];) erfasst. In dieser Arbeit werden diese Daten nicht berücksichtigt.

== Stichprobe:
<stichprobe>
#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
Die Teilnehmenden waren erwachsene Freizeitsportler:innen mit einem bestimmten Trainingsziel.

Um ein aussagekräftiges Modell zu bekommen, wird eine Korrelation von 0,5 empfohlen \[LIT\]. Basierend auf einer Poweranalyse mit 1 – β = 0.95, α = 0.05, und #emph[r] = 0.5 ergab sich eine ideale Teilnehmerzahl von 38. Die Frage der Poweranalyse für Mehrebenenanalysen ist allerdings komplex (#link(<ref-Hox2017-dx>)[Hox et al., 2017];). Der Versuch, post-hoc Power des bestimmten Modells durch Simulation mit dem Paket `simr` (#link(<ref-Green2016>)[Green & MacLeod, 2016];) zu bestimmen, scheiterte, was teils auf die kleine Stichprobe, teils auf die Datenstruktur zurückgeführt werden kann. #emph[N] = 48 Fälle ergaben sich in dem Rohdatensatz. #emph[n] = 9 wurden von der Analyse und weiteren Befragungen ausgeschlossen, weil sie angaben, nicht an einem systematischen Trainings- oder Bewegungsprogramm teilzunehmen. Aufgrund von mehr als 50 fehlenden Datenpunkten wurden #emph[n] = 12 Teilnehmende ausgeschlossen. Die deskriptiven Statistiken zu den demografischen Daten sind in Tabelle 1.

#box(image("MD_BAEW_files/figure-typst/Demografietabelle-1.png"))

#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
Die Teilnehmenden waren durchschnittlich 34.52 Jahre alt (#emph[SD] = 13.82). Das Alter reichte von 19 bis64.

== Durchführung:
<durchführung>
=== #emph[Prozedur]
<prozedur>
#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
Diese Studie wurde von der Ethikkommission der Uni Leipzig genehmigt (Fallnr. 2023.05.03 \_cb\_196).

Die Teilnehmenden wurden über Inserate in sozialen Medien und persönliche Kontakte rekrutiert, es handelt sich also um eine Gelegenheitsstichprobe. Es gab zwei Inklusionskriterien: Die Teilnehmenden mussten erwachsen sein und ein konkretes Trainingsziel verfolgen. Ebenso wurde darauf hingewiesen, dass bei Krankheiten oder relevanten Verletzungen eine Studienteilnahme nicht möglich ist.

Die Freizeitsportler:innen berichteten vor der Studie in einer separaten Sitzung ihr generelles Affekterleben innerhalb der letzten zehn Tage mit der Positive and Negative Affect-Scale (PANAS) von Watson et al. (#link(<ref-Watson1988>)[1988];). Der Prä-Test enthielt außerdem die Skala von Peterson et al. (#link(<ref-Peterson1982>)[1982];) zum Attributionsstil und die Items zu demografischen Daten.

Darauf folgte die Trainingsphase, in der die Trainierenden über einen Zeitraum von maximal zwei Monaten sechs Trainingseinheiten dokumentieren sollten. Dazu wurden sie per E-Mail benachrichtigt und sie wurden gebeten, nach der Trainingseinheit einen Fragebogen auszufüllen. In den Befragungen wurden jeweils der positive und negative Affekt, Session RPE und die trainingsbezogenen Variablen abgefragt.

Die Daten der einzelnen Versuchspersonen wurden mittels eines Pseudonyms zugeordnet, das keinen Rückschluss auf Einzelpersonen ermöglichte. Nach der Datenerhebung wurden der Datensatz komplett anonymisiert. Die Versuchspersonen wurden schriftlich debrieft.

== Statistische Analyse
<statistische-analyse>
=== #strong[Datenbereinigung und Transformation]
<datenbereinigung-und-transformation>
#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
Diese Arbeit wurde mit R (Version 4.5.0 ) und Quarto (Version 1.6.32) erstellt. Die reproduzierbare Version dieses Artikels ist auf GitHub unter #link("https://github.com/Enno-W/BAEW") verfügbar.

Versuchspersonen, die angaben, kein konkretes Trainingsziel zu verfolgen oder mehr als 50 fehlende Werte hatten, wurden von der weiteren Analyse ausgeschlossen. Die Variablen zu Stolz und der Motivstruktur wurden eliminert, da sie in dieser Arbeit nicht berücksichtigt werden. Für Werte, bei denen die Versuchspersonen einen Bereich (z.B. 10-15 km) angegeben hatten, wurde der Durchschnitt der beiden Werte gebildet. Da die Skalen von Affekt und Attribution keine gegensätzlich gepolten Items beinhalten, wurde keine Umpolung vorgenommen. Angaben zur Sportart mit dem Wortbestandteil "lauf" wurden zu "Laufen", und mit "kraft" zu "Kraftsport" zusammengefasst.

In dem bereinigten Datensatz befanden sich immer noch 404 fehlende Werte, die per multipler Imputation per #emph[predictive mean matching] mit 5 Iterationen geschätzt wurden. Bei diesem Verfahren werden in mehreren Durchläufen bzw. Iterationen plausible Werte anhand der Verteilungen und Beziehungen der Variablen untereinander geschätzt (#link(<ref-Li2015>)[Li et al., 2015];).

Die Daten bestanden aus einem Baseline-Test und Angaben aus einem Fragebogen zu sechs darauffolgenden Trainingseinheiten. Diese wöchentlichen Angaben wurden zu Mittelwerten zusammengefasst. Außerdem wurde ausgezählt, wie viele Trainingseinheiten eine Person ausgefüllt hatte. Für die Berechnung der Hierarchischen linearen Modelle wurden die Daten in ein Langformat transformiert, und zwar so, dass jeder Messzeitpunkt in einer Zeile aufgeführt war. So entstanden für jede Versuchsperson sechs Zeilen für die 6 Messzeitpunkte. Die Werte, die nur zu einem Messzeitpunkt erhoben wurden, wie etwa Attributionsstil oder Alter, wiederholten sich in jeder dieser sechs Zeilen.

\#\#\#\*\*Statistische Tests

In der Präregistrierung wurde als Haupttest eine Mehrebenenanalyse, auch bekannt als #emph[Hierarchisches Lineares Modell] spezifiziert. Die Voraussage der Trainingsausfälle durch ein Mehrebeneenmodell scheiterte daran, dass diese Variable nur 6 diskrete Ausprägungen (Anzahl der abgeschlossenen Trainingseinheiten) hatte, wodurch nicht genug Variablität entstand und das Modell somit nicht konvergieren konnte. Als Alternative wurden Hypothesen 1.1 und 2.1 mit einem verallgemeinerten Linearen Modell (GLMM) getestet. Für Hypothesen 1.2 und 2.2 konnte ein Mehrebenenmodell berechnet werden.

#strong[Hierarchische Lineare Modelle] Hierarchische Lineare Modelle stellen komplexe statistische Verfahren dar, die besonders gut für längsschnittliche Daten geeignet sind (#link(<ref-Nezlek2006>)[Nezlek et al., 2006];). Der Vorteil solcher Modelle ist, dass "genestete" Daten in die Analyse mit einbezogen werden können. Im vorliegenden Datensatz sind die Daten eines Individuums über die sechs Messzeitpunkte hinweg nicht unabhängig voneinander, sondern die Daten sind im Individuum "genestet". Schließlich reagiert jede Versuchsperson unterschiedlich auf die Anforderungen dieser Studie. Bei manchen Versuchspersonen könnte die wahrgenommene Zielerreichung über die Zeit hinweg steigen, bei anderen wiederum könnte sie weniger stark steigen oder sogar sinken. Das Interzept, also der Startpunkt der Vorhersagegeraden eines jeden Individuums, könnte sich ebenso unterscheiden, schließlich starten nicht alle mit genau dem gleichen Ausmaß an wahrgenommener Zielerreichung. Im vorliegenden Modell wird dem Rechnung getragen, indem die Veränderung über die Zeit je nach Person (ID) als #strong[random effect] in das Modell eingeht. Dabei wird im Modell zugellassen, dass sowohl das Interzept als auch die Steigung ("slope") frei variieren. Daneben gibt es aber auch Effekte, von denen erwartet wird, dass sie alle Versuchspersonen in gleicher Weise beeinflussen, wenn auch unterschiedlich stark. In diesem Modell sind das der negative Affekt nach der Trainingseinheit, beziehungsweise der Lokus und die Variabilität des Attributionsstils.

Bei hierarchischen linearen Modellen ergibt sich die Frage der Zentrierung.

Um die Intraklassen-Korrelation zu berechnen, wurde zunächst ein Nullmodell berechnet. Bei einem Nullmodell handelt es sich um die einfachste mögliche Form eines hierarchischen linearen Modells.

Dafür wurden die wiederholten Angaben zu negativem Affekt zentriert. Die Residuen der aufgestellten Modelle erfüllten allerdings die Voraussetzung der Homoskedastizität und Normalverteilung nicht. Daher wurde explorativ …

= Ergebnisse
<ergebnisse>
== Deskriptive Statistiken
<deskriptive-statistiken>
#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
#link(<tbl-stattable>)[Tabelle~1] zeigt einen Überblick über deskriptive Statistiken aller relevanten Variablen. Das Alter weist eine leichte Rechtsschiefe auf, es gab also viele jüngere und wenig ältere Teilnehmende.

Die Daten zum Attribution (Locus und Variabilität) folgen einer Normalverteilung, aber alle Werte fielen relativ hoch aus. Sie reichten von 4.3 bis 6.9 für den Lokus und von 4.3 bis 7 für die Variabilität.

Für die Variablen, die nach jeder Trainingseinheit erfasst wurden, wurden für eine deskriptive Einschätzung Durchschnittswerte gebildet. Die Anzahl der abgeschlossenen Trainingseinheiten, von denen es insgesamt sechs in der Studie gab, weist eine extrem linksschiefe Verteilung auf, da 16 Personen alle Einheiten beendet haben. Ein ähnliches Muster mit vielen hohen Werten weist die durchschnittliche wahrgenommene Zielerreichung auf. 14 Personen hatten Werte größer als neunzig, und sämtliche Durchschnittswerte der Ziellerreichung reichten von 62.5 bis 100. Die Variablen der durchschnittlich gelaufenen Kilometer und Minuten pro Einheit wies viele Werte am unteren Ende der Verteilung auf und einige extreme Werte am oberen Ende. Von allen Variablen, die auf zu den Trainingseinheiten erhoben wurden, war nur die durchschnittliche wahrgenommene Trainingsintensität normalverteilt. Der negative Affekt nach jeder Trainingseinheit hatte einen Maximalwert von 3 und eine hohe Konzentration von Werten am unteren Ende der Verteilung.

Die Baseline-Werte waren allesamt rechtsschief und nicht normalverteilt. Die wöchentlich gelaufenen Kilometer wiesen zwei Ausreißer mit extremen Werten von mehr als 200km pro Woche auf. Diese Werte waren nicht plausibel, da sie, mit der wöchentlichen Laufzeit verrechnet, eine Durchschnittsgeschwindigkeit von jeweils 136.5 und 21 Kilometern pro Stunde suggerierten. Da diese Baseline-Werte aber nicht in die statistischen Analysen einbezogen wurden, wurden die Daten dieser Versuchspersonen beibehalten.

#figure([
#box(image("MD_BAEW_files/figure-typst/tbl-stattable-1.png"))

], caption: figure.caption(
position: top, 
[
Deskriptive Statistiken
]), 
kind: "quarto-float-tbl", 
supplement: "Tabelle", 
)
<tbl-stattable>


#block[
#emph[Hinweis];. Das Konfidenzintervall ist als Abstand vom Mittelwert zum unteren bzw. oberen Konfidenzintervall notiert.

]
== Korrelationen
<korrelationen>
#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
Laut Cohen (#link(<ref-Cohen2013>)[2013];) kann der Korrelationskoeffizient in den Sozialwissenschaften ab #emph[r] = .1 als schwach, ab #emph[r] = .3 als mittelmäßig und von #emph[r] = .5 bis #emph[r] = 1 als stark interpretiert werden. Die Korrelationen für relevante Variablen sind in #link(<tbl-corrtable>)[Tabelle~2] dargestellt. In Bezug auf die Hypothesen ist lediglich einer der Korrelationswerte bedeutsam. Zwischen dem negativen Affekt und der wahrgenommen Zielerreichung zeigte sich eine signifikante, mittelmäßig stark ausgeprägte Korrelation. Je mehr negativen Affekt Versuchspersonen also nach einer Trainingseinheit im Durchschnitt berichteten, desto schlechter erlebten sie ihre Zielerreichung. Negativer Affekt nach der Trainingseinheit war ebenso mit einer höheren RPE im Durchschnitt assoziiert.

Die Trainingsbezogen Variablen brachten mehrere starke, signifikante Korrelationen hervor. Die Baseline-Werte für Trainingszeit und Distanz waren sowohl mit der gemittelten Trainingszeit als auch der Distanz aus den einzelnen Trainingseinheiten signifikant korreliert. Auch die anderen Baseline-Werte korrelierten mit den Durchschnittswerten aus den einzelnen Trainingseinheiten.

Sowohl als zwischen Baseline-RPE und Baseline negativem Affekt als auch der durchschnittlichen Session-RPE und negativem Affekt ergaben sich positive Korrelationen. Je höher der negative Affekt ausgeprägt war, desto anstrengender erlebten Personen ihre Trainingseinheit.

#set page(flipped: true)
#figure([
#box(image("MD_BAEW_files/figure-typst/tbl-corrtable-1.png"))

], caption: figure.caption(
position: top, 
[
Korrelationstabelle der gemittelten durchschnittlichen Werte aus den Werten der einzelnen Sessions
]), 
kind: "quarto-float-tbl", 
supplement: "Tabelle", 
)
<tbl-corrtable>


#block[
#emph[Hinweis];. \*p \< 0.05; \*\*p \< 0.01

]
#set page(flipped: false)
#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
#link(<fig-overview>)[Abbildung~1] zeigt eine Übersicht der Variablen, zur Überprüfung der Hypothesen herangezogen wurden.

#figure([
#box(image("MD_BAEW_files/figure-typst/fig-overview-1.svg"))
], caption: figure.caption(
position: top, 
[
The Figure Caption
]), 
kind: "quarto-float-fig", 
supplement: "Abbildung", 
)
<fig-overview>


#block[
#emph[Hinweis];. This is the note below the figure.

]
== Regressionsmodelle
<regressionsmodelle>
#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
Für jede Hypothese gab es ein Modell. Die Modelle zur Vorhersage der abgeschlossenen Trainingseinheiten sind in #link(<tbl-hlmtable1>)[Tabelle~3] zu sehen, und die Modelle zur Vorhersage von wahrgenommener Zielerreichung sind in #link(<tbl-hlmtable2>)[Tabelle~4] .

=== Vorhersagen von Trainingsausfällen
<vorhersagen-von-trainingsausfällen>
#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
Teilnehmende wurden in der Studie dazu aufgefordert, sechs Trainingseinheiten zu dokumentieren und die Fragebögen dazu auszufüllen. Ich habe ausgezählt, zu wie vielen Trainingseinheiten Daten vorlagen und diese Anzahl als Dummy-Variable eingetragen. Die resultierende Variable, also die Anzahl der Trainingsausfälle, war linksschief, und da es nur sechs diskrete Ausprägungen gab, scheiterte die Analyse mit einem Hierarchischen Linearen Modell. Stattdessen habe ich ein #strong[General Linear Model] verwendet, was passender für Daten mit diskreten Ausprägungen ist.

#figure([
───────────────────────────────────────────────────────────────────────── Attributions-Modell (H Affekt-Modell (H 2.1) \
1.1) \
────────────────────────────────────────────────────────── (Intercept) 1.499 \*\*\* (0.110) 1.499 \*\*\* (0.110) \
Locus\_center -0.105~~~~ (0.125) ~~~~~~~~ ~~~~~ \
ed \
Dynamics\_cen 0.021~~~~ (0.119) ~~~~~~~~ ~~~~~ \
tered \
sd\_\_(Interce 0.378~~~~ (NA)~~~~~ 0.374~~~~ (NA)~~~~~ \
pt) \
NA\_base\_cent ~~~~~~~~ ~~~~~ 0.205~~~~ (0.203) \
ered \
NegativeAffe ~~~~~~~~ ~~~~~ -0.000~~~~ (0.088) \
ct\_cm\_center \
ed \
────────────────────────────────────────────────────────── nobs 162~~~~~~~~ ~~~~~ 162~~~~~~~~ ~~~~~ \
nobs.1 162.000~~~~ ~~~~~ 162.000~~~~ ~~~~~ \
sigma 1.000~~~~ ~~~~~ 1.000~~~~ ~~~~~ \
logLik -307.751~~~~ ~~~~~ -307.599~~~~ ~~~~~ \
AIC 625.501~~~~ ~~~~~ 625.198~~~~ ~~~~~ \
BIC 640.939~~~~ ~~~~~ 640.636~~~~ ~~~~~ \
deviance 9.002~~~~ ~~~~~ 9.225~~~~ ~~~~~ \
df.residual 157.000~~~~ ~~~~~ 157.000~~~~ ~~~~~ \
───────────────────────────────────────────────────────────────────────── \*\*\* p \< 0.001; \*\* p \< 0.01; \* p \< 0.05.

Column names: names, Attributions-Modell (H 1.1), Attributions-Modell (H 1.1).error, Affekt-Modell (H 2.1), Affekt-Modell (H 2.1).error

], caption: figure.caption(
position: top, 
[
Modellkoeffizienten des General Linear Models zur Vorhersage der abgeschlossenen Trainingseinheiten.
]), 
kind: "quarto-float-tbl", 
supplement: "Tabelle", 
)
<tbl-hlmtable1>


#block[
#emph[Hinweis];. {}

]
= Hierarchiche Lineare Modelle
<hierarchiche-lineare-modelle>
#figure([
─────────────────────────────────────────────────────────────────────────── Nullmodell Modell 1 Modell 2 \
──────────────────────────────────────────────────────────────── (Interce 4.365 (0.048) 4.552 (0.108) 4.562 (0.107) \
pt) \*\*\* \*\*\* \*\*\* \
Time ~~~~~~~~ ~~~~~ -0.053~~ (0.028) -0.056 (0.027) \
~~ #emph[~~ \
Locus\_ce ~~~~~~~~ ~~~~~ 0.027~~~ (0.073) 0.027~~~ (0.073) \
ntered ~ ~ \
Dynamics ~~~~~~~~ ~~~~~ 0.005~~~ (0.070) 0.005~~~ (0.070) \
#emph[centere ~ ~ \
d \
Negative ~~~~~~~~ ~~~~~ ~~~~~~~~ ~~~~~ -0.209~~ (0.117) \
Affect\_c ~~ \
m\_center \
ed \
NA\_base] ~~~~~~~~ ~~~~~ ~~~~~~~~ ~~~~~ ~~~~~~~~ ~~~~~ \
centered \
──────────────────────────────────────────────────────────────── nobs 162~~~~~ ~~~~~ 162~~~~~ ~~~~~ 162~~~~~ ~~~~~ \
~~~ ~~~ ~~~ \
nobs.1 162.000~ ~~~~~ 162.000~ ~~~~~ 162.000~ ~~~~~ \
~~~ ~~~ ~~~ \
sigma 0.633~~~ ~~~~~ 0.623~~~ ~~~~~ 0.616~~~ ~~~~~ \
~ ~ ~ \
logLik -155.666 ~~~~~ -153.777 ~~~~~ -152.148 ~~~~~ \
~~~~ ~~~~ ~~~~ \
AIC 319.331~ ~~~~~ 321.554~ ~~~~~ 320.296~ ~~~~~ \
~~~ ~~~ ~~~ \
BIC 331.681~ ~~~~~ 343.167~ ~~~~~ 344.996~ ~~~~~ \
~~~ ~~~ ~~~ \
deviance 311.331~ ~~~~~ 307.554~ ~~~~~ 304.296~ ~~~~~ \
~~~ ~~~ ~~~ \
df.resid 135.000~ ~~~~~ 134.000~ ~~~~~ 133.000~ ~~~~~ \
ual ~~~ ~~~ ~~~ \
─────────────────────────────────────────────────────────────────────────── ];\*\* p \< 0.001; \*\* p \< 0.01; \* p \< 0.05.

Column names: names, Nullmodell, Nullmodell.error, Modell 1, Modell 1.error, Modell 2, Modell 2.error, Modell 3, Modell 3.error

7/9 columns shown.

], caption: figure.caption(
position: top, 
[
Hierarchisches Modell zur Vorhersage von wahrgenommener Zielerreichung
]), 
kind: "quarto-float-tbl", 
supplement: "Tabelle", 
)
<tbl-hlmtable2>


#block[
#emph[Hinweis];. Die Interzepte und Slopes gingen als Random-Effekt in die Modellberechnung ein. Das heißt, jede Person konnte als am Anfang eine unterschiedliche wahrgenommene Zielerreichung haben, und die Veränderung über die Zeit konnte variieren.

]
#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
Das Nullmodell für die Vorhersage der wahrgenommenen Zielerreichung hatte einen Intraklassenkoeffizienten von #emph[ICC] = -0.03. Der Intraklassenkoeffizient gibt wieder, wie viel Varianz auf die Unterschiede zwischen Gruppen zurückgeführt werden kann. Für beide Modelle waren lediglich der Interzept signifikant.

#figure([
#box(image("MD_BAEW_files/figure-typst/fig-model_overview-1.svg"))
], caption: figure.caption(
position: top, 
[
Vorhergesagte Veränderung der wahrgenommenen Zielerreichung über die Zeit hinweg
]), 
kind: "quarto-float-fig", 
supplement: "Abbildung", 
)
<fig-model_overview>


#block[
#emph[Hinweis];. Die Abbildung links zeigt die Veränderung je Teilnehmer:in, die Abbildung rechts die Gesamtvorhersage des Modells.

]
#block[
```
[[1]]
```

]
#block[
```

[[2]]
```

]
#block[
```

[[3]]
```

]
#figure([
#box(image("MD_BAEW_files/figure-typst/fig-homoscedasticity-1.svg"))
], caption: figure.caption(
position: top, 
[
The Figure Caption
]), 
kind: "quarto-float-fig", 
supplement: "Abbildung", 
)
<fig-homoscedasticity-1>


#block[
#figure([
#box(image("MD_BAEW_files/figure-typst/fig-homoscedasticity-2.svg"))
], caption: figure.caption(
position: top, 
[
The Figure Caption
]), 
kind: "quarto-float-fig", 
supplement: "Abbildung", 
)
<fig-homoscedasticity-2>


]
#block[
#figure([
#box(image("MD_BAEW_files/figure-typst/fig-homoscedasticity-3.svg"))
], caption: figure.caption(
position: top, 
[
The Figure Caption
]), 
kind: "quarto-float-fig", 
supplement: "Abbildung", 
)
<fig-homoscedasticity-3>


]
#block[
#emph[Hinweis];. This is the note below the figure.

]
#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
Die Residuen aus den beiden Modellen zur Zielerreichung aus jeweils Attribution und Affekt waren nicht normalverteilt. Ein Shapiro-Wilk-Test ergab einen p-Wert von #emph[p] \< .001 für das erstere, und

#emph[Attributionsstil:]

= Diskussion
<diskussion>
#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
Multiple imputation

Ein hoher ICC ist aus verschiedenen Gründen wünschenswert … Laut Nezlek (#link(<ref-nezlek2008introduction>)[2008];) sind hierarchische lineare Modelle aber auch sinnvoll, wenn der ICC niedrig ist, weil …

In der Präregistrierung war ein HLM für alle Vorhersagen angedacht. Außerdem wurde nicht wie in der Präregistrierung beschrieben das Pakte lme4 (#link(<ref-lme42025>)[Bates et al., 2015];) verwendet, sondern nlme (#link(<ref-nlme2023>)[Pinheiro et al., 2023];), da es für Längsschnittstudien besser geeignet ist (#link(<ref-ellis2020hierarchical>)[Ellis & Mayer, 2020];).

= Literaturverzeichnis
<literaturverzeichnis>
#set par(first-line-indent: 0in, hanging-indent: 0.5in)
#block[
#block[
Ainsworth, B. E., & Der Ananian, C. (2020). Physical Activity Promotion. In G. Tenenbaum & R. C. Eklund (Hrsg.), #emph[Handbook of Sport Psychology: Bd. II] (4th Aufl., S. 773–794). John Wiley & Sons, Inc. #link("https://doi.org/doi.org/10.1002/9781119568124.ch37")

] <ref-Ainsworth2020>
#block[
Amireault, S., Godin, G., & Vézina-Im, L.-A. (2013). Determinants of physical activity maintenance: a systematic review and meta-analyses. #emph[Health Psychology Review];, #emph[7];(1), 55–91. #link("https://doi.org/10.1080/17437199.2012.701060")

] <ref-Amireault2013>
#block[
Bandura, A. (1977). #emph[Social Learning Theory];. Prentice-Hall.

] <ref-bandura1977social>
#block[
Bates, D., Mächler, M., Bolker, B., & Walker, S. (2015). Fitting Linear Mixed-Effects Models Using lme4. #emph[Journal of Statistical Software];, #emph[67];(1), 1–48. #link("https://doi.org/10.18637/jss.v067.i01")

] <ref-lme42025>
#block[
Buckworth, J., & Dishman, R. K. (2007). Exercise adherence. In G. Tenenbaum & R. C. Eklund (Hrsg.), #emph[Handbook of Sport Psychology] (S. 509–536). John Wiley & Sons, Inc. #link("https://doi.org/10.1002/9781118270011")

] <ref-Buckworth2007>
#block[
Carter, T., Morres, I. D., Meade, O., & Callaghan, P. (2016). The Effect of Exercise on Depressive Symptoms in Adolescents: A Systematic Review and Meta-Analysis. #emph[Journal of the American Academy of Child &; Adolescent Psychiatry];, #emph[55];(7), 580–590. #link("https://doi.org/10.1016/j.jaac.2016.04.016")

] <ref-Carter2016>
#block[
Caspersen, C. J., Powell, K. E., & Christenson, G. M. (1985). Physical activity, exercise, and physical fitness: definitions and distinctions for health-related research. #emph[Public Health Reports];, #emph[100];, 126–131.

] <ref-caspersen1985physical>
#block[
Cohen, J. (2013). #emph[Statistical Power Analysis for the Behavioral Sciences];. Routledge. #link("https://doi.org/10.4324/9780203771587")

] <ref-Cohen2013>
#block[
Cooney, D., GM, & Mead, G. (2013). Exercise for depression. #emph[Cochrane Database of Systematic Reviews];, #emph[9];. #link("https://doi.org/10.1002/14651858.CD004366.pub6")

] <ref-Cooney08>
#block[
Corr, P. J., & Gray, J. A. (1996). Structure and Validity of the Attributional Style Questionnaire: A Cross-Sample Comparison. #emph[The Journal of Psychology];, #emph[130];(6), 645–657. #link("https://doi.org/10.1080/00223980.1996.9915038")

] <ref-Corr1996>
#block[
Crawford, J. R., & Henry, J. D. (2004). The Positive and Negative Affect Schedule (PANAS): Construct validity, measurement properties and normative data in a large non‐clinical sample. #emph[British Journal of Clinical Psychology];, #emph[43];(3), 245–265. #link("https://doi.org/10.1348/0144665031752934")

] <ref-Crawford2004>
#block[
Day, M. L., McGuigan, M. R., Brice, G., & Foster, C. (2004). Monitoring Exercise Intensity During Resistance Training Using the Session RPE Scale. #emph[The Journal of Strength and Conditioning Research];, #emph[18];(2), 353. #link("https://doi.org/10.1519/r-13113.1")

] <ref-Day2004>
#block[
Ding, D., Lawson, K. D., Kolbe-Alexander, T. L., Finkelstein, E. A., Katzmarzyk, P. T., Mechelen, W. van, & Pratt, M. (2016). The economic burden of physical inactivity: a global analysis of major non-communicable diseases. #emph[The Lancet];, #emph[388];(10051), 1311–1324. #link("https://doi.org/10.1016/s0140-6736(16)30383-x")

] <ref-Ding2016>
#block[
Ellis, A., & Mayer, B. (2020). #emph[Introduction to R, Chapter 12: Hierarchical Linear Models];. #link("https://methodenlehre.github.io/intro-to-rstats/index.html")

] <ref-ellis2020hierarchical>
#block[
Engel, G. L. (1977). The Need for a New Medical Model: A Challenge for Biomedicine. #emph[Science];, #emph[196];(4286), 129–136. #link("https://doi.org/10.1126/science.847460")

] <ref-Engel1977>
#block[
Foster, C., Boullosa, D., McGuigan, M., Fusco, A., Cortis, C., Arney, B. E., Orton, B., Dodge, C., Jaime, S., Radtke, K., Erp, T. van, Koning, J. J. de, Bok, D., Rodriguez-Marroyo, J. A., & Porcari, J. P. (2021). 25 Years of Session Rating of Perceived Exertion: Historical Perspective and Development. #emph[International Journal of Sports Physiology and Performance];, #emph[16];(5), 612–621. #link("https://doi.org/10.1123/ijspp.2020-0599")

] <ref-Foster2021>
#block[
Foster, C., Florhaug, J. A., Franklin, J., Gottschall, L., Hrovatin, L. A., Parker, S., Doleshall, P., & Dodge, C. (2001). A New Approach to Monitoring Exercise Training. #emph[Journal of Strength and Conditioning Research];, #emph[15];(1), 109–115.

] <ref-Foster2001>
#block[
Gillison, F. B., Skevington, S. M., Sato, A., Standage, M., & Evangelidou, S. (2009). The effects of exercise interventions on quality of life in clinical and healthy populations; a meta-analysis. #emph[Social Science &; Medicine];, #emph[68];(9), 1700–1710. #link("https://doi.org/10.1016/j.socscimed.2009.02.028")

] <ref-Gillison2009>
#block[
Green, P., & MacLeod, C. J. (2016). \<scp\>SIMR\</scp\>: an R package for power analysis of generalized linear mixed models by simulation. #emph[Methods in Ecology and Evolution];, #emph[7];(4), 493–498. #link("https://doi.org/10.1111/2041-210x.12504")

] <ref-Green2016>
#block[
Hollmann, W., & Strüder, H. K. (2009). #emph[Sportmedizin. Grundlagen für physische Aktivität, Training und Präventivmedizin] (5th Aufl.). Schattauer.

] <ref-hollmann2009sportmedizin>
#block[
Hox, J., Moerbeek, M., & Schoot, R. van de. (2017). #emph[Multilevel Analysis : Techniques and Applications, Third Edition];. Taylor & Francis Group.

] <ref-Hox2017-dx>
#block[
Krug, S., Jordan, S., Mensink, G., Müters, S., Finger, J., & Lampert, T. (2013). Körperliche Aktivität. In #emph[Bundesgesundheitsblatt - Gesundheitsforschung - Gesundheitsschutz] (Bd. 56). Robert Koch-Institut, Epidemiologie und Gesundheitsberichterstattung. #link("https://doi.org/10.1007/s00103-012-1661-6")

] <ref-Krug2013>
#block[
Ledochowski, L., Stark, R., Ruedl, G., & Kopp, M. (2016). Körperliche Aktivität als therapeutische Intervention bei Depression. #emph[Der Nervenarzt];, #emph[88];(7), 765–778. #link("https://doi.org/10.1007/s00115-016-0222-x")

] <ref-Ledochowski2016>
#block[
Lee, I.-M., Shiroma, E. J., Lobelo, F., Puska, P., Blair, S. N., & Katzmarzyk, P. T. (2012). Effect of physical inactivity on major non-communicable diseases worldwide: an analysis of burden of disease and life expectancy. #emph[The Lancet];, #emph[380];(9838), 219–229. #link("https://doi.org/10.1016/s0140-6736(12)61031-9")

] <ref-Lee2012>
#block[
Li, P., Stuart, E. A., & Allison, D. B. (2015). Multiple Imputation: A Flexible Tool for Handling Missing Data. #emph[JAMA];, #emph[314];(18), 1966. #link("https://doi.org/10.1001/jama.2015.15281")

] <ref-Li2015>
#block[
Marcus, B. H., Bock, B. C., & Pinto, B. M. (1997). Initiation and Maintenance of Exercise Behavior. In #emph[Handbook of Health Behavior Research II] (S. 335–352). Springer US. #link("https://doi.org/10.1007/978-1-4899-1760-7_18")

] <ref-Marcus1997>
#block[
Marcus, B. H., & Stanton, A. L. (1993). Evaluation of Relapse Prevention and Reinforcement Interventions to Promote Exercise Adherence in Sedentary Females. #emph[Research Quarterly for Exercise and Sport];, #emph[64];(4), 447–452. #link("https://doi.org/10.1080/02701367.1993.10607598")

] <ref-Marcus1993>
#block[
Marlatt, G. A., & George, W. H. (1984). Relapse Prevention: Introduction and Overview of the Model. #emph[British Journal of Addiction];, #emph[79];(4), 261–273. #link("https://doi.org/10.1111/j.1360-0443.1984.tb03867.x")

] <ref-Marlatt1984>
#block[
Martin, J. e., Dubbert, P. M., Katell, A. D., Thompson, J. K., Raczynski, J. R., Lake, M., Smith, P. O., Webster, J. S., Sikora, T., & Cohen, R. E. (1984). Behavioral control of exercise in sedentary adults: Studies 1 through 6. #emph[Journal of Consulting and Clinical Psychology];, #emph[52];(5), 795–811. #link("https://doi.org/10.1037/0022-006x.52.5.795")

] <ref-Martin1984>
#block[
Mcauley, E., Courneya, K. S., Rudolph, D. L., & Lox, C. L. (1994). Enhancing Exercise Adherence in Middle-Aged Males and Females. #emph[Preventive Medicine];, #emph[23];(4), 498–506. #link("https://doi.org/10.1006/pmed.1994.1068")

] <ref-Mcauley1994>
#block[
Mikkelsen, K., Stojanovska, L., Polenakovic, M., Bosevski, M., & Apostolopoulos, V. (2017). Exercise and mental health. #emph[Maturitas];, #emph[106];, 48–56. #link("https://doi.org/10.1016/j.maturitas.2017.09.003")

] <ref-Mikkelsen2017>
#block[
Morres, I. D., Hatzigeorgiadis, A., Stathi, A., Comoutos, N., Arpin-Cribbie, C., Krommidas, C., & Theodorakis, Y. (2018). Aerobic exercise for adult patients with major depressive disorder in mental health services: A systematic review and meta-analysis. #emph[Depression and Anxiety];, #emph[36];(1), 39–53. #link("https://doi.org/10.1002/da.22842")

] <ref-Morres2018>
#block[
Myers, J., Kaykha, A., George, S., Abella, J., Zaheer, N., Lear, S., Yamazaki, T., & Froelicher, V. (2004). Fitness versus physical activity patterns in predicting mortality in men. #emph[The American Journal of Medicine];, #emph[117];(12), 912–918. #link("https://doi.org/10.1016/j.amjmed.2004.06.047")

] <ref-Myers2004>
#block[
Nezlek, J. B. (2008). An Introduction to Multilevel Modeling for Social and Personality Psychology. #emph[Social and Personality Psychology Compass];, #emph[2];(2), 842–860.

] <ref-nezlek2008introduction>
#block[
Nezlek, J. B., Schröder-Abé, M., & Schütz, A. (2006). Mehrebenenanalysen in der psychologischen Forschung. #emph[Psychologische Rundschau];, #emph[57];(4), 213–223. #link("https://doi.org/10.1026/0033-3042.57.4.213")

] <ref-Nezlek2006>
#block[
Paluska, S. A., & Schwenk, T. L. (2000). Physical Activity and Mental Health: Current Concepts. #emph[Sports Medicine];, #emph[29];(3), 167–180. #link("https://doi.org/10.2165/00007256-200029030-00003")

] <ref-Paluska2000>
#block[
Peterson, C., Semmel, A., Baeyer, C. von, Abramson, L. Y., Metalsky, G. I., & Seligman, M. E. P. (1982). The attributional Style Questionnaire. #emph[Cognitive Therapy and Research];, #emph[6];(3), 287–299. #link("https://doi.org/10.1007/bf01173577")

] <ref-Peterson1982>
#block[
Piggin, J. (2020). What Is Physical Activity? A Holistic Definition for Teachers, Researchers and Policy Makers. #emph[Frontiers in Sports and Active Living];, #emph[2];. #link("https://doi.org/10.3389/fspor.2020.00072")

] <ref-Piggin2020>
#block[
Pinheiro, J., Bates, D., & R Core Team. (2023). #emph[nlme: Linear and Nonlinear Mixed Effects Models];. #link("https://CRAN.R-project.org/package=nlme")

] <ref-nlme2023>
#block[
Robert Koch-Institut. (2022). #emph[Dashboard zu Gesundheit in Deutschland aktuell - GEDA 2019/2020];. Robert Koch-Institut. #link("https://doi.org/10.25646/9362")

] <ref-RKI_2022>
#block[
Sokolowski, K., Schmalt, H.-D., Langens, T. A., & Puca, R. M. (2000). Assessing Achievement, Affiliation, and Power Motives All at Once: The Multi-Motive Grid (MMG). #emph[Journal of Personality Assessment];, #emph[74];(1), 126–145. #link("https://doi.org/10.1207/s15327752jpa740109")

] <ref-Sokolowski2000>
#block[
Stetson, B. A., Beacham, A. O., Frommelt, S. J., Boutelle, K. N., Cole, J. D., Ziegler, C. H., & Looney, S. W. (2005). Exercise slips in high-risk situations and activity patterns in long-term exercisers: An application of the relapse prevention model. #emph[Annals of Behavioral Medicine];, #emph[30];(1), 25–35. #link("https://doi.org/10.1207/s15324796abm3001_4")

] <ref-Stetson2005>
#block[
Tracy, J. L., & Robins, R. W. (2007). Authentic And Hubristic Pride Scales. #emph[PsycTESTS Dataset];. #link("https://doi.org/10.1037/t06465-000")

] <ref-Tracy2007>
#block[
Warburton, D. E. R. (2006). Health benefits of physical activity: the evidence. #emph[Canadian Medical Association Journal];, #emph[174];(6), 801–809. #link("https://doi.org/10.1503/cmaj.051351")

] <ref-Warburton2006>
#block[
Watson, D., Clark, L. A., & Tellegen, A. (1988). Development and validation of brief measures of positive and negative affect: The PANAS scales. #emph[Journal of Personality and Social Psychology];, #emph[54];(6), 1063–1070. #link("https://doi.org/10.1037/0022-3514.54.6.1063")

] <ref-Watson1988>
#block[
Winter, D. G. (1994). #emph[Manual for scoring motive imagery in running text:(Version 4.2)];. Winter.

] <ref-winter1994manual>
#block[
World Health Organization (Hrsg.). (2010). #emph[Global Recommendations on Physical Activity for Health];. World Health Organization. #link("https://iris.who.int/bitstream/handle/10665/44399/9789241599979_eng.pdf?sequence=1")

] <ref-WHO2010>
] <refs>
#set par(first-line-indent: 0.5in, hanging-indent: 0in)
= Anhang
<anhang>


 
  
#set bibliography(style: "../\_extensions/wjschne/apaquarto/apa.csl") 


