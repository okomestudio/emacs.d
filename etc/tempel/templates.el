org-mode

(latex-equation "\\begin{equation}" r> n> "\\end{equation}")
(latex-equation-star "\\begin{equation*}" r> n> "\\end{equation*}")

(transclude "#+transclude: " r> " :level 2 :exclude-elements \"drawer keyword\"")

(anki-note-basic "* Anki note :anki:
:PROPERTIES:
:VISIBILITY: folded
:ANKI_DECK: Default
:ANKI_NOTE_TYPE: Basic
:END:
** Front
** Back" n)

(anki-note-basic-reverse "* Anki note :anki:
:PROPERTIES:
:VISIBILITY: folded
:ANKI_DECK: Default
:ANKI_NOTE_TYPE: Basic (and reversed card)
:END:
** Front
** Back" n)

(anki-note-cloze "* Anki note :anki:
:PROPERTIES:
:VISIBILITY: folded
:ANKI_DECK: Default
:ANKI_NOTE_TYPE: Cloze
:END:
** Text" n)

(anki-note-list "* Anki notes :anki:
:PROPERTIES:
:VISIBILITY: folded
:ANKI_DECK: Default
:ID:       " (org-id-uuid) n
":END:
** Note
:PROPERTIES:
:ANKI_NOTE_TYPE: Basic (and reversed card)
:END:
*** Front
*** Back" n)
