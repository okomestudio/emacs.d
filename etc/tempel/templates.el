emacs-lisp-mode

(locvar-nameless ";; Local Variables:
;; nameless-aliases: ((\"\" . \"prefix\"))
;; End:")

lisp-data-mode

(dir-locals-python
 "(
 (nil
  . ((projectile-project-compilation-cmd . \"pip install -e .[dev,test] && pre-commit install\")
     (projectile-project-configure-cmd . \"pyenv virtualenv 3.12.2 $(basename $PWD) && pyenv local $(basename $PWD)\")
     (projectile-project-run-cmd . \"python -m mypackage \")
     (projectile-project-test-cmd . \"pytest\")))
 )
")

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
:END:
** Note
:PROPERTIES:
:ANKI_NOTE_TYPE: Basic (and reversed card)
:END:
*** Front
*** Back" n)

(locvar-org-ja "* Local variables :noexport:
# Local Variables:
# lsp-ltex-language: \"ja-JP\"
# End:")
