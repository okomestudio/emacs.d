emacs-lisp-mode

(locvar-nameless
 ";; Local Variables:" n
 ";; nameless-aliases: ((\"\" . \"prefix\"))" n
 ";; End:" n)

(template-begins
 ";;; " (s module) ".el --- " (s module) "  -*- lexical-binding: t -*-" n)

(template-copyright
 (include-file "elisp-copyright.el"))

(template-commentary
 ";;; Commentary:" n
 ";;" n
 ";;" n
 ";;" n
 ";;; Code:" n)

(template-ends
 "(provide '" (s module) ")" n
 ";;; " (s module) ".el ends here" n)

(template
 (p "module: " module t)
 (i template-begins)
 ";;" n
 (i template-copyright)
 ";;" n
 (i template-commentary)
 n n n
 (i template-ends))

gfm-mode

(readme (include-file "readme.md"))

lisp-data-mode

(projectile-cmds
 "(" n
 " (nil" n
 "  ." n
 "  ((projectile-project-compilation-cmd . \"pip install -e .[dev,test] && pre-commit install\")" n
 "   (projectile-project-configure-cmd . \"pyenv virtualenv 3.12.2 $(basename $PWD) && pyenv local $(basename $PWD)\")" n
 "   (projectile-project-run-cmd . \"python -m mypackage \")" n
 "   (projectile-project-test-cmd . \"pytest\")))" n
 " )" n)

org-mode

(anki-note-basic
 "* Anki note :anki:" n
 ":PROPERTIES:" n
 ":ANKI_DECK: Default" n
 ":ANKI_NOTE_TYPE: Basic" n
 ":END:" n
 "** Front" n
 "** Back" n)

(anki-note-basic-reverse
 "* Anki note :anki:" n
 ":PROPERTIES:" n
 ":ANKI_DECK: Default" n
 ":ANKI_NOTE_TYPE: Basic (and reversed card)" n
 ":END:" n
 "** Front" n
 "** Back" n)

(anki-note-cloze
 "* Anki note :anki:" n
 ":PROPERTIES:" n
 ":ANKI_DECK: Default" n
 ":ANKI_NOTE_TYPE: Cloze" n
 ":END:" n)

(anki-note-list
 "* Anki notes :anki:" n
 ":PROPERTIES:" n
 ":ANKI_DECK: Default" n
 ":END:" n
 "** Note" n
 ":PROPERTIES:" n
 ":ANKI_NOTE_TYPE: Cloze" n
 ":END:" n)

(begin "#+begin_" (s name) n> r> n "#+end_" name)

(begin-src-python
 (p "noweb-ref: " noweb-ref t)
 "#+begin_src python :noweb-ref " noweb-ref n> n>
 "#+end_src" n> n>
 (dotimes (_ (1+ (org-current-level)))
   (insert "*"))
 (insert " Unit tests :noexport:") n>
 ":PROPERTIES:" n>
 ":VISIBILITY: folded" n>
 ":END:" n> n>
 "#+begin_src python :noweb yes :exports none" n
 "  from __future__ import annotations" n
 "  from typing import *" n n
 "  import pytest" n n
 "  <<" noweb-ref ">>" n n
 "  class Test:" n
 "      def test(self):" n
 "          pass" n
 "#+end_src" n>)

(latex-equation "\\begin{equation}" r> n> "\\end{equation}")

(latex-equation-star "\\begin{equation*}" r> n> "\\end{equation*}")

(locvar-org-ja
 "* Local variables :noexport:" n
 "# Local Variables:" n
 "# lsp-ltex-language: \"ja-JP\"" n
 "# End:" n)

(page "(p. " (deactivate-input-method) p ")")

(pages "(pp. " (deactivate-input-method) p "–" p ")")

(solution
 (org-insert-heading nil nil (1+ (org-current-level))) "Solution" n
 ":PROPERTIES:" n
 ":VISIBILITY: folded" n
 ":END:" n n)

(transclude-general
 "#+transclude: " r>
 " :level 2 :exclude-elements \"drawer keyword\"")

(transclude-only-contents
 "#+transclude: " r> " :only-contents")

bash-ts-mode

(template-bash (include-file "bash.sh"))
