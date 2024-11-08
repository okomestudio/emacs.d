emacs-lisp-mode

(template-begins
 ";;; " module ".el --- " module "  -*- lexical-binding: t -*-" n)

(template-copyright
 (include-file "elisp-copyright.el"))

(template-commentary
 ";;; Commentary:" n
 ";;" n
 ";;" n
 ";;" n
 ";;; Code:" n)

(template-ends
 "(provide '" module ")" n
 ";;; " module ".el ends here" n)

(template
 (p "module: " module t)
 (i template-begins)
 ";;" n
 (i template-copyright)
 ";;" n
 (i template-commentary)
 n n n
 (i template-ends))

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

(solution (org-insert-heading nil nil (1+ (org-current-level)))
          "Solution" n
          ":PROPERTIES:" n
          ":VISIBILITY: folded" n
          ":END:" n n)

(page "(p. " (deactivate-input-method) p ")")
(pages "(pp. " (deactivate-input-method) p "–" p ")")

(begin "#+begin_" (s name) n> r> n "#+end_" name)

(latex-equation "\\begin{equation}" r> n> "\\end{equation}")
(latex-equation-star "\\begin{equation*}" r> n> "\\end{equation*}")

;; "* Python source code" n n
;; "** Unit tests" n
;; (progn
;;   (org-insert-heading nil nil (1+ (org-current-level)))
;;   (insert "Unit tests :noexport:")) n
;; (org-insert-heading nil nil )

(begin-src-python
 (p "noweb-ref: " noweb-ref t)
 "#+begin_src python :noweb-ref " noweb-ref n> n>
 "#+end_src" n> n>
 (dotimes (_ (1+ (org-current-level))) (insert "*"))
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

(transclude-general "#+transclude: " r>
                    " :level 2 :exclude-elements \"drawer keyword\"")
(transclude-only-contents "#+transclude: " r> " :only-contents")

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

(locvar-org-ja "* Local variables :noexport:
# Local Variables:
# lsp-ltex-language: \"ja-JP\"
# End:")

bash-ts-mode

(template-bash (include-file "bash.sh"))
