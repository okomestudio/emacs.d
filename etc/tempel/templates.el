text-mode
(page (p "Page: " page t)
      (zenkaku "(p. " "（") (s page) (zenkaku ")" "頁）")
      :ann "Page number"
      :doc "Insert a page number.")
(pages (p "Page start: " start t) (p "Page end: " end t)
       (zenkaku "(pp. " "（") (s start) "-" (s end) (zenkaku ")" "頁）")
       :ann "Page number range"
       :doc "Insert a page number range.")

emacs-lisp-mode
(autoload ";;;###autoload")
(file-header ";;; " (s module) ".el --- " (s module)
             "  -*- lexical-binding: t -*-" n)
(file-copyright (include-file "elisp-copyright.el"))
(file-commentary ";;; Commentary:" n
                 ";;" n
                 ";;" n
                 ";;" n
                 ";;; Code:" n)
(file-footer "(provide '" (s module) ")" n
             ";;; " (s module) ".el ends here" n)
(file-init (p "module: " module t)
           (i file-header)
           ";;" n
           (i file-copyright)
           ";;" n
           (i file-commentary)
           n n n
           (i file-footer))
(file-local-variables ";; Local Variables:" n
                      ";; nameless-aliases: ((\"\" . \"prefix\"))" n
                      ";; foo: bar" n
                      ";; End:" n)

gfm-mode
(readme (include-file "readme.md"))

lisp-data-mode
(elisp-classic (include-file "ldm-elisp-classic.el"))
(elisp-default (include-file "ldm-elisp-default.el"))
(python-default (include-file "ldm-python-default.el"))

org-mode
(anki-note-basic "* Anki note :anki:" n
                 ":PROPERTIES:" n
                 ":ANKI_DECK: Default" n
                 ":ANKI_NOTE_TYPE: Basic" n
                 ":END:" n
                 "** Front" n
                 "** Back" n)
(anki-note-basic-reverse "* Anki note :anki:" n
                         ":PROPERTIES:" n
                         ":ANKI_DECK: Default" n
                         ":ANKI_NOTE_TYPE: Basic (and reversed card)" n
                         ":END:" n
                         "** Front" n
                         "** Back" n)
(anki-note-cloze "* Anki note :anki:" n
                 ":PROPERTIES:" n
                 ":ANKI_DECK: Default" n
                 ":ANKI_NOTE_TYPE: Cloze" n
                 ":END:" n)
(anki-note-list "* Anki notes :anki:" n
                ":PROPERTIES:" n
                ":ANKI_DECK: Default" n
                ":END:" n
                "** Note" n
                ":PROPERTIES:" n
                ":ANKI_NOTE_TYPE: Cloze" n
                ":END:" n)
(block "#+begin_" (s name) n> r> n "#+end_" name)
(block-python (p "noweb-ref: " noweb-ref t)
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
(file-local-variables "* Local variables :noexport:" n
                      "# Local Variables:" n
                      "# foo: bar" n
                      "# End:" n)
(file-local-variables-ja "* Local variables :noexport:" n
                         "# Local Variables:" n
                         "# lsp-ltex-language: \"ja-JP\"" n
                         "# foo: bar" n
                         "# End:" n)
(latex-equation "\\begin{equation}" r> n> "\\end{equation}")
(latex-equation-star "\\begin{equation*}" r> n> "\\end{equation*}")
(list-item "  - " p n>)
(list-item-with-description "  - " p " :: " p n>)
(name "#+name: ")
(solution (org-insert-heading nil nil (1+ (org-current-level))) "Solution" n
          ":PROPERTIES:" n
          ":VISIBILITY: folded" n
          ":END:" n n)
(target "<<" r> ">>")
(timing (p "Time (e.g., hh:mm:ss): " hhmmss t)
        (zenkaku "(" "（") (s hhmmss) (zenkaku ")" "）"))
(transclude-general "#+transclude: " r>
                    " :level 2 :exclude-elements \"drawer keyword\"")
(transclude-only-contents "#+transclude: " r> " :only-contents")

bash-ts-mode
(file-init (include-file "bash.sh"))
