emacs-lisp-mode

(elisp-begins-here ";;; replace-me.el --- Replace Me  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
")

(elisp-copyright ";; Copyright (C) 2024 Taro Sato
;;
;; Author: Taro Sato <okomestudio@gmail.com>
;; URL: https://github.com/okomestudio/
;; Version: 0.1
;; Keywords:
;; Package-Requires: ((emacs \"29.1\"))
;;
;;; License:
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
")

(elisp-ends-here "(provide 'replace-me)
;;; replace-me ends here")

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

(begin "#+begin_" (s name) n> r> n "#+end_" name)

(latex-equation "\\begin{equation}" r> n> "\\end{equation}")
(latex-equation-star "\\begin{equation*}" r> n> "\\end{equation*}")

(begin-src-python
 "#+begin_src python :noweb-ref " (s noweb-ref) n n
 "#+end_src" n n
 "* Unit tests" n
 ":PROPERTIES:" n
 ":VISIBILITY: folded" n
 ":END:" n n
 "#+begin_src shell :var in=\"" (s py) "\" :exports none :results output" n
 "  ob-pytest \"$in\"" n
 "#+end_src" n n
 "#+begin_src python :noweb yes :exports none :tangle " py n
 "  from typing import *" n n
 "  import pytest" n n
 "  <<" noweb-ref ">>" n n
 "  class Test:" n
 "      def test(self):" n
 "          pass" n
 "#+end_src" n)

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
