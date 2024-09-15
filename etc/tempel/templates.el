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

(src-pytest-runner "#+begin_src shell :var in=\"" p "\" :exports none :results output
ob-pytest \"$in\"
#+end_src" n)
(src-python "#+begin_src python :no-web-ref " p n "
#+end_src")
(src-pytest "#+begin_src python :noweb yes :exports none :tangle " p n "
#+end_src")

(transclude-general "#+transclude: " r>
                    " :level 2 :exclude-elements \"drawer keyword\"")
(transclude-only-contents "#+transclude: " r> " :only-contents")

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
