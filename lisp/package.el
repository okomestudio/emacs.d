;;; package.el --- Package  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configure `use-package' with `package'.
;;
;;; Code:

(require 'package)

(defvar package-archives)
(setopt package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                           ("melpa" . "https://melpa.org/packages/")
                           ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(unless (bound-and-true-p package--initialize)
  (package-initialize))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-and-compile
  (setopt use-package-always-ensure t))

(eval-when-compile
  (require 'use-package))

;;; package.el ends here
