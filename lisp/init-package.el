;;; init-package.el --- Package Initialization  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Initialize with `package'.
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

(provide 'init-package)
;;; init-package.el ends here
