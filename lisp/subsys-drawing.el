;;; subsys-drawing.el --- Drawing Subsystem  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Set up the drawing subsystem.
;;
;;; Code:

(use-package org-excalidraw
  ;; Excalidraw in Org.
  ;;
  ;; A typical usage goes like this. In an Org document,
  ;;
  ;;   M-x org-excalidraw-create-drawing
  ;;
  ;; To create an Excalidraw file. Edit on the Chrome progressive web
  ;; app (PWA). When done, save the .excalidraw file. Also export an
  ;; SVG file to ~/export.svg. Back in the Org document, use
  ;;
  ;;   M-x org-excalidraw-ok-copy-svg-to-cwd
  ;;
  ;; to copy the export file to the local directory and also create a
  ;; link.
  ;;
  ;; ---------
  ;;
  ;; Note that there are a couple of versions of `org-exalidraw'. The
  ;; @wdavew version is clean but @4honor version is more
  ;; customizable, in particular, two options exist for SVG export
  ;; (`kroki' and `excalidraw_export').
  ;;
  ;; In the end, SVG export has a general issue in font handling. The
  ;; only reliable solution is to embed fonts into the SVG file
  ;; itself. `excalidraw_export' does not work reliably, and `kroki'
  ;; doesn't embed fonts.
  ;;
  ;; Excalidraw now supports font-embedded SVGs by default, and the
  ;; implementation is efficient, only embedding the glyphs actually
  ;; used. The use of manual export is thus currently recommended
  ;; approach.
  ;;
  :commands (org-excalidraw-create-drawing)
  :hook (org-mode . (lambda () (require 'org-excalidraw)))
  :config
  (defun org-excalidraw-ok-copy-svg-to-cwd (exported-file new-file)
    "Copy an Excalidraw-export SVG file to the current working directory."
    (interactive
     (list (read-file-name "Exported SVG file: "
                           (expand-file-name "~/") nil nil "export.svg")
           (read-string "New SVG file name: ")))
    (let ((new-file (or (and (string-suffix-p ".svg" new-file t) new-file)
                        (format "%s.svg" new-file))))
      (copy-file exported-file (expand-file-name new-file))
      (insert (format "#+name: %1$s\n[[./%1$s]]" new-file)))))

(provide 'subsys-drawing)
;;; subsys-drawing.el ends here
