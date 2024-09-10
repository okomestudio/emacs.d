;;; org-with-lualatex.el --- Org with LuaLaTeX
;;; Commentary:
;;
;; `org-mode' with LuaLaTeX initialization.
;;
;;; Code:

(use-package org
  :custom ((org-preview-latex-image-directory ".ltximg/"))
  :ensure-system-package
  (latex . "sudo apt install -y texlive texlive-latex-extra texlive-lang-cjk texlive-extra-utils texlive-luatex texlive-science")
  (pdfcropmargins . "pip install pdfCropMargins")

  :config
  ;; LATEX PREVIEW
  (setopt org-preview-latex-default-process 'lualatexpdf)
  ;; (setq org-latex-inputenc-alist '(("utf8" . "utf8x")))
  ;; (add-to-list 'org-latex-packages-alist '("" "unicode-math"))
  (add-to-list
   'org-preview-latex-process-alist
   '(lualatexpdf
     :programs ("lualatex" "dvisvgm")
     :description "pdf > svg"
     :message "you need to install the programs: lualatex and dvisvgm."
     :image-input-type "pdf"
     :image-output-type "svg"
     :image-size-adjust (1.7 . 1.5)
     :latex-compiler ("lualatex -interaction nonstopmode --shell-escape -output-directory %o %f")
     :image-converter ("pdfcropmargins -v -p 0 -a -5 %f -o /tmp/cropped.pdf ; dvisvgm -P /tmp/cropped.pdf -n -b min -c %S -o %O")))
  (add-to-list
   'org-preview-latex-process-alist
   '(lualatexdvi
     :programs ("lualatex" "dvisvgm")
     :description "dvi > svg"
     :message "you need to install the programs: lualatex and dvisvgm."
     :image-input-type "dvi"
     :image-output-type "svg"
     :image-size-adjust (1.7 . 1.5)
     :latex-compiler ("dvilualatex -interaction nonstopmode --shell-escape -output-directory %o %f")
     :image-converter ("dvisvgm %f -n -b min -c %S -o %O"))))

(use-package ox-latex
  :straight nil
  :after ox
  :custom ((org-latex-pdf-process
            '("lualatex -interaction nonstopmode -shell-escape %f"
              "lualatex -interaction nonstopmode -shell-escape %f"))))

;;; org-with-lualatex.el ends here
