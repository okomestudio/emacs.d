;;; subsys-weather.el --- Weather Subsystem  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configure the weather subsystem.
;;
;; Checkout
;; --------
;;
;; - sparkweather (github.com/aglet/sparkweather)
;;
;;; Code:

(use-package boem-weather
  ;; A weather forecast client for the Open Metro API.
  ;;
  ;; Type `M-x boem-weather RET' to open the child frame.
  ;;
  :custom (;; San Francisco, CA
           (boem-weather-latitude 37.7793)
           (boem-weather-longitude -122.4193)
           (boem-weather-time-zone "America/Los_Angeles") )
  :commands (boem-weather)
  :config
  (defun ok-boem-weather--url (fun &rest _rest)
    (let ((url (apply fun _rest))
          (params '(("temperature_unit" "fahrenheit")
                    ("wind_speed_unit" "mph"))))
      (concat url "&" (url-build-query-string params))))
  (advice-add #'boem-weather--url :around #'ok-boem-weather--url))

(provide 'subsys-weather)
;;; subsys-weather.el ends here
