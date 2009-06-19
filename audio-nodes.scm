(define my-print-au-info
  (lambda (unit-type-const title)
     (print title)
     (au:print-audiounits unit-type-const)))  ;; cals au:get-node-strings

(begin
  ; See kAudioUnitType_* constants defined in AUComponent.h
  (my-print-au-info "auou" "Outputs")
  (my-print-au-info "aumu" "Audio Units")
  (my-print-au-info "aufx" "FX Units")
  (my-print-au-info "aufc" "Format Converters, Spliters, and Mergers")
  (my-print-au-info "aumx" "Mixers")
  (my-print-au-info "aupn" "Panners")
  (my-print-au-info "auol" "Offline Effects")
  (my-print-au-info "augn" "Generators")
)

(au:print-graph)

; create these dynamically by using au:get-node-strings and picking apart the
; returned strings
(define *my-audio-units*
  '(
    (*free-alpha*              "aumu" "AlpF" "LinP")
    (*crystal*                 "aumu" "AtFr" "GOSW")
    (*remedy*                  "aumu" "KSRM" "KTOS")
    (*kontakt-player*          "aumu" "NiP2" "-NI-")
    (*orca*                    "aumu" "ORCA" "FXPN")
    (*automat*                 "aumu" "auto" "Alfa")
    (*apple-dls*               "aumu" "dls " "appl")))
(define *my-fx-units*
  '(
    (*hf-ring-mod*             "aufx" "=\x09\x03\x00" "PiMa")
    (*average-injector*        "aufx" "=\x0B\x04\x00" "PiMa")
    (*generic-effect*          "aufx" "=\x0C\x04\x00" "PiMa")
    (*chamber-verb*            "aufx" "=\x0D\x04\x00" "PiMa")
    (*resonation*              "aufx" "=\x16\x03\x00" "PiMa")
    (*jet*                     "aufx" "=\x17\x03\x00" "PiMa")
    (*feedback-network*        "aufx" "=$\x01\x00" "PiMa")
    (*filter-taps*             "aufx" "=%\x01\x00" "PiMa")
    (*spectral-filter*         "aufx" "='\x01\x00" "PiMa")
    (*nebula*                  "aufx" "=/\x01\x00" "PiMa")
    (*resosweep*               "aufx" "=7\x04\x00" "PiMa")
    (*limi*                    "aufx" "=J\x01\x00" "PiMa")
    (*amplitube-2duole*        "aufx" "Adl2" "ikm_")
    (*ampeg-svxunole*          "aufx" "Aul1" "ikm_")
    (*au-bandpass*             "aufx" "bpas" "appl")
    (*au-dynamics-processor*   "aufx" "dcmp" "appl")
    (*au-delay*                "aufx" "dely" "appl")
    (*au-filter*               "aufx" "filt" "appl")
    (*au-graphic-eq*           "aufx" "greq" "appl")
    (*au-hipass*               "aufx" "hpas" "appl")
    (*au-high-shelf-filter*    "aufx" "hshf" "appl")
    (*au-peak-limiter*         "aufx" "lmtr" "appl")
    (*au-lowpass*              "aufx" "lpas" "appl")
    (*au-lows-helf-filter*     "aufx" "lshf" "appl")
    (*au-multiband-compressor* "aufx" "mcmp" "appl")
    (*au-matrix-reverb*        "aufx" "mrev" "appl")
    (*au-net-send*             "aufx" "nsnd" "appl")
    (*au-parametric-eq*        "aufx" "pmeq" "appl")
    (*au-sample-delay*         "aufx" "sdly" "appl")
    (*au-pitch*                "aufx" "tmpt" "appl")))

(define-macro (my-make-node name)
  `(define ,name (let ((vals (assoc ',name *my-nodes*)))
		   (au:make-node (cadr vals) (caddr vals) (cadddr vals)))))

; testing

(define test-vals (assoc '*jet* *my-nodes*))
(print test-vals)
(define *jet* (au:make-node (cadr test-vals) (caddr test-vals) (cadddr test-vals)))))
(print *jet*)

; testing

(my-make-node *jet*)
