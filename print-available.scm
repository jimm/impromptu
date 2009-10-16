;;; See also audio-nodes.scm

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

(io:print-midi-sources)
(io:print-midi-destinations)
