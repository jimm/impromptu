;; MIDI constants.
;;
;; Copy this file to ~/Library/Application Support/Impromptu and it will be
;; loaded when impromptu starts.

;; Number of MIDI channels
(define *midi-channels* 16)
;; Number of note per MIDI channel
(define *notes-per-channel* 128)

;; Standard MIDI File meta event defs.
(define *meta-event* #xff)
(define *meta-seq-num* 0)
(define *meta-text* 1)
(define *meta-copyright* 2)
(define *meta-seq-name* 3)
(define *meta-instrument* 4)
(define *meta-lyric* 5)
(define *meta-marker* 6)
(define *meta-cue* 7)
(define *meta-track-end* 47)
(define *meta-set-tempo* 81)
(define *meta-smpte* 83)
(define *meta-time-sig* 88)
(define *meta-key-sig* 89)
(define *meta-seq-specif* 127)

;; Channel messages

;; Note that events sent by io:midi-out use just the high nibble, shifted down
;; four bits. The values predefined by Impromptu are note-on (*io:midi-on*),
;; note-off (*io:midi-off*), control change (*io:midi-cc*) and program change
;; (*io:midi-pc*) - of course you can supply your own hex number for message
;; types that have no constant (i.e. pitch bend messages are #xE).

;; Note, val
(define *note-off* #x80)
;; Note, val
(define *note-on* #x90)
;; Note, val
(define *poly-pressure* #xa0)
;; Controller number, val
(define *controller* #xbo)
;; Program number
(define *program-change* #xc0)
;; Channel pressure
(define *channel-pressure* #xd0)
;; LSB, MSB
(define *pitch-bend* #xe0)

;; System common messages
;; System exclusive start
(define *sysex* #xf0)
;; Beats from top: LSB/MSB 6 ticks = 1 beat
(define *song-pointer* #xf2)
;; Val = number of song
(define *song-select* #xf3)
;; Tune request
(define *tune-request* #xf6)
;; End of system exclusive
(define *eox* #xf7)

;; System realtime messages
;; MIDI clock (24 per quarter note)
(define *clock* #xf8)
;; Sequence start
(define *start* #xfa)
;; sequence continue
(define *continue* #xfb)
;; Sequence stop
(define *stop* #xfc)
;; Active sensing (sent every 300 ms when nothing else being sent)
(define *active-sense* #xfe)
;; System reset
(define *system-reset* #xff)

;; Controller numbers
;; 0 - 31 = continuous, MSB
;; 32 - 63 = continuous, LSB
;; 64 - 97 = switches
(define *cc-mod-wheel* 1)
(define *cc-breath-controller* 2)
(define *cc-foot-controller* 4)
(define *cc-portamento-time* 5)
(define *cc-data-entry-msb* 6)
(define *cc-volume* 7)
(define *cc-balance* 8)
(define *cc-pan* 10)
(define *cc-expression-controller* 11)
(define *cc-gen-purpose-1* 16)
(define *cc-gen-purpose-2* 17)
(define *cc-gen-purpose-3* 18)
(define *cc-gen-purpose-4* 19)

;; [32 - 63] are LSB for [0 - 31]
(define *cc-data-entry-lsb* 38)

;; Momentaries:
(define *cc-sustain* 64)
(define *cc-portamento* 65)
(define *cc-sustenuto* 66)
(define *cc-soft-pedal* 67)
(define *cc-hold-2* 69)
(define *cc-gen-purpose-5* 50)
(define *cc-gen-purpose-6* 51)
(define *cc-gen-purpose-7* 52)
(define *cc-gen-purpose-8* 53)
(define *cc-tremelo-depth* 92)
(define *cc-chorus-depth* 93)
(define *cc-detune-depth* 94)
(define *cc-phaser-depth* 95)
(define *cc-data-increment* 96)
(define *cc-data-decrement* 97)
(define *cc-nreg-param-lsb* 98)
(define *cc-nreg-param-msb* 99)
(define *cc-reg-param-lsb* 100)
(define *cc-reg-param-msb* 101)

;; Channel mode message values
;; Val 0 == off, 127 == on
(define *cm-local-control* 122)
(define *cm-all-notes-off* 123)         ; val must be 0
(define *cm-omni-mode-off* 124)         ; val must be 0
(define *cm-omni-mode-on* 125)		; val must be 0
(define *cm-mono-mode-on* 126)		; val = number of chans
(define *cm-poly-mode-on* 127)		; val must be 0

;; Controller names
(define *controller-names* '(
"0"
"Modulation"
"Breath Control"
"3"
"Foot Controller"
"Portamento Time"
"Data Entry"
"Volume"
"Balance"
"9"
"Pan"
"Expression Control"
"12" "13" "14" "15"
"General Controller 1"
"General Controller 2"
"General Controller 3"
"General Controller 4"
"20" "21" "22" "23" "24" "25" "26" "27" "28" "29"
"30" "31"
"32" "33" "34" "35" "36" "37" "38" "39" "40" "41"
"42" "43" "44" "45" "46" "47" "48" "49" "50" "51"
"52" "53" "54" "55" "56" "57" "58" "59" "60" "61"
"62" "63"
"Sustain Pedal"
"Portamento"
"Sostenuto"
"Soft Pedal"
"68"
"Hold 2"
"70" "71" "72" "73" "74" "75" "76" "77" "78" "79"
"General Controller 5"
"Tempo Change"
"General Controller 7"
"General Controller 8"
"84" "85" "86" "87" "88" "89" "90"
"External Effects Depth"
"Tremolo Depth"
"Chorus Depth"
"Detune (Celeste) Depth"
"Phaser Depth"
"Data Increment"
"Data Decrement"
"Non-Registered Param LSB"
"Non-Registered Param MSB"
"Registered Param LSB"
"Registered Param MSB"
"102" "103" "104" "105" "106" "107" "108" "109"
"110" "111" "112" "113" "114" "115" "116" "117"
"118" "119" "120"
"Reset All Controllers"
"Local Control"
"All Notes Off"
"Omni Mode Off"
"Omni Mode On"
"Mono Mode On"
"Poly Mode On"
))

;; General MIDI patch names
(define *gm-patch-names* '(
;; Pianos
"Acoustic Grand Piano"
"Bright Acoustic Piano"
"Electric Grand Piano"
"Honky-tonk Piano"
"Electric Piano 1"
"Electric Piano 2"
"Harpsichord"
"Clavichord"
;; Tuned Idiophones
 "Celesta"
"Glockenspiel"
"Music Box"
"Vibraphone"
"Marimba"
"Xylophone"
"Tubular Bells"
"Dulcimer"
;; Organs
 "Drawbar Organ"
"Percussive Organ"
"Rock Organ"
"Church Organ"
"Reed Organ"
"Accordion"
"Harmonica"
"Tango Accordion"
;; Guitars
 "Acoustic Guitar (nylon)"
"Acoustic Guitar (steel)"
"Electric Guitar (jazz)"
"Electric Guitar (clean)"
"Electric Guitar (muted)"
"Overdriven Guitar"
"Distortion Guitar"
"Guitar harmonics"
;; Basses
"Acoustic Bass"
"Electric Bass (finger)"
"Electric Bass (pick)"
"Fretless Bass"
"Slap Bass 1"
"Slap Bass 2"
"Synth Bass 1"
"Synth Bass 2"
;; Strings
"Violin"
"Viola"
"Cello"
"Contrabass"
"Tremolo Strings"
"Pizzicato Strings"
"Orchestral Harp"
"Timpani"
;; Ensemble strings and voices
"String Ensemble 1"
"String Ensemble 2"
"SynthStrings 1"
"SynthStrings 2"
"Choir Aahs"
"Voice Oohs"
"Synth Voice"
"Orchestra Hit"
;; Brass
"Trumpet"
"Trombone"
"Tuba"
"Muted Trumpet"
"French Horn"
"Brass Section"
"SynthBrass 1"
"SynthBrass 2"
;; Reeds
"Soprano Sax"                           ; 64
"Alto Sax"
"Tenor Sax"
"Baritone Sax"
"Oboe"
"English Horn"
"Bassoon"
"Clarinet"
;; Pipes
"Piccolo"
"Flute"
"Recorder"
"Pan Flute"
"Blown Bottle"
"Shakuhachi"
"Whistle"
"Ocarina"
;; Synth Leads
"Lead 1 (square)"
"Lead 2 (sawtooth)"
"Lead 3 (calliope)"
"Lead 4 (chiff)"
"Lead 5 (charang)"
"Lead 6 (voice)"
"Lead 7 (fifths)"
"Lead 8 (bass + lead)"
;; Synth Pads
"Pad 1 (new age)"
"Pad 2 (warm)"
"Pad 3 (polysynth)"
"Pad 4 (choir)"
"Pad 5 (bowed)"
"Pad 6 (metallic)"
"Pad 7 (halo)"
"Pad 8 (sweep)"
;; Effects
"FX 1 (rain)"
"FX 2 (soundtrack)"
"FX 3 (crystal)"
"FX 4 (atmosphere)"
"FX 5 (brightness)"
"FX 6 (goblins)"
"FX 7 (echoes)"
"FX 8 (sci-fi)"
;; Ethnic
"Sitar"
"Banjo"
"Shamisen"
"Koto"
"Kalimba"
"Bag pipe"
"Fiddle"
"Shanai"
;; Percussion
"Tinkle Bell"
"Agogo"
"Steel Drums"
"Woodblock"
"Taiko Drum"
"Melodic Tom"
"Synth Drum"
"Reverse Cymbal"
;; Sound Effects
"Guitar Fret Noise"
"Breath Noise"
"Seashore"
"Bird Tweet"
"Telephone Ring"
"Helicopter"
"Applause"
"Gunshot"
))

;; Pianos
(define *gm-pc-acoustic-grand-piano* 0)
(define *gm-pc-bright-acoustic-piano* 1)
(define *gm-pc-electric-grand-piano* 2)
(define *gm-pc-honky-tonk-piano* 3)
(define *gm-pc-electric-piano-1* 4)
(define *gm-pc-electric-piano-2* 5)
(define *gm-pc-harpsichord* 6)
(define *gm-pc-clavichord* 7)
;; Tuned Idiophones
(define *gm-pc-celesta* 8)
(define *gm-pc-glockenspiel* 9)
(define *gm-pc-music-box* 10)
(define *gm-pc-vibraphone* 11)
(define *gm-pc-marimba* 12)
(define *gm-pc-xylophone* 13)
(define *gm-pc-tubular-bells* 14)
(define *gm-pc-dulcimer* 15)
;; Organs
(define *gm-pc-drawbar-organ* 16)
(define *gm-pc-percussive-organ* 17)
(define *gm-pc-rock-organ* 18)
(define *gm-pc-church-organ* 19)
(define *gm-pc-reed-organ* 20)
(define *gm-pc-accordion* 21)
(define *gm-pc-harmonica* 22)
(define *gm-pc-tango-accordion* 23)
;; Guitars
(define *gm-pc-acoustic-guitar-nylon* 24)
(define *gm-pc-acoustic-guitar-steel* 25)
(define *gm-pc-electric-guitar-jazz* 26)
(define *gm-pc-electric-guitar-clean* 27)
(define *gm-pc-electric-guitar-muted* 28)
(define *gm-pc-overdriven-guitar* 29)
(define *gm-pc-distortion-guitar* 30)
(define *gm-pc-guitar-harmonics* 31)
;; Basses
(define *gm-pc-acoustic-bass* 32)
(define *gm-pc-electric-bass-finger* 33)
(define *gm-pc-electric-bass-pick* 34)
(define *gm-pc-fretless-bass* 35)
(define *gm-pc-slap-bass-1* 36)
(define *gm-pc-slap-bass-2* 37)
(define *gm-pc-synth-bass-1* 38)
(define *gm-pc-synth-bass-2* 39)
;; Strings
(define *gm-pc-violin* 40)
(define *gm-pc-viola* 41)
(define *gm-pc-cello* 42)
(define *gm-pc-contrabass* 43)
(define *gm-pc-tremolo-strings* 44)
(define *gm-pc-pizzicato-strings* 45)
(define *gm-pc-orchestral-harp* 46)
(define *gm-pc-timpani* 47)
;; Ensemble strings and voices
(define *gm-pc-string-ensemble-1* 48)
(define *gm-pc-string-ensemble-2* 49)
(define *gm-pc-synthstrings-1* 50)
(define *gm-pc-synthstrings-2* 51)
(define *gm-pc-choir-aahs* 52)
(define *gm-pc-voice-oohs* 53)
(define *gm-pc-synth-voice* 54)
(define *gm-pc-orchestra-hit* 55)
;; Brass
(define *gm-pc-trumpet* 56)
(define *gm-pc-trombone* 57)
(define *gm-pc-tuba* 58)
(define *gm-pc-muted-trumpet* 59)
(define *gm-pc-french-horn* 60)
(define *gm-pc-brass-section* 61)
(define *gm-pc-synthbrass-1* 62)
(define *gm-pc-synthbrass-2* 63)
;; Reeds
(define *gm-pc-soprano-sax* 64)
(define *gm-pc-alto-sax* 65)
(define *gm-pc-tenor-sax* 66)
(define *gm-pc-baritone-sax* 67)
(define *gm-pc-oboe* 68)
(define *gm-pc-english-horn* 69)
(define *gm-pc-bassoon* 70)
(define *gm-pc-clarinet* 71)
;; Pipes
(define *gm-pc-piccolo* 72)
(define *gm-pc-flute* 73)
(define *gm-pc-recorder* 74)
(define *gm-pc-pan-flute* 75)
(define *gm-pc-blown-bottle* 76)
(define *gm-pc-shakuhachi* 77)
(define *gm-pc-whistle* 78)
(define *gm-pc-ocarina* 79)
;; Synth Leads
(define *gm-pc-lead-1-square* 80)
(define *gm-pc-lead-2-sawtooth* 81)
(define *gm-pc-lead-3-calliope* 82)
(define *gm-pc-lead-4-chiff* 83)
(define *gm-pc-lead-5-charang* 84)
(define *gm-pc-lead-6-voice* 85)
(define *gm-pc-lead-7-fifths* 86)
(define *gm-pc-lead-8-bass+lead* 87)
;; Synth Pads
(define *gm-pc-pad-1-new-age* 88)
(define *gm-pc-pad-2-warm* 89)
(define *gm-pc-pad-3-polysynth* 90)
(define *gm-pc-pad-4-choir* 91)
(define *gm-pc-pad-5-bowed* 92)
(define *gm-pc-pad-6-metallic* 93)
(define *gm-pc-pad-7-halo* 94)
(define *gm-pc-pad-8-sweep* 95)
;; Effects
(define *gm-pc-fx-1-rain* 96)
(define *gm-pc-fx-2-soundtrack* 97)
(define *gm-pc-fx-3-crystal* 98)
(define *gm-pc-fx-4-atmosphere* 99)
(define *gm-pc-fx-5-brightness* 100)
(define *gm-pc-fx-6-goblins* 101)
(define *gm-pc-fx-7-echoes* 102)
(define *gm-pc-fx-8-sci-fi* 103)
;; Ethnic
(define *gm-pc-sitar* 104)
(define *gm-pc-banjo* 105)
(define *gm-pc-shamisen* 106)
(define *gm-pc-koto* 107)
(define *gm-pc-kalimba* 108)
(define *gm-pc-bag-pipe* 109)
(define *gm-pc-fiddle* 110)
(define *gm-pc-shanai* 111)
;; Percussion
(define *gm-pc-tinkle-bell* 112)
(define *gm-pc-agogo* 113)
(define *gm-pc-steel-drums* 114)
(define *gm-pc-woodblock* 115)
(define *gm-pc-taiko-drum* 116)
(define *gm-pc-melodic-tom* 117)
(define *gm-pc-synth-drum* 118)
(define *gm-pc-reverse-cymbal* 119)
;; Sound Effects
(define *gm-pc-guitar-fret-noise* 120)
(define *gm-pc-breath-noise* 121)
(define *gm-pc-seashore* 122)
(define *gm-pc-bird-tweet* 123)
(define *gm-pc-telephone-ring* 124)
(define *gm-pc-helicopter* 125)
(define *gm-pc-applause* 126)
(define *gm-pc-gunshot* 127)

;; GM drum notes start at 35 (C), so subtrack GM-DRUM-NOTE-LOWEST from your
;; note number before using this array.
(define *gm-drum-note-lowest* 35)

; GM drum note numbers
(define *gm-kick* 35)
(define *gm-kick-2* 36)
(define *gm-side-stick* 37)
(define *gm-snare* 38)
(define *gm-hand-clap* 39)
(define *gm-snare-2* 40)
(define *gm-low-floor-tom* 41)
(define *gm-closed-hi-hat* 42)
(define *gm-hi-floor-tom* 43)
(define *gm-pedal-hi-hat* 44)
(define *gm-low-tom* 45)
(define *gm-open-hi-hat* 46)
(define *gm-low-mid-tom* 47)
(define *gm-hi-mid-tom* 48)
(define *gm-crash* 49)
(define *gm-hi-tom* 50)
(define *gm-ride* 51)
(define *gm-chinese* 52)
(define *gm-ride-bell* 53)
(define *gm-tambourine* 54)
(define *gm-splash* 55)
(define *gm-cowbell* 56)
(define *gm-crash-2* 57)
(define *gm-vibraslap* 58)
(define *gm-ride-2* 59)
(define *gm-hi-bongo* 60)
(define *gm-low-bongo* 61)
(define *gm-mute-hi-conga* 62)
(define *gm-hi-conga* 63)
(define *gm-low-conga* 64)
(define *gm-hi-timbale* 65)
(define *gm-low-timbale* 66)
(define *gm-hi-agogo* 67)
(define *gm-low-agogo* 68)
(define *gm-cabasa* 69)
(define *gm-maracas* 70)
(define *gm-short-whistle* 71)
(define *gm-long-whistle* 72)
(define *gm-short-guiro* 73)
(define *gm-long-guiro* 74)
(define *gm-claves* 75)
(define *gm-hi-wood-block* 76)
(define *gm-low-wood-block* 77)
(define *gm-mute-cuica* 78)
(define *gm-open-cuica* 79)
(define *gm-mute-triangle* 80)
(define *gm-open-triangle* 81)

;; General MIDI drum channel note names.
(define *gm-drum-note-names* '(
"Acoustic Bass Drum"			; 35 C
"Bass Drum 1"				; 36 C#
"Side Stick"				; 37 D
"Acoustic Snare"			; 38 D#
"Hand Clap"				; 39 E
"Electric Snare"			; 40 F
"Low Floor Tom"				; 41 F#
"Closed Hi Hat"				; 42 G
"High Floor Tom"			; 43 G#
"Pedal Hi-Hat"				; 44 A
"Low Tom"				; 45 A#
"Open Hi-Hat"				; 46 B
"Low-Mid Tom"				; 47 C
"Hi Mid Tom"				; 48 C#
"Crash Cymbal 1"			; 49 D
"High Tom"				; 50 D#
"Ride Cymbal 1"				; 51 E
"Chinese Cymbal"			; 52 F
"Ride Bell"				; 53 F#
"Tambourine"				; 54 G
"Splash Cymbal"				; 55 G#
"Cowbell"				; 56 A
"Crash Cymbal 2"			; 57 A#
"Vibraslap"				; 58 B
"Ride Cymbal 2"				; 59 C
"Hi Bongo"				; 60 C#
"Low Bongo"				; 61 D
"Mute Hi Conga"				; 62 D#
"Open Hi Conga"				; 63 E
"Low Conga"				; 64 F
"High Timbale"				; 65 F#
"Low Timbale"				; 66 G
"High Agogo"				; 67 G#
"Low Agogo"				; 68 A
"Cabasa"				; 69 A#
"Maracas"				; 70 B
"Short Whistle"				; 71 C
"Long Whistle"				; 72 C#
"Short Guiro"				; 73 D
"Long Guiro"				; 74 D#
"Claves"				; 75 E
"Hi Wood Block"				; 76 F
"Low Wood Block"			; 77 F#
"Mute Cuica"				; 78 G
"Open Cuica"				; 79 G#
"Mute Triangle"				; 80 A
"Open Triangle"				; 81 A#
))
