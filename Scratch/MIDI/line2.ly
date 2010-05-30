\version "2.12.3"

melody = \relative c' {
  \clef treble
  \key c \major
  \time 4/4
  % 1
  c4 d8 d e16 e e e f32 f f f g8|
}

\score {
  \new Staff {
    \tempo 4 = 120
    \set Staff.midiInstrument=#"rock organ"
    \melody
  }
  \layout {}
  \midi {}
}