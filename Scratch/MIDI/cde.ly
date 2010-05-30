\version "2.12.3"

melody = \relative c' {
  \clef treble
  \key c \major
  \time 4/4
  % 1
  c4 d e r |
  % 2
  <c d e>1 |
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