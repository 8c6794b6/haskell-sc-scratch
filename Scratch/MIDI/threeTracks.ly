\version "2.12.3"

global = {
  \time 4/4
  \key c \major
}

voiceOne = \new Voice \relative c'' {
  \clef treble
  c2 d e1 
  \bar "||"
}

voiceTwo = \new Voice \relative c'' {
  \clef treble
  g2 f e1
}

voiceThree = \new Voice \relative c' {
  \clef bass
  c2 b a1 
}

\score {
  \new StaffGroup <<
    \new Staff << 
      \set Staff.midiInstrument=#"pad 1 (new age)"
      \global \voiceOne 
    >>
    \new Staff << 
      \set Staff.midiInstrument=#"pad 2 (warm)"
      \global \voiceTwo 
    >>
    \new Staff << 
      \set Staff.midiInstrument=#"pad 3 (polysynth)"
      \global \voiceThree 
    >>
  >>
  \layout {}
  \midi {}
}