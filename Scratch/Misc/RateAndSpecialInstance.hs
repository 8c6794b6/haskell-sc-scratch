import Sound.SC3

instance Read Rate where
  readsPrec _ str = case str of
    ('I':'R':rest) -> [(IR, rest)]
    ('K':'R':rest) -> [(KR, rest)]
    ('A':'R':rest) -> [(AR, rest)]
    ('D':'R':rest) -> [(DR, rest)]
    _              -> error "Prelude.read: no parse"

instance Read Special where
  readsPrec _ str = case str of
    'S':'p':'e':'c':'i':'a':'l':xs -> [(Special i, rest)]
      where [(i, rest)] = (readsPrec 0 xs :: [(Int, String)])

instance Read UGen
