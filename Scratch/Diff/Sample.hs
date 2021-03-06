module Sample where

import Sound.SC3
import Sound.SC3.Lepton.Tree

t1 :: SCNode
t1 =
  Group 0
    [Group 1
      [Group 10
        [Group 100
         [Synth 1001 "lfnz"
           ["out":=100,"mul":=1500,"add":=1500,"freq":=0.125]
         ,Synth 1002 "lftri"
           ["out":=101,"mul":=1760,"add":=1.8,"freq":=0.5]]
        ,Synth 101 "foo"
          ["amp":=0.1,"freq":<-100]
        ,Synth 102 "foo"
          ["amp":=0.1,"freq":<-101]]
      ,Group 11
        [Group 110
         [Synth 1101 "lfsin"
           ["out":=110,"mul":=0.1,"add":=0.1,"freq":=0.25]
         ,Synth 1102 "lftri"
           ["out":=111,"mul":=0.1,"add":=0.1,"freq":=0.3]
         ,Synth 1103 "lfnz"
           ["out":=112,"mul":=0.4,"add":=0.4,"freq":=0.4]]
        ,Synth 111 "bar"
           ["amp":=0.2,"freq":=4399.8]
        ,Synth 112 "bar"
           ["amp":<-112,"freq":=6601]]
      ,Group 12
        [Group 120
         [Synth 1201 "lfsin"
           ["out":=121,"freq":=0.0232]
         ,Synth 1202 "lftri"
           ["out":=122,"freq":=0.0899]
         ,Synth 1203 "lfnz"
           ["out":=123,"freq":=0.0713]
         ,Synth 1204 "lfsin"
           ["out":=124,"freq":=0.1203]
         ,Synth 1205 "lfnz"
           ["out":=125,"freq":=0.0983]]
        ,Synth 121 "buzz"
           ["amp":=0.07,"freq":=440,"pan":<-121]
        ,Synth 122 "buzz"
          ["amp":=0.07,"freq":=554.365,"pan":<-122]
        ,Synth 123 "buzz"
          ["amp":=0.07,"freq":=660,"pan":<-123]
        ,Synth 124 "buzz"
          ["amp":=0.07,"freq":=880,"pan":<-124]
        ,Synth 125 "buzz"
          ["amp":=0.07,"freq":=1110,"pan":<-125]]
      ,Group 13
        [Synth 131 "quux"
          ["amp":=0.4,"freq":=9327,"pan":=0.7]
        ,Synth 132 "quux"
          ["amp":=0.3,"freq":=3422,"pan":=0.08]
        ,Synth 133 "quux"
          ["amp":=0.4,"freq":=121,"pan":=(-0.12)]
        ,Synth 134 "quux"
          ["amp":=0.2,"freq":=1893,"pan":=(-0.6)]]
      ,Group 14
        [Group 141
          [Synth 1410 "lftrig"
            ["freq":=1,"out":=141]
          ,Synth 1411 "lftrig"
            ["freq":=2,"out":=142]]
        ,Synth 1401 "hoge"
           ["t_trig":<-141]
        ,Synth 1402 "hoge"
           ["t_trig":<-142,"flo":=880,"fhi":=1320]
        ,Synth 1403 "hoge"
           ["t_trig":<-141,"flo":=180,"fhi":=320]]]]

t2 =
  Group 0
    [Group 1
      [Group 10
        [Group 100
         [Synth 1001 "lfnz"
           ["out":=100,"mul":=1500,"add":=1500,"freq":=0.125]
         ,Synth 1002 "lftri"
           ["out":=101,"mul":=1760,"add":=1.8,"freq":=0.5]]
        ,Synth 101 "foo"
          ["amp":=0.3,"freq":<-100]
        ,Synth 102 "foo"
          ["amp":=0.4,"freq":<-101]]
      ,Group 11
        [Group 110
         [Synth 1101 "lfsin"
           ["out":=110,"mul":=0.1,"add":=0.1,"freq":=0.25]
         ,Synth 1102 "lftri"
           ["out":=111,"mul":=0.1,"add":=0.1,"freq":=0.3]
         ,Synth 1103 "lfnz"
           ["out":=112,"mul":=0.4,"add":=0.4,"freq":=0.4]]
        ,Synth 111 "bar"
          ["amp":=0.2,"freq":=4399.8]
        ,Synth 112 "bar"
          ["amp":<-112,"freq":=7700]]
      ,Group 12
        [Group 120
         [Synth 1201 "lfsin"
           ["out":=121,"freq":=0.0232]
         ,Synth 1202 "lftri"
           ["out":=122,"freq":=0.0899]
         ,Synth 1203 "lfnz"
           ["out":=123,"freq":=0.0713]
         ,Synth 1204 "lfsin"
           ["out":=124,"freq":=0.1203]
         ,Synth 1205 "lfnz"
           ["out":=125,"freq":=0.0889]]
        ,Synth 121 "buzz"
          ["amp":=0.07,"freq":=440,"pan":<-121]
        ,Synth 122 "buzz"
          ["amp":=0.07,"freq":=554.365,"pan":<-122]
        ,Synth 123 "buzz"
          ["amp":=0.07,"freq":=660,"pan":<-123]
        ,Synth 124 "buzz"
          ["amp":=0.07,"freq":=880,"pan":<-124]
        ,Synth 125 "buzz"
          ["amp":=0.07,"freq":=1110,"pan":<-125]]
      ,Group 13
        [Synth 131 "quux"
          ["amp":=0.4,"freq":=9327,"pan":=0.7]
        ,Synth 132 "quux"
          ["amp":=0.3,"freq":=3422,"pan":=0.08]
        ,Synth 133 "quux"
          ["amp":=0.4,"freq":=121,"pan":=(-0.12)]
        ,Synth 134 "quux"
          ["amp":=0.2,"freq":=1893,"pan":=(-0.6)]]
      ,Group 14
        [Group 141
          [Synth 1410 "lftrig"
            ["freq":=1,"out":=141]
          ,Synth 1411 "lftrig"
            ["freq":=2,"out":=142]]
        ,Synth 1401 "hoge"
          ["t_trig":<-141]
        ,Synth 1402 "hoge"
          ["t_trig":<-142,"flo":=880,"fhi":=1320]
        ,Synth 1402 "hoge"
          ["t_trig":<-141,"flo":=180,"fhi":=320]]]]

-- Tree treated as base.
t3 =
  Group 0
  [Group 1
   [Group 14
    [Group 141
     [Synth 1410 "lftrig" ["freq":=1,"out":=141]
     ,Synth 1411 "lftrig" ["freq":=2,"out":=142]]
    ,Synth 1401 "hoge" ["t_trig":<-141]
    ,Synth 1402 "hoge" ["t_trig":<-142,"flo":=880,"fhi":=1320]
    ,Synth 1403 "hoge" ["t_trig":<-141,"flo":=180,"fhi":=320]]]]

-- Tree with node#1411 has been removed from t3.
-- Should return 'n_free 1411' from diff(t3,t5)
--
-- XXX: Reordering not working properly when updating t3 <-> t4.
t4 =
  Group 0
  [Group 1
   [Group 14
    [Group 141
     [Synth 1410 "lftrig" ["freq":=1,"out":=141]]
    ,Synth 1401 "hoge" ["t_trig":<-141]
    ,Synth 1402 "hoge" ["t_trig":<-142,"flo":=880,"fhi":=1320]
    ,Synth 1403 "hoge" ["t_trig":<-141,"flo":=180,"fhi":=320]]]]

-- Tree with 'freq' in node#1410 has been modified to 3.
-- Make 'n_set' message from diff(t3,t5)
t5 =
  Group 0
  [Group 1
   [Group 14
    [Group 141
     [Synth 1410 "lftrig" ["freq":=2,"out":=141]
     ,Synth 1411 "lftrig" ["freq":=3,"out":=142]]
    ,Synth 1401 "hoge" ["t_trig":<-141]
    ,Synth 1402 "hoge" ["t_trig":<-142,"flo":=440,"fhi":=1320]
    ,Synth 1403 "hoge" ["t_trig":<-141,"flo":=180,"fhi":=320]]]]

-- Tree with node#1404 inserted after node#1401 to t3.
t6 =
  Group 0
  [Group 1
   [Group 14
    [Group 141
     [Synth 1410 "lftrig" ["freq":=1,"out":=141]
     ,Synth 1411 "lftrig" ["freq":=2,"out":=142]]
    ,Synth 1401 "hoge" ["t_trig":<-141]
    ,Synth 1402 "hoge" ["t_trig":<-142,"flo":=880,"fhi":=1320,"prob":=0.9]
    ,Synth 1403 "hoge" ["t_trig":<-141,"flo":=180,"fhi":=320,"prob":=1]
    ,Synth 1404 "hoge" ["t_trig":<-142,"flo":=660,"fhi":=770,"prob":=0.25]]]]

p1 = ["foo":=100]

p2 = ["foo":=200]

-- | Base node
t00 :: SCNode
t00 =
  Group 0
    [Group 2
      [Group 20
        [Synth 2000 "foo" ["amp":=0.3,"freq":=440,"out":=0]
        ,Synth 2001 "foo" ["amp":=0.3,"freq":=330,"out":=0]]]]

-- | Modified freq of node#2000 from t00
t01 :: SCNode
t01 =
  Group 0
    [Group 2
      [Group 20
        [Synth 2000 "foo" ["amp":=0.3,"freq":=880,"out":=0]
        ,Synth 2001 "foo" ["amp":=0.3,"freq":=330,"out":=0]]]]

-- | Insert node#2002 after node#2001 from t00
t02 :: SCNode
t02 =
  Group 0
    [Group 2
      [Group 20
        [Synth 2000 "foo" ["amp":=0.3,"freq":=440,"out":=0]
        ,Synth 2001 "foo" ["amp":=0.3,"freq":=330,"out":=0]
        ,Synth 2002 "foo" ["amp":=0.3,"freq":=1320,"out":=0]]]]

-- | Delete node#2001 from t0
t03 :: SCNode
t03 =
  Group 0
    [Group 2
      [Group 20
        [Synth 2000 "foo" ["amp":=0.3,"freq":=440,"out":=0]]]]

-- | Modified node#2000, insert node#2002 after node#2001 from t00
t04 :: SCNode
t04 =
  Group 0
    [Group 2
      [Group 20
        [Synth 2000 "foo" ["amp":=0.3,"freq":=550,"out":=0]
        ,Synth 2001 "foo" ["amp":=0.3,"freq":=330,"out":=0]
        ,Synth 2002 "foo" ["amp":=0.3,"freq":=1320,"out":=0]]]]

-- | Modified node#, insert node#2002 after node#2001 from t00
t05 :: SCNode
t05 =
  Group 0
    [Group 2
      [Group 20
        [Synth 2001 "foo" ["amp":=0.3,"freq":=660,"out":=0]
        ,Synth 2002 "foo" ["amp":=0.3,"freq":=990,"out":=0]
        ,Synth 2003 "foo" ["amp":=0.3,"freq":=880,"out":=0]]]]

-- | Swapped position of node#2000 with node#2001.
-- Currently not working.
t06 :: SCNode
t06 =
  Group 0
    [Group 2
      [Group 20
        [Synth 2001 "foo" ["amp":=0.3,"freq":=330,"out":=0]
        ,Synth 2000 "foo" ["amp":=0.3,"freq":=440,"out":=0]]]]

-- | Modified synth name of node#2000.
t07 :: SCNode
t07 =
  Group 0
    [Group 2
      [Group 20
        [Synth 2000 "bar" ["amp":=0.3,"freq":=2800,"out":=0]
        ,Synth 2001 "foo" ["amp":=0.3,"freq":=330,"out":=0]]]]

-- | Base node to play with moving nodes around.
u00 :: SCNode
u00 =
  Group 0
    [Group 2
       [Group 20
          [Synth 2000 "foo" ["amp":=0.2,"freq":=330,"out":=0]
          ,Synth 2001 "foo" ["amp":=0.2,"freq":=333,"out":=0]]
       ,Group 21
          [Synth 2100 "foo" ["amp":=0.2,"freq":=440,"out":=0]
          ,Synth 2101 "foo" ["amp":=0.2,"freq":=439,"out":=0]]
       ,Group 22
          [Synth 2200 "foo" ["amp":=0.2,"freq":=440,"out":=0]
          ,Synth 2201 "foo" ["amp":=0.2,"freq":=439,"out":=0]]
       ,Group 23
          [Synth 2300 "foo" ["amp":=0.2,"freq":=660,"out":=0]
          ,Synth 2301 "foo" ["amp":=0.2,"freq":=670,"out":=0]]]]

u01 :: SCNode
u01 =
  Group 0
    [Group 2
       [Group 20
          [Synth 2000 "foo" ["amp":=0.2,"freq":=330,"out":=0]
          ,Synth 2001 "foo" ["amp":=0.2,"freq":=333,"out":=0]]
       ,Group 21
          [Synth 2100 "foo" ["amp":=0.2,"freq":=440,"out":=0]
          ,Synth 2101 "foo" ["amp":=0.2,"freq":=439,"out":=0]]
       ,Group 22
          [Synth 2200 "foo" ["amp":=0.2,"freq":=660,"out":=0]
          ,Synth 2201 "foo" ["amp":=0.2,"freq":=670,"out":=0]]]]

u02 :: SCNode
u02 =
  Group 0
    [Group 2
       [Group 20
          [Synth 2000 "foo" ["amp":=0.2,"freq":=330,"out":=0]
          ,Synth 2001 "foo" ["amp":=0.2,"freq":=333,"out":=0]]
       ,Group 22
          [Synth 2200 "foo" ["amp":=0.2,"freq":=440,"out":=0]
          ,Synth 2201 "foo" ["amp":=0.2,"freq":=439,"out":=0]]
       ,Group 21
          [Synth 2100 "foo" ["amp":=0.2,"freq":=440,"out":=0]
          ,Synth 2101 "foo" ["amp":=0.2,"freq":=439,"out":=0]]
       ,Group 23
          [Synth 2300 "foo" ["amp":=0.2,"freq":=660,"out":=0]
          ,Synth 2301 "foo" ["amp":=0.2,"freq":=670,"out":=0]]]]
