{-# LANGUAGE OverloadedStrings #-}
{-|
Copyright    : 8c6794b6, 2014
License      : BSD3

Maintainer  : 8c6794b6@gmail.com
Stability   : experimental
Portability : unknown

Benchmark for hsc3-tree.

-}
module Main where

import Criterion.Main

import Sound.OSC
import Sound.SC3.Tree

main :: IO ()
main = do
  defaultMain
       [ bench "query name = \"p:1570426241:wet:1652251979\""
         (nf (queryN (synthName ==? "p:1570426241:wet:1652251979")) node)
       , bench "parse sample message"
         (nf parseNode queryTree_reply)
       ]

node :: SCNode
node = Group 0
  [Group 1
     [Group 10
        [Group 100
           [Synth 100000 "metro"
              ["out":=128.0,
               "bpm":=120.0,
               "beat":=4.0,
               "count":=127.0]],
         Group 101
           [Synth (-24) "p:2081608770:pan:3478606274"
              ["count":=24.0,
               "tr":<-128,
               "out":=257.0],
            Synth (-32) "p:2081608770:t_tr:1191114141"
              ["count":=24.0,
               "tr":<-128,
               "out":=258.0],
            Synth (-40) "p:2081608770:cf:3433652882"
              ["count":=24.0,
               "tr":<-128,
               "out":=259.0],
            Synth 2081608770 "nz01"
              ["pan":<-257,
               "rq":=0.47058823704719543,
               "cf":<-259,
               "out":=18.0,
               "t_tr":<-258],
            Synth (-48) "p:1777095663:wet:3958409290"
              ["count":=24.0,
               "out":=260.0],
            Synth (-56) "p:1777095663:dcy:1128213552"
              ["count":=24.0,
               "tr":<-128,
               "out":=261.0],
            Synth 1777095663 "ap01"
              ["wet":<-260,
               "dcy":<-261,
               "in":=18.0,
               "out":=18.0],
            Synth (-64) "p:1425562103:wet:2347817295"
              ["count":=24.0,
               "tr":<-128,
               "out":=262.0],
            Synth (-72) "p:1425562103:dcy:2586523859"
              ["count":=24.0,
               "out":=263.0],
            Synth (-80) "p:1425562103:dlt:395041877"
              ["count":=24.0,
               "out":=264.0],
            Synth 1425562103 "cmb02"
              ["dcy":<-263,
               "dlt":<-264,
               "in":=18.0,
               "wet":<-262,
               "out":=18.0],
            Synth (-88) "p:1938860940:wet:1652251979"
              ["count":=24.0,
               "tr":<-128,
               "out":=265.0],
            Synth 1938860940 "dc01"
              ["coef":=0.9950000047683716,
               "in":=18.0,
               "wet":<-265,
               "out":=18.0],
            Synth (-480) "p:10199:amp:3929839498"
              ["out":=266.0,
               "count":=2536.0],
            Synth 10199 "router"
              ["amp":<-266,
               "in":=18.0,
               "out":=16.0]],
         Group 102
           [Synth (-104) "p:686545166:t_tr0:972151571"
              ["count":=24.0,
               "tr":<-128,
               "out":=267.0],
            Synth (-112) "p:686545166:amp:3166586127"
              ["count":=24.0,
               "tr":<-128,
               "out":=268.0],
            Synth 686545166 "bd03"
              ["pan":=0.5,
               "amp":<-268,
               "dur2":=0.20000000298023224,
               "frq2":=0.10000000149011612,
               "dur1":=0.20000000298023224,
               "frq1":=0.10000000149011612,
               "out":=20.0,
               "t_tr0":<-267],
            Synth (-120) "p:871295697:wet:1714096096"
              ["count":=24.0,
               "tr":<-128,
               "out":=269.0],
            Synth (-128) "p:871295697:dcy:541785498"
              ["count":=24.0,
               "out":=270.0],
            Synth 871295697 "ap01"
              ["wet":<-269,
               "dcy":<-270,
               "in":=20.0,
               "out":=20.0],
            Synth (-136) "p:519762137:wet:3449735436"
              ["count":=24.0,
               "out":=271.0],
            Synth (-144) "p:519762137:dcy:2111951799"
              ["count":=24.0,
               "out":=272.0],
            Synth (-152) "p:519762137:dlt:3303316031"
              ["count":=24.0,
               "out":=273.0],
            Synth 519762137 "cmb02"
              ["dcy":<-272,
               "dlt":<-273,
               "in":=20.0,
               "wet":<-271,
               "out":=20.0],
            Synth (-160) "p:1033060974:wet:1652251979"
              ["count":=24.0,
               "tr":<-128,
               "out":=274.0],
            Synth 1033060974 "dc01"
              ["coef":=0.9950000047683716,
               "in":=20.0,
               "wet":<-274,
               "out":=20.0],
            Synth (-168) "p:10299:amp:976852501"
              ["count":=24.0,
               "tr":<-128,
               "out":=275.0],
            Synth 10299 "router"
              ["amp":<-275,
               "in":=20.0,
               "out":=16.0]],
         Group 103
           [Synth (-176) "p:504029009:pan:3976128808"
              ["count":=24.0,
               "out":=276.0],
            Synth (-184) "p:504029009:dur:3900346958"
              ["count":=24.0,
               "tr":<-128,
               "out":=277.0],
            Synth (-192) "p:504029009:t_tr:1108665780"
              ["count":=24.0,
               "tr":<-128,
               "out":=278.0],
            Synth (-200) "p:504029009:freq:2173731152"
              ["count":=24.0,
               "tr":<-128,
               "out":=279.0],
            Synth 504029009 "sin02"
              ["pan":<-276,
               "dur":<-277,
               "freq":<-279,
               "out":=22.0,
               "t_tr":<-278],
            Synth (-208) "p:397792658:wet:1652251979"
              ["count":=24.0,
               "tr":<-128,
               "out":=280.0],
            Synth (-216) "p:397792658:dcy:3921743354"
              ["count":=24.0,
               "out":=281.0],
            Synth 397792658 "ap01"
              ["wet":<-280,
               "dcy":<-281,
               "in":=22.0,
               "out":=22.0],
            Synth (-224) "p:749326218:wet:1747107833"
              ["count":=24.0,
               "out":=282.0],
            Synth (-232) "p:749326218:dcy:1960446383"
              ["count":=24.0,
               "out":=283.0],
            Synth (-240) "p:749326218:dlt:1877260943"
              ["count":=24.0,
               "tr":<-128,
               "out":=284.0],
            Synth 749326218 "cmb02"
              ["dcy":<-283,
               "dlt":<-284,
               "in":=22.0,
               "wet":<-282,
               "out":=22.0],
            Synth (-512) "p:10399:amp:808756501"
              ["out":=285.0,
               "count":=2648.0],
            Synth 10399 "router"
              ["amp":<-285,
               "in":=22.0,
               "out":=16.0]],
         Group 104
           [Synth (-256) "p:941544659:freq:388707019"
              ["count":=24.0,
               "tr":<-128,
               "out":=286.0],
            Synth (-264) "p:941544659:dur:1140105196"
              ["count":=24.0,
               "tr":<-128,
               "out":=287.0],
            Synth (-272) "p:941544659:pan:393804523"
              ["count":=24.0,
               "out":=288.0],
            Synth (-280) "p:941544659:t_tr0:3187017542"
              ["count":=24.0,
               "tr":<-128,
               "out":=289.0],
            Synth 941544659 "poly01"
              ["pan":<-288,
               "dur":<-287,
               "freq":<-286,
               "out":=24.0,
               "t_tr0":<-289],
            Synth (-288) "p:1904729927:wet:646587226"
              ["count":=24.0,
               "out":=290.0],
            Synth (-296) "p:1904729927:dcy:2511272801"
              ["count":=24.0,
               "out":=291.0],
            Synth 1904729927 "ap01"
              ["wet":<-290,
               "dcy":<-291,
               "in":=24.0,
               "out":=24.0],
            Synth (-304) "p:2038703809:wet:2779418432"
              ["count":=24.0,
               "tr":<-128,
               "out":=292.0],
            Synth (-312) "p:2038703809:dcy:1140105196"
              ["count":=24.0,
               "tr":<-128,
               "out":=293.0],
            Synth (-320) "p:2038703809:dlt:43917995"
              ["count":=24.0,
               "tr":<-128,
               "out":=294.0],
            Synth 2038703809 "cmb02"
              ["dcy":<-293,
               "dlt":<-294,
               "in":=24.0,
               "wet":<-292,
               "out":=24.0],
            Synth (-328) "p:10499:amp:1376817275"
              ["count":=24.0,
               "out":=295.0],
            Synth 10499 "router"
              ["amp":<-295,
               "in":=24.0,
               "out":=16.0]],
         Group 105
           [Synth (-344) "p:595059763:freq:2202675348"
              ["count":=32.0,
               "tr":<-128,
               "out":=297.0],
            Synth (-352) "p:595059763:cf:209772435"
              ["count":=32.0,
               "tr":<-128,
               "out":=298.0],
            Synth (-360) "p:595059763:amp:1621994330"
              ["count":=32.0,
               "out":=299.0],
            Synth (-488) "p:595059763:lagt:3781100682"
              ["out":=296.0,
               "count":=2576.0],
            Synth 595059763 "saw01"
              ["amp":<-299,
               "rq":=0.10000000149011612,
               "cf":<-298,
               "lagt":<-296,
               "freq":<-297,
               "out":=26.0],
            Synth (-368) "p:1832690602:wet:1479076838"
              ["count":=32.0,
               "tr":<-128,
               "out":=300.0],
            Synth (-376) "p:1832690602:dcy:4090842369"
              ["count":=32.0,
               "out":=301.0],
            Synth 1832690602 "ap01"
              ["wet":<-300,
               "dcy":<-301,
               "in":=26.0,
               "out":=26.0],
            Synth (-384) "p:1481157042:wet:1283351465"
              ["count":=32.0,
               "out":=302.0],
            Synth (-392) "p:1481157042:dcy:2058641498"
              ["count":=32.0,
               "out":=303.0],
            Synth (-400) "p:1481157042:dlt:3534274232"
              ["count":=32.0,
               "out":=304.0],
            Synth 1481157042 "cmb02"
              ["dcy":<-303,
               "dlt":<-304,
               "in":=26.0,
               "wet":<-302,
               "out":=26.0],
            Synth (-408) "p:1994455879:wet:1652251979"
              ["count":=32.0,
               "tr":<-128,
               "out":=305.0],
            Synth 1994455879 "dc01"
              ["coef":=0.9950000047683716,
               "in":=26.0,
               "wet":<-305,
               "out":=26.0],
            Synth (-496) "p:10599:amp:392656399"
              ["out":=306.0,
               "count":=2576.0],
            Synth 10599 "router"
              ["amp":<-306,
               "in":=26.0,
               "out":=16.0]],
         Group 106
           [Synth (-424) "p:1564674976:t_tr:1348072379"
              ["count":=32.0,
               "tr":<-128,
               "out":=307.0],
            Synth (-432) "p:1564674976:pan:2840051617"
              ["count":=32.0,
               "out":=308.0],
            Synth 1564674976 "pv03"
              ["pan":<-308,
               "bufn":=12.0,
               "t_tr":<-307,
               "out":=28.0],
            Synth (-440) "p:1057127404:wet:1389225814"
              ["count":=32.0,
               "out":=309.0],
            Synth (-448) "p:1057127404:dcy:4274328903"
              ["count":=32.0,
               "out":=310.0],
            Synth (-456) "p:1057127404:dlt:1326901169"
              ["count":=32.0,
               "out":=311.0],
            Synth 1057127404 "cmb02"
              ["dcy":<-310,
               "dlt":<-311,
               "in":=28.0,
               "wet":<-309,
               "out":=28.0],
            Synth (-464) "p:1570426241:wet:1652251979"
              ["count":=32.0,
               "tr":<-128,
               "out":=312.0],
            Synth 1570426241 "dc01"
              ["coef":=0.9950000047683716,
               "in":=28.0,
               "wet":<-312,
               "out":=28.0],
            Synth (-472) "p:10699:amp:1261176564"
              ["count":=32.0,
               "out":=313.0],
            Synth 10699 "router"
              ["amp":<-313,
               "in":=28.0,
               "out":=16.0]],
         Group 107
           [Synth 10799 "router"
              ["amp":=0.0,
               "in":=30.0,
               "out":=16.0]],
         Group 108
           [Synth 10899 "router"
              ["amp":=0.0,
               "in":=32.0,
               "out":=16.0]]],
      Group 99
        [Synth (-16) "p:9999:amp:2375896622"
           ["count":=8.0,
            "out":=256.0],
         Synth 9999 "router"
           ["amp":<-256,
            "in":=16.0,
            "out":=0.0]]],
   Group 2
     []]

queryTree_reply :: Message
queryTree_reply = Message {messageAddress = "/g_queryTree.reply", messageDatum = [Int32 {d_int32 = 1},Int32 {d_int32 = 101},Int32 {d_int32 = 15},Int32 {d_int32 = -24},Int32 {d_int32 = -1},ASCII_String {d_ascii_string = "p:2081608770:pan:3478606274"},Int32 {d_int32 = 3},ASCII_String {d_ascii_string = "count"},Float {d_float = 24.0},ASCII_String {d_ascii_string = "tr"},ASCII_String {d_ascii_string = "c128"},ASCII_String {d_ascii_string = "out"},Float {d_float = 257.0},Int32 {d_int32 = -32},Int32 {d_int32 = -1},ASCII_String {d_ascii_string = "p:2081608770:t_tr:1191114141"},Int32 {d_int32 = 3},ASCII_String {d_ascii_string = "count"},Float {d_float = 24.0},ASCII_String {d_ascii_string = "tr"},ASCII_String {d_ascii_string = "c128"},ASCII_String {d_ascii_string = "out"},Float {d_float = 258.0},Int32 {d_int32 = -40},Int32 {d_int32 = -1},ASCII_String {d_ascii_string = "p:2081608770:cf:3433652882"},Int32 {d_int32 = 3},ASCII_String {d_ascii_string = "count"},Float {d_float = 24.0},ASCII_String {d_ascii_string = "tr"},ASCII_String {d_ascii_string = "c128"},ASCII_String {d_ascii_string = "out"},Float {d_float = 259.0},Int32 {d_int32 = 2081608770},Int32 {d_int32 = -1},ASCII_String {d_ascii_string = "nz01"},Int32 {d_int32 = 5},ASCII_String {d_ascii_string = "pan"},ASCII_String {d_ascii_string = "c257"},ASCII_String {d_ascii_string = "rq"},Float {d_float = 0.47058824},ASCII_String {d_ascii_string = "cf"},ASCII_String {d_ascii_string = "c259"},ASCII_String {d_ascii_string = "out"},Float {d_float = 18.0},ASCII_String {d_ascii_string = "t_tr"},ASCII_String {d_ascii_string = "c258"},Int32 {d_int32 = -48},Int32 {d_int32 = -1},ASCII_String {d_ascii_string = "p:1777095663:wet:3958409290"},Int32 {d_int32 = 2},ASCII_String {d_ascii_string = "count"},Float {d_float = 24.0},ASCII_String {d_ascii_string = "out"},Float {d_float = 260.0},Int32 {d_int32 = -56},Int32 {d_int32 = -1},ASCII_String {d_ascii_string = "p:1777095663:dcy:1128213552"},Int32 {d_int32 = 3},ASCII_String {d_ascii_string = "count"},Float {d_float = 24.0},ASCII_String {d_ascii_string = "tr"},ASCII_String {d_ascii_string = "c128"},ASCII_String {d_ascii_string = "out"},Float {d_float = 261.0},Int32 {d_int32 = 1777095663},Int32 {d_int32 = -1},ASCII_String {d_ascii_string = "ap01"},Int32 {d_int32 = 4},ASCII_String {d_ascii_string = "wet"},ASCII_String {d_ascii_string = "c260"},ASCII_String {d_ascii_string = "dcy"},ASCII_String {d_ascii_string = "c261"},ASCII_String {d_ascii_string = "in"},Float {d_float = 18.0},ASCII_String {d_ascii_string = "out"},Float {d_float = 18.0},Int32 {d_int32 = -64},Int32 {d_int32 = -1},ASCII_String {d_ascii_string = "p:1425562103:wet:2347817295"},Int32 {d_int32 = 3},ASCII_String {d_ascii_string = "count"},Float {d_float = 24.0},ASCII_String {d_ascii_string = "tr"},ASCII_String {d_ascii_string = "c128"},ASCII_String {d_ascii_string = "out"},Float {d_float = 262.0},Int32 {d_int32 = -72},Int32 {d_int32 = -1},ASCII_String {d_ascii_string = "p:1425562103:dcy:2586523859"},Int32 {d_int32 = 2},ASCII_String {d_ascii_string = "count"},Float {d_float = 24.0},ASCII_String {d_ascii_string = "out"},Float {d_float = 263.0},Int32 {d_int32 = -80},Int32 {d_int32 = -1},ASCII_String {d_ascii_string = "p:1425562103:dlt:395041877"},Int32 {d_int32 = 2},ASCII_String {d_ascii_string = "count"},Float {d_float = 24.0},ASCII_String {d_ascii_string = "out"},Float {d_float = 264.0},Int32 {d_int32 = 1425562103},Int32 {d_int32 = -1},ASCII_String {d_ascii_string = "cmb02"},Int32 {d_int32 = 5},ASCII_String {d_ascii_string = "dcy"},ASCII_String {d_ascii_string = "c263"},ASCII_String {d_ascii_string = "dlt"},ASCII_String {d_ascii_string = "c264"},ASCII_String {d_ascii_string = "in"},Float {d_float = 18.0},ASCII_String {d_ascii_string = "wet"},ASCII_String {d_ascii_string = "c262"},ASCII_String {d_ascii_string = "out"},Float {d_float = 18.0},Int32 {d_int32 = -88},Int32 {d_int32 = -1},ASCII_String {d_ascii_string = "p:1938860940:wet:1652251979"},Int32 {d_int32 = 3},ASCII_String {d_ascii_string = "count"},Float {d_float = 24.0},ASCII_String {d_ascii_string = "tr"},ASCII_String {d_ascii_string = "c128"},ASCII_String {d_ascii_string = "out"},Float {d_float = 265.0},Int32 {d_int32 = 1938860940},Int32 {d_int32 = -1},ASCII_String {d_ascii_string = "dc01"},Int32 {d_int32 = 4},ASCII_String {d_ascii_string = "coef"},Float {d_float = 0.995},ASCII_String {d_ascii_string = "in"},Float {d_float = 18.0},ASCII_String {d_ascii_string = "wet"},ASCII_String {d_ascii_string = "c265"},ASCII_String {d_ascii_string = "out"},Float {d_float = 18.0},Int32 {d_int32 = -96},Int32 {d_int32 = -1},ASCII_String {d_ascii_string = "p:10199:amp:1501253528"},Int32 {d_int32 = 2},ASCII_String {d_ascii_string = "count"},Float {d_float = 24.0},ASCII_String {d_ascii_string = "out"},Float {d_float = 266.0},Int32 {d_int32 = 10199},Int32 {d_int32 = -1},ASCII_String {d_ascii_string = "router"},Int32 {d_int32 = 3},ASCII_String {d_ascii_string = "amp"},ASCII_String {d_ascii_string = "c266"},ASCII_String {d_ascii_string = "in"},Float {d_float = 18.0},ASCII_String {d_ascii_string = "out"},Float {d_float = 16.0}]}
