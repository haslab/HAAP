{-# LANGUAGE ViewPatterns #-}

module JSImages where

import qualified CodeWorld as CW
import Graphics.Gloss hiding ((.*.),(.+.),(.-.))
import Graphics.Gloss.Data.Display

--lava   = makeImage 100 100 "lava"
--nitro1 = makeImage 81 62   "nitro1"
--nitro2 = makeImage 81 62   "nitro2"
--nitro3 = makeImage 81 62   "nitro3"
--nitro4 = makeImage 81 62   "nitro4"
--puff   = makeImage 60 40   "puff"
--bar1   = makeImage 9 12    "bar1"
--bar2   = makeImage 9 12    "bar2"
--bar3   = makeImage 9 12    "bar3"
--bar4   = makeImage 9 12    "bar4"
--p1     = makeImage 60 60   "p1"
--p2     = makeImage 60 60   "p2"
--p3     = makeImage 60 60   "p3"
--p4     = makeImage 60 60   "p4"
--c1     = makeImage 60 100  "c1"
--c2     = makeImage 60 100  "c2"
--c3     = makeImage 60 100  "c3"
--c4     = makeImage 60 100  "c4"
--n1     = makeImage 19 29   "n1"
--n2     = makeImage 19 29   "n2"
--n3     = makeImage 19 29   "n3"
--n4     = makeImage 19 29   "n4"
--btt    = makeImage 11 11   "btt"
--mrt    = makeImage 11 11   "mrt"
--f0     = makeImage 60 120  "f0"
--f1     = makeImage 70 120  "f1"
--f2     = makeImage 60 120  "f2"
--f3     = makeImage 60 120  "f3"
--f4     = makeImage 60 120  "f4"
--f5     = makeImage 60 120  "f5"
--f6     = makeImage 60 120  "f6"
--f7     = makeImage 60 120  "f7"
--f8     = makeImage 60 120  "f8"
--f9     = makeImage 60 120  "f9"
--fa     = makeImage 52 120  "fa"
--fb     = makeImage 54 120  "fb"
--fc     = makeImage 52 120  "fc"
--fd     = makeImage 52 120  "fd"
--fe     = makeImage 48 120  "fe"
--ff     = makeImage 48 120  "ff"
--fg     = makeImage 52 120  "fg"
--fh     = makeImage 52 120  "fh"
--fi     = makeImage 45 120  "fi"
--fj     = makeImage 52 120  "fj"
--fk     = makeImage 52 120  "fk"
--fl     = makeImage 56 120  "fl"
--fm     = makeImage 86 120  "fm"
--fn     = makeImage 52 120  "fn"
--fo     = makeImage 52 120  "fo"
--fp     = makeImage 48 120  "fp"
--fq     = makeImage 52 120  "fq"
--fr     = makeImage 52 120  "fr"
--fs     = makeImage 48 120  "fs"
--ft     = makeImage 44 120  "ft"
--fu     = makeImage 52 120  "fu"
--fv     = makeImage 50 120  "fv"
--fw     = makeImage 82 120  "fw"
--fx     = makeImage 52 120  "fx"
--fy     = makeImage 50 120  "fy"
--fz     = makeImage 44 120  "fz"
--timer  = makeImage 660 590 "timer"

fixedscreenx,fixedscreeny :: Int
fixedscreenx = 1024
fixedscreeny = 768

placeImageTopLeft :: Float -> Int -> Int -> Picture -> Picture
placeImageTopLeft tamanho (realToFrac -> sizex) (realToFrac -> sizey) pic = Translate (sizex' / 2) (-sizey' / 2) $ Scale scalex scaley pic
    where
    scalex = tamanho / sizex
    scaley = tamanho / sizey
    sizex' = scalex * sizex
    sizey' = scaley * sizey

placeImageCenter :: Float -> Int -> Int -> Picture -> Picture
placeImageCenter tamanho (realToFrac -> sizex) (realToFrac -> sizey) pic = Scale scalex scaley pic
    where
    scalex = tamanho / sizex
    scaley = tamanho / sizey

loadImages :: Float -> Display -> IO [(String,Picture)]
loadImages tamanho screen@(Display screenx screeny) = do
    lava   <- loadImageById "lava"
    nitro1 <- loadImageById "nitro1"
    nitro2 <- loadImageById "nitro2"
    nitro3 <- loadImageById "nitro3"
    nitro4 <- loadImageById "nitro4"
    puff   <- loadImageById "puff"
    bar1   <- loadImageById "bar1"
    bar2   <- loadImageById "bar2"
    bar3   <- loadImageById "bar3"
    bar4   <- loadImageById "bar4"
    p1     <- loadImageById "p1"
    p2     <- loadImageById "p2"
    p3     <- loadImageById "p3"
    p4     <- loadImageById "p4"
    c1     <- loadImageById "c1"
    c2     <- loadImageById "c2"
    c3     <- loadImageById "c3"
    c4     <- loadImageById "c4"
    n1     <- loadImageById "n1"
    n2     <- loadImageById "n2"
    n3     <- loadImageById "n3"
    n4     <- loadImageById "n4"
    btt    <- loadImageById "btt"
    mrt    <- loadImageById "mrt"
    f0     <- loadImageById "f0"
    f1     <- loadImageById "f1"
    f2     <- loadImageById "f2"
    f3     <- loadImageById "f3"
    f4     <- loadImageById "f4"
    f5     <- loadImageById "f5"
    f6     <- loadImageById "f6"
    f7     <- loadImageById "f7"
    f8     <- loadImageById "f8"
    f9     <- loadImageById "f9"
    fa     <- loadImageById "fa"
    fb     <- loadImageById "fb"
    fc     <- loadImageById "fc"
    fd     <- loadImageById "fd"
    fe     <- loadImageById "fe"
    ff     <- loadImageById "ff"
    fg     <- loadImageById "fg"
    fh     <- loadImageById "fh"
    fi     <- loadImageById "fi"
    fj     <- loadImageById "fj"
    fk     <- loadImageById "fk"
    fl     <- loadImageById "fl"
    fm     <- loadImageById "fm"
    fn     <- loadImageById "fn"
    fo     <- loadImageById "fo"
    fp     <- loadImageById "fp"
    fq     <- loadImageById "fq"
    fr     <- loadImageById "fr"
    fs     <- loadImageById "fs"
    ft     <- loadImageById "ft"
    fu     <- loadImageById "fu"
    fv     <- loadImageById "fv"
    fw     <- loadImageById "fw"
    fx     <- loadImageById "fx"
    fy     <- loadImageById "fy"
    fz     <- loadImageById "fz"
    let imgs = [("lava",placeImageTopLeft tamanho 100 100 lava)
               ,("nitro1",placeImageCenter tamanho 81 62 nitro1)
               ,("nitro2",placeImageCenter tamanho 81 62 nitro2)
               ,("nitro3",placeImageCenter tamanho 81 62 nitro3)
               ,("nitro4",placeImageCenter tamanho 81 62 nitro4)
               ,("puff"  ,placeImageCenter tamanho 60 40 puff)
               ,("bar1",bar1)
               ,("bar2",bar2)
               ,("bar3",bar3)
               ,("bar4",bar4)
               --,("timer",Translate (scalenunox 14) (scalenunoy 14) $ Scale s s timer)
               ,("1",n1)
               ,("2",n2)
               ,("3",n3)
               ,("4",n4)
               ,("btt",btt)
               ,("mrt",mrt)
               ,("p1",Scale 0.5 0.5 p1)
               ,("p2",Scale 0.5 0.5 p2)
               ,("p3",Scale 0.5 0.5 p3)
               ,("p4",Scale 0.5 0.5 p4)
               ,("c1",Rotate 90 $ Scale 0.5 0.5 $ placeImageCenter tamanho 60 100 c1)
               ,("c2",Rotate 90 $ Scale 0.5 0.5 $ placeImageCenter tamanho 60 100 c2)
               ,("c3",Rotate 90 $ Scale 0.5 0.5 $ placeImageCenter tamanho 60 100 c3)
               ,("c4",Rotate 90 $ Scale 0.5 0.5 $ placeImageCenter tamanho 60 100 c4)
               ,("f0",f0)
               ,("f1",f1)
               ,("f2",f2)
               ,("f3",f3)
               ,("f4",f4)
               ,("f5",f5)
               ,("f6",f6)
               ,("f7",f7)
               ,("f8",f8)
               ,("f9",f9)
               ,("fa",fa)
               ,("fb",fb)
               ,("fc",fc)
               ,("fd",fd)
               ,("fe",fe)
               ,("ff",ff)
               ,("fg",fg)
               ,("fh",fh)
               ,("fi",fi)
               ,("fj",fj)
               ,("fk",fk)
               ,("fl",fl)
               ,("fm",fm)
               ,("fn",fn)
               ,("fo",fo)
               ,("fp",fp)
               ,("fq",fq)
               ,("fr",fr)
               ,("fs",fs)
               ,("ft",ft)
               ,("fu",fu)
               ,("fv",fv)
               ,("fw",fw)
               ,("fx",fx)
               ,("fy",fy)
               ,("fz",fz)
               ]
    return imgs
