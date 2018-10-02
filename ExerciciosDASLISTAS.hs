module Exercicios where

import PicturesSVG
import UsePictures

exer2_2:: Picture
exer2_2 = (white `above`black) `beside` (black `above` white)

exer2_3a:: Picture
exer2_3a = (horse `above` blackHorse) `beside` ( blackHorse `above` horse) 

exer2_3b:: Picture
exer2_3b = (horse `above` flipV blackHorse) `beside` ( blackHorse `above` flipV horse) 

exer2_4:: Picture
exer2_4 = (horse `above` flipH blackHorse) `beside` ( blackHorse `above` flipH horse) 


-- Redefina a função fourPics abaixo utilizando as construções let and where:


mfourPics::Picture -> Picture 
mfourPics pic = 
	 (pic `above` invertColour pic)
	     `beside`
	  (invertColour . flipV)(pic `above` invertColour pic)

page55::IO()
page55 = render (fourPics horse)


fourImgs :: Picture -> Picture
fourImgs pic =
	let esq = (pic `above` invertColour pic)
	    dir = (invertColour . flipV)(pic `above` invertColour pic)
	in esq `beside` dir


fourPics2 :: Picture -> Picture
fourPics2 pic = 
	top `above` bottom
		where
		 top = pic `beside`  flipV(invertColour pic)
		 bottom = invertColour pic `beside` flipV (pic)


-- Exercicio 4.5

exer4_5:: Picture -> Picture
exer4_5 pic = 
		left `beside` right
		where
			invPic = invertColour pic
			left = pic `above` invPic
			right = (invertColour (flipV left))

exer4_5B:: Picture -> Picture
exer4_5B pic = 
	let invPic = invertColour pic
	    left = pic `above` invPic
	    right = (invertColour (flipV left))
	in left `beside` right




blackSquares:: Integer -> Picture
blackSquares a | a <= 1 = black
	     | otherwise = black `beside` blackSquares (a-1)





whiteBlack:: Integer ->  Picture
whiteBlack b  | b <=1 = red
	      | otherwise = red `beside` blackWhite(b-1)

blackWhite:: Integer -> Picture
blackWhite c | c<=1 = black
	     | otherwise = black `beside` whiteBlack(c-1)

blackChess:: Integer -> Integer -> Picture
blackChess a b | a <= 1 = blackWhite b
	       | otherwise = blackWhite b `above` whiteChess (a-1) b


whiteChess:: Integer -> Integer -> Picture
whiteChess a b  | a <= 1 = whiteBlack b
		| otherwise = whiteBlack b `above` blackChess(a-1) b


exer4_26:: Picture -> Integer -> Picture
exer4_26 pic a | a <= 1 = pic
               |otherwise = pic `above` exer4_26 pic (a-1)

exer4_27:: Integer -> Integer ->  Picture -> Picture
exer4_27 n m p |n == 0 = horse 
	       | n == m = exer4_27 (n) (m-1) black
               | m == 0 = exer4_27 (n-1) (m) red
               | otherwise = exer4_27 (n) (m-1) red 
             
	 












