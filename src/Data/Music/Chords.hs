{-# LANGUAGE ExistentialQuantification #-}
module Data.Music.Chords
    (
    -- * Chord definition
      Chord (Chord, chordScale, chordNotes, chordSlash), mkChord, (</>)
    , addToChord
    , chordDegrees
    , chordName
    -- * Triads, tetrads and pentachords
    , triad, tetrad, pentachord
    -- * Common chords
    , majorChord, major7Chord, dominant7Chord, minorChord, minor7Chord, minor7b5Chord, minorMajor7Chord
    , major9Chord, minor9Chord, dominant9Chord
    , diminishedChord, augmentedChord
    , susChord, sus2Chord, sus4Chord, sus7Chord
    , major6Chord, minor6Chord, major69Chord, minor69Chord
    ) where

import Data.Monoid
import Data.Set hiding (map)
import qualified Data.Set as S

import Data.Music.Scales

-- | A chord is defined by a scale and a list of degrees.
data Chord = Chord
    { chordScale :: Scale
    , chordNotes :: Set Degree
    , chordSlash :: Degree
    }

mkChord :: Scale -> [Degree] -> Chord
mkChord s l = Chord s (fromList l) (Degree I natural)

(</>) :: Chord -> Degree -> Chord
c </> d = c { chordSlash = d }

addToChord :: Chord -> Degree -> Chord
addToChord (Chord s l sl) d = Chord s (insert d l) sl

-- | Read a chord's degrees.
chordDegrees :: Chord -> Set Degree
chordDegrees (Chord s l sl) = S.map (degreeN s) l

-- TODO: Prove that
--      forall s l: size chordDegrees (Chord s l sl) == size l

instance Show Chord where
    show c = show $ chordDegrees c

matchWithRemainder :: (Ord a) => Set a -> Set a -> Maybe (Set a)
matchWithRemainder a b
    | b `isSubsetOf` a  = Just $ a `difference` b
    | otherwise         = Nothing

-- | Try to guess the name of a chord (incomplete, TODO)
chordName c = chordName' $ chordDegrees c
    where chordName' degrees = case getFirst (matcher degrees) of
            Nothing -> ""
            Just (name, remainder) -> name ++ (chordName' remainder)

          matchWithRemainder' d name m = case matchWithRemainder d m of
            Nothing -> First Nothing
            Just rem -> First $ Just (name, rem)

          matcher d = mconcat $
            [ matchWithRemainder' d "9" $ fromList
                [Degree I natural, Degree II natural, Degree III natural, Degree V natural, Degree VII flat]
            , matchWithRemainder' d "m9" $ fromList
                [Degree I natural, Degree II natural, Degree III flat, Degree V natural, Degree VII flat]
            , matchWithRemainder' d "maj9" $ fromList
                [Degree I natural, Degree II natural, Degree III flat, Degree V natural, Degree VII natural]
            , matchWithRemainder' d "" $ fromList
                [Degree I natural, Degree III natural, Degree V natural]
            , matchWithRemainder' d "m" $ fromList
                [Degree I natural, Degree III flat, Degree V natural]
            , matchWithRemainder' d "+" $ fromList
                [Degree I natural, Degree III natural, Degree V sharp]
            , matchWithRemainder' d "m7b5" $ fromList
                [Degree I natural, Degree III flat, Degree V flat, Degree VII flat]
            , matchWithRemainder' d "dim" $ fromList
                [Degree I natural, Degree III flat, Degree V flat]
            , matchWithRemainder' d "sus4" $ fromList
                [Degree I natural, Degree IV natural, Degree V natural]
            , matchWithRemainder' d "sus2" $ fromList
                [Degree I natural, Degree II natural, Degree V natural]
            , matchWithRemainder' d "6" $ fromList
                [Degree I natural, Degree III natural, Degree VI natural]
            , matchWithRemainder' d "m6" $ fromList
                [Degree I natural, Degree III flat, Degree VI natural]
            , matchWithRemainder' d "maj7" $ fromList
                [Degree VII natural]
            , matchWithRemainder' d "7" $ fromList
                [Degree VII flat]
            , matchWithRemainder' d "9" $ fromList
                [Degree II natural]
            , matchWithRemainder' d "b9" $ fromList
                [Degree II flat]
            , matchWithRemainder' d "#9" $ fromList
                [Degree II sharp]
            , matchWithRemainder' d "11" $ fromList
                [Degree IV natural]
            , matchWithRemainder' d "#11" $ fromList
                [Degree IV sharp]
            , matchWithRemainder' d "13" $ fromList
                [Degree VI natural]
            , matchWithRemainder' d "b13" $ fromList
                [Degree VI flat]
            ]

degreeFunction :: Degree -> String
degreeFunction (Degree I    (Accidental 0)) = "1"
degreeFunction (Degree II   (Accidental 0)) = "9"
degreeFunction (Degree III  (Accidental 0)) = "3"
degreeFunction (Degree IV   (Accidental 0)) = "11"
degreeFunction (Degree V    (Accidental 0)) = "5"
degreeFunction (Degree VI   (Accidental 0)) = "13"
degreeFunction (Degree VII  (Accidental 0)) = "maj7"
degreeFunction (Degree VII  (Accidental (-1))) = "7"
degreeFunction (Degree d    m) = (show m) ++ (degreeFunction $ Degree d (Accidental 0))


-- | A triad.
triad :: Scale -> Chord
triad s = mkChord s [Degree I natural, Degree III natural, Degree V natural]

-- | A tetrad.
tetrad :: Scale -> Chord
tetrad s = mkChord s [Degree I natural, Degree III natural, Degree V natural, Degree VII natural]

-- | A pentachord.
pentachord :: Scale -> Chord
pentachord s = mkChord s [Degree I natural, Degree III natural, Degree V natural, Degree VII natural, Degree II natural]

majorChord = triad ionianScale
major7Chord = tetrad ionianScale
dominant7Chord = tetrad mixolydianScale
minorChord = triad aeolianScale
minor7Chord = tetrad aeolianScale
minor7b5Chord = tetrad locrianScale
minorMajor7Chord = tetrad phrygianScale
diminishedChord = triad locrianScale
augmentedChord = triad ionianAugmentedScale

major9Chord = pentachord ionianScale
minor9Chord = pentachord aeolianScale
dominant9Chord = pentachord mixolydianScale

susChord = mkChord ionianScale [Degree I natural, Degree II natural, Degree IV natural]
sus2Chord = mkChord ionianScale [Degree I natural, Degree II natural, Degree V natural]
sus4Chord = mkChord ionianScale [Degree I natural, Degree IV natural, Degree V natural]
sus7Chord = mkChord mixolydianScale [Degree I natural, Degree II natural, Degree IV natural, Degree VII natural]

major6Chord = mkChord ionianScale [Degree I natural, Degree III natural, Degree VI natural]
minor6Chord = mkChord dorianScale [Degree I natural, Degree III natural, Degree VI natural]
major69Chord = mkChord ionianScale [Degree I natural, Degree III natural, Degree VI natural, Degree II natural]
minor69Chord = mkChord dorianScale [Degree I natural, Degree III natural, Degree VI natural, Degree II natural]
