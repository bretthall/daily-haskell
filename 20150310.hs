{--
Solution to @1HaskellADay http://lpaste.net/6972955679380209664 by @bretthall

Okay, here we go. We continue the dialogue between the king and Scheherazade:

116 )) What type is Bulikaya?

"I like that one," said the king. "Give me another meta-puzzle."

"Very well," said Scheherazade. "I'll give you a metapuzzle about the 
Mazdaysians and Aharmanites.  As you recall, the Mazdaysians worship the good
god Mazda, and always tell the truth; whereas the Aharmanites worship the
evil god Aharman and always lie.  Now, one of the inhabitants of this town was
named Bulikaya.

"He was a rather mysterious individual and no one seemed to know whether he 
was a Mazdaysian or an Aharmanite. Well, at one point it became quite
important to find out, and so the town council asked Omar to investigate the
case.  Omar came across Bulikaya with his friend Ayn Zar. Now, just because
they are friends doesn't mean that they worship the same god."

"How could two friends not worship the same god?" asked the king.

"These people are very tolerant of other people's religions," replied
Scheherazade.  "Anyhow, Omar asked Any Zar: 'Is either of you a Mazdaysian?'
Ayn Zar answered, but Omar could not tell what Bulikaya was. Then Omar asked
Bulikaya: 'Did Ayn Zar answer truthfully?' Bulikaya answered, and then Omar
knew whether he was a Mazdaysian or an Aharmanite. Which was he?"

"Now, this is very baffling," said the king. "Is it really possible to solve
this without being told what either one answered?"

"Yes, it is," replied Scheherazade.

What is the solution? --}

import Data.Maybe
    
data Name = AynZar | Bulkaya
   deriving (Eq, Ord, Enum, Show)

data Religion = Aharmanite | Mazdaysian
   deriving (Eq, Ord, Enum, Show)
religions = enumFrom Aharmanite
            
type Answer = Bool
    
firstQuestion :: [(Religion, Answer, Maybe Religion)] -- possibilities after Ayn's answer
firstQuestion = [fromJust $ answer r a | r <- religions, a <- [True, False], isJust $ answer r a]
    where
      answer Aharmanite True  = Just (Aharmanite, True, Nothing)
      answer Aharmanite False = Just (Aharmanite, False, Just Mazdaysian)
      answer Mazdaysian True  = Just (Mazdaysian, True, Nothing)
      answer Mazdaysian False = Nothing

needSecondQuestion :: [(Religion, Answer, Maybe Religion)] -> [(Religion, Answer)]
needSecondQuestion = map reduce . filter noBulikReligion
    where
      noBulikReligion (_, _, b) = isNothing b
      reduce (x,y,z) = (x,y)

secondQuestionPossiblities :: [(Religion, Answer)] -> [(Religion, Answer, Answer)]
secondQuestionPossiblities as = [(r, a1, a2) | (r, a1) <- as, a2 <- [True, False]]

aharmanitesAlwaysLie :: [(Religion, Answer, Answer)] -> [(Religion, Answer, Answer)]
aharmanitesAlwaysLie  = filter aharmanitesLie
    where
      aharmanitesLie (Aharmanite, _, ans) = not ans
      aharmanitesLie (Mazdaysian, _, _) = True

mazdaysianNeverLie :: [(Religion, Answer, Answer)] -> [(Religion, Answer, Answer)]
mazdaysianNeverLie  = filter tellsTruth
    where
      tellsTruth (Mazdaysian, _, ans) = ans
      tellsTruth (Aharmanite, _, _) = True
                                          
secondAnswer :: [(Religion, Answer, Answer)] -> [(Religion, Answer, Answer, Religion)]
secondAnswer as = [(r1, a1, a2, r2) | (r1, a1, a2) <- as, r2 <- religions, consistent r1 a1 a2 r2]
    where
      consistent Aharmanite True False Mazdaysian = False
      consistent Aharmanite True False Aharmanite = False
      consistent Mazdaysian True True Mazdaysian = True
      consistent Mazdaysian True True Aharmanite = False

bulikayasReligion :: Religion
bulikayasReligion = r
    where
      (_, _, _, r) = head $ secondAnswer $ mazdaysianNeverLie $ aharmanitesAlwaysLie $ secondQuestionPossiblities $ needSecondQuestion firstQuestion

-- *Main> bulikayasReligion
-- Mazdaysian
