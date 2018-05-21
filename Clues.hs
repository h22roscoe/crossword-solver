module Clues where
 
import Types

clue :: Int -> Clue
clue 1
  = ("companion shredded corset",6) -- ESCORT
clue 2
  = ("notice in flying coat", 6) -- JACKET 
clue 3
  = ("companion found in oklahoma terminal", 4)
clue 4
  = ("a new member returned a woman", 6)
clue 5
  = ("pause at these i fancy", 8) -- Everyman 3526, clue 1   ["athetise","hesitate"] 
clue 6
  = ("ankle was twisted in ballet", 8) -- Everyman 3526, clue 3
clue 7
  = ("flyer needed by funfair manager", 6)
clue 8
  = ("put food in this stuff on barge at sea", 9) 
clue 9
  = ("notice supervisor is going nuts at first", 4)
clue 10
  = ("animal is mistake crossing one river", 7)
clue 11
  = ("maria not a fickle lover", 9)
clue 12
  = ("hope for high praise", 6)  
clue 13
  = ("Not fed partly twigged", 5)
clue 14 
  = ("Messy bit of lung next to part of kempton", 7)
clue 15 
  = ("bee leaves branch for station", 5)
clue 16
  = ("Man changing line around Gates head", 5)
clue 17 
  = ("Animal returns to grass", 4)
clue 18 
  = ("bums mix for deals without energy", 9)
clue 19 
  = ("liberal posh wearing platinum with fancy cars to give rich people", 10)
clue 20
  = ("indications show surprising gains for example after recovery",7)
clue 21 
  = ("Scholarly head of languages brought in", 7)
clue 22 
  = ("A zebra dropping guts, munching bitter plant",6)
clue 23
  = ("Withdraw Carter - about time!",7)
clue 24 -- STUN G26508
  = ("Heads turn in shock",4)
-- OFFERS - not Ximenean (Guardian)
clue 25
  = ("Proposals for heartless transgressors",6) 
clue 26
  = ("Servant, a sober worker, receives tip", 9)
clue 27
  = ("Its served on board found in apricot tarts", 7) -- ricotta
clue 28
  = ("cheese found in apricot tarts", 7)
clue 29
  = ("cutting ditch next to a new junction", 9)
clue 30 
  = ("The reason a son left location", 5)

-- Guardian clues 10/6/16
clue 31
  = ("info about large valley", 4) -- glen
clue 32
  = ("vegetable everyone put in picture", 7) -- shallot

-- Guardian 12/7/17 - this takes a while and "l" has to be early in the list of 
-- synonyms of "left"
clue 33
  = ("Left to invade African country with armies, not revolutionary masses",15)
-- conglomerations

-- Random tests of operators...
clue 34
  = ("bird crowns at odds with backward staff", 6) -- condor
clue 35
  = ("part of boss-head, even money flipper", 4) -- shoe
clue 36
  = ("opening can of nails treck mostly back to event", 7) -- concert
clue 37
  = ("one hundred and one late orders demanded first in fort",7) -- citadel

-- Not Ximenean as "re" is part of anagram
clue 50
  = ("Partner having left, really in debt, sums wasted, money spent", 12)

-- Solveable but no synonym for answer
clue 101
  = ("Poorly made Russian fighter returns with a bang",8) -- gimcrack
clue 102
  = ("Skin disease going around English riding school",6) -- manege
clue 103
  = ("Limit area where cattle may graze",5) -- range
clue 104
  = ("They take advantage of a sailor needing employers",7) -- abusers
clue 105
  = ("Package returned has address with added detail",9) -- elaborate


paperClue 1
  = ("Do run to church", 6) -- fleece
paperClue 2
  = ("Body essence", 6) -- 
paperClue 3
  = ("Loose reconstruction of old suites", 9)
paperClue 4
  = ("Cancel article in disgust", 6)
paperClue 5 
  = ("Rule in theatre ignored", 5)
paperClue 6
  = ("pawn in duel regularly set aside",5) -- annul E3694
paperClue 7
  = ("furious buccaneer deprived of power",5) -- irate E3694
paperClue 8
  = ("flightless bird caught finally in ditch",4) -- moat E3693
-- Fined - indirect homophone (E3694)
paperClue 9
  = ("reported discovery, penalised",5)
paperClue 10 -- E3672 entail
  = ("call for extremes of exertion before end", 6)
paperClue 11 -- G27269 stretta
  = ("musical passage starts to sound terrific really exciting to the audience",7)
paperClue 12 -- G27285 parisian
  = ("French citizen taking part in sin - and endlessly",8)
paperClue 13 -- G27288 stir
  = ("commotion caused by heads of state talking in riddles", 4)
paperClue 14 -- G27288 intricate
  = ("Fancy popular gallery keeping racist leader in charge!",9)
paperClue 15 -- G27288 dinner
  = ("revolutionary coming back to eat pub grub?",6)


-- https://www.crosswordgiant.com/crossword-puzzle/121080/The-Times-Cryptic/Times-Cryptic-26783-July-21-2017/2017-07-21
-- grass: can transform partof (reverse text)) to reverse (partof text)...
times 1
  = ("Shop less, arguably, after going around stores", 5) 
times 2
  = ("Arab I transported across Bible Land", 7)
times 3
  = ("hurry on by to see holiday apartment",9)
times 4
  = ("A poem regularly reflecting on mostly sad state", 5)
-- Solvable with reverse dict.
times 5
  = ("A poem regularly reflecting on mostly sad state of islands", 5)
times 6
  = ("title or medal for wrestling", 7)
times 7 -- pooh-poohs
  = ("Upset, small twin rings and knocks",9)
times 8 -- afraid
  = ("A hand round brother, cowering", 6)
times 9 -- wield
  = ("One involved in joint exercise", 5)
times 10 -- fibres
  = ("Threads of story remain incomplete", 6)
times 11
-- SHAM [forged] + EL ESS [two letters "written out"]
-- shameless; can be parsed as "letters, for example", in which
-- case plurals may also trigger Duplicates. Then,
-- ExampleOf (...) ["written","out"] (Text ["letters]") can
-- be parsed using Duplicate because the "letters" is plural.
  = ("Forged letters written out in bold", 9)
-- threw
times 12
  = ("called out thanks to cast", 5)
-- was
times 13
  = ("Used to be clean after wiping hard", 3)
-- Richter: cannot be solved without indirect subtext, i.e.
-- SubText txt ind (ExampleOf txt' ind' ws)
times 14
  = ("Fat Hilary maybe briefly rocking scale", 7)
times 15
  = ("Following on, batting to open the day", 7)

testClue :: Clue
testClue
  = ("stuffed position reversed moor just", 18) -- standing room only
-- ("stuffed","",Concatenate [Synonym "position",Reversal ["reversed"] (Synonym
-- "moor"),Synonym "just"] [],18)
-- The following parse is very expensive, but is OK for clue length 12, say:
-- *Main> solveParseTree ("stuffed position reversed moor just",18) (("stuffed
-- position reversed moor just",18),["just"],[],Insertion ["stuffed"] (Synonym
-- ["moor"]) (Concatenate [Synonym ["position"],Synonym ["reversed"]]))

expensiveClue :: Clue
-- Position 1214 from 24688 parses
expensiveClue -- G27269
  = ("story of church fellow in charge, having drink, losing head",9) 

noahClue :: Clue
noahClue 
  = ("noahs son rose heading off in a muddle", 9)

-- Solvable using reverse dictionary...
--
-- Obscenity. 
clue' 1
  = ("Naughty word shocked nicest boy", 9) 
-- mayoress
clue' 2
  = ("Official service covers times gone by", 8)
clue' 3 -- dialectic G27269
  = ("System of reasoning set up has frantic hospital missing out", 9)
clue' 4
  = ("novel by incomer in its early stages", 9)

-- Unsolvable, with explanation

-- California. Distribute peeling
clue'' 1 
  = ("Peeling paint, profit slack, upset, in a state", 10) 
-- Castrate. Takes testicles would work with rev dic
clue'' 2
  = ("Actors' fees take balls", 8)
-- Decompose. Needs anagram after mostly is applied. Easily fixed by allowing
-- literal subtext under an anagram (needs extra parsing rule for subtext
-- without synonyms.
clue'' 3
  = ("Ask to support alternative comedy, mostly rot", 9)
clue'' 4
  = ("After disturbance, centre court's crowd finally gets composed again", 13)

guardian 1 -- tearful (27396)
  = ("distressed tenor given a wigging",7)
guardian 2 -- marinate (27396)
  = ("steep terrace unoccupied behind mooring area",8)
guardian 3 -- extra (27396)
  = ("more cunning times puzzle ultimately rejected",5)

