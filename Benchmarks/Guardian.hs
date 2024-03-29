module Benchmarks.Guardian where

import Types

guardian :: [(Clue, String)]
guardian = [
  (("Mobile medical spout",7), "DECLAIM"),
  (("Rocky lays into Patten flier of the future",9), "CHRYSALIS"),
  (("Secure backing to break left hand somewhere in Scotland",5), "LEITH"),
  (("Grass spades advantage",5), "SEDGE"),
  (("Week remains Doris's cleaning time",7), "WASHDAY"),
  (("Game sacrifice of good compass",6), "GAMBIT"),
  (("A sum for recording Believe",6), "CREDIT"),
  (("Cry off limiting gene modification in a certain style",7), "REGENCY"),
  (("Who dislikes foreign types of plastic phone boxes no end",9), "XENOPHOBE"),
  (("Crossword setters craft",5), "TRAMP"),
  (("As is evident its heart is quite a different organ",7), "CLEARLY"),
  (("Very backward name of female god",6), "OSIRIS"),
  (("Top removed from hut made an observation tool",7), "HACKSAW"),
  (("Tempted to have iodine for energy - sensational",5), "LURID"),
  (("Tearing hip",7), "DASHING"),
  (("In France Le Blanc is a tabloid target",5), "CELEB"),
  (("He scored Harlem Winds",6), "MAHLER"),
  (("Sounding most upperclass and Victorialike",6), "PLUMMY"),
  (("Reduction of hard bowls area set in green",9), "SHRINKAGE"),
  (("Upturned stone with top marking young mans pile of money",9), "MEGABUCKS"),
  (("Run against the blue opponent of Stalinism",7), "TROTSKY"),
  (("Regulation about Spanish article for space between bed and wall",6), "RUELLE"),
  (("A team or those who have been pulled out",7), "YANKEES"),
  (("Outlandish ultimately futile kiss on the ear",6), "EXOTIC"),
  (("American western type of hat bishop discarded",5), "OATER"),
  (("Shift operative time of sportsmans appointment",8), "BACKDATE"),
  (("Husband in novelists latest yarn recoils in horror",6), "AGHAST"),
  (("Charmer More vexed hosts cheer when hes gone",8), "SORCERER"),
  (("Ten with hip complaint all coming together",6), "INFLUX"),
  (("Harry keeps home functioning",5), "DOING"),
  (("Put small stone inside to make a crackling sound",9), "CREPITATE"),
  (("Aunts et al badly treated in Muslim autocracy",9), "SULTANATE"),
  (("Respond to order about turn",5), "REACT"),
  (("Tablets coating causing harm in mouth",6), "PLAQUE"),
  (("Sole diversion at island welcomed by coppers",8), "PATIENCE"),
  (("Present set back leftwinger after payment of tax",6), "TENDER"),
  (("Arch over seat accommodating two leaders in trial",8), "STRADDLE"),
  (("Lived with carnal thoughts upping temperature",5), "DWELT"),
  (("Canal worker one doomed to carry fallen idol",9), "GONDOLIER"),
  (("Well absolutely empty",4), "ABLY"),
  (("Old mate featured in Times passage",4), "TEXT"),
  (("US graduate overlooked climbing plant",5), "ERICA"),
  (("Strikes up a bit of an argument",4), "SPAT"),
  (("Large bird with down cut",4), "SAWN"),
  (("Spring put an end to game",9), "HOPSCOTCH"),
  (("Caught offensive character",5), "CRANK"),
  (("Many fit in lodge",5), "DWELL"),
  (("Saw about very small worker for profit",9), "ADVANTAGE"),
  (("Was familiar with Kingsleys first novel",4), "KNEW"),
  (("Cavalry dance",7), "LANCERS"),
  (("Time husband should care",7), "THOUGHT"),
  (("Unrestrained tirade about a politician",7), "RAMPANT"),
  (("Dandified Swimmer keeps nothing very quiet",7), "FOPPISH"),
  (("Tuck in a sweatshirt",4), "EATS"),
  (("Weighty foreign product contains bronze",9), "IMPORTANT"),
  (("Ward off state terrorism initially",5), "AVERT"),
  (("Ride around island in Italy",5), "TURIN"),
  (("Little girl meets poet in the chemist",9), "DISPENSER"),
  (("Vegetation thats hard to trim",5), "HEDGE"),
  (("Another name for dopy men US style",9), "PSEUDONYM"),
  (("Injuries in Sumatra disaster",7), "TRAUMAS"),
  (("Beat honoured actor heartlessly",4), "CANE"),
  (("Once more making a profit",5), "AGAIN"),
  (("Thief dressed in scarlet",9), "LARCENIST"),
  (("Frank lugs Elsie around",9), "GUILELESS"),
  (("A king died under military vehicle The mug",7), "TANKARD"),
  (("Its safe to break up for holidays",7), "FIESTAS"),
  (("Narrow strip by river",5), "TAPER"),
  (("Trade union gains prominence in school",5), "TUTOR"),
  (("Press club",4), "IRON"),
  (("Disneyland also lost rides - in a way thats supercilious",7), "SNIDELY"),
  (("Box possibly turned over right before Beeb essentially",4), "TREE"),
  (("American parent left cat",6), "MAMMAL"),
  (("Those attracted to ones who breed for pleasure",8), "FANCIERS"),
  (("Deliverer of baby in section - textbook at either end",5), "STORK"),
  (("Most shortlived footballer around official drinking single",8), "BRIEFEST"),
  (("Where and how superhero might label his faucet",4), "BATH"),
  (("Threads run through stomachs",7), "STRANDS"),
  (("Peace group",7), "NIRVANA"),
  (("A death on Dallas was one shot taking Ewings head",5), "DREAM"),
  (("Handout from kinky fella on film",7), "LEAFLET"),
  (("A company to turn off functions for old computers",6), "ACORNS"),
  (("One seen with clips on Aussie station exEngland captain",7), "SHEARER"),
  (("Outbreak blocked by resistance of the outer skin",9), "EPIDERMIC"),
  (("Relating to Dawn French with vicars last word thats spoken",7), "AURORAL"),
  (("Heading to Angelina Brad finally settled on description for some of their children",7), "ADOPTED"),
  (("One coming last after good sex",6), "GENDER"),
  (("American bisexual in city to get external body parts",5), "LABIA"),
  (("Film star no more tragically",6), "MONROE"),
  (("A king badly hurt at the end",6), "ARTHUR"),
  (("Injure player on side",4), "WING"),
  (("Clean house again",6), "HOOVER"),
  (("Population of Ireland say is riven by malicious gossip",9), "ISLANDERS"),
  (("Supporting diamonds as bridge alternative",4), "FORD"),
  (("Travel in a way hard in wild terrain",4), "BUSH"),
  (("Continue singing including one great number",8), "TRILLION"),
  (("Men on board announced times for retirement",6), "NIGHTS"),
  (("Arranger oddly enough a regular Prom contributor",4), "ARNE"),
  (("In front of mirror taking advantage of reflection",6), "MUSING"),
  --(("Drivers temperature taken by nurse say",6), "CARTER"),
  (("Nice houses builders produced covering island",7), "MAISONS"),
  --(("Desert for example in largely arid state",5), "NEGEV"),
  --(("Mated with variation exposed without bishop and queen",7), "OUTBRED"),
  (("Again try to get male parent outside",6), "REHEAR"),
  (("Two chaps with ecstasy one preferring grass",9), "HERBIVORE"),
  (("Put in new position took a break around one",7), "RESITED"),
  (("They provide pardons for instance covering me initially then you",9), "ABSOLVERS"),
  (("Rely on capital from Asia to produce growth in Australia",7), "BANKSIA"),
  (("Moderate losing head with ruler on more occasions",7), "OFTENER"),
  (("Former Labour leader slow in changing",6), "WILSON"),
  (("Gift from relative coming on time",5), "GRANT"),
  (("Hospital gets distinction and with time gets reverence",6), "HOMAGE"),
  (("Programs for waste regeneration",8), "SOFTWARE"),
  (("Show sense",5), "SIGHT"),
  (("Clergyman promises to be acting for another",9), "VICARIOUS"),
  (("Burn running through housing estate",5), "SINGE"),
  (("Woman shelters a dead duck",5), "EVADE"),
  (("Begin to speak about independent spirit",9), "ORIGINATE"),
  (("Collecting corps soldiers during battle",9), "ACCRETION"),
  (("Teacher losing maidens flower",5), "ASTER"),
  (("Making impervious tiles after initial postponement",8), "PROOFING"),
  (("Goulash so rich it is celebrated",8), "HISTORIC"),
  (("Artists friend retains determination",8), "MAGRITTE"),
  (("Writhe on crumbling doorstep",9), "THRESHOLD"),
  (("Full value of a horse",6), "AMOUNT"),
  (("Followed girl within reason",6), "ENSUED"),
  (("Shower drops on small dish",8), "SPLATTER"),
  (("Finished receiving attention and charmed",8), "ENDEARED"),
  (("Edward caught tucking into dripping in desert",6), "DECAMP"),
  (("Oscar supports largely tactless cowboy",6), "GAUCHO"),
  (("Raider losing copper is angry",5), "IRATE"),
  (("Acrimony between bloggers not involving Hitler",7), "POSTWAR"),
  (("Place kept in identical taste",6), "SAMPLE"),
  (("Playing for time Henry is hurt",6), "ACHING"),
  (("Draw level shortly",3), "TIE"),
  (("Constable for one needs good cloth",3), "RAG"),
  (("Perhaps boxers vulnerable point finally hit at the end - it could be curtains",6), "CHINTZ"),
  (("In front of a parent shake chain",8), "WAGAMAMA"),
  (("Not 20 conservationists in the country",6), "ENTIRE"),
  (("Girl is stupid to return to pub",7), "PHYLLIS"),
  (("Mischievous type caught in UKIPs toils - I would leave",4), "PUCK"),
  (("Woman would go to pot here",4), "SHED"),
  (("Not having a wash So a good night",8), "WAKELESS"),
  (("King upset American element having no Latin",5), "RUFUS"),
  (("Charge according to amount employed",6), "ACCUSE"),
  (("Endless deception covered by Imogen in this system",6), "METRIC"),
  (("Birds sound asleep hard to rouse",6), "HERONS"),
  (("Forces new trial in my case and one is held",8), "MILITARY"),
  (("Girl cuts sister - she uses sharp blades",6), "SKATER"),
  (("Vehicle parking for walk",5), "TRAMP"),
  (("Rich cloak announced for 1 of 6 church leaders",4), "PAUL"),
  (("Socratic method so revolutionary for 1 of 8 here",4), "QAOS"),
  (("Undoing screws I seem to break edges of nuts",7), "NEMESIS"),
  (("Jump into well - or dive",7), "FLEAPIT"),
  (("Cook was sick after falling over",5), "DELIA"),
  (("Underwear for the tits Popular for ass",9), "BIRDBRAIN"),
  (("Glance through introduction to sad Kipling novel",4), "SKIM"),
  (("Currency source of opportunity way across the Channel On the contrary",4), "EURO"),
  (("Tory prime minister in decline contributing to damn all Im Cameron",9), "MACMILLAN"),
  (("Educated girl eating a dish from India",5), "RAITA"),
  (("Ladder out for fugitive",7), "RUNAWAY"),
  (("Pig far from genuine in bed",7), "HAMMOCK"),
  (("Brown up first one tanning all over",6), "NUDIST"),
  (("Young activist in education a degree superior to such a land of impossibility",6), "MALALA"),
  (("Serious thus drink taken halfheartedly",5), "SOBER"),
  (("Tree then or kind of plant",9), "FIRETHORN"),
  (("Island qualified for uprising",4), "ELBA"),
  (("Idiot put on weight - its the whale diet",8), "PLANKTON"),
  (("Rented accommodation where a number stick without roof",8), "TENEMENT"),
  (("Commercial trial at sea in this setters naval law",9), "ADMIRALTY"),
  (("Headlining toddler",8), "SCREAMER"),
  (("US pastors conclusion about religious instruction starts to come apart",8), "AMERICAN"),
  (("Perfect child overcomes first of obstacles in martial art",6), "AIKIDO"),
  (("Gospeller supporting religious teachings its said",6), "REMARK"),
  (("Champ artist",5), "MUNCH"),
  (("Opinion the four of us brought up",4), "VIEW"),
  (("Therapy with effervescent sounding ring to it",6), "PHYSIO"),
  (("Force employees to have hospital filling teeth",8), "CHOPPERS"),
  (("Native American has not a lot going on nose to tail with relatives",8), "ALGONKIN"),
  (("Mawkish medic one with flowers in ’is ’air",6), "DRIPPY"),
  (("2 in the 22 of shop one cant get repaired",9), "CASH"),
  (("One Turin game thats free of charge",8), "NEUTRINO"),
  (("Finished deliveries in Llandovery",4), "OVER"),
  (("Servant uses 2 in the 22 in common location withdrawing large amount",6), "BATMAN"),
  (("Club I visit briefly comic saying the opposite of what is meant",8), "IRONICAL"),
  (("Those taking over position initially held by moneylenders",8), "USURPERS"),
  (("Agile doctor into line dancing",6), "NIMBLE"),
  (("Women moved out of complete dump",4), "HOLE"),
  (("Alternative name held by island archipelago",6), "ORKNEY"),
  (("DIY mum one surprisingly in element appearing nude regularly at 60",9), "NEODYMIUM"),
  (("Aloofness first seen in Scandinavian thats cold inside",8), "DISTANCE"),
  --(("Meet a radio presenter at 2 in  …",6), "ADJOIN"),
  --(("…  the 22 in Paris - an African native often nocturnal",5), "LEMUR"),
  (("Barrier hit after leaving work",4), "WALL"),
  (("Criticise long dash",7), "PANACHE"),
  (("Alternative article in gold",5), "OTHER"),
  (("Offer fantastic reduction",9), "INTRODUCE"),
  (("An imbibing of French port",4), "ADEN"),
  (("Food on the turn",4), "TACK"),
  (("Female relative in liaison showing off",9), "FLAUNTING"),
  (("Soldier in ebbing sea gets support",5), "AEGIS"),
  (("High point of religious architecture",7), "STEEPLE"),
  (("Like hell cats in a melee",7), "SATANIC"),
  (("Selection of superior quality",6), "CHOICE"),
  (("First signs of spring have arrived lets have a ball",6), "SPHERE"),
  (("It enables one to turn ones hand to many things",5), "WRIST"),
  (("Its up to Prior to provide an assortment",9), "POTPOURRI"),
  (("Absence of agreement regarding recess",4), "NOOK"),
  (("A potboiler",8), "CAULDRON"),
  (("All the same the head makes a regular appearance",8), "EVENNESS"),
  (("Unusually fierce sea battle ends with it",9), "CEASEFIRE"),
  (("Art masters pets are dogs",8), "MASTIFFS"),
  (("Vessel - showing its nationality",6), "FLAGON"),
  (("In radiography sickness has a cure",6), "PHYSIC"),
  (("Do they give feelings of elation to meteorologists",5), "HIGHS"),
  (("Cut price bargain",4), "SNIP"),
  (("Doctor at filming location when a certain body sinks",7), "MOONSET"),
  (("Opposing the monarchs entourage in battle",9), "AGINCOURT"),
  (("Use piety falsely as a High Church type",8), "PUSEYITE"),
  (("One had ones work cut out getting man to embrace woman",5), "RODIN"),
  (("Wilder fashion - something that gets under the skin initially",8), "THORNTON"),
  (("Italian in the Lake District maybe when its less cold",7), "UMBRIAN"),
  (("Fool wearing something with feathers as long garment",7), "CASSOCK"),
  (("Ringing of bells around Ireland gets one in an erogenous zone",8), "PERINEAL"),
  (("Audibly show contempt for an offence - ones more than a heel",8), "MOCCASIN"),
  (("Popular panel may be wounding",6), "INJURY"),
  (("Measure of opposition that is restricting politician on trip",9), "IMPEDANCE"),
  (("Yesteryears maestros lost in the morning in the wood",5), "BEECH"),
  (("Lies in legal writ",5), "TALES"),
  (("Mechanical device to give girl breathing difficulties",8), "WINDLASS"),
  (("Unruly behaviour brings strain to port",6), "RIOTRY"),
  (("Change needed for Easter - put in new pews",6), "RESEAT"),
  (("Those spotted behind bushes office heads in womens clothing",6), "BODICE"),
  (("An icon of literary espionage",6), "SMILEY"),
  (("Fifth off 2 or 3 perhaps to be formal",4), "PRIM"),
  (("Bird thrashing about",6), "LINNET"),
  (("Captivated by yeti many drooled over potential killer",8), "DYNAMITE"),
  (("Relative in rocking chair drama queen all conclude",4), "GRAN"),
  (("Reprimand a little bit hasty",8), "SLAPDASH"),
  (("Fill vessel among others containing yummy starters",6), "OCCUPY"),
  (("See good artist turning over knitting pattern",6), "ARGYLE"),
  (("Italian artist - sculptor finally embraced by country Italy",7), "BERNINI"),
  (("End of battle among soldiers on surface of great Chinese river",6), "MEKONG"),
  (("Did you hear Im repulsed by the sight of a sunbather in Mexican state",7), "YUCATAN"),
  (("System in which one calls plastic polythene",9), "TELEPHONY"),
  (("Nerve required to enter place housing choppers Thats here",7), "HELIPAD"),
  (("Carrying poles trail back",7), "SPONSOR"),
  (("Majority to ditch leader in mature response",7), "RIPOSTE"),
  (("Fish finds river in scene of chaos",6), "BARBEL"),
  (("Excessively fast bowler perhaps takes Surrey opener with closing volley",5), "HASTY"),
  (("Last words betray empty jealousy",5), "ENVOY"),
  (("Native solver is speaking in Zulu region",7), "NATURAL"),
  (("Having got up fast I ran to look inside",5), "ASTIR"),
  (("Bust of Edmund I oddly effaced",3), "DUD"),
  (("Endless show of warmth hasten round to shelter from the cold",5), "IGLOO"),
  (("Slightly more than three francs taken in high peril",7), "PITFALL"),
  (("Cheat to win golf in last round by one point",7), "FINAGLE"),
  (("Worker really gutted gets drunk",5), "BEERY"),
  (("Sickly sundae gobbled devoured made sick",9), "NAUSEATED"),
  (("Kind of rhyming as kid set on insect",8), "ASSONANT"),
  (("A kind face",4), "TYPE"),
  (("Failed to manage dire slum reform",8), "MISRULED"),
  (("Motto not quick on the draw say",6), "SLOGAN"),
  (("Be quick to get into alternative musical",6), "OLIVER"),
  (("Boatmans way of hailing Araucaria his leading place a towering old man here",4), "AHOY"),
  (("Quicksand of sorts",5), "SHARP"),
  (("Fast not fast",5), "LOOSE"),
  (("Runs away from deadly duel fast",8), "DEFAULTS"),
  (("Boot option to raise speed",6), "TOECAP"),
  (("Almost sick after fast food for vegetarians",6), "LENTIL"),
  (("Speed zone",4), "BELT"),
  (("Miss start in haste to get back to front in Asia",4), "EAST"),
  (("Could be 0112 0203 0310 0411",4), "DATE")]
