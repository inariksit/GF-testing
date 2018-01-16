## Testing PrepNP without context

### Old grammar

PrepNP : Prep_133 → _411 → Adv_7
--------------------------------

PrepNP without_Prep (UsePron i_Pron) : Adv_7
zonder me

PrepIP : Prep_133 → _430 → IAdv_31
----------------------------------

PrepIP without_Prep whoSg_IP : IAdv_31
zonder wie




### New grammar

PrepNP : Prep_146 → _684 → Adv_8
--------------------------------
PrepNP without_Prep (UsePron i_Pron) : Adv_8
zonder me

PrepNP : Prep_146 → _685 → Adv_8
--------------------------------

PrepNP without_Prep (UsePron it_Pron) : Adv_8
zonder het

PrepNP : Prep_147 → _684 → Adv_8
--------------------------------

PrepNP with_Prep (UsePron i_Pron) : Adv_8
met me

PrepNP : Prep_147 → _686 → Adv_8
--------------------------------

PrepNP with_Prep (UsePron it_Pron) : Adv_8
ermee

PrepIP : Prep_146 → _811 → IAdv_36
----------------------------------

PrepIP without_Prep whoSg_IP : IAdv_36
zonder wie

PrepIP : Prep_146 → _812 → IAdv_36
----------------------------------

PrepIP without_Prep whatSg_IP : IAdv_36
zonder wat

PrepIP : Prep_147 → _811 → IAdv_36
----------------------------------

PrepIP with_Prep whoSg_IP : IAdv_36
met wie

PrepIP : Prep_147 → _812 → IAdv_36
----------------------------------

PrepIP with_Prep whatSg_IP : IAdv_36
waarmee


## Testing DetNP with context

### Old grammar


#### DetNP : _477 → NP_105


DetNP (DetQuant DefArt NumSg) : NP_105
die

Now showing that tree in context:
UseCl (TTAnt TPres ASimul) PPos (ExistNP (NP_105))
er is [(NP_105)]
er is die

AdvS (PrepNP without_Prep (NP_105)) (UseCl (TTAnt TPres ASimul) PPos (ExistNP something_NP))
zonder [(NP_105)] is er iets
zonder die is er iets


#### DetNP : _477 → NP_105

DetNP every_Det : NP_105
elk

Now showing that tree in context:
UseCl (TTAnt TPres ASimul) PPos (ExistNP (NP_105))
er is [(NP_105)]
er is elk
AdvS (PrepNP without_Prep (NP_105)) (UseCl (TTAnt TPres ASimul) PPos (ExistNP something_NP))
zonder [(NP_105)] is er iets
zonder elk is er iets

#### DetNP : _478 → NP_107


DetNP (DetQuant IndefArt NumPl) : NP_107
een

Now showing that tree in context:
UseCl (TTAnt TPres ASimul) PPos (ExistNP (NP_107))
er is [(NP_107)]
er zijn een
AdvS (PrepNP without_Prep (NP_107)) (UseCl (TTAnt TPres ASimul) PPos (ExistNP something_NP))
zonder [(NP_107)] is er iets
zonder een is er iets

### New grammar

#### DetNP : _759 → NP_94
DetNP (DetQuant (PossPron i_Pron) NumSg) : NP_94
het mijne

Now showing that tree in context:
UseCl (TTAnt TPres ASimul) PPos (ExistNP (NP_94))
er is [(NP_94)]
er is het mijne
AdvS (PrepNP without_Prep (NP_94)) (UseCl (TTAnt TPres ASimul) PPos (ExistNP something_NP))
zonder [(NP_94)] is er iets
zonder het mijne is er iets

#### DetNP : _759 → NP_94
DetNP (DetQuant DefArt NumSg) : NP_94
die

Now showing that tree in context:
UseCl (TTAnt TPres ASimul) PPos (ExistNP (NP_94))
er is [(NP_94)]
er is die
AdvS (PrepNP without_Prep (NP_94)) (UseCl (TTAnt TPres ASimul) PPos (ExistNP something_NP))
zonder [(NP_94)] is er iets
zonder die is er iets

#### DetNP : _759 → NP_94
DetNP someSg_Det : NP_94
enig

Now showing that tree in context:
UseCl (TTAnt TPres ASimul) PPos (ExistNP (NP_94))
er is [(NP_94)]
er is enig
AdvS (PrepNP without_Prep (NP_94)) (UseCl (TTAnt TPres ASimul) PPos (ExistNP something_NP))
zonder [(NP_94)] is er iets
zonder enig is er iets

#### DetNP : _760 → NP_96
DetNP (DetQuant DefArt NumPl) : NP_96
die

Now showing that tree in context:
UseCl (TTAnt TPres ASimul) PPos (ExistNP (NP_96))
er is [(NP_96)]
er zijn die
AdvS (PrepNP without_Prep (NP_96)) (UseCl (TTAnt TPres ASimul) PPos (ExistNP something_NP))
zonder [(NP_96)] is er iets
zonder die is er iets

#### DetNP : _761 → NP_118
DetNP (DetQuant this_Quant NumSg) : NP_118
dit

Now showing that tree in context:
UseCl (TTAnt TPres ASimul) PPos (ExistNP (NP_118))
er is [(NP_118)]
er is dit
AdvS (PrepNP without_Prep (NP_118)) (UseCl (TTAnt TPres ASimul) PPos (ExistNP something_NP))
zonder [(NP_118)] is er iets
zonder dit is er iets
AdvS (PrepNP with_Prep (NP_118)) (UseCl (TTAnt TPres ASimul) PPos (ExistNP something_NP))
met [(NP_118)] is er iets
hiermee is er iets

#### DetNP : _762 → NP_120
DetNP (DetQuant this_Quant NumPl) : NP_120
deze

Now showing that tree in context:
UseCl (TTAnt TPres ASimul) PPos (ExistNP (NP_120))
er is [(NP_120)]
er zijn deze
AdvS (PrepNP without_Prep (NP_120)) (UseCl (TTAnt TPres ASimul) PPos (ExistNP something_NP))
zonder [(NP_120)] is er iets
zonder deze is er iets
AdvS (PrepNP with_Prep (NP_120)) (UseCl (TTAnt TPres ASimul) PPos (ExistNP something_NP))
met [(NP_120)] is er iets
hiermee is er iets
