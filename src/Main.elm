module Main exposing (..)

import Dict exposing (Dict)
import Browser
import Html exposing (..)
import Html.Attributes exposing (class, value, type_, checked)
import Html.Events exposing (..)
import Random

-- MAIN

main : Program () Model Msg
main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

-- MODEL
type AttackType
  = MeleeWeaponAttack
  | MeleeSpellAttack
  | RangedSpellAttack
attackTypes: List AttackType
attackTypes = 
  [ MeleeWeaponAttack
  , MeleeSpellAttack
  , RangedSpellAttack ]
attackTypeToString: AttackType -> String
attackTypeToString attackType = case attackType of
  MeleeWeaponAttack -> "Melee Weapon Attack"
  MeleeSpellAttack -> "Melee Spell Attack"
  RangedSpellAttack -> "Ranged Spell Attack"
attackTypeFromString: String -> Maybe AttackType
attackTypeFromString string = case string of
  "Melee Weapon Attack" -> Just MeleeWeaponAttack
  "Melee Spell Attack" -> Just MeleeSpellAttack
  "Ranged Spell Attack" -> Just RangedSpellAttack
  _ -> Nothing

touchOfDeathBonus: Int -> DamageDescriptor
touchOfDeathBonus level = 
  { amount = SupplementedAmount NoAmount (5 + (2 * level))
  , damageType = Necrotic }

type Weapon = WhisperingScythe
weapons: List Weapon
weapons = [ WhisperingScythe ]
weaponToString: Weapon -> String
weaponToString weapon = case weapon of
    WhisperingScythe -> "Whispering Scythe"
weaponFromString: String -> Maybe Weapon
weaponFromString string = case string of
    "Whispering Scythe" -> Just WhisperingScythe
    _ -> Nothing
weaponDamageOf: Weapon -> DamageDescriptor
weaponDamageOf weapon = case weapon of
    WhisperingScythe -> 
      { amount = SupplementedAmount (DiceAmount 2 D4) 1
      , damageType = Slashing }

blessedStrikesBonus: DamageDescriptor
blessedStrikesBonus = 
  { amount = DiceAmount 1 D8 
  , damageType = Necrotic }

spiritShroudBonus: Int -> DamageDescriptor
spiritShroudBonus spellslot = 
  { amount = DiceAmount (1 + (spellslot - 3) // 2) D8
  , damageType = Necrotic }

type Spell 
  = InflictWounds
  | TollTheDead
  | VampiricTouch
meleeSpells: List Spell
meleeSpells = [ InflictWounds, VampiricTouch ]
rangedSpells: List Spell
rangedSpells = [ TollTheDead ]
spellDamageOf: Spell -> Int -> Int -> Bool -> DamageDescriptor
spellDamageOf spell level spellslot enemyDamaged = case spell of
    InflictWounds -> 
      { amount = DiceAmount (2 + spellslot) D10
      , damageType = Necrotic }
    TollTheDead -> 
      let
        diceType = if enemyDamaged then D12 else D8
        diceAmount = if (level < 5) then 2 else if (level < 11) then 3 else 4
      in { amount = DiceAmount diceAmount diceType, damageType = Necrotic}
    VampiricTouch ->
      { amount = DiceAmount spellslot D6, damageType = Necrotic}
spellToString: Spell -> String
spellToString spell = case spell of
    InflictWounds -> "Inflict Wounds"
    TollTheDead -> "Toll the Dead"
    VampiricTouch -> "Vampiric Touch"
spellFromString: String -> Maybe Spell
spellFromString spell = case spell of
    "Inflict Wounds" -> Just InflictWounds
    "Toll the Dead" -> Just TollTheDead
    "Vampiric Touch" -> Just VampiricTouch
    _ -> Nothing

type DiceType 
  = D4
  | D6
  | D8
  | D10
  | D12
diceTypeToString: DiceType -> String
diceTypeToString dice = case dice of
  D4 -> "D4"
  D6 -> "D6"
  D8 -> "D8"
  D10 -> "D10"
  D12 -> "D12"

type DamageType
  = Necrotic
  | Poison
  | Slashing
damageTypeToString: DamageType -> String
damageTypeToString damageType = case damageType of
    Necrotic -> "Necrotic"
    Poison -> "Poison"
    Slashing -> "Slashing"
damageTypeFromString: String -> Maybe DamageType
damageTypeFromString damageType = case damageType of
  "Necrotic" -> Just Necrotic
  "Poison" -> Just Poison
  "Slashing" -> Just Slashing
  _ -> Nothing

type DamageAmount
  = NoAmount
  | DiceAmount Int DiceType
  | SupplementedAmount DamageAmount Int
  | CombinedAmount DamageAmount DamageAmount

type alias DamageDescriptor = 
  { amount: DamageAmount
  , damageType: DamageType }
calculateDamageDescriptors: Model -> List DamageDescriptor
calculateDamageDescriptors model = case model.attack of
    MeleeWeaponAttack -> calculateMeleeWeaponAttackDamageDescriptors model
    MeleeSpellAttack -> calculateMeleeSpellAttackDamageDescriptors model
    RangedSpellAttack -> calculateRangedSpellAttackDamageDescriptors model
calculateMeleeWeaponAttackDamageDescriptors: Model -> List DamageDescriptor
calculateMeleeWeaponAttackDamageDescriptors model = 
  let
    weaponDamage = Just (weaponDamageOf model.weapon)
    bonusOfTouchOfDeath = if model.touchOfDeath then Just (touchOfDeathBonus model.level) else Nothing
    bonusOfBlessedStrikes = if model.blessedStrikes then Just blessedStrikesBonus else Nothing
    bonusOfSpiritShroud = if model.spiritShroud then Just (spiritShroudBonus 3) else Nothing
  in [ weaponDamage, bonusOfTouchOfDeath, bonusOfBlessedStrikes, bonusOfSpiritShroud ]
    |> List.filterMap identity
    |> combineDamageDescriptors
calculateMeleeSpellAttackDamageDescriptors: Model -> List DamageDescriptor
calculateMeleeSpellAttackDamageDescriptors model =
  let
    spellDamage = Just (spellDamageOf model.spell model.level model.spellslot model.enemyDamaged)
    bonusOfTouchOfDeath = if model.touchOfDeath then Just (touchOfDeathBonus model.level) else Nothing
    bonusOfSpiritShroud = if model.spiritShroud then Just (spiritShroudBonus 3) else Nothing
  in [ spellDamage, bonusOfTouchOfDeath, bonusOfSpiritShroud]
    |> List.filterMap identity
    |> combineDamageDescriptors

calculateRangedSpellAttackDamageDescriptors: Model -> List DamageDescriptor
calculateRangedSpellAttackDamageDescriptors model =
  let
    spellDamage = Just (spellDamageOf model.spell model.level model.spellslot model.enemyDamaged)
  in [ spellDamage ]
    |> List.filterMap identity
    |> combineDamageDescriptors

combineDamageAmounts: DamageAmount -> DamageAmount -> DamageAmount
combineDamageAmounts firstAmount secondAmount = case (firstAmount, secondAmount) of
    (NoAmount, _ as amount) -> amount
    (_ as amount, NoAmount) -> amount
    (DiceAmount firstFactor firstDice as first, DiceAmount secondFactor secondDice as second) -> 
      if (firstDice == secondDice)
      then DiceAmount (firstFactor + secondFactor) firstDice
      else CombinedAmount first second
    (DiceAmount _ _ as first, SupplementedAmount second supplement) -> 
      SupplementedAmount (combineDamageAmounts first second) supplement
    (DiceAmount _ _ as diceAmount, CombinedAmount first second) -> CombinedAmount (combineDamageAmounts diceAmount first) second
    (SupplementedAmount firstSupplementedAmount firstSupplement, SupplementedAmount secondSupplementedAmount secondSupplement) -> SupplementedAmount 
      (combineDamageAmounts firstSupplementedAmount secondSupplementedAmount) 
      (firstSupplement + secondSupplement)
    (SupplementedAmount _ _ as supplementedAmount, CombinedAmount first second) -> CombinedAmount (combineDamageAmounts supplementedAmount first) second
    (SupplementedAmount _ _ as first, _ as second) -> combineDamageAmounts second first
    (CombinedAmount combinedFirst combinedSecond, CombinedAmount _ _ as second) -> CombinedAmount (combineDamageAmounts combinedFirst second) combinedSecond
    (CombinedAmount _ _ as first, _ as second) -> combineDamageAmounts second first

combineDamageDescriptors: List DamageDescriptor -> List DamageDescriptor
combineDamageDescriptors descriptors = descriptors
  |> combineDamageDescriptorsInternal
  |> Dict.toList 
  |> List.map (\(damageType, amount) -> damageType 
    |> damageTypeFromString 
    |> Maybe.map (DamageDescriptor amount))
  |> List.filterMap identity

combineDamageDescriptorsInternal: List DamageDescriptor -> Dict String DamageAmount
combineDamageDescriptorsInternal descriptors = List.foldl
  (\desc acc ->
    let
      currentAmount = Dict.get desc.damageType acc
    in case currentAmount of
        Just actualAmount -> Dict.insert desc.damageType (combineDamageAmounts actualAmount desc.amount) acc
        Nothing -> Dict.insert desc.damageType desc.amount acc
  )
  Dict.empty
  (descriptors |> List.map (\descriptor -> 
    { damageType = damageTypeToString descriptor.damageType
    , amount = descriptor.amount }))

type alias Model =
  { dieFace : Int
  , level: Int
  , attack: AttackType
  , touchOfDeath: Bool
  , weapon: Weapon
  , blessedStrikes: Bool
  , spiritShroud: Bool
  , spell: Spell
  , spellslot: Int
  , enemyDamaged: Bool
  }

init : () -> (Model, Cmd Msg)
init _ =
  ( Model 1 1 MeleeWeaponAttack False WhisperingScythe True False InflictWounds 1 False
  , Cmd.none
  )

-- UPDATE

type Msg
  = Roll
  | NewFace Int
  | LevelChanged String
  | AttackTypeChanged String
  | TouchOfDeathToggled Bool
  | WeaponChanged String
  | BlessedStrikesToggled Bool
  | SpiritShroudToggled Bool
  | SpellChanged String
  | SpellslotChanged String
  | EnemyDamagedToggled Bool

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Roll ->
      ( model
      , Random.generate NewFace (Random.int 1 6)
      )
    NewFace newFace ->
      ( Model newFace model.level model.attack model.touchOfDeath model.weapon model.blessedStrikes model.spiritShroud model.spell model.spellslot model.enemyDamaged
      , Cmd.none
      )
    LevelChanged level -> case String.toInt level of
        Just parsedLevel -> ( { model | level = parsedLevel }, Cmd.none )
        Nothing -> ( model, Cmd.none )
    AttackTypeChanged attack -> case attackTypeFromString attack of
        Just attackType -> case attackType of 
          MeleeWeaponAttack -> ( { model | attack = attackType }, Cmd.none )
          MeleeSpellAttack -> ( { model 
            | attack = attackType
            , spell = InflictWounds }, Cmd.none )
          RangedSpellAttack -> ( { model 
            | attack = attackType
            , spell = TollTheDead }, Cmd.none )
        Nothing -> ( model, Cmd.none )
    TouchOfDeathToggled _ -> ( { model | touchOfDeath = not model.touchOfDeath }, Cmd.none )
    WeaponChanged weapon -> case weaponFromString weapon of
        Just parsedWeapon -> ( { model | weapon = parsedWeapon }, Cmd.none )
        Nothing -> ( model, Cmd.none )
    BlessedStrikesToggled _ -> ( { model | blessedStrikes = not model.blessedStrikes }, Cmd.none )
    SpiritShroudToggled _ -> ( { model | spiritShroud = not model.spiritShroud }, Cmd.none )
    SpellChanged spell -> case spellFromString spell of
        Just parsedSpell -> ( { model | spell = parsedSpell }, Cmd.none )
        Nothing -> ( model, Cmd.none )
    SpellslotChanged slot -> case String.toInt slot of
        Just parsedSlot -> ( { model | spellslot = parsedSlot }, Cmd.none )
        Nothing -> ( model, Cmd.none )
    EnemyDamagedToggled _ -> ( { model | enemyDamaged = not model.enemyDamaged }, Cmd.none )

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none

-- VIEW
view : Model -> Html Msg
view model =
  article [ class "card margin padding" ]
    [ setLevel model.level
    , selectAttackType
    , case model.attack of
        MeleeWeaponAttack -> div [] 
          [ selectWeapon
          , setModifiers model ]
        MeleeSpellAttack -> div [] 
          [ selectSpell meleeSpells
          , setSpellSlot model.spellslot
          , setModifiers model ]
        RangedSpellAttack -> div [] 
          [ selectSpell rangedSpells
          , setSpellSlot model.spellslot
          , setModifiers model ]
    , damageDescriptorView model ]

setLevel: Int -> Html Msg
setLevel level = div [] 
  [ label [] [ text "Cleric Class Level" ]
  , input
    [ type_ "number"
    , level |> String.fromInt |> value
    , onInput LevelChanged ] [] ]

checkTouchOfDeath: Bool -> Html Msg
checkTouchOfDeath isActive = label []
  [ input
      [ type_ "checkbox"
      , checked isActive
      , onCheck TouchOfDeathToggled ] []
  , span [ class "checkable" ] [ text " Touch of Death" ] ]

selectAttackType: Html Msg
selectAttackType = div []
  [ label [] [ text "Attack Type" ]
  , select 
    [ class "attack-type"
    , onInput AttackTypeChanged ] 
    ( attackTypes |> List.map attackTypeToString |> List.map toOption ) ]

toOption: String -> Html Msg
toOption description = option [description |> value] [description |> text]

selectWeapon: Html Msg
selectWeapon = div []
  [ label [] [ text "Weapon" ]
  , select 
    [ class "weapon"
    , onInput WeaponChanged ] 
    ( weapons |> List.map weaponToString |> List.map toOption ) ]

checkBlessedStrikes: Bool -> Html Msg
checkBlessedStrikes isActive = label []
  [ input
      [ type_ "checkbox"
      , checked isActive
      , onCheck BlessedStrikesToggled ] []
  , span [ class "checkable" ] [ text " Blessed Strikes" ] ]

checkSpiritShroud: Bool -> Html Msg
checkSpiritShroud isActive = label []
  [ input
      [ type_ "checkbox"
      , checked isActive
      , onCheck SpiritShroudToggled ] []
  , span [ class "checkable" ] [ text " Spirit Shroud" ] ]

selectSpell: List Spell -> Html Msg
selectSpell spells = div []
  [ label [] [ text "Spell" ]
  , select 
    [ class "spell"
    , onInput SpellChanged ] 
    ( spells |> List.map spellToString |> List.map toOption ) ]

setSpellSlot: Int -> Html Msg
setSpellSlot slot = div []
  [ label [] [ text "Spell Slot" ]
  , input
    [ class "spellslot"
    , type_ "number" 
    , onInput SpellslotChanged 
    , slot |> String.fromInt |> value ] [] ]

checkEnemyDamaged: Bool -> Html Msg
checkEnemyDamaged isActive = label []
  [ input
      [ type_ "checkbox"
      , checked isActive
      , onCheck EnemyDamagedToggled ] []
  , span [ class "checkable" ] [ text " Enemy damaged" ] ]

setModifiers: Model -> Html Msg
setModifiers model = div []
  [ label [] [ text "Modifiers" ]
  , article [ class "card padding" ] 
    ( case model.attack of
      MeleeWeaponAttack -> 
        [ checkTouchOfDeath model.touchOfDeath
        , checkBlessedStrikes model.blessedStrikes
        , checkSpiritShroud model.spiritShroud ]
      MeleeSpellAttack -> 
        [ checkTouchOfDeath model.touchOfDeath
        , checkEnemyDamaged model.enemyDamaged
        , checkSpiritShroud model.spiritShroud ]
      RangedSpellAttack -> 
        [ checkEnemyDamaged model.enemyDamaged ] ) ]

damageAmountView: DamageAmount -> Html Msg
damageAmountView amount =
  case amount of
      NoAmount -> span [ class "no-amount" ] []
      DiceAmount factor dice -> span 
        [ class "dice-amount" ] 
        [ factor |> String.fromInt |> text
        , text " ⨉ "
        , dice |> diceTypeToString |> text  ]
      SupplementedAmount supplementedAmount supplement -> span 
        [ class "supplemented-amount" ] 
        [ damageAmountView supplementedAmount
        , (
          case supplementedAmount of 
            NoAmount -> text ""
            _ -> text " + "
        )
        , supplement |> String.fromInt |> text ]
      CombinedAmount first second -> span 
        [ class "combined-amount" ]
        [ damageAmountView first
        , text " + " 
        , damageAmountView second ]

damageDescriptorView: Model -> Html Msg
damageDescriptorView model = model 
  |> calculateDamageDescriptors 
  |> List.map (\descriptor -> span 
    [class "damage-descriptor label"] 
    [ (damageAmountView descriptor.amount)
    , text " "
    , (descriptor.damageType |> damageTypeToString |> text) ])
  |> List.intersperse (text " + ")
  |> div [class "damage-descriptors"]
