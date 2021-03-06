port module Main exposing (..)

import Browser
import Browser.Hash as Hash
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (style, class, href, for)
import Html.Events exposing (..)
import Url
import Url.Parser as Url exposing (Parser, (</>))
import Bootstrap.Navbar as Navbar
import Bootstrap.Spinner as Spinner
import Bootstrap.Grid as Grid
import Bootstrap.Button as Button
import Bootstrap.Dropdown as Dropdown
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Checkbox as Checkbox
import Bootstrap.Text as Text
import Bootstrap.Progress as Progress
import Bootstrap.ListGroup as ListGroup
import Bootstrap.Breadcrumb exposing (item)
import Json.Decode as Decode exposing (Decoder, int, string, float, field, bool, list, maybe)
import Json.Decode.Pipeline exposing (required, optional, hardcoded)
import Json.Encode as Encode
import Http
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (usLocale)
import Uuid
import Random exposing (Seed, initialSeed)

-- MAIN

type alias Flag =
  { baseUrl : String
  , currentDate : String
  , seed : Int
  , apiKey : Maybe String
  }

type alias LoginInfo =
  { username : String
  , password : String
  }

loginInfoEncoder : LoginInfo -> Encode.Value
loginInfoEncoder loginInfo =
  Encode.object
    [ ( "username", Encode.string loginInfo.username )
    , ( "password", Encode.string loginInfo.password )
    ]

main : Program Flag Model Msg
main =
  Hash.application
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    , onUrlChange = UrlChanged
    , onUrlRequest = LinkClicked
    }

-- PORTS

port logout : () -> Cmd msg
port deleteStockInAlert : StockIn -> Cmd msg
port deleteStockIn : (String -> msg) -> Sub msg
port setApiKey : (Maybe String) -> Cmd msg

-- PAGE
type Page
  = Index
  | ProjectPage
  | ProjectDetail String
  | ItemPage
  | ItemDetail String
  | TransactionPage
  | TransactionDetail String
  | StockInPage String

-- URL PARSER
urlParser : Parser (Page -> a) a
urlParser =
  Url.oneOf
    [ Url.map Index Url.top
    , Url.map ProjectPage (Url.s "projects")
    , Url.map ProjectDetail (Url.s "projects" </> Url.string)
    , Url.map ItemPage (Url.s "items")
    , Url.map ItemDetail (Url.s "items" </> Url.string)
    , Url.map TransactionPage (Url.s "transactions")
    , Url.map TransactionDetail (Url.s "transactions" </> Url.string)
    , Url.map StockInPage (Url.s "items" </> Url.string </> Url.s "stockins")
    ]

-- MODEL

type alias Model =
  { key : Nav.Key
  , url : Url.Url
  , currentDate : String
  , loginState : LoginModel
  , baseUrl : String
  , loggedIn : Bool
  , navbarState : Navbar.State
  , projectState : ProjectModel
  , transactionState : TransactionModel
  , itemState : ItemModel 
  , seed : Seed
  , apiKey : Maybe String
  }

type alias LoginModel =
  { loginInfo : LoginInfo
  , requestStatus : RequestStatus
  }
  
initialLoginInfo : LoginInfo
initialLoginInfo =
  { username = ""
  , password = ""
  }

initialLoginModel : LoginModel
initialLoginModel =
  { loginInfo = initialLoginInfo
  , requestStatus = NotAsked
  }

type alias ProjectModel =
  { requestStatus : RequestStatus
  , project : Project
  , projects : ProjectsView
  }

type alias ItemModel =
  { item : Item
  , requestStatus : RequestStatus
  , itemStockViews : List ItemStockView
  , searchInput : String
  , addInitialStock : Bool
  , initialStock : Int
  , itemStockIns : ItemStockInsView
  , stockIn : StockIn
  , selectedProject : Maybe Project
  , projectDropdown : Dropdown.State
  }

initialItemStockInsView : ItemStockInsView
initialItemStockInsView =
  { item = initialItem
  , stockIns = []
  }

type alias ItemStockInsView =
  { item : Item
  , stockIns : List StockIn
  }

itemStockInsViewDecoder : Decoder ItemStockInsView
itemStockInsViewDecoder =
  Decode.succeed ItemStockInsView
    |> required "item" itemDecoder
    |> required "stockIns" (Decode.list stockInDecoder)

type alias TransactionModel =
  { requestStatus : RequestStatus
  , projects : List Project
  , projectsDropdown : Dropdown.State
  , projectTransactionsView : ProjectTransactionsView
  , selectedProject : String
  , transactionView : TransactionView
  , foundItems : List ItemStockView
  , itemTransactionForm : ItemTransaction
  , selectedItem : Maybe Item
  , searchByItem : String
  , itemTransactionDeleteIds : List Int
  }

initialTransactionModel : TransactionModel
initialTransactionModel =
  { requestStatus = NotAsked
  , projects = []
  , projectsDropdown = Dropdown.initialState
  , projectTransactionsView = initialProjectTransationsView
  , selectedProject = "Select Project"
  , transactionView = initialTransactionView
  , foundItems = []
  , itemTransactionForm = initialItemTransaction
  , selectedItem = Nothing
  , searchByItem = ""
  , itemTransactionDeleteIds = [ ]
  }

type RequestStatus 
  = NotAsked
  | Loading
  | Error
  | Success

-- DB MODEL

type alias ApiKey = 
  { id : Int
  , apiKey : String
  , createdAt: String
  , updatedAt: String
  }

apiKeyDecoder : Decoder ApiKey
apiKeyDecoder =
  Decode.succeed ApiKey
    |> required "id" int
    |> required "apiKey" string
    |> required "created_at" string
    |> required "updated_at" string

type alias Project =
  { id : Int
  , uid: String
  , name: String
  , startDate: String
  , updatedAt: String
  , createdAt: String
  }

initialProject : Project
initialProject =
  { id = 0
  , uid = ""
  , name = ""
  , startDate = ""
  , updatedAt = ""
  , createdAt = ""
  }

projectDecoder : Decoder Project
projectDecoder =
  Decode.succeed Project
    |> required "id" int
    |> required "uid" string
    |> required "name" string
    |> required "startDate" string
    |> required "updated_at" string
    |> required "created_at" string

projectEncoder : Project -> Encode.Value
projectEncoder project =
  Encode.object
    [ ("id", Encode.int project.id)
    , ("uid", Encode.string project.uid)
    , ("name", Encode.string project.name)
    , ("startDate", Encode.string project.startDate)
    -- , ("updatedAt", Encode.string project.updatedAt)
    -- , ("createdAt", Encode.string project.createdAt)
    ]


type alias Item =
  { id : Int
  , uid : String
  , name : String
  , description : String
  , price : Int
  , manufacturingPrice : Int
  , updatedAt: String
  , createdAt: String
  }

initialItem : Item
initialItem =
  { id = 0
  , uid = ""
  , name = ""
  , description = ""
  , price = 0
  , manufacturingPrice = 0
  , createdAt = ""
  , updatedAt = ""
  }

itemDecoder : Decoder Item
itemDecoder =
  Decode.succeed Item
    |> required "id" int
    |> required "uid" string
    |> required "name" string
    |> required "description" string
    |> required "price" int
    |> required "manufacturingPrice" int
    |> required "updated_at" string
    |> required "created_at" string
itemEncoder : Item -> Encode.Value
itemEncoder item =
  Encode.object 
    [ ( "id", Encode.int item.id )
    , ( "uid", Encode.string item.uid )
    , ( "name", Encode.string item.name )
    , ( "description", Encode.string item.description )
    , ( "price", Encode.int item.price )
    , ( "manufacturingPrice", Encode.int item.manufacturingPrice )
    -- , ( "updated_at", Encode.string item.updatedAt )
    -- , ( "created_at", Encode.string item.createdAt )
    ]

type alias Transaction =
  { id : Int
  , uid : String
  , cashier : String
  , priceIsCustom : Bool
  , customPrice: Int
  , projectId: Int
  , createdAt: String
  , updatedAt : String
  }

initialTransaction =
  { id = 0
  , uid = ""
  , cashier = ""
  , priceIsCustom = False
  , customPrice = 0
  , projectId = 0
  , createdAt = ""
  , updatedAt = ""
  }

transactionDecoder : Decoder Transaction
transactionDecoder =
  Decode.succeed Transaction
    |> required "id" int
    |> required "uid" string
    |> required "cashier" string
    |> required "priceIsCustom" bool
    |> required "customPrice" int
    |> required "projectId" int
    |> required "updated_at" string
    |> required "created_at" string
transactionEncoder : Transaction -> Encode.Value
transactionEncoder transaction =
  Encode.object
    [ ( "id", Encode.int transaction.id )
    , ( "uid", Encode.string transaction.uid )
    , ( "cashier", Encode.string transaction.cashier )
    , ( "priceIsCustom", Encode.bool transaction.priceIsCustom )
    , ( "customPrice", Encode.int transaction.customPrice )
    , ( "projectId", Encode.int transaction.projectId )
    -- , ( "created_at", Encode.string transaction.createdAt )
    -- , ( "updated_at", Encode.string transaction.updatedAt ) 
    ]

type alias StockIn = 
  { id : Int
  , uid : String
  , pic : String
  , itemId: Int
  , projectId : Int
  , qty : Int
  , updatedAt: String
  , createdAt: String
  }

initialStockIn : StockIn
initialStockIn =
  { id = 0
  , uid = ""
  , pic = ""
  , itemId = 0
  , projectId = 0
  , qty = 0
  , updatedAt = ""
  , createdAt = ""
  }

stockInDecoder : Decoder StockIn
stockInDecoder =
  Decode.succeed StockIn
    |> required "id" int
    |> required "uid" string
    |> required "pic" string
    |> required "itemId" int
    |> required "projectId" int
    |> required "qty" int
    |> required "updated_at" string
    |> required "created_at" string

stockInEncoder : StockIn -> Encode.Value
stockInEncoder stockIn =
  Encode.object
    [ ( "id", Encode.int stockIn.id )
    , ( "uid", Encode.string stockIn.uid )
    , ( "pic", Encode.string stockIn.pic )
    , ( "itemId", Encode.int stockIn.itemId )
    , ( "projectId", Encode.int stockIn.projectId )
    , ( "qty", Encode.int stockIn.qty ) 
    ]

type alias ItemTransaction =
  { id : Int
  , uid : String
  , itemId : Int
  , transactionId : Int
  , qty: Int
  , createdAt : String
  , updatedAt : String
  }
initialItemTransaction : ItemTransaction
initialItemTransaction =
  { id = 0
  , uid = ""
  , itemId = 0
  , transactionId = 0
  , qty = 0
  , createdAt = ""
  , updatedAt = ""
  }
itemTransactionDecoder : Decoder ItemTransaction
itemTransactionDecoder =
  Decode.succeed ItemTransaction
    |> required "id" int
    |> required "uid" string
    |> required "itemId" int
    |> required "transactionId" int
    |> required "qty" int
    |> required "updated_at" string
    |> required "created_at" string  
itemTransactionEncoder : ItemTransaction -> Encode.Value
itemTransactionEncoder itemTransaction =
  Encode.object
    [ ( "id", Encode.int itemTransaction.id )
    , ( "uid", Encode.string itemTransaction.uid )
    , ( "itemId", Encode.int itemTransaction.itemId )
    , ( "transactionId", Encode.int itemTransaction.transactionId )
    , ( "qty", Encode.int itemTransaction.qty )
    -- , ( "updated_at", Encode.string itemTransaction.updatedAt )
    -- , ( "created_at", Encode.string itemTransaction.createdAt )
    ]

type alias ItemStockIn =
  { id : Int
  , uid : String
  , itemId : Int
  , stockInId : Int
  , qty : Int
  , createdAt : String
  , updatedAt : String
  }

itemStockInDecoder : Decoder ItemStockIn
itemStockInDecoder =
  Decode.succeed ItemStockIn
    |> required "id" int
    |> required "uid" string
    |> required "itemId" int
    |> required "stockInId" int
    |> required "qty" int
    |> required "updated_at" string
    |> required "created_at" string
type alias ItemProject =
  { id : Int
  , uid : String
  , itemId : Int
  , projectId : Int
  , qty : Int
  , createdAt : String
  , updatedAt : String
  }

itemProjectDecoder : Decoder ItemProject
itemProjectDecoder =
  Decode.succeed ItemProject
    |> required "id" int
    |> required "uid" string
    |> required "itemId" int
    |> required "projectId" int
    |> required "qty" int
    |> required "updated_at" string
    |> required "created_at" string
    
-- DB VIEW
type alias ProjectView =
  { project : Project
  , income : Int
  , totalManufacturingPrice : Int
  }

projectViewDecoder : Decoder ProjectView
projectViewDecoder =
  Decode.succeed ProjectView
    |> required "project" projectDecoder
    |> required "income" int
    |> required "totalManufacturingPrice" int
type alias ProjectsView =
  { projects : List ProjectView
  , totalIncome : Int
  }
projectsViewDecoder : Decoder ProjectsView
projectsViewDecoder =
  Decode.succeed ProjectsView
    |> required "projects" (Decode.list projectViewDecoder)
    |> required "totalIncome" int

type alias ProjectTransactionsView =
  { project : Maybe Project
  , transactions : List TransactionView
  }
projectTransactionsViewDecoder : Decoder ProjectTransactionsView
projectTransactionsViewDecoder =
  Decode.succeed ProjectTransactionsView
    |> required "project" (maybe projectDecoder)
    |> required "transactions" (Decode.list transactionViewDecoder)

initialProjectTransationsView : ProjectTransactionsView
initialProjectTransationsView =
  { project = Nothing
  , transactions = []
  }

type alias TransactionView =
  { transaction : Transaction
  , itemTransactions : List ItemTransactionView
  , totalPrice : Int
  }

initialTransactionView : TransactionView
initialTransactionView =
  { transaction = initialTransaction
  , itemTransactions = []
  , totalPrice = 0
  }

transactionViewDecoder : Decoder TransactionView
transactionViewDecoder =
  Decode.succeed TransactionView
    |> required "transaction" transactionDecoder
    |> required "itemTransactions" (Decode.list itemTransactionViewDecoder)
    |> required "totalPrice" int

type alias ItemTransactionView =
  { itemTransaction : ItemTransaction
  , item : Item
  }

itemTransactionViewDecoder : Decoder ItemTransactionView
itemTransactionViewDecoder =
  Decode.succeed ItemTransactionView
    |> required "itemTransaction" itemTransactionDecoder
    |> required "item" itemDecoder

itemTransactionViewEncoder :  ItemTransactionView -> Encode.Value
itemTransactionViewEncoder itemTransactionView =
  Encode.object
    [ ( "itemTransaction", itemTransactionEncoder itemTransactionView.itemTransaction )
    , ( "item", itemEncoder itemTransactionView.item )
    ]

type alias ItemStockView =
  { item : Maybe Item
  , inStock : Int
  }

initialItemStockView : ItemStockView
initialItemStockView =
  { item = Nothing
  , inStock = 0
  }

itemStockViewDecoder : Decoder ItemStockView
itemStockViewDecoder =
  Decode.succeed ItemStockView
     |> required "item" (maybe itemDecoder)
     |> required "inStock" int

-- DB POST BODY
type alias TransactionPostBody =
  { transaction : Transaction
  , itemTransactions : List ItemTransactionView
  , itemTransactionDeleteIds : List Int
  }

transactionPostBodyEncoder : TransactionPostBody -> Encode.Value
transactionPostBodyEncoder transactionPostBody =
  Encode.object
    [ ( "transaction", transactionEncoder transactionPostBody.transaction )
    , ( "itemTransactions", (Encode.list itemTransactionViewEncoder) transactionPostBody.itemTransactions )
    , ( "itemTransactionDeleteIds", (Encode.list Encode.int) transactionPostBody.itemTransactionDeleteIds )
    ]

type alias ItemPostBody =
  { item : Item
  , withInitialStock : Bool
  , initialStockQty : Int
  }

itemPostBodyDecoder : Decoder ItemPostBody
itemPostBodyDecoder =
  Decode.succeed ItemPostBody
    |> required "item" itemDecoder
    |> required "withInitialStock" Decode.bool
    |> required "initialStockQty" Decode.int

itemPostBodyEncoder : ItemPostBody -> Encode.Value
itemPostBodyEncoder itemPostBody =
  Encode.object
    [ ( "item", itemEncoder itemPostBody.item )
    , ( "withInitialStock", Encode.bool itemPostBody.withInitialStock )
    , ( "initialStockQty", Encode.int itemPostBody.initialStockQty )
    ]

init : Flag -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flag url key =
  let
    (navbarState, navbarCmd) = Navbar.initialState NavbarMsg

    initialProjectModel : ProjectModel
    initialProjectModel =
      { project = initialProject
      , requestStatus = NotAsked
      , projects = initialProjectsView
      }

    initialProjectsView : ProjectsView
    initialProjectsView =
      { projects = []
      , totalIncome = 0
      }

    initialItemModel : ItemModel
    initialItemModel =
      { item = initialItem
      , requestStatus = NotAsked
      , itemStockViews = []
      , searchInput = ""
      , addInitialStock = False
      , initialStock = 0
      , itemStockIns = initialItemStockInsView
      , stockIn = initialStockIn
      , selectedProject = Nothing
      , projectDropdown = Dropdown.initialState
      }
    loggedIn =
      case flag.apiKey of
        Just _ ->
          True

        _ ->
          False

    initialModel : Model 
    initialModel =
      { key = key
      , url = url
      , baseUrl = flag.baseUrl
      , currentDate = flag.currentDate
      , loggedIn = loggedIn
      , loginState = initialLoginModel
      , navbarState = navbarState
      , projectState = initialProjectModel
      , transactionState = initialTransactionModel
      , itemState = initialItemModel
      , seed = initialSeed flag.seed
      , apiKey = flag.apiKey
      }
  in
  ( initialModel
  , Cmd.batch 
      [ navbarCmd
      , sendRequest
          initialModel.baseUrl
          "GET"
          "/projects"
          Http.emptyBody
          (Http.expectJson GotProjects (Decode.list projectDecoder))
      ] 
  )




-- UPDATE


type Msg
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url
  | NavbarMsg Navbar.State
  -- Auth
  | Login
  | ChangeLoginUsername String 
  | ChangeLoginPassword String
  | GotLoginResponse (Result Http.Error String)
  | Logout
  -- Project & Transaction
  | ToggleProject Dropdown.State
  | SelectProject Project
  | GotProject (Result Http.Error (Project))
  | GotProjects (Result Http.Error (List Project))
  | GotProjectsView (Result Http.Error ProjectsView)
  | GotItems (Result Http.Error (List ItemStockView))
  -- Project
  | InputProjectName String
  | InputProjectDate String
  | SaveProject
  | SavedProject (Result Http.Error String)
  -- Transaction
  | GotProjectTransaction (Result Http.Error ProjectTransactionsView)
  | GotTransaction (Result Http.Error Transaction)
  | GotTransactionView (Result Http.Error TransactionView)
  | CheckPriceIsCustom
  | ChangeCustomPrice String
  | SearchItem String
  | GotSearchedItems (Result Http.Error (List ItemStockView))
  | ChangeItemTransactionFormQty String
  | InputSearchByItem String
  | SelectItemToAdd Item
  | InsertItemToList
  | DeleteItemTransaction ItemTransaction
  | SaveTransaction
  | SavedTransaction (Result Http.Error String)
  -- Item
  | InputSearchItem String
  | ChangeItemName String
  | ChangeItemDescription String
  | ChangeItemPrice String
  | ChangeItemManufacturingPrice String
  | ToggleInitialStock
  | ChangeInitialStock String
  | GotItem (Result Http.Error Item)
  | SaveItem
  | SavedItem (Result Http.Error String)
  -- Stockin
  | GotItemStockInsView (Result Http.Error ItemStockInsView)
  | AddStockIn
  | SavedStockIns (Result Http.Error ())
  | ChangeStockInQty String
  | DeleteStockInAlert StockIn
  | DeleteStockIn String
  | ToggleStockInDropdownState Dropdown.State
  | SelectStockInProject Project

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    LinkClicked urlRequest ->
      case urlRequest of
        Browser.Internal url ->
          ( model, Nav.pushUrl model.key (Url.toString url) )

        Browser.External href ->
          ( model, Nav.load href )

    UrlChanged url ->
      fetchByUrl { model | url = url  }

    Login ->
      let
        loginState = model.loginState
        newLoginState = { loginState | requestStatus = Loading }
      in
      ( { model | loginState = newLoginState }
      , Http.request
          { method = "POST"
          , headers = []
          , url = model.baseUrl ++ "/login"
          , body = Http.jsonBody (loginInfoEncoder model.loginState.loginInfo)
          , expect = Http.expectString GotLoginResponse
          , timeout = Nothing
          , tracker = Nothing
          }
      )
      -- fetchByUrl { model | loggedIn = True }

    ChangeLoginUsername username ->
      let
        loginState = model.loginState
        loginInfo = loginState.loginInfo
        newLoginInfo = { loginInfo | username = username }
        newLoginState = { loginState | loginInfo = newLoginInfo }
      in
      ( { model | loginState = newLoginState }, Cmd.none )

    ChangeLoginPassword password ->
      let
        loginState = model.loginState
        loginInfo = loginState.loginInfo
        newLoginInfo = { loginInfo | password = password }
        newLoginState = { loginState | loginInfo = newLoginInfo }
      in
      ( { model | loginState = newLoginState }, Cmd.none )

    GotLoginResponse res ->
      case res of
        Ok apiKey ->
          let
            loginState = model.loginState
            newLoginState = { loginState | requestStatus = Success }
            newModel = 
              { model 
              | apiKey = Just apiKey 
              , loginState = newLoginState
              , loggedIn = True
              }
          in
          ( newModel
          , Cmd.batch
              [ setApiKey (Just apiKey)
              ]
          )
        
        Err _ ->
          let
            loginState = model.loginState
            newLoginState = { loginState | requestStatus = Error }
          in
          ( { model | loginState = newLoginState }, Cmd.none )

    Logout ->
      ( { model 
        | loggedIn = False 
        , apiKey = Nothing
        }
      , setApiKey Nothing
      )

    NavbarMsg state -> 
      ( { model | navbarState = state }, Cmd.none )

    ToggleProject state ->
      let
        transactionState = model.transactionState
        newTransactionState = { transactionState | projectsDropdown = state }
      in
      ( { model | transactionState = newTransactionState }, Cmd.none )
    
    SelectProject project ->
      let
        transactionState = model.transactionState
        projectTransactionsView = transactionState.projectTransactionsView

        newProjectTransactionsView = { projectTransactionsView | project = Just project }
        newTransactionState = 
          { transactionState  
          | requestStatus = Loading
          , selectedProject = project.name
          , projectTransactionsView = newProjectTransactionsView
          }
      in
        ( { model | transactionState = newTransactionState }
        , sendRequest
            model.baseUrl
            "GET"
            ("/projects/" ++ String.fromInt project.id ++ "/transactions")
            Http.emptyBody
            (Http.expectJson GotProjectTransaction projectTransactionsViewDecoder)
        )

    GotProjects res ->
      case res of
        Ok projects ->
          let
            transactionState = model.transactionState
            newTransactionState = { transactionState | projects = projects, requestStatus = Success }
          in
          ( { model | transactionState = newTransactionState }, Cmd.none )

        Err _ ->
          ( model, Cmd.none )

    GotProjectsView res ->
      case res of
        Ok projectViews ->
          let
            projectState = model.projectState
            newProjectState = { projectState | projects = projectViews, requestStatus = Success }
          in
          ( { model | projectState = newProjectState }, Cmd.none )

        Err e ->
          ( model, Cmd.none )

    GotProject res ->
      case res of
        Ok project ->
          let
            projectState = model.projectState
            newProjectState = { projectState | project = project }
          in
          ( { model | projectState = newProjectState }, Cmd.none )

        Err _ ->
          ( model, Cmd.none )

    GotItems res ->
      case res of
        Ok itemStockViews ->
          let
            itemState = model.itemState
            newItemState = { itemState | itemStockViews = itemStockViews, requestStatus = Success }
          in
          ( { model | itemState = newItemState }, Cmd.none )
        
        Err _ ->
          let
            itemState = model.itemState
            newItemState = { itemState | requestStatus = Error }
          in
          ( { model | itemState = newItemState }, Cmd.none )

    InputProjectName name ->
      let
        projectState = model.projectState
        project = projectState.project

        newProject = { project | name = name }
        newProjectState = { projectState | project = newProject }
      in
      ( { model | projectState = newProjectState }, Cmd.none )

    InputProjectDate date ->
      let
        projectState = model.projectState
        project = projectState.project

        newProject = { project | startDate = date }
        newProjectState = { projectState | project = newProject }
      in
      ( { model | projectState = newProjectState }, Cmd.none )

    SaveProject ->
      let
        project = model.projectState.project
        startDate = if project.startDate == "" then model.currentDate else project.startDate
        parsedProject = { project | startDate = String.slice 0 10 startDate }
      in
      ( model
      , sendRequest
          model.baseUrl
          "POST"
          "/projects"
          (Http.jsonBody (projectEncoder <| parsedProject))
          (Http.expectString SavedProject) 
      )

    SavedProject res ->
      case res of
        Ok str ->
          ( model, 
            Cmd.batch
              [ sendRequest
                  model.baseUrl
                  "GET"
                  "/projects"
                  Http.emptyBody
                  (Http.expectJson GotProjects (Decode.list projectDecoder))
              , Nav.pushUrl model.key "/#/projects"
              ] 
          )

        Err e ->
          ( model, Cmd.none )

    GotProjectTransaction res ->
      let
        transactionState = model.transactionState
      in
      case res of
        Ok projectTransactionsView ->
          let
            newTransactionState = { transactionState | requestStatus = Success, projectTransactionsView = projectTransactionsView }
          in
          ( { model | transactionState = newTransactionState}, Cmd.none )
        
        Err e ->
          let
            newTransactionState = { transactionState | requestStatus = Error }
          in
          ( { model | transactionState = newTransactionState}, Cmd.none )

    GotTransaction res ->
      let
        transactionState = model.transactionState
      in
      case res of
        Ok transaction ->
          let
            newTransactionState = { transactionState | requestStatus = Success }
          in 
            ( { model | transactionState = newTransactionState }, Cmd.none )

        Err _ ->
          let
            newTransactionState = { transactionState | requestStatus = Error }
          in 
            ( { model | transactionState = newTransactionState }, Cmd.none )

    GotTransactionView res ->
      let
        transactionState = model.transactionState
      in
      case res of
        Ok transactionView ->
          let
            newTransactionState = 
              { transactionState 
              | requestStatus = Success
              , transactionView = transactionView 
              }
          in
          ( { model | transactionState = newTransactionState }, Cmd.none )

        Err _ ->
          let
            newTransactionState = { transactionState | requestStatus = Error }
          in 
          ( { model | transactionState = newTransactionState }, Cmd.none )

    CheckPriceIsCustom ->
      let
        transactionState = model.transactionState
        transactionView = transactionState.transactionView
        transaction = transactionView.transaction

        newTransaction =  { transaction | priceIsCustom = not transaction.priceIsCustom }
        newTransactionView = { transactionView | transaction = newTransaction }
        newTransactionState = { transactionState | transactionView = newTransactionView  }
      in
        ( { model | transactionState = newTransactionState }, Cmd.none )

    ChangeCustomPrice customPrice ->
      let
        transactionState = model.transactionState
        transactionView = transactionState.transactionView
        transaction = transactionView.transaction

        newTransaction = { transaction | customPrice = Maybe.withDefault 0 <| String.toInt customPrice }
        newTransactionView = { transactionView | transaction = newTransaction }
        newTransactionState = { transactionState | transactionView = newTransactionView }
      in
        ( { model | transactionState = newTransactionState }, Cmd.none )

    SearchItem itemName ->
      ( model
      , sendRequest 
          model.baseUrl
          "GET"
          ("/itemsearch?name=" ++ itemName)
          Http.emptyBody
          (Http.expectJson GotSearchedItems (Decode.list itemStockViewDecoder))
      )

    GotSearchedItems res ->
      case res of
        Ok items ->
          let
            transactionState = model.transactionState
            newTransactionState = { transactionState | foundItems = items }
          in
          ( { model | transactionState = newTransactionState }, Cmd.none )
        
        _ ->
          ( model, Cmd.none )

    ChangeItemTransactionFormQty qtyString ->
      let
        qty =
          case String.toInt qtyString of
            Just q ->
              q

            _ ->
              0
        
        transactionState = model.transactionState
        itemTransactionForm = transactionState.itemTransactionForm
        
        newItemTransactionForm = { itemTransactionForm | qty = qty }
        newTransactionState = { transactionState | itemTransactionForm = newItemTransactionForm }
      in
      ( { model | transactionState = newTransactionState } , Cmd.none)

    InputSearchByItem searchInput ->
      let
        transactionState = model.transactionState
        newTransactionState = { transactionState | searchByItem = searchInput }
      in
        ( { model | transactionState = newTransactionState }, Cmd.none )

    SelectItemToAdd item ->
      let
        transactionState = model.transactionState
        newTransactionState = { transactionState | selectedItem = Just item, foundItems = [] }
      in
      ( { model | transactionState = newTransactionState }, Cmd.none )

    InputSearchItem searchInput ->
      let
        itemState = model.itemState
        newItemState = { itemState | searchInput = searchInput }
      in
        ( { model | itemState = newItemState }, Cmd.none )

    InsertItemToList ->
      let
        ( newUuid, newSeed ) = Random.step Uuid.uuidGenerator model.seed
        transactionState = model.transactionState
        transactionView = transactionState.transactionView
        itemTransactions = transactionView.itemTransactions

        newItemTransactionView : Maybe ItemTransactionView
        newItemTransactionView =
          case model.transactionState.selectedItem of
            Just item -> -- TODO : add UID
              Just
                { itemTransaction = 
                    { initialItemTransaction
                    | qty = model.transactionState.itemTransactionForm.qty
                    , uid = Uuid.toString newUuid
                    , itemId = item.id
                    , transactionId = model.transactionState.transactionView.transaction.id
                    }
                , item = item 
                }

            _ ->
              Nothing
              
        newItemTransactions =
          case newItemTransactionView of
            Just itemTransactionView ->
              itemTransactions ++ [itemTransactionView]

            Nothing ->
              itemTransactions
        
        newTransactionView = { transactionView | itemTransactions = newItemTransactions }
        newTransactionState = { transactionState | transactionView = newTransactionView } 
      in
      ( { model 
        | transactionState = newTransactionState 
        , seed = newSeed  
        }
      , Cmd.none 
      )

    DeleteItemTransaction itemTransaction ->
      let
        transactionState = model.transactionState
        transactionView = transactionState.transactionView
        itemTransactions = transactionView.itemTransactions

        newItemTransactions = 
          List.filter 
          (\itemTransactionView -> itemTransactionView.itemTransaction.uid /= itemTransaction.uid) 
          itemTransactions
        newTransactionView = { transactionView | itemTransactions = newItemTransactions }
        newTransactionState = 
          { transactionState 
          | transactionView = newTransactionView 
          , itemTransactionDeleteIds = transactionState.itemTransactionDeleteIds ++ [ itemTransaction.id ] 
          }
      in
      ( { model | transactionState = newTransactionState }, Cmd.none )

    SaveTransaction ->
      -- TODO: Save Transaction
      let
        transaction = model.transactionState.transactionView.transaction

        projectId =
          case model.transactionState.projectTransactionsView.project of
            Just project ->
              project.id
            
            _ ->
              0

        newTransaction = { transaction | projectId = projectId } 
        transactionPostBody : TransactionPostBody
        transactionPostBody =
          { transaction = newTransaction
          , itemTransactions = model.transactionState.transactionView.itemTransactions
          , itemTransactionDeleteIds = model.transactionState.itemTransactionDeleteIds
          } 

        transactionState = model.transactionState
        newTransactionState = { transactionState | requestStatus = Loading }
      in
      ( { model 
        | transactionState = newTransactionState 
        }
      , Http.post
          { url = model.baseUrl ++ "/transactionsave"
          , body = Http.jsonBody (transactionPostBodyEncoder transactionPostBody)
          , expect = Http.expectString SavedTransaction
          }
      )

    SavedTransaction res ->
       let
        transactionState = model.transactionState
       in
       case res of
          Ok _ ->
            let
              newTransactionState = { transactionState | requestStatus = Success }
              newModel = { model | transactionState = newTransactionState }
            in
            ( newModel
            , Cmd.batch
                [ Nav.pushUrl newModel.key "/#/transactions"
                , case model.transactionState.projectTransactionsView.project of
                    Just project ->
                      Http.get
                        { url = model.baseUrl ++ "/projects/" ++ String.fromInt project.id ++ "/transactions"
                        , expect = Http.expectJson GotProjectTransaction projectTransactionsViewDecoder
                        }
                    
                    Nothing ->
                      Cmd.none
                ]
            )

          _ ->
            let
              newTransactionState = { transactionState | requestStatus = Error }
            in
            ( { model | transactionState = newTransactionState }
            , Cmd.none
            )

    ChangeItemName name ->
      let
        itemState = model.itemState
        item = itemState.item
        newItem = { item | name = name }
        newItemState =  { itemState | item = newItem }
      in
        ( { model | itemState = newItemState }, Cmd.none )

    ChangeItemDescription description ->
      let

        itemState = model.itemState
        item = itemState.item
        newItem = { item | description = description }
        newItemState =  { itemState | item = newItem }
      in
        ( { model | itemState = newItemState }, Cmd.none )

    ChangeItemPrice priceString ->
      let
        parsedPrice = Maybe.withDefault 0 (String.toInt priceString)

        itemState = model.itemState
        item = itemState.item
        newItem = { item | price = parsedPrice }
        newItemState =  { itemState | item = newItem }
      in
        ( { model | itemState = newItemState }, Cmd.none )
    
    ChangeItemManufacturingPrice manufacturingPriceString ->
      let
        parsedManufacturingPrice = Maybe.withDefault 0 (String.toInt manufacturingPriceString)

        itemState = model.itemState
        item = itemState.item
        newItem = { item | manufacturingPrice = parsedManufacturingPrice }
        newItemState = { itemState | item = newItem }
      in
        ( { model | itemState = newItemState }, Cmd.none )

    ToggleInitialStock ->
      let
        itemState = model.itemState
        newItemState = { itemState | addInitialStock = not itemState.addInitialStock }
      in
      ( { model | itemState = newItemState}, Cmd.none )

    ChangeInitialStock initialStock ->
      let
        parsedInitialStock = Maybe.withDefault 0 (String.toInt initialStock)

        itemState = model.itemState
        newItemState = { itemState | initialStock = parsedInitialStock }
      in
        ( { model | itemState = newItemState }, Cmd.none )

    GotItem res ->
      let
        itemState = model.itemState
      in
      case res of
        Ok item ->
          let
            newItemState = { itemState | item = item, requestStatus = Success }
          in
          ( { model | itemState = newItemState }, Cmd.none )

        Err _ ->
          let
            newItemState = { itemState | requestStatus = Error }
          in
          ( { model | itemState = newItemState }, Cmd.none )

    SaveItem ->
      let
        itemPostBody : ItemPostBody
        itemPostBody =
          { item = model.itemState.item
          , withInitialStock = model.itemState.addInitialStock
          , initialStockQty = model.itemState.initialStock
          }

        itemState = model.itemState
        newItemState = { itemState | requestStatus = Loading }
      in
      ( { model | itemState = newItemState }
      , sendRequest 
          model.baseUrl
          "POST"
          "/itemsave"
          -- (Http.jsonBody <| itemEncoder model.itemState.item)
          (Http.jsonBody <| itemPostBodyEncoder itemPostBody)
          (Http.expectString SavedItem)
      )

    SavedItem res ->
      let
        itemState = model.itemState
      in
      case res of
        Ok _ ->
          let
            newItemState = { itemState | requestStatus = Success }
          in  
          ( { model | itemState = newItemState }, Nav.pushUrl model.key "/#/items" )

        _ ->
          let
            newItemState = { itemState | requestStatus = Error }
          in  
          ( { model | itemState = newItemState }, Cmd.none )

    GotItemStockInsView res ->
      let
        itemState = model.itemState 
      in
      case res of
        Ok itemStockInsView ->
          let
            stockIn = model.itemState.stockIn
            newStockIn = { stockIn | itemId = itemStockInsView.item.id }
            newItemState = 
              { itemState 
              | itemStockIns = itemStockInsView
              , stockIn = newStockIn
              , requestStatus = Success 
              }
          in
          ( { model | itemState = newItemState }, Cmd.none )

        _ ->
          let
            newItemState = { itemState | requestStatus = Error }
          in
          ( { model | itemState = newItemState }, Cmd.none  )
    
    AddStockIn ->
      let
        itemState = model.itemState
        newItemState = { itemState | requestStatus = Loading }
      in
      ( { model | itemState = newItemState }
      , Http.request
          { method = "POST"
          , headers = []
          , url = model.baseUrl ++ "/stockins"
          , body = Http.jsonBody <| stockInEncoder model.itemState.stockIn
          , expect = Http.expectWhatever SavedStockIns
          , timeout = Nothing
          , tracker = Nothing
          }
      )

    SavedStockIns res ->
      case res of
        Ok _ ->
          ( model
          , Http.request
              { method = "GET"
              , headers = []
              , url = model.baseUrl ++ "/items/" ++ String.fromInt model.itemState.itemStockIns.item.id ++ "/stockins"
              , body = Http.emptyBody
              , expect = Http.expectJson GotItemStockInsView itemStockInsViewDecoder
              , timeout = Nothing
              , tracker = Nothing
              }
          )

        Err _ ->
          ( model, Cmd.none )
      
    ChangeStockInQty qtyString ->
      let
        parsedQty = Maybe.withDefault 0 (String.toInt qtyString)

        itemState = model.itemState
        stockIn = itemState.stockIn

        newStockIn = { stockIn | qty = parsedQty }
        newItemState = { itemState | stockIn = newStockIn }
      in
      ( { model | itemState = newItemState }, Cmd.none )

    DeleteStockInAlert stockIn ->
      ( model, deleteStockInAlert stockIn )

    DeleteStockIn idString ->
      let
        itemState = model.itemState
        newItemState = { itemState | requestStatus = Loading }
      in

      ( { model | itemState = newItemState }
      , Http.request
          { method = "DELETE"
          , headers = []
          , url = model.baseUrl ++ "/stockins/" ++ idString
          , body = Http.emptyBody
          , expect = Http.expectWhatever SavedStockIns
          , timeout = Nothing
          , tracker = Nothing
          }
      )

    ToggleStockInDropdownState state ->
      let
        itemState = model.itemState
        newItemState = { itemState | projectDropdown = state }
      in
      ( { model | itemState = newItemState }, Cmd.none )

    SelectStockInProject project ->
      let
        itemState = model.itemState
        stockIn = itemState.stockIn

        newStockIn = { stockIn | projectId = project.id }
        newItemState = 
          { itemState 
          | selectedProject = Just project 
          , stockIn = newStockIn
          }
        -- transactionState = model.transactionState
        -- projectTransactionsView = transactionState.projectTransactionsView

        -- newProjectTransactionsView = { projectTransactionsView | project = Just project }
        -- newTransactionState = 
        --   { transactionState  
        --   | requestStatus = Loading
        --   , selectedProject = project.name
        --   , projectTransactionsView = newProjectTransactionsView
        --   }
      in
        ( { model | itemState = newItemState }
        , Cmd.none
        )

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch 
    [ Dropdown.subscriptions model.transactionState.projectsDropdown ToggleProject 
    , Dropdown.subscriptions model.itemState.projectDropdown ToggleStockInDropdownState
    , Navbar.subscriptions model.navbarState NavbarMsg
    , deleteStockIn DeleteStockIn
    ]

-- VIEW


view : Model -> Browser.Document Msg
view model =
  let
    page = Maybe.withDefault Index <| Url.parse urlParser <| model.url
    currentPage =
      if not model.loggedIn then
        ("Login", loginPage model)
      else
        case page of
          Index ->
            ("Index", transactionPage model)

          ProjectPage ->
            ("Projects", projectPage model)

          ProjectDetail projectId ->
            ("Project Detail", projectDetailPage model projectId)

          ItemPage ->
            ("Items", itemPage model)

          ItemDetail itemId ->
            ("Item Detail", itemDetailPage model itemId)

          TransactionPage ->
            ("Transactions", transactionPage model)

          TransactionDetail transactionId ->
            ("Transaction Detail", transactionDetail model transactionId)

          StockInPage stockInId ->
            ("Stock In Detail", stockInPage model stockInId )

    (title, body) = currentPage 
  in
  { title = "Cozy PoS | " ++ title
  , body =
      [ body
      -- , text "The current URL is: "
      -- , b [] [ text (Url toString model.url) ]
      ]
  }

loginPage : Model -> Html Msg
loginPage model =
  div [ class "d-flex flex-column w-100 justify-content-center align-items-center bg-dark", style "height" "100vh" ]
    [ h3 [ class "text-white" ] [ text "Cozy PoS" ]
    , Form.form []
        [ Form.group []
            [ Input.text
                [ Input.placeholder "Username..."
                , Input.attrs [ class "m-2" ] 
                , Input.value model.loginState.loginInfo.username
                , Input.onInput ChangeLoginUsername
                ]
            , Input.password
                [ Input.placeholder "Password..."
                , Input.attrs [ class "m-2" ] 
                , Input.value model.loginState.loginInfo.password
                , Input.onInput ChangeLoginPassword
                ]
            ]
        ]
    -- , Grid.container []
    --     [div [ class "text-wrap" ] [ text "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaahelloworld" ] 
    --     ]
    -- , div [ class "text-white text-wrap" ] [ text <| "Apikey: " ++ Maybe.withDefault "" model.apiKey ]
    , div [] 
        [ if model.loginState.requestStatus == Loading then
            Spinner.spinner [ Spinner.color Text.light ] []
          else if model.loginState.requestStatus == Error then
            div [ style "color" "red", class "d-flex justify-content-center" ] [ text <| "Error logging in, invalid username/password." ]
          else
            span [] []
        ]
    , div [] [ Button.button [ Button.onClick Login, Button.light ] [ text "Login" ] ]
    ]

mainPage : Model -> Html Msg
mainPage model =
  div []
    [ navbar model
    , text "This is the main page."
    , button [ onClick Logout ] [ text "Logout" ]
    ]

navbar : Model -> Html Msg
navbar model =
  Navbar.config NavbarMsg
    |> Navbar.withAnimation
    |> Navbar.brand [ href "/#/" ] [ text "Cozy PoS" ]
    |> Navbar.light
    |> Navbar.items
        [ Navbar.itemLink [ href "/#/projects" ] [ text "Project" ]
        , Navbar.itemLink [ href "/#/items" ] [ text "Item" ]
        , Navbar.itemLink [ href "/#/transactions" ] [ text "Transaction" ] 
        ]
    |> Navbar.customItems
        [ Navbar.formItem []
            [ Button.button 
                [ Button.danger
                , Button.onClick Logout
                ] 
                [ text "Logout" ] 
            ]
        ]
    |> Navbar.view model.navbarState

stockInPage : Model -> String -> Html Msg
stockInPage model stockInId =
  div [] 
    [ navbar model
    , div []
        [ div [ class "d-flex" ]
            [ a [ href "/#/items" ]
                [ Button.button [ Button.secondary ] [ text <| "Back" ] ]
            , if model.itemState.requestStatus == Loading then
                Spinner.spinner [] []
              else
                span [] []
            ]
        , h3 [ class "d-flex justify-content-center" ] [ text <| "Stock-ins: " ++ model.itemState.itemStockIns.item.name ]
        , div []
            [ Form.form []
                [ div []
                    [ Dropdown.dropdown
                        model.itemState.projectDropdown
                          { options = []
                          , toggleMsg = ToggleStockInDropdownState
                          , toggleButton =
                              Dropdown.toggle 
                                [ Button.secondary ] 
                                [ text <| 
                                    case model.itemState.selectedProject of
                                      Just project ->
                                        project.name

                                      Nothing ->
                                        "Select Project"
                                ]
                          , items =  List.map projectStockInDropdownItem model.transactionState.projects
                          }
                    ]
                , Form.group []
                    [ Form.label [ for "stockinqty" ] [ text "Stock in qty" ]
                    , Input.number 
                        [ Input.id "stockinqty" 
                        , Input.placeholder "Qty..."
                        , Input.onInput ChangeStockInQty
                        ]
                    ]
                , div [] 
                    [ Button.button 
                        [ Button.primary 
                        , Button.onClick AddStockIn
                        ] 
                        [ text "Add" ] 
                    ]
                ]
            ]
        , div []
            [ ListGroup.ul (List.map stockInCard model.itemState.itemStockIns.stockIns)
            ]
        ]
    ]

projectStockInDropdownItem : Project -> Dropdown.DropdownItem Msg
projectStockInDropdownItem project =
  Dropdown.buttonItem 
    [ onClick (SelectStockInProject project) ] 
    [ text project.name ]

stockInCard : StockIn -> ListGroup.Item Msg
stockInCard stockIn =
  ListGroup.li []
    [ div [ class "d-flex justify-content-between" ] 
        [ h3 [] [ text <| "x " ++ String.fromInt stockIn.qty ]
        , Button.button 
            [ Button.danger, Button.small 
            -- , Button.onClick (DeleteStockIn <| String.fromInt stockIn.id)
            , Button.onClick <| DeleteStockInAlert stockIn
            ]
            [ text "Delete" ] 
        ]
    , div [ class "d-flex justify-content-between align-items-center" ]
        [ div [] [ text <| stockIn.createdAt ]
        ]
    ]
  
transactionPage : Model -> Html Msg
transactionPage model =
  let
    addButton =
      case model.transactionState.projectTransactionsView.project of
        Just _ ->
          div [ class "d-flex justify-content-end mx-3" ]
            [ a [ href <| "/#/transactions/new" ]
              [ Button.button [ Button.primary ] [ text "Add" ] ]
            ]
      
        _ ->
          div [] []

    
  in
  div []
    [ navbar model
    , div [ class "d-flex justify-content-between m-2" ]
        [ Dropdown.dropdown
            model.transactionState.projectsDropdown
              { options = []
              , toggleMsg = ToggleProject
              , toggleButton =
                  Dropdown.toggle [ Button.primary ] [ text model.transactionState.selectedProject ]
              , items =
                  List.map projectDropdownItem model.transactionState.projects
              }
        , addButton
        ]
    , div [ class "d-flex justify-content-between" ] 
        [ if model.transactionState.requestStatus == Loading then
            Spinner.spinner [] []
          else  
            span [] []
        ]
    , div []
        [ let
            projectTransactionsView = model.transactionState.projectTransactionsView
          in
          case projectTransactionsView.project of
            Just project ->
              div []
                [ h3 [] [ text project.name ]
                , div []
                    [ Input.text
                        [ Input.placeholder "Search by item..."
                        , Input.onInput InputSearchByItem
                        , Input.value model.transactionState.searchByItem
                        ]
                    ]
                , ListGroup.ul
                    <| List.map transactionCard 
                    <| List.filter 
                        (filterByItemTransactionName model.transactionState.searchByItem) 
                        projectTransactionsView.transactions
                ]

            Nothing ->
              div [ class "my-3" ] [text "No project selected."]
        ]
    ]

projectDropdownItem : Project -> Dropdown.DropdownItem Msg
projectDropdownItem project =
  Dropdown.buttonItem 
    [ onClick (SelectProject project) ] 
    [ text project.name ]

transactionCard : TransactionView -> ListGroup.Item msg
transactionCard transactionView =
  let
    itemTransactions = transactionView.itemTransactions
    items = 
      List.map 
      (\itemTransactionView -> itemTransactionView.item.name ++ " " ++ String.fromInt itemTransactionView.itemTransaction.qty ++ "x") 
      itemTransactions
  in
  ListGroup.li []
    [ div [] 
      [ div [ class "d-flex justify-content-between" ] 
          [ text <| ("ID no." ++ String.fromInt transactionView.transaction.id) 
          , a [ href <| "/#/transactions/" ++ String.fromInt transactionView.transaction.id ] 
              [ Button.button 
                  [ Button.info
                  , Button.small
                  , Button.attrs [ class "mx-2" ] 
                  ]
                  [ text "Details" ]
              ] 
          ]
      , div [ class "d-flex justify-content-between align-items-center" ] 
          [ h4 [ class "text-success" ] 
              [ text <|
                  "Rp" ++ format usLocale
                    ( if transactionView.transaction.priceIsCustom then 
                        toFloat transactionView.transaction.customPrice
                    else 
                        toFloat transactionView.totalPrice 
                    )
              ] 
          , text transactionView.transaction.cashier
          ]
      , div []
          [ text <| "Orig: Rp" ++ format usLocale (toFloat transactionView.totalPrice) ]
      , div []
          [ b [] [text <| String.join ", " items] ]
      ] 
    ]

transactionDetail : Model -> String -> Html Msg
transactionDetail model transactionId =
  div [] 
    [ navbar model
    , case model.transactionState.projectTransactionsView.project of
        Just _ ->
          transactionDetailMainPage model transactionId

        _ ->
          div []
            [ a [ href "/#/transactions" ]
              [ Button.button [ Button.secondary ] [ text "Back" ] ]
            , div [] [ text "No project selected." ]
            ]
    ]

transactionDetailMainPage : Model -> String -> Html Msg
transactionDetailMainPage model transactionId =
  let
    projectName =
      case model.transactionState.projectTransactionsView.project of
        Just project ->
          project.name

        _ ->
          ""
    
    transactionType =
      case String.toInt transactionId of
        Just _ ->
          "Edit"

        _ ->
          "Add"

    transactionView = model.transactionState.transactionView
    totalPrice = List.foldl (\itemTransactionView acc -> acc + itemTransactionView.item.price * itemTransactionView.itemTransaction.qty) 0 model.transactionState.transactionView.itemTransactions
  in
  div [ class "mx-1" ]
    [ div []
        [ a [ href "/#/transactions" ] [ Button.button [ Button.secondary ] [ text "Back" ] ]
        , Button.button 
            [ Button.primary
            , Button.attrs [ class "mx-1" ] 
            , Button.onClick SaveTransaction
            ] [ text "Save" ]
        , if model.transactionState.requestStatus == Loading then
            Spinner.spinner [] []
          else
            span [] []
        ]
    , div [] 
        [ h4 [] [ text <| "Transaction  " ++ transactionType ++ ": " ++ projectName ]
        ]
    , div []
        [ Checkbox.checkbox 
            [ Checkbox.checked transactionView.transaction.priceIsCustom 
            , Checkbox.attrs [ onClick CheckPriceIsCustom ]
            ] "Custom Price?"
        , if transactionView.transaction.priceIsCustom then
            Form.group [ Form.attrs [ class "mx-1" ] ]
              [ Form.label [ for "customPrice" ] [ text "Custom Price" ]
              , Input.number 
                  [ Input.id "customPrice"
                  , Input.placeholder "Custom Price..." 
                  , Input.value <| String.fromInt transactionView.transaction.customPrice
                  , Input.onInput ChangeCustomPrice
                  ]
              ]
          else
            span [] []
        ]
    , div [ class "dropdown-divider" ] []
    , div []
        [ Form.group []
            [ Form.label [ ] [ text "Select Item" ]
            , Input.text
                [ Input.placeholder "Search Item..."
                , Input.onInput SearchItem
                ]
            ]
        ]
    , div []
        [ ListGroup.custom (List.map foundItemCard model.transactionState.foundItems) ]
    , div []
        [ Form.group []
            [ Form.label [] [ text "Qty"]
            , Input.number
                [ Input.value (String.fromInt model.transactionState.itemTransactionForm.qty)
                , Input.onInput ChangeItemTransactionFormQty
                ]
            ]
        ]
    , div [] 
        [ div [] 
            [ text 
                <| "Selected: "
                ++  ( case model.transactionState.selectedItem of
                        Just item ->
                          (item.name ++ " x" ++ String.fromInt model.transactionState.itemTransactionForm.qty)
                        
                        _ ->
                          "None selected"
                    ) 
                ]
        , div [] [ b [] [ text "" ] ]
        ] 
    , div []
        [ Button.button 
            [ Button.secondary 
            , Button.onClick InsertItemToList
            ] 
            [ text "Insert to List " ]
        ]
    , div [ class "dropdown-divider" ] []
    , div [] [ h4 [] [ text "Selected items:" ]]
    , div []
        [ ListGroup.ul (List.map itemTransactionCard model.transactionState.transactionView.itemTransactions) 
        ]
    , div [ class "dropdown-divider" ] []
    , div [] [ h4 [] [ text "Grand Total:" ] ]
    , div []
        [ h5 [ class "text-info" ] [ text <| "Custom price: " ++ if model.transactionState.transactionView.transaction.priceIsCustom then "(Yes)" else "(No)" ]
        , h4 [ class "text-success" ] 
            [ text
                <| "Rp"
                ++ 
                format usLocale (toFloat model.transactionState.transactionView.transaction.customPrice) 
            ]
        , h5 [ class "text-info" ] [ text "Original price:" ]
        , h4 [ class "text-success" ]
            [ text <| "Rp" ++ format usLocale (toFloat totalPrice) ]
        , h5 [ class "text-info" ] [ text "Final price:" ]
        , h4 [ class "text-success" ] 
            [ text <|
                "Rp" ++
                format usLocale 
                  (if model.transactionState.transactionView.transaction.priceIsCustom then
                    toFloat model.transactionState.transactionView.transaction.customPrice
                  else
                    toFloat totalPrice
                  )
            ]
        ]
    ]

itemTransactionCard : ItemTransactionView -> ListGroup.Item Msg 
itemTransactionCard itemTransactionView =
  ListGroup.li [] 
    [ div [ class "d-flex justify-content-between" ]
        [ text <| itemTransactionView.item.name ++ " x" ++ String.fromInt itemTransactionView.itemTransaction.qty 
        , Button.button 
            [ Button.danger
            , Button.small 
            , Button.onClick (DeleteItemTransaction itemTransactionView.itemTransaction)
            ] [ text "Delete" ]
        ]
    , div []
        [ h4 [] [ text <| "Rp" ++ format usLocale (toFloat <| itemTransactionView.item.price * itemTransactionView.itemTransaction.qty) ] ] 
    ]

foundItemCard : ItemStockView -> ListGroup.CustomItem Msg
foundItemCard itemStockView =
  let
    item =
      case itemStockView.item of
        Just i ->
          i

        _ ->
          initialItem
  in
  ListGroup.button [ ListGroup.attrs [ onClick (SelectItemToAdd item) ] ]
    [ div []
        [ text <| item.name
        , b [] [ text <| ": " ++ String.fromInt itemStockView.inStock ++ " in stock" ]
        ]
    ]


itemPage : Model -> Html Msg
itemPage model =
  let
    filterItemStockView itemStockView =
      case itemStockView.item of
        Just item ->
          String.contains model.itemState.searchInput (String.toLower item.name)

        Nothing ->
          False 

    filteredItems = List.filter filterItemStockView model.itemState.itemStockViews
  in
  div [] 
    [ navbar model
    , div [ class "d-flex justify-content-end" ]
        [ Input.text [ Input.placeholder "Search item...", Input.onInput InputSearchItem ]
        , a [ href "/#/items/add" ]  
            [ Button.button [ Button.primary ] [ text "Add" ] ]
        ]
    , div []
        [ if model.itemState.requestStatus == Loading then
            Spinner.spinner [ Spinner.attrs [class "mx-2"] ] []
          else
            span [] []
        ]
    , div []
        [ ListGroup.ul (List.map itemCard filteredItems) ]
    ]

itemCard : ItemStockView -> ListGroup.Item Msg
itemCard itemStockView =
  let
    item =
      case itemStockView.item of
        Just unwrappedItem ->
          unwrappedItem

        Nothing ->
          initialItem
  in
  ListGroup.li [] 
    [ div []
        [ a 
          [ href ("/#/items/" ++ String.fromInt item.id) ] 
          [ h5 [] [ text <| item.name ] ]
        , div [] [ h5 [] [ text <| "Rp" ++ format usLocale (toFloat item.price)] ]
        , div [] [ text <| "Manuf.price: Rp" ++ format usLocale (toFloat item.manufacturingPrice) ]
        , div [] [ text item.description ]
        , div [ class "d-flex justify-content-between align-items-center" ] 
            [ div []
                [ text "In stock: "
                , span [] [ b [] [ text <| String.fromInt itemStockView.inStock ] ]
                ]
            , div []
                [ a [ href ("/#/items/" ++ String.fromInt item.id ++  "/stockins") ]
                    [ Button.button [ Button.info, Button.small ] [ text "Stock in" ] ] 
                ] 
            ]
        ] 
    ]

itemDetailPage : Model -> String -> Html Msg
itemDetailPage model itemId =
  div []
      [ navbar model
      , div [] [ text "This is the item detail page"] 
      , div []
          [ a [ href "/#/items" ]  
              [ Button.button [ Button.secondary ] [ text "Back" ] ]
          , Button.button 
              [ Button.primary
              , Button.attrs 
                  [ class "mx-1" ]
              , Button.onClick SaveItem
              ] 
              [ text "Save" ]
          , if model.itemState.requestStatus == Loading then
              Spinner.spinner [] []
            else
              span [] []
          ]
      , div [] [ text <| "Item id: " ++ itemId ]
      , div [ class "mx-1" ]
          [ Form.group []
              [ Form.label [ for "name" ] [ text "Name" ]
              , Input.text 
                  [ Input.id "name"
                  , Input.placeholder "Name..." 
                  , Input.value model.itemState.item.name
                  , Input.onInput ChangeItemName
                  ]
              ]
          , Form.group []
              [ Form.label [ for "description" ] [ text "Description" ]
              , Input.text 
                  [ Input.id "description"
                  , Input.placeholder "Description..." 
                  , Input.value model.itemState.item.description
                  , Input.onInput ChangeItemDescription
                  ]
              ]
          , Form.group []
              [ Form.label [ for "price" ] [ text "Price" ]
              , Input.text 
                  [ Input.id "price", Input.placeholder "Price..."
                  , Input.value <| String.fromInt model.itemState.item.price
                  , Input.onInput ChangeItemPrice
                  ]
              ]
          , Form.group []
              [ Form.label [ for "manufacturingPrice" ] [ text "Manufacturing Price" ]
              , Input.text 
                  [ Input.id "manufacturingPrice"
                  , Input.placeholder "Manufacturing price..." 
                  , Input.value <| String.fromInt model.itemState.item.manufacturingPrice
                  , Input.onInput ChangeItemManufacturingPrice
                  ]
              ]
          ]
      , case String.toInt itemId of
          Just _ ->
            span [] []

          _ ->
            div []
              [ Checkbox.checkbox 
                [ Checkbox.id "initialStockCheck"
                , Checkbox.checked model.itemState.addInitialStock 
                , Checkbox.attrs [ onClick ToggleInitialStock ]
                ] 
                "Add initial stock?"
              , Form.group 
                  [ Form.attrs 
                      [ class "mx-1"
                      , style "display" (if model.itemState.addInitialStock then "block" else "none")
                      ] 
                  ]
                  [ Form.label [ for "initialStockForm" ] [ text "Initial Stock" ]
                  , Input.text
                      [ Input.id "initialStockForm"
                      , Input.placeholder "Initial Stock..."
                      , Input.value <| String.fromInt model.itemState.initialStock
                      , Input.onInput ChangeInitialStock 
                      ]
                  ] 
              ]
      ]

projectPage : Model -> Html Msg
projectPage model =
  div [] 
  [ navbar model 
  -- , div [] [ text "This is the project page" ]
  , div [ class "d-flex justify-content-between m-3" ]
      [ h3 [] [ text "Projects" ]
      , Button.linkButton
        [ Button.primary 
        , Button.attrs [ href "/#/projects/new" ]
        ]
        [ text "Add" ]
      ]
  , div [] 
      [ if model.projectState.requestStatus == Loading then
          Spinner.spinner [] []
        else
          span [] []
      ]
  , div []
      [ ListGroup.ul
          (List.map projectCard model.projectState.projects.projects)
      ]
  ]

projectCard : ProjectView -> ListGroup.Item  Msg
projectCard projectView =
  ListGroup.li []
    [ div []
        [ div [ class "d-flex justify-content-between" ]
            [ a [ href ("/#/projects/" ++ String.fromInt projectView.project.id) ] 
              [ h4 [] [ text projectView.project.name ] ]
            , div [] [ text <| "Date: " ++ String.slice 0 10 projectView.project.startDate ]
            ]
        , div [] [ text <| "Income: Rp" ++ format usLocale (toFloat projectView.income) ]
        ]
    ]


projectDetailPage : Model -> String -> Html Msg
projectDetailPage model projectId =
  div []
    [ navbar model
    , div []
        [ a [ href "/#/projects" ] [ Button.button [ Button.secondary ] [ text "Back" ]  ] ]
    -- , text ("This is the project detail page, project id: " ++ projectId)
    -- , div [] [ text "Some form" ]
    , div []
        [ Form.form [ class "m-2" ]
            [ Form.group []
                [ Form.label [ for "projectname" ] [ text "Project Name" ]
                , Input.text 
                    [ Input.id "projectname"
                    , Input.onInput InputProjectName
                    , Input.value model.projectState.project.name
                    ]
                ]
            , Form.group []
                [ Form.label [ for "projectdate" ] [ text "Project Date" ]
                , Input.date 
                    [ Input.id "projectdate"
                    , Input.onInput InputProjectDate 
                    , Input.value (String.slice 0 10 model.projectState.project.startDate)
                    ]
                ]
            , Button.button [ Button.primary, Button.onClick SaveProject ] [ text "Save" ]
            ]
        ]
    ]

-- HELPERS

filterByItemTransactionName : String -> TransactionView -> Bool
filterByItemTransactionName name transactionView =
  let
    names = List.foldl (\itemTransaction acc -> acc ++ String.toLower itemTransaction.item.name) "" transactionView.itemTransactions
  in
    String.contains name names
sendRequest : String -> String -> String -> Http.Body -> Http.Expect msg -> Cmd msg
sendRequest baseUrl method target body expect =
  Http.request
    { method = method
    , headers = []
    , url = baseUrl ++ target
    , body = body
    , expect = expect
    , timeout = Nothing
    , tracker = Nothing
    }

fetchByUrl : Model -> (Model, Cmd Msg)
fetchByUrl model =
  let
    page = Maybe.withDefault Index <| Url.parse urlParser <| model.url
  in
  case page of
    ProjectPage ->
      let
        projectState = model.projectState
        newProjectState = { projectState | requestStatus = Loading }
      in
      ( { model | projectState = newProjectState } 
      , sendRequest 
          model.baseUrl 
          "GET" 
          "/projectsview" 
          Http.emptyBody 
          (Http.expectJson GotProjectsView projectsViewDecoder) 
      )

    ProjectDetail projectId ->
      let
        projectIdInt = String.toInt projectId
      in
        case projectIdInt of
            Just id ->
              ( model 
              , sendRequest 
                  model.baseUrl
                  "GET" 
                  ("/projects/" ++ String.fromInt id) 
                  Http.emptyBody 
                  (Http.expectJson GotProject projectDecoder) 
              )

            Nothing ->
              let 
                projectState = model.projectState
              
                newProjectState = { projectState | project = initialProject }
              in
              ( { model | projectState = newProjectState }
              , Cmd.none
              )

    Index ->
      ( { model | transactionState = initialTransactionModel }
      , sendRequest
          model.baseUrl
          "GET"
          "/projects"
          Http.emptyBody
          (Http.expectJson GotProjects (Decode.list projectDecoder)) 
      )
      
    -- TransactionPage ->
    --   ( { model | transactionState = initialTransactionModel }
    --   , sendRequest
    --       model.baseUrl
    --       "GET"
    --       "/projects"
    --       Http.emptyBody
    --       (Http.expectJson GotProjects (Decode.list projectDecoder)) 
    --   )

    TransactionDetail transactionId ->
      let
        transactionState = model.transactionState
      in
        case String.toInt transactionId of
          Just _ ->
            let
              newTransactionState = { transactionState | requestStatus = Loading }
            in
            ( { model | transactionState = newTransactionState }
            , sendRequest
                model.baseUrl
                "GET"
                ("/transactions/view/" ++ transactionId)
                Http.emptyBody
                (Http.expectJson GotTransactionView transactionViewDecoder)
            )

          _ ->
            let
              newTransactionState = { transactionState | transactionView = initialTransactionView }
            in
            ( { model | transactionState = newTransactionState }, Cmd.none )

    StockInPage itemId ->
      let
        itemState = model.itemState
        newItemState = { itemState | requestStatus = Loading }
      in
      ( { model | itemState = newItemState }
      , Cmd.batch
          [ Http.request
              { method = "GET"
              , headers = []
              , url = model.baseUrl ++ "/items/" ++ itemId ++ "/stockins"
              , body = Http.emptyBody
              , expect = Http.expectJson GotItemStockInsView itemStockInsViewDecoder 
              , timeout = Nothing
              , tracker = Nothing
              }
          ] 
      )

    ItemPage ->
      let
        itemState = model.itemState
        newItemState = { itemState | requestStatus = Loading }
      in
      ( { model | itemState = newItemState }
      , sendRequest
          model.baseUrl
          "GET"
          "/itemstocks"
          Http.emptyBody
          (Http.expectJson GotItems (Decode.list itemStockViewDecoder)) 
      )

    ItemDetail itemId ->
      let
        itemState = model.itemState
      in
        case String.toInt itemId of
          Just _ ->
            let
              newItemState = { itemState | requestStatus = Loading }
            in
            ( { model | itemState = newItemState } 
            , sendRequest
                model.baseUrl
                "GET"
                ("/items/" ++ itemId)
                Http.emptyBody
                (Http.expectJson GotItem itemDecoder)
            )

          _ ->
            let
              newItemState = { itemState | item = initialItem } 
            in
            ( { model | itemState = newItemState }, Cmd.none )

    _ ->
      ( model, Cmd.none )