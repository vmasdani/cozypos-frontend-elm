module Main exposing (..)

import Browser
import Browser.Hash as Hash
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Url
import Url.Parser as Url exposing (Parser, (</>))
import Bootstrap.Navbar as Navbar
import Bootstrap.Button as Button
import Bootstrap.Dropdown as Dropdown
import Bootstrap.Breadcrumb exposing (item)
import Json.Decode as Decode exposing (Decoder, int, string, float, field, bool, list)
import Http

-- MAIN


main : Program () Model Msg
main =
  Hash.application
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    , onUrlChange = UrlChanged
    , onUrlRequest = LinkClicked
    }



-- PAGE
type Page
  = Index
  | ProjectPage
  | ProjectDetail String
  | ItemPage
  | ItemDetail String
  | TransactionPage
  | TransactionDetail String

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
    ]

-- MODEL

type alias Model =
  { key : Nav.Key
  , url : Url.Url
  , loggedIn : Bool
  , navbarState : Navbar.State
  , projectState : ProjectModel
  , transactionState : TransactionModel
  }

type alias ProjectModel =
  { requestStatus : RequestStatus
  , project : Dropdown.State
  , selectedProject : String
  }

type alias TransactionModel =
  { requestStatus : RequestStatus
  , projects : List Project
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
  }

apiKeyDecoder =
  Decode.map2 ApiKey
    (field "id" int)
    (field "apiKey" string)

type alias Project =
  { id : Int
  , uid: String
  , name: String
  , startDate: String
  , updatedAt: String
  , createdAt: String
  }

projectDecoder =
  Decode.map6 Project
    (field "id" int)
    (field "uid" string)
    (field "name" string)
    (field "startDate" string)
    (field "updated_at" string)
    (field "created_at" string)

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

itemDecoder =
  Decode.map8 Item
    (field "id" int)
    (field "uid" string)
    (field "name" string)
    (field "description" string)
    (field "price" int)
    (field "manufacturingPrice" int)
    (field "updated_at" string)
    (field "created_at" string)

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

transactionDecoder =
  Decode.map8 Transaction
    (field "id" int)
    (field "uid" string)
    (field "cashier" string)
    (field "priceIsCustom" bool)
    (field "customPrice" int)
    (field "projectId" int)
    (field "updated_at" string)
    (field "created_at" string)

type alias StockIn = 
  { id : Int
  , uid : String
  , itemId: Int
  , qty : Int
  , updatedAt: String
  , createdAt: String
  }

stockInDecoder =
  Decode.map6 StockIn
    (field "id" int)
    (field "uid" string)
    (field "itemId" int)
    (field "qty" int)
    (field "updated_at" string)
    (field "created_at" string)

type alias ItemTransaction =
  { id : Int
  , uid : String
  , itemId : Int
  , transactionId : Int
  , qty: Int
  , createdAt : String
  , updatedAt : String
  }

itemTransactionDecoder =
  Decode.map7 ItemTransaction
    (field "id" int)
    (field "uid" string)
    (field "itemId" int)
    (field "transactionId" int)
    (field "qty" int)
    (field "updated_at" string)
    (field "created_at" string)
    

type alias ItemStockIn =
  { id : Int
  , uid : String
  , itemId : Int
  , stockInId : Int
  , qty : Int
  , createdAt : String
  , updatedAt : String
  }

itemStockInDecoder =
  Decode.map7 ItemStockIn
    (field "id" int)
    (field "uid" string)
    (field "itemId" int)
    (field "stockInId" int)
    (field "qty" int)
    (field "updated_at" string)
    (field "created_at" string)

type alias ItemProject =
  { id : Int
  , uid : String
  , itemId : Int
  , projectId : Int
  , qty : Int
  , createdAt : String
  , updatedAt : String
  }

itemProjectDecoder =
  Decode.map7 ItemProject
    (field "id" int)
    (field "uid" string)
    (field "itemId" int)
    (field "projectId" int)
    (field "qty" int)
    (field "updated_at" string)
    (field "created_at" string)

-- DB VIEW
type alias ProjectView =
  { project : Project
  , income : Int
  , totalManufacturingPrice : Int
  }

projectViewDecoder =
  Decode.map3 ProjectView
    (field "project" projectDecoder)
    (field "income" int)
    (field "totalManufacturingPrice" int)

type alias ProjectsView =
  { projects : List ProjectView
  , totalIncome : Int
  }

projectsViewDecoder =
  Decode.map2 ProjectsView
    (field "projects" (Decode.list projectViewDecoder))
    (field "totalIncome" int)

type alias ProjectTransactionsView =
  { project : Project
  , transactions : List TransactionView
  }

projectTransactionsViewDecoder =
  Decode.map2 ProjectTransactionsView
    (field "project" projectDecoder)
    (field "transactions" (Decode.list transactionViewDecoder))

type alias TransactionView =
  { transaction : Transaction
  , itemTransactions : List ItemTransactionView
  , totalPrice : Int
  }

transactionViewDecoder =
  Decode.map3 TransactionView
    (field "transaction" transactionDecoder)
    (field "itemTransactions" (Decode.list itemTransactionViewDecoder))
    (field "totalPrice" int)

type alias ItemTransactionView =
  { itemTransaction : ItemTransaction
  , item : Item
  }

itemTransactionViewDecoder =
  Decode.map2 ItemTransactionView
    (field "itemTransaction" itemTransactionDecoder)
    (field "item" itemDecoder)

type alias ItemStockView =
  { item : Item
  , inStock : Int
  }

itemStockViewDecoder =
  Decode.map2 ItemStockView
    (field "item" itemDecoder)
    (field "inStock" int)

init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
  let
    (navbarState, navbarCmd) = Navbar.initialState NavbarMsg

    initialProjectModel : ProjectModel
    initialProjectModel =
      { project = Dropdown.initialState
      , requestStatus = NotAsked
      , selectedProject = "Select Project"
      }

    initialTransactionModel : TransactionModel
    initialTransactionModel =
      { requestStatus = NotAsked
      , projects = [] 
      }

    initialModel : Model 
    initialModel =
      { key = key
      , url = url
      , loggedIn = False
      , navbarState = navbarState
      , projectState = initialProjectModel
      , transactionState = initialTransactionModel
      }
  in
  
  ( initialModel, Cmd.batch [ navbarCmd ] )



-- UPDATE


type Msg
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url
  | NavbarMsg Navbar.State
  | Login
  | Logout
  | ToggleProject Dropdown.State
  | SelectProject String
  | GotProjects (Result Http.Error (List Project))


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
      ( { model | url = url }
      , Cmd.none
      )

    Login ->
      let 
        getProjects = 
          Http.request
            { method = "GET"
            , headers = []
            , url = "http://localhost:8080/projects"
            , body = Http.emptyBody
            , expect = Http.expectJson GotProjects (Decode.list projectDecoder)
            , timeout = Nothing
            , tracker = Nothing
            }

        transactionState = model.transactionState
        newTransactionState = { transactionState | requestStatus = Loading }
      in
      ( { model | loggedIn = True, transactionState = newTransactionState }, Cmd.batch [ getProjects ] )

    Logout ->
      ( { model | loggedIn = False }, Cmd.none )

    NavbarMsg state -> 
      ( { model | navbarState = state }, Cmd.none )

    ToggleProject state ->
      let
        projectState = model.projectState
        newProjectState = { projectState | project = state }
      in
      ( { model | projectState = newProjectState }, Cmd.none )
    
    SelectProject projectName ->
      let
        projectState = model.projectState
        newProjectState = { projectState | selectedProject = projectName }
      in
        ( { model | projectState = newProjectState }, Cmd.none )

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

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch [ 
    Dropdown.subscriptions model.projectState.project ToggleProject
  ]

-- VIEW


view : Model -> Browser.Document Msg
view model =
  let
    page = Maybe.withDefault Index <| Url.parse urlParser <| model.url
    currentPage =
      if not model.loggedIn then
        loginPage model
      else
        case page of
          Index ->
            transactionPage model

          ProjectPage ->
            projectPage model

          ProjectDetail projectId ->
            projectDetailPage model projectId

          ItemPage ->
            itemPage model

          ItemDetail itemId ->
            itemPage model

          TransactionPage ->
            transactionPage model

          TransactionDetail transactionId ->
            transactionPage model
            
  in
  { title = "URL Interceptor"
  , body =
      [ currentPage
      , text "The current URL is: "
      , b [] [ text (Url.toString model.url) ]
      ]
  }

   
loginPage model =
  div []
    [ text "Cozy PoS"
    , button [ onClick Login ] [ text "Login" ] 
    ]

mainPage model =
  div []
    [ navbar model
    , text "This is the main page."
    , button [ onClick Logout ] [ text "Logout" ]
    ]

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


transactionPage model =
  div [ ]
    [ navbar model
    , div [ class "m-2" ]
        [ Dropdown.dropdown
            model.projectState.project
              { options = []
              , toggleMsg = ToggleProject
              , toggleButton =
                  Dropdown.toggle [ Button.primary ] [ text model.projectState.selectedProject ]
              , items =
                  [ Dropdown.buttonItem [ onClick <| SelectProject "test project 1" ] [ text "Test project 1" ]
                  , Dropdown.buttonItem [ onClick <| SelectProject "test projext 2" ] [ text "Test project 2" ]
                  ]
              }
        ]
    , div [] 
        [ if model.transactionState.requestStatus == Loading then
            text "Loading......."
          else  
            text <| Debug.toString model.transactionState.projects ]
        ]

itemPage model =
  div [] 
    [ navbar model
    , text "This is the item page" 
    ]

projectPage model =
  div [] 
  [ navbar model 
  , text "This is the project page"
  , Button.linkButton
      [ Button.primary 
      , Button.attrs [ href "/#/projects/new" ]
      ]
      [ text "Add" ]
  ]

projectDetailPage model projectId =
  div []
    [ navbar model
    , text ("This is the project detail page, project id: " ++ projectId)
    , div [] [ text "Some form" ]
    ]