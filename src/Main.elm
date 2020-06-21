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
import Bootstrap.Spinner as Spinner
import Bootstrap.Button as Button
import Bootstrap.Dropdown as Dropdown
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Text as Text
import Bootstrap.Progress as Progress
import Bootstrap.ListGroup as ListGroup
import Bootstrap.Breadcrumb exposing (item)
import Json.Decode as Decode exposing (Decoder, int, string, float, field, bool, list, maybe)
import Json.Encode as Encode
import Http
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (usLocale)

-- MAIN

type alias Flag =
  { baseUrl : String
  , currentDate : String
  }

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
  , currentDate : String
  , baseUrl : String
  , loggedIn : Bool
  , navbarState : Navbar.State
  , projectState : ProjectModel
  , transactionState : TransactionModel
  , itemState : ItemModel
  }

type alias ProjectModel =
  { requestStatus : RequestStatus
  , project : Project
  , projects : ProjectsView
  }

type alias ItemModel =
  { item : Item
  , itemStockViews : List ItemStockView
  , searchInput : String
  }

type alias TransactionModel =
  { requestStatus : RequestStatus
  , projects : List Project
  , projectsDropdown : Dropdown.State
  , projectTransactionsView : ProjectTransactionsView
  , selectedProject : String
  }

initialTransactionModel : TransactionModel
initialTransactionModel =
  { requestStatus = NotAsked
  , projects = []
  , projectsDropdown = Dropdown.initialState
  , projectTransactionsView = initialProjectTransationsView
  , selectedProject = "Select Project"
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

apiKeyDecoder : Decoder ApiKey
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

initialProject : Project
initialProject =
  { id = 0
  , uid = ""
  , name = ""
  , startDate = ""
  , updatedAt = ""
  , createdAt = ""
  }

projectDecoder =
  Decode.map6 Project
    (field "id" int)
    (field "uid" string)
    (field "name" string)
    (field "startDate" string)
    (field "updated_at" string)
    (field "created_at" string)

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
  { project : Maybe Project
  , transactions : List TransactionView
  }

projectTransactionsViewDecoder =
  Decode.map2 ProjectTransactionsView
    (field "project" (maybe projectDecoder))
    (field "transactions" (Decode.list transactionViewDecoder))

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
  { item : Maybe Item
  , inStock : Int
  }

initialItemStockView : ItemStockView
initialItemStockView =
  { item = Nothing
  , inStock = 0
  }

itemStockViewDecoder =
  Decode.map2 ItemStockView
    (field "item" (maybe itemDecoder))
    (field "inStock" int)

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
      , itemStockViews = []
      , searchInput = ""
      }

    initialModel : Model 
    initialModel =
      { key = key
      , url = url
      , baseUrl = flag.baseUrl
      , currentDate = flag.currentDate
      , loggedIn = False
      , navbarState = navbarState
      , projectState = initialProjectModel
      , transactionState = initialTransactionModel
      , itemState = initialItemModel
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
  | Login
  | Logout
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
  -- Item
  | InputSearchItem String

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
      let
        newModel = ( { model | url = url } )
      in
      fetchByUrl newModel

    Login ->
      let
        newModel = ( { model | loggedIn = True } )
      in
      fetchByUrl newModel

    Logout ->
      ( { model | loggedIn = False }, Cmd.none )

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
          (Debug.log <| Debug.toString newProjectState)
          ( { model | projectState = newProjectState }, Cmd.none )

        Err e ->
          (Debug.log <| "Error decoding project views" ++ Debug.toString e)
          ( model, Cmd.none )

    GotProject res ->
      case res of
        Ok project ->
          let
            projectState = model.projectState
            newProjectState = { projectState | project = project }
          in
          (Debug.log <| Debug.toString project)
          ( { model | projectState = newProjectState }, Cmd.none )

        Err _ ->
          ( model, Cmd.none )

    GotItems res ->
      case res of
        Ok itemStockViews ->
          let
            itemState = model.itemState
            newItemState = { itemState | itemStockViews = itemStockViews }
          in
          ( { model | itemState = newItemState }, Cmd.none )
        
        Err _ ->
          ( model, Cmd.none )

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
      (Debug.log <| Debug.toString parsedProject)
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
          ( model, Nav.pushUrl model.key "/#/projects" )

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
        
        Err _ ->
          let
            newTransactionState = { transactionState | requestStatus = Error }
          in
          ( { model | transactionState = newTransactionState}, Cmd.none )

    InputSearchItem searchInput ->
      let
        itemState = model.itemState
        newItemState = { itemState | searchInput = searchInput }
      in
        ( { model | itemState = newItemState }, Cmd.none )

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch 
    [ Dropdown.subscriptions model.transactionState.projectsDropdown ToggleProject ]

-- VIEW


view : Model -> Browser.Document Msg
view model =
  let
    page = Maybe.withDefault Index <| Url.parse urlParser <| model.url
    currentPage =
      if not model.loggedIn then
        Tuple.pair "Login" (loginPage model)
      else
        case page of
          Index ->
            Tuple.pair "Index" (transactionPage model)

          ProjectPage ->
            Tuple.pair "Projects" (projectPage model)

          ProjectDetail projectId ->
            Tuple.pair "Project Detail" (projectDetailPage model projectId)

          ItemPage ->
            Tuple.pair "Items" (itemPage model)

          ItemDetail itemId ->
            Tuple.pair "Item Detail" (itemDetailPage model)

          TransactionPage ->
            Tuple.pair "Transactiosns" (transactionPage model)

          TransactionDetail transactionId ->
            Tuple.pair "Transaction Detail" (transactionPage model)
            
  in
  { title = ("Cozy PoS | " ++ Tuple.first currentPage)
  , body =
      [ Tuple.second currentPage
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


transactionPage : Model -> Html Msg
transactionPage model =
  let
    addButton =
      case model.transactionState.projectTransactionsView.project of
        Just _ ->
          a [ href <| "/#/transactions/new" ]
            [ Button.button [ Button.primary ] [ text "Add" ] ]
      
        _ ->
          div [] []
  in
  div []
    [ navbar model
    , div [ class "m-2" ]
        [ Dropdown.dropdown
            model.transactionState.projectsDropdown
              { options = []
              , toggleMsg = ToggleProject
              , toggleButton =
                  Dropdown.toggle [ Button.primary ] [ text model.transactionState.selectedProject ]
              , items =
                  List.map (\project ->  Dropdown.buttonItem [ onClick (SelectProject project) ] [ text project.name ] ) model.transactionState.projects
              }
        ]
    , div [] 
        [ if model.transactionState.requestStatus == Loading then
            Spinner.spinner [ Spinner.grow ] []
          else  
            -- text <| Debug.toString model.transactionState.projects 
            text "Load complete"
        ]
    , div [] [ Spinner.spinner [] [] ]
    , div [] [ addButton ]
    , div []
        [ let
            projectTransactionsView = model.transactionState.projectTransactionsView
          in
          case projectTransactionsView.project of
            Just project ->
              div []
                [ h3 [] [ text project.name ]
                , ListGroup.ul (List.map transactionCard projectTransactionsView.transactions )
                ]

            Nothing ->
              div [ style "background-color" "lightblue", class "my-3" ] [text "No projects to show."]
        ]
    ]

transactionCard : TransactionView -> ListGroup.Item msg
transactionCard transactionView =
  ListGroup.li []
    [ div [] 
      [ div [] [ text <| ("ID no." ++ String.fromInt transactionView.transaction.id) ]
      , div [ class "d-flex justify-content-between align-items-center" ] 
          [ h4 [] 
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
      ] 
    ]

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
    , div [] [ text "This is the item page"]
    , div []
        [ a [ href "/#/items/add" ]  
            [ Button.button [ Button.primary ] [ text "Add" ] ]
        ]
    , div [ class "my-2" ] 
        [ Input.text [ Input.placeholder "Search item...", Input.onInput InputSearchItem ] ]        
    , div []
        [ ListGroup.ul (List.map itemCard filteredItems) ]
    ]

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
        , div [] [ text "In stock: ", span [] [ b [] [ text <| String.fromInt itemStockView.inStock ] ] ]
        ] 
    ]

itemDetailPage model =
  div [] 
      [ navbar model
      , div [] [ text "This is the item detail page"] 
      , div []
          [ a [ href "/#/items" ]  
              [ Button.button [] [ text "Back" ] ]
          ]
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
  , div []
      (List.map projectCard model.projectState.projects.projects)
  ]

projectCard projectView =
  div []
    [ a [ href ("/#/projects/" ++ String.fromInt projectView.project.id) ] 
        [ text projectView.project.name ]
    ]


projectDetailPage model projectId =
  div []
    [ navbar model
    , div []
        [ a [ href "/#/projects" ] [ text "Back" ] ]
    , text ("This is the project detail page, project id: " ++ projectId)
    , div [] [ text <| Debug.toString model.projectState.project ]
    , div [] [ text "Some form" ]
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

fetchByUrl model =
  let
    page = Maybe.withDefault Index <| Url.parse urlParser <| model.url
  in
  case page of
    ProjectPage ->
      ( model 
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

    ItemPage ->
      ( model
      , sendRequest
          model.baseUrl
          "GET"
          "/itemstocks"
          Http.emptyBody
          (Http.expectJson GotItems (Decode.list itemStockViewDecoder)) 
      )

    _ ->
      ( model, Cmd.none )