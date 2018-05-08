{-# LANGUAGE FlexibleContexts, TypeOperators, RankNTypes, DoAndIfThenElse, TypeFamilies, TupleSections, TemplateHaskell, DeriveDataTypeable, EmptyDataDecls, DeriveGeneric, OverloadedStrings, ScopedTypeVariables #-}

module Main where

import HAAP hiding ((.=))

import qualified Data.Vector as Vector
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe
import Data.Default
import Data.IORef
import Data.Acid
import Data.Map (Map(..))
import qualified Data.Map as Map
import Data.List.Split
import Data.Traversable
import Data.Unique
import Data.Foldable
import Data.SafeCopy
import Data.String
import Data.Time.LocalTime
import Data.Time.Format
import Data.Time.Calendar
import Data.List
import Data.Proxy
import Data.Binary
import qualified Data.ByteString.Lazy as ByteString

import System.Random.Shuffle
import System.Environment
import System.FilePath
import System.Directory
import System.Timeout
import System.Random
import System.Process

import Control.DeepSeq
import Control.Monad
import Control.Monad.Except
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.State as State

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A hiding (lang)

import Test.QuickCheck.Gen

import LI11718
import OracleT1 as T1
import OracleT2 as T2
import OracleT3 as T3
import OracleT4 as T4
import OracleT6 as T6
import SimulateT6 as SimT6
import Mapas

import Text.Printf

import GHC.Generics (Generic(..))
import GHC.Exception

import Safe

import qualified Shelly

-- * Data types

data PLab

type PLab_Tourney = HaapTourneyDB TourneyGroup

newtype TourneyGroup = TourneyGroup { unTourneyGroup :: (Either Group Unique,Maybe Link) }
    deriving (Generic)

--lookupTourneyGroup :: Int -> [TourneyGroup] -> TourneyGroup
--lookupTourneyGroup i [] = error $ "lookupTourneyGroup " ++ show i
--lookupTourneyGroup i (tg@(TourneyGroup (Left g,_)):xs)
--    | plabGroupId g == i = tg
--    | otherwise = lookupTourneyGroup i xs
--lookupTourneyGroup i (tg@(TourneyGroup (Right _,_)):xs) = lookupTourneyGroup i xs

instance Eq TourneyGroup where
    (TourneyGroup x) == (TourneyGroup y) = fst x == fst y

instance Ord TourneyGroup where
    compare (TourneyGroup x) (TourneyGroup y) = fst x `compare` fst y

instance Out TourneyGroup where
    docPrec i x = doc x
    doc (TourneyGroup (Right i,_)) = text "random"
    doc (TourneyGroup (Left g,_)) = doc $ plabGroupId g

instance NFData TourneyGroup

instance TourneyPlayer TourneyGroup where
    defaultPlayer = do
        i <- newUnique
        return $ TourneyGroup (Right i,Just "")
    isDefaultPlayer = isJust . snd . unTourneyGroup
    renderPlayer g@(TourneyGroup (Right i,_)) = H.preEscapedToMarkup (fromString $ ("random") :: String)
    renderPlayer g@(TourneyGroup (_,Nothing)) = H.preEscapedToMarkup (pretty g)
    renderPlayer g@(TourneyGroup (_,Just link)) = H.a ! A.href (fromString link) $ fromString $ pretty g

data PLab_DB = PLab_DB
    { plabGroups :: Map PLab_GroupId PLab_Group
    , plabTourney :: PLab_Tourney
    }

type PLab_DBArgs = AcidDBArgs PLab_DB

data PLab_Group = PLab_Group
    { groupSource :: FilePathSource
    , groupT3Rank :: [PercentageScore]
    }

type PLab_GroupId = Int

plabGroupId :: Group -> PLab_GroupId
plabGroupId = read . drop 8 . groupId
  
-- * DB instances

$(deriveSafeCopy 0 'base ''Unique)  
$(deriveSafeCopy 0 'base ''PLab_DB)
$(deriveSafeCopy 0 'base ''PLab_Group)
$(deriveSafeCopy 0 'base ''TourneyGroup)

queryTourney :: Query PLab_DB PLab_Tourney
queryTourney = do
    db <- Reader.ask
    return $ (plabTourney db)

updateTourney :: PLab_Tourney -> Update PLab_DB ()
updateTourney v = State.modify $ \db -> db { plabTourney = v }

queryGroupSource :: Group -> Query PLab_DB (Maybe FilePathSource)
queryGroupSource g = do
    db <- Reader.ask
    return $ fmap groupSource $ Map.lookup (plabGroupId g) (plabGroups db)

queryGroupT3Rank :: Group -> Query PLab_DB [PercentageScore]
queryGroupT3Rank g = do
    db <- Reader.ask
    return $ maybe [] id $ fmap groupT3Rank $ Map.lookup (plabGroupId g) (plabGroups db)

updateGroupT3Rank :: Group -> [PercentageScore] -> Update PLab_DB ()
updateGroupT3Rank g i = updateGroup g (\s -> s { groupT3Rank = i })

updateGroup :: Group -> (PLab_Group -> PLab_Group) -> Update PLab_DB ()
updateGroup g f = do
    State.modify $ \st -> st { plabGroups = Map.update (Just . f) (plabGroupId g) (plabGroups st) }

$(makeAcidic ''PLab_DB
    ['queryGroupSource,'queryGroupT3Rank
    ,'updateGroupT3Rank,'queryTourney,'updateTourney]
    )

lnsTourney :: DBLens (AcidDB PLab_DB) PLab_Tourney
lnsTourney = DBLens
    (AcidDBQuery QueryTourney)
    (\st -> AcidDBUpdate $ UpdateTourney st )

-- * Project

mkPLab_DBArgs :: FilePath -> PLab_DB -> PLab_DBArgs
mkPLab_DBArgs file db = AcidDBArgs { acidDBFile = file , acidDBInit = db, acidDBIOArgs = def { ioTimeout = Just 1000} }

mkPLab_Group :: FilePathSource -> PLab_Group
mkPLab_Group src = PLab_Group src []

plab_Groups :: IO [(Group,PLab_Group)]
plab_Groups = do
    folders <- runShCoreIO def $ shFindGlob "svn/2017li1g*"
    let groups = map mkGroup folders
    return groups

mkGroup :: (String) -> (Group,PLab_Group)
mkGroup (g) = (group,mkPLab_Group source)
    where
    group = Group (takeFileName g) []
    source = FilePathSource ("svn" </> takeFileName g)

plab_Tasks :: [Task]
plab_Tasks = [t1,t2,t3,t4,t5,t6]
    where
    mkTarefa :: Int -> HaapFile
    mkTarefa i = HaapFile (mkLocal i) (mkRemote i) HaapTemplateFile
    mkLocal i = "oracle" </> "Tarefa" ++ show i ++ ".hs"
    mkRemote i = "src" </> "Tarefa" ++ show i ++ "_${group}.hs"
    mkLib = HaapFile "oracle/PLab1718.hs" "src/PLab1718.hs" HaapLibraryFile
    mkMapas = HaapFile "oracle/Mapas.hs" "src/Mapas.hs" HaapLibraryFile
    mkRel1 = HaapFile "oracle/relatorio/calvin.jpg" "relatorio/calvin.jpg" HaapBinaryFile
    mkRel2 = HaapFile "oracle/relatorio/relatorio.tex" "relatorio/relatorio.tex" HaapBinaryFile
    mkOracle f = HaapFile ("oracle" </> f) ("src" </> f) HaapOracleFile
    t1 = Task "Tarefa 1" [mkTarefa 1,mkLib,mkOracle "RunT1.hs"]
    t2 = Task "Tarefa 2" [mkTarefa 2,mkLib,mkOracle "RunT2.hs"]
    t3 = Task "Tarefa 3" [mkTarefa 3,mkLib,mkOracle "RunT3.hs",mkOracle "CollisionSimulator.hs"]
    t4 = Task "Tarefa 4" [mkTarefa 4,mkLib,mkOracle "RunT4.hs"]
    t5 = Task "Tarefa 5" [mkTarefa 5,mkLib]
    t6 = Task "Tarefa 6" [mkTarefa 6,mkLib,mkMapas,mkRel1,mkRel2]

plab_ProjectDB :: IO (Project,PLab_DB)
plab_ProjectDB = do
    groups <- plab_Groups
    let proj = Project
                { projectName = "PLab"
                , projectPath = "."
                , projectTmpPath = "tmp"
                , projectGroups = map fst groups
                , projectTasks = plab_Tasks
                }
    let db = PLab_DB (Map.fromList $ map (mapFst plabGroupId) groups) emptyHaapTourneyDB
    return (proj,db)

-- * Main

main = do
    (project,db) <- plab_ProjectDB
    let dbargs = mkPLab_DBArgs "acidDB" db
    
    runHaap project $ do
        useAcidDB (dbargs) (script)
        return ()

-- * Script

cwImgPath = "images"

cwImgs =
    [("lava",cwImgPath </> "lava.jpg")
    ,("nitro1",cwImgPath </> "nitro1.png")
    ,("nitro2",cwImgPath </> "nitro2.png")
    ,("nitro3",cwImgPath </> "nitro3.png")
    ,("nitro4",cwImgPath </> "nitro4.png")
    ,("puff","images/puff.png")
    ,("bar1","images/bar1.png")
    ,("bar2","images/bar2.png")
    ,("bar3","images/bar3.png")
    ,("bar4","images/bar4.png")
    ,("p1",cwImgPath </> "p1.png")
    ,("p2",cwImgPath </> "p2.png")
    ,("p3",cwImgPath </> "p3.png")
    ,("p4",cwImgPath </> "p4.png")
    ,("c1",cwImgPath </> "c1.png")
    ,("c2",cwImgPath </> "c2.png")
    ,("c3",cwImgPath </> "c3.png")
    ,("c4",cwImgPath </> "c4.png")
    ,("n1","images/1.png")
    ,("n2","images/2.png")
    ,("n3","images/3.png")
    ,("n4","images/4.png")
    ,("btt","images/btt.png")
    ,("mrt","images/mrt.png")
    ,("timer","images/timer.png")
    ,("f0","images/fonts/0.bmp")
    ,("f1","images/fonts/1.bmp")
    ,("f2","images/fonts/2.bmp")
    ,("f3","images/fonts/3.bmp")
    ,("f4","images/fonts/4.bmp")
    ,("f5","images/fonts/5.bmp")
    ,("f6","images/fonts/6.bmp")
    ,("f7","images/fonts/7.bmp")
    ,("f8","images/fonts/8.bmp")
    ,("f9","images/fonts/9.bmp")
    ,("fa","images/fonts/a.bmp")
    ,("fb","images/fonts/b.bmp")
    ,("fc","images/fonts/c.bmp")
    ,("fd","images/fonts/d.bmp")
    ,("fe","images/fonts/e.bmp")
    ,("ff","images/fonts/f.bmp")
    ,("fg","images/fonts/g.bmp")
    ,("fh","images/fonts/h.bmp")
    ,("fi","images/fonts/i.bmp")
    ,("fj","images/fonts/j.bmp")
    ,("fk","images/fonts/k.bmp")
    ,("fl","images/fonts/l.bmp")
    ,("fm","images/fonts/m.bmp")
    ,("fn","images/fonts/n.bmp")
    ,("fo","images/fonts/o.bmp")
    ,("fp","images/fonts/p.bmp")
    ,("fq","images/fonts/q.bmp")
    ,("fr","images/fonts/r.bmp")
    ,("fs","images/fonts/s.bmp")
    ,("ft","images/fonts/t.bmp")
    ,("fu","images/fonts/u.bmp")
    ,("fv","images/fonts/v.bmp")
    ,("fw","images/fonts/w.bmp")
    ,("fx","images/fonts/x.bmp")
    ,("fy","images/fonts/y.bmp")
    ,("fz","images/fonts/z.bmp")
    ]

newtype T3Group = T3Group (Group,(FilePath,[PercentageScore]))

instance Out T3Group where
    docPrec i = doc
    doc (T3Group (g,p)) = text $ groupId g

plabGroupString,plabGroupStringPad :: Group -> String
plabGroupString g = "2017li1g" ++ pretty (plabGroupId g)
plabGroupStringPad g = "2017li1g" ++ printf "%03.0f" ((realToFrac $ plabGroupId g) :: Float)

groupFile g = "svn" </> plabGroupStringPad g </> "src" </> "Tarefa6_" ++ plabGroupString g ++ ".hs"

tourneyGroupFile (TourneyGroup (_,Just _)) = "oracle/Tarefa6_random.hs" 
tourneyGroupFile (TourneyGroup (Left g,Nothing)) = groupFile g

groupModule g = "Tarefa6_" ++ plabGroupString g

tourneyGroupModule (TourneyGroup (_,Just _)) = "Tarefa6_random" 
tourneyGroupModule (TourneyGroup (Left g,Nothing)) = groupModule g

tourneyGroupName (TourneyGroup (Right i,_)) = "random"
tourneyGroupName (TourneyGroup (Left g,_)) = plabGroupString g

tourneyGroupBot (TourneyGroup (_,Just _)) i = "GUINone"
tourneyGroupBot (TourneyGroup (Left g,Nothing)) i = "(GUIBot P" ++ show i ++ ".bot)"

groupBot g i = "(GUIBot P" ++ show i ++ ".bot)"

refreshTmp :: (MonadIO m,HaapStack t m) => Haap t m ()
refreshTmp = do
    projpath <- getProjectPath
    projtmp <- getProjectTmpPath
    orDefault () $ runBaseSh $ shRm $ projpath </> projtmp
    orDefault () $ runBaseIO $ createDirectoryIfMissing True $ projpath </> projtmp

script :: (MonadIO m,HasPlugin (AcidDB PLab_DB) t m) => Haap t m ()
script = do
    lastupdate <- runBaseIO' $ getZonedTime
    
    let hp0 = defaultHakyllP
    
    projpath <- getProjectPath
    projtmp <- getProjectTmpPath
    -- initialization
    orDefault () $ runBaseIO $ createDirectoryIfMissing True $ projpath </> "svn"
    refreshTmp
    
    let ghcjsargs = def { ghcjsSafe = False }
    let iocmd = Just "env" --else Nothing
    let ioenv = []
    let ioargs = addIOCmd iocmd $ addIOEnv ioenv $ def { ioTimeout = Just 60 }
    let cwioargs = ioargs { ioTimeout = Just 120 }
    
    grupos <- getProjectGroups
    
    do
            let emptypath = "nothing.html"
            
            let runHakyll :: forall t m a . (MonadIO m,HaapStack t m) => Bool -> Bool -> HakyllP -> Haap (HakyllT :..: t) m a -> Haap t m a
                runHakyll doClean doCopy hp m = useHakyll (HakyllArgs def doClean doCopy hp) $ do
                    -- load images
                    let loadImages = do
                        hakyllRules $ do
                            match (fromGlob ("images" </> "*")) $ do
                                route idRoute
                                compile copyFileCompiler
                            match (fromGlob ("images" </> "fonts" </> "*")) $ do
                                route idRoute
                                compile copyFileCompiler
                            match (fromGlob ("images" </> "xmas" </> "*")) $ do
                                route idRoute
                                compile copyFileCompiler
                    loadImages
                    m
            
            -- T1 map viewer
            mapviewerpath <- runHakyll True True hp0 $ do
                let tip = "Mapa ((1,0),Este) [[Peca (Curva Norte) 2,Peca Recta 2,Peca (Curva Este) 2],[Peca (Curva Oeste) 2,Peca Recta 2,Peca (Curva Sul) 2]]"
                let mapviewer = CodeWorldArgs (Left "oracle/MapViewer.hs") "Visualizador de Caminhos/Mapas" (CWDraw CWDrawButton tip) (ghcjsargs) (cwioargs) "mapviewer" cwImgs []
                useAndRunCodeWorld mapviewer
                
            -- T3 collision viewer
            collisionviewerpath <- runHakyll False False hp0 $ do
                let tip = "(3,Carro {posicao = (6.8,3.3), direcao = 45, velocidade = (0.5,1)})"
                let collisionviewer = CodeWorldArgs (Left "oracle/CollisionViewer.hs") "Visualizador de Colisões" (CWDraw CWDrawButton tip) (ghcjsargs) (cwioargs) "collisionviewer" cwImgs []
                useAndRunCodeWorld collisionviewer
            
            -- T4 move viewer
            moveviewerpath <- runHakyll False False hp0 $ do
                let tip = "(0.4, Jogo {mapa = Mapa ((2,1),Este) [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 0,Peca (Curva Sul) 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]], pista = (Propriedades {k_atrito = 2, k_pneus = 1, k_acel = 4, k_peso = 2, k_nitro = 5, k_roda = 90}), carros = [Carro {posicao = (1.7,2.3), direcao = 5, velocidade = (1,0)}], nitros = [5], historico = [[(1,2)]]}, Acao {acelerar = True, travar = False, esquerda = True, direita = False, nitro = Nothing})"
                let moveviewer = CodeWorldArgs (Left "oracle/MoveViewer.hs") "Visualizador de Movimentos" (CWDraw CWDrawButton tip) (ghcjsargs) (cwioargs) "moveviewer" cwImgs []
                useAndRunCodeWorld moveviewer
            
            -- run for each group
            groups <- groupsFeedback grupos (runHakyll False False hp0) ghcjsargs cwioargs ioargs mapviewerpath collisionviewerpath moveviewerpath
            runHakyll False False hp0 $ groupsPage $ map (mapSnd fst) groups
            
            do
                -- T3 ranking
                runHakyll False False hp0 $ do
                    let t3Rank = HaapRank "ranks/t3.html" "Rankings (Task 3)" "Group" Nothing "Ranking" (map T3Group groups) rankT3Group
                        rankT3Group (T3Group (g,(path,scores))) = return scores
                    useRank $ renderHaapRank t3Rank
                    return ()
                
            do
                -- T6 pre-processing
                tourneyplayers <- runHakyll False False hp0 $ forM groups $ \(p,_) -> orDo (\e -> return $ TourneyGroup (Left p,Just $ pretty e)) $ do
                    let modu = (groupModule p)
                    let tourneyioargs = ioargs { ioTimeout = Just 120 }
                    let ghcargs = def { ghcRTS = True, ghcArgs = [], ghcSafe = False }
                    let (dir,path) = splitFileName (groupFile p)
                    iores <- orIOResult $ runBaseShWith (tourneyioargs) $ do
                        shCd dir
                        shGhcWith ghcargs [path]
                    if (resOk iores)
                        then return $ TourneyGroup (Left p,Nothing)
                        else addMessageToError (pretty iores) $ do
                            tno <- liftM tourneyNo $ queryDB $ AcidDBQuery QueryTourney
                            let groupfile = addExtension ("tourney" ++ pretty tno </> plabGroupString p) "html"
                            hakyllRules $ create [fromString $ "tourneys/t6" </> groupfile] $ do
                                route $ idRoute `composeRoutes` funRoute (hakyllRoute hp0)
                                compile $ do
                                    makeItem (pretty iores) >>= hakyllCompile hp0
                            return $ TourneyGroup (Left p,Just groupfile)
                tourneyplayersLR <- return $ Left tourneyplayers
            
                -- T6 tourney
                tourneymaps <- runBaseIO $ shuffleM mapas4
                tourneyprops <- runBaseIO $ shuffleM propriedades
                let tourneyrandoms = zip (concat $ repeat tourneymaps) (concat $ repeat tourneyprops)
                tourneynum <- runBaseIO $ newIORef 0
                runHakyll False False hp0 $ do
                    let t6bestof :: Int -> Int
                        t6bestof 128 = 1
                        t6bestof 64 = 3
                        t6bestof 16 = 3
                        t6bestof 4 = 5
                    let match tno rno mno ps@[p1,p2,p3,p4] = do
                        let tourneyioargs = ioargs { ioTimeout = Just 240 }
                        let rts = ["+RTS","-K800m","-M800m","-RTS"] :: [String]
                        let tpath = "tourney" ++ pretty tno </> "round" ++ pretty rno </> "match" ++ pretty mno
                        let folder = projtmp </> "t6" </> tpath
                        let root = dirToRoot folder
                        let files = map (takeDirectory . tourneyGroupFile) $ filter (not . isDefaultPlayer) ps
                        let tourneyincludes = "-i"++ unSplitOn ":" (map (root </>) (files++["oracle"]))
                        let ghcargs = def { ghcSafe = False, ghcRTS = True, ghcArgs = [tourneyincludes], ghcIO = tourneyioargs }
                        let matchhtml = "tourneys/t6" </> addExtension ( tpath) "html"
                        
                        let rnoindex :: Int -> Int
                            rnoindex 128 = 0
                            rnoindex 64 = 1
                            rnoindex 16 = 2
                            rnoindex 4 = 3
                        
                        (mapa,pista) <- do
                                n <- runBaseIO $ readIORef tourneynum
                                runBaseIO $ writeIORef tourneynum $ succ n
                                return $ tourneyrandoms !! n
                        
                        -- run simulator
                        mb_sim <- orEither $ haapRetry 2 $ do
                            runBaseSh $ shMkDir folder
                            let simulatectx = fieldContext "mapa" (show mapa)
                                    `mappend` fieldContext "pista" (show pista)
                                    `mappend` fieldContext "player1" (tourneyGroupModule p1)
                                    `mappend` fieldContext "player2" (tourneyGroupModule p2)
                                    `mappend` fieldContext "player3" (tourneyGroupModule p3)
                                    `mappend` fieldContext "player4" (tourneyGroupModule p4)
                                    `mappend` fieldContext "bot1" (tourneyGroupBot p1 1)
                                    `mappend` fieldContext "bot2" (tourneyGroupBot p2 2)
                                    `mappend` fieldContext "bot3" (tourneyGroupBot p3 3)
                                    `mappend` fieldContext "bot4" (tourneyGroupBot p4 4)
                            let simulatefile = "Simulate.hs"
                            haapRetry 2 $ runBaseShWith (tourneyioargs) $ do
                                shLoadApplyAndCopyTemplate simulatectx ("oracle/SimulateT6Match.hs") (folder </> simulatefile)
                            ghcres <- orIOResult $ runBaseShWith (tourneyioargs) $ do
                                shCd folder
                                shGhcWith ghcargs [simulatefile]
                            (frames,positions) :: (Frames,[Int]) <- addMessageToError (pretty ghcres) $ do
                                runBaseShWith (tourneyioargs) $ do
                                    shCd folder
                                    exec <- shExec "Simulate"
                                    shCommandToFileWith_ tourneyioargs exec (rts) (folder </> "bin")
                                runBaseIOWith tourneyioargs $ decodeFile (folder </> "bin")
                            let rank = zip ps positions
                            return (frames,rank)
                        
                        -- run animator
                        haapRetry 3 $ do
                            case mb_sim of
                                Left err -> throwError err
                                Right (frames,rank) -> do
                                    let players = map (tourneyGroupName . fst) rank
                                    let animatectx = fieldContext "mapa" (show mapa)
                                           `mappend` fieldContext "pista" (show pista)
                                           `mappend` fieldContext "frames" (show frames)
                                           `mappend` fieldContext "players" (show players)
                                    let animatefile = "Animate.hs"
                                    haapRetry 2 $ runBaseShWith (tourneyioargs) $ do
                                        shLoadApplyAndCopyTemplate animatectx ("oracle/AnimateT6Match.hs") (folder </> animatefile)
                                    
                                    -- T6 match viewer
                                    let ghcjsargs' = ghcjsargs { ghcjsArgs = ghcjsArgs ghcjsargs ++ [tourneyincludes] }
                                    matchviewerpath <- do
                                        let tip = ""
                                        let title = "Race Viewer"
                                        let matchviewer = CodeWorldArgs (Left $ folder </> animatefile) title (CWDraw CWDrawFullscreen tip) (ghcjsargs') (cwioargs) ("tourneys/t6" </> tpath) cwImgs []
                                        useAndRunCodeWorld matchviewer
                                    
                                    return (rank, "../.." </> matchviewerpath)
                    let render link = return link
                    let delete tno = do
                        runBaseSh $ shRm $ projtmp </> "t6" </> "tourney" ++ pretty tno
                        orIOResult $ runBaseShWith (ioargs) $ shCommandWith ioargs "rsync" ["-vr","--delete","$(mktemp -d)/","plab@web.lsd.di.uminho.pt:public/tourneys/t6/tourney" ++ pretty tno]
                        return ()
                    let t6Tourney = HaapTourney 10 "Task 6" t6bestof "Group" tourneyplayersLR "tourneys/t6" lnsTourney match render delete
                    withHakyllP hp0 $ useRank $ useTourney $ renderHaapTourney t6Tourney
                    return ()
            
            libfiles <- liftM (nub . filter ((/= HaapOracleFile) . haapFileType)) $ getProjectTaskFiles
            
            runHakyll False False hp0 $ hakyllRules $ do
                forM_ libfiles $ \libfile -> do
                    match (fromGlob $ haapLocalFile libfile) $ do
                        route $ customRoute (const $ haapRemoteFile libfile)
                        compile copyFileCompiler
                
                let title = "PLab"
                create ["nothing.html"] $ do
                    route $ idRoute `composeRoutes` funRoute (hakyllRoute hp0)
                    compile $ do
                        let indexCtx = constField "projectname" title
                                     `mappend` constField "projectpath" projpath
                        makeItem "" >>= loadAndApplyHTMLTemplate "templates/nothing.html" indexCtx >>= hakyllCompile hp0 
                
                create ["index.html"] $ do
                    route $ idRoute `composeRoutes` funRoute (hakyllRoute hp0)
                    compile $ do
                        let libCtx = field "libpath" (return . haapRemoteFile . itemBody)
                                     `mappend` field "libname" (return . haapRemoteFile . itemBody)
                        let indexCtx = constField "title" title
                                     `mappend` constField "lastupdate" (show lastupdate)
                                     `mappend` constField "projectpath" projpath
                                     `mappend` listField "libfiles" libCtx (mapM makeItem libfiles)
                        makeItem "" >>= loadAndApplyHTMLTemplate "templates/index.html" indexCtx >>= hakyllCompile hp0
            
            return ()

--bracketPlayers :: [TourneyGroup] -> IO [[TourneyGroup]]
--bracketPlayers gs = mapM (mapM getplayer) brackets_final
--    where
--    getplayer :: Int -> IO TourneyGroup
--    getplayer 000 = defaultPlayer
--    getplayer i = return $ lookupTourneyGroup i gs
    
-- generate groups feedback
groupsFeedback :: (MonadIO m,HasPlugin (AcidDB PLab_DB) t m) => [Group] -> (forall a. Haap (HakyllT :..: t) m a -> Haap t m a) -> GHCJSArgs -> IOArgs -> IOArgs -> FilePath -> FilePath -> FilePath -> Haap t m [(Group,(FilePath,[PercentageScore]))]
groupsFeedback grupos run ghcjsargs cwioargs ioargs mapviewerpath collisionviewerpath moveviewerpath = do
    forM grupos $ \g -> liftM (g,) $ groupFeedback run ghcjsargs cwioargs ioargs mapviewerpath collisionviewerpath moveviewerpath g

groupFeedback :: (MonadIO m,HasPlugin (AcidDB PLab_DB) t m) => (forall a. Haap (HakyllT :..: t) m a -> Haap t m a) -> GHCJSArgs -> IOArgs -> IOArgs -> FilePath -> FilePath -> FilePath -> Group -> Haap t m (FilePath,[PercentageScore])
groupFeedback run ghcjsargs cwioargs ioargs mapviewerpath collisionviewerpath moveviewerpath g = do
--    refreshSvn
    run $ do
        -- initialization
        let hpRoute f = if isRelative f
            then case splitOn "#" f of
                (x:xs) -> unSplitOn "#" (phpFunRoute x:xs)
                otherwise -> f
            else f
        let hpLogin = defaultHakyllP
        hp0 <- readHakyllP
        let hp = hp0 `mappend` hpLogin
        projname <- getProjectName
        projpath <- getProjectPath
        let sandboxcfg = "cabal.sandbox.config"
        let gpage = "grupos" </> addExtension (show $ plabGroupId g) "html"
        let ghcargs = def
        let svnargs = def { svnHidden = False }
        
        let gctx = fieldContext "group" (plabGroupString g)
        
        let emptygrouppath = "../nothing.html"
        let emptypath = "nothing.html"
        let testsioargs = ioargs { ioTimeout = Just 100 }
        let testioargs = ioargs { ioTimeout = Just 10 }
        let feedbackmode = HaapSpecArgs HaapSpecQuickCheck Nothing testioargs
        
        mbsource <- queryDB $ AcidDBQuery $ QueryGroupSource g
        orErrorHakyllPage gpage (gpage,[]) $ case mbsource of
            Nothing -> throwError $ HaapException $ "Group source not found"
            Just source -> useFilePathSource def $ do
                -- update the source
                getSource source
                
                newfiles <- orLogDefault [] $ populateGroupSource gctx False g source
                
                do
                        gfiles <- listGroupSourceFiles gctx True g source
                        let gsrcfiles = map (makeRelative "src") gfiles
                        
                        let gpath = sourcePath source
                        let gsrcpath = gpath </> "src"
                        let ghtml = "grupos" </> show (plabGroupId g)
                        --let gdate = fmap svnDate info
                        
                        let rts = ["+RTS","-K100m","-M100m","-RTS"] :: [String]
                        
                        -- run feedback script
                        
                        -- T1
                        let gt1html = (ghtml </> "t1.html")
                        (specT1path,hpcT1path) <- withHakyllP hp $ do
                            let hpcpath1 = Just (ghtml </> "hpcT1")
                            let hpcT1 = HpcArgs (gsrcpath </> "RunT1") (ghcargs) (ioargs) Nothing hpcpath1 True
                            (specT1path,hpcT1path) <- useAndRunHpc hpcT1 gt1html $ \ghcT1res -> orErrorHakyllPage gt1html (hakyllRoute hp $ ghtml </> "t1.html") $ addMessageToError (pretty ghcT1res) $ do
                                testsT1 <- haapRetry 2 $ runBaseShWith (testsioargs) $ do
                                        shCd $ gsrcpath
                                        exec <- shExec "RunT1"
                                        shPipeWith testsioargs exec ("testes":rts) ()
                                testsT1' <- forM (zip [1..] testsT1) $ \(i,caminho) -> do
                                    let tip = show caminho
                                    let title = "Track Viewer"
                                    let mapviewer = CodeWorldArgs (Right $ "mapviewer" </> "MapViewer.jsexe") title (CWDraw CWDrawFixed tip) (ghcjsargs) (cwioargs) (ghtml </> "t1" </> show i) cwImgs []
                                    mapviewerpathT1 <- withHakyllP hp0 $ useAndRunCodeWorld mapviewer
                                    let url = dirToRoot ghtml </> mapviewerpathT1
                                    let prettyT1 c = doc $ H.a ! A.href (fromString url) $ do
                                        H.preEscapedToMarkup $ pretty c
                                    return $ Pretty prettyT1 caminho
                                let specT1 = bounded "Caminho" testsT1' $ \(Pretty _ c) -> testEqualIO (return $ PrettyMapa $ constroi c) $ do
                                    runShCoreIO ioargs $ do
                                        shCd $ gsrcpath
                                        exec <- shExec "RunT1"
                                        liftM PrettyMapa $ shPipeWith testioargs exec ("constroi":rts) c
                                useSpec feedbackmode $ renderHaapSpec gt1html "Tarefa 1" (pretty ghcT1res) specT1
                            return (specT1path,hpcT1path)
                            
                        -- T2
                        let gt2html = (ghtml </> "t2.html")
                        (specT2path,hpcT2path) <- withHakyllP hp $ do
                            let hpcpath2 = Just (ghtml </> "hpcT2")
                            let hpcT2 = HpcArgs (gsrcpath </> "RunT2") (ghcargs) (ioargs) Nothing hpcpath2 True
                            (specT2path,hpcT2path) <- useAndRunHpc hpcT2 gt2html $ \ghcT2res -> orErrorHakyllPage gt2html (hakyllRoute hp $ ghtml </> "t2.html") $ addMessageToError (pretty ghcT2res) $ do
                                testsT2 <- haapRetry 2 $ runBaseShWith (testsioargs) $ do
                                        shCd $ gsrcpath
                                        exec <- shExec "RunT2"
                                        shPipeWith testsioargs exec ("testes":rts) ()
                                testsT2' <- forM (zip [1..] testsT2) $ \(i,tab) -> do
                                    let tip = show tab
                                    let title = "Track Viewer"
                                    let mapviewer = CodeWorldArgs (Right $ "mapviewer" </> "MapViewer.jsexe") title (CWDraw CWDrawFixed tip) (ghcjsargs) (cwioargs) (ghtml </> "t2" </> show i) cwImgs []
                                    mapviewerpathT2 <- withHakyllP hp0 $ useAndRunCodeWorld mapviewer
                                    let url = dirToRoot ghtml </> mapviewerpathT2
                                    let prettyT2 t = doc $ H.a ! A.href (fromString url) $ do
                                        H.preEscapedToMarkup $ pretty t
                                    return $ Pretty prettyT2 tab
                                let specT2 = bounded "Tabuleiro" testsT2' $ \(Pretty _ t) ->
                                             unbounded "Posicao" [] (genPosicao t) $ \p ->
                                             unbounded "Orientação" [] genOrientacao $ \o ->
                                             let m = Mapa (p,o) t in
                                             testEqualIO (return $ valida m) $ do
                                                runShCoreIO ioargs $ do
                                                    shCd $ gsrcpath
                                                    exec <- shExec "RunT2"
                                                    shPipeWith testioargs exec ("valida":rts) m
                                useSpec feedbackmode $ renderHaapSpec gt2html "Tarefa 2" (pretty ghcT2res) specT2
                            return (specT2path,hpcT2path)
                            
                        -- T3
                        let gt3html = (ghtml </> "t3.html")
                        (specT3path,hpcT3path) <- withHakyllP hp $ do
                            let hpcpath3 = Just (ghtml </> "hpcT3")
                            let hpcT3 = HpcArgs (gsrcpath </> "RunT3") (ghcargs) (ioargs) Nothing hpcpath3 True
                            (specT3path,hpcT3path) <- useAndRunHpc hpcT3 gt3html $ \ghcT3res -> orErrorHakyllPage gt3html (hakyllRoute hp $ ghtml </> "t3.html") $ addMessageToError (pretty ghcT3res) $ do
                                testsT3 <- haapRetry 2 $ runBaseShWith (testsioargs) $ do
                                        shCd $ gsrcpath
                                        exec <- shExec "RunT3"
                                        shPipeWith testsioargs exec ("testes":rts) ()
                                testsT3' <- forM (zip [1..] testsT3) $ \(i,(tab,tempo,carro)) -> do
                                    let tip = show (tab,tempo,carro)
                                    let title = "Physics Viewer"
                                    let collisionviewer = CodeWorldArgs (Right $ "collisionviewer" </> "CollisionViewer.jsexe") title (CWDraw CWDrawFixed tip) (ghcjsargs) (cwioargs) (ghtml </> "t3" </> show i) cwImgs []
                                    collisionviewerpath <- withHakyllP hp0 $ useAndRunCodeWorld collisionviewer
                                    let url = dirToRoot ghtml </> collisionviewerpath
                                    let prettyT3 t = doc $ do
                                        H.preEscapedToMarkup ("("::String)
                                        H.a ! A.href (fromString url) $ do
                                            H.preEscapedToMarkup $ pretty tab
                                        H.preEscapedToMarkup $ "\n"++printTab tab
                                        H.preEscapedToMarkup $ "," ++ pretty tempo
                                        H.preEscapedToMarkup $ "," ++ pretty carro
                                        H.preEscapedToMarkup (")"::String)              
                                    return $ Pretty prettyT3 (tab,tempo,carro)
                                let specT3 = bounded "(Tabuleiro,Tempo,Carro)" testsT3' $ \(Pretty _ (tab,tempo,carro)) ->
                                        testMessageIO $ do
                                            unless (validaCarro tab carro) $ do
                                                error $ "Posição inicial do carro inválida: " ++ show carro -- ++ "\noracle: " ++ show idealsol
                                            unless (validaTabuleiro tab) $ do
                                                error $ "Tabuleiro inválido:\n" ++ pretty tab
                                            groupsol <- runShCoreIO ioargs $ do
                                                shCd $ gsrcpath
                                                exec <- shExec "RunT3"
                                                shPipeWith testioargs exec ("movimenta":rts) (tab,tempo,carro)
                                            unless (maybe True (validaCarro tab) groupsol) $ do
                                                error $ "Posição final do carro inválida: " ++ show groupsol -- ++ "\noracle: " ++ show idealsol
                                            
                                            idealranks <- traceCarros 20 tab (carTracing) carro $ \carro' -> do
                                                let idealsol = T3.colide tab tempo carro'
                                                let maxdistance = dist (posicao carro') (posicao carro' .+. (tempo .*. velocidade carro'))
                                                let rank = T3.compareT3Solutions maxdistance groupsol idealsol
                                                return (idealsol,rank)
                                            let (idealsol,idealrank) = headNote "no solution" $ sortBy (flip compareSnd) idealranks
                                            let msg = "precision: " ++ printDouble idealrank 2 ++ "%\n"++"oracle: "++pretty idealsol++"\nsolution: "++ pretty groupsol
                                            return msg
                                useSpec feedbackmode $ renderHaapSpec gt3html "Tarefa 3" (pretty ghcT3res) specT3
                            return (specT3path,hpcT3path)
                        
                        -- T4
                        let feedbackmodeT4 = HaapSpecArgs HaapSpecQuickCheck (Just 4) testioargs
                        let gt4html = (ghtml </> "t4.html")
                        (specT4path,hpcT4path) <- withHakyllP hp $ do
                            let hpcpath4 = Just (ghtml </> "hpcT4")
                            let hpcT4 = HpcArgs (gsrcpath </> "RunT4") (ghcargs) (ioargs) Nothing hpcpath4 True
                            (specT4path,hpcT4path) <- useAndRunHpc hpcT4 gt4html $ \ghcT4res -> orErrorHakyllPage gt4html (hakyllRoute hp $ ghtml </> "t4.html") $ addMessageToError (pretty ghcT4res) $ do
                                testsT4 <- haapRetry 2 $ runBaseShWith (testsioargs) $ do
                                        shCd $ gsrcpath
                                        exec <- shExec "RunT4"
                                        shPipeWith testsioargs exec ("testes":rts) ()
                                testsT4' <- forM (zip [1..] testsT4) $ \(i,(tempo,jogo,acao)) -> do
                                    let tip = show (tempo,jogo,acao)
                                    let title = "Command Viewer"
                                    let moveviewer = CodeWorldArgs (Right $ "moveviewer" </> "MoveViewer.jsexe") title (CWDraw CWDrawFixed tip) (ghcjsargs) (cwioargs) (ghtml </> "t4" </> show i) cwImgs []
                                    moveviewerpathT4 <- withHakyllP hp0 $ useAndRunCodeWorld moveviewer
                                    let url = dirToRoot ghtml </> moveviewerpathT4
                                    let prettyT4 t = doc $ H.a ! A.href (fromString url) $ do
                                        H.preEscapedToMarkup $ pretty t
                                    return $ Pretty prettyT4 (tempo,jogo,acao)
                                let specT4 = bounded "(Tempo,Jogo,Acao)" testsT4' $ \(Pretty _ (tempo,jogo,acao)) ->
                                             unbounded "Jogador" [] (genJogadores jogo) $ \jogador -> do
                                                 if validaJogo tempo jogo jogador
                                                     then do
                                                        let jogo' = T4.atualiza tempo jogo jogador acao
                                                        testEqualIOWith (comparaJogo 0.2) (return jogo') $ do
                                                           runShCoreIO ioargs $ do
                                                               shCd $ gsrcpath
                                                               exec <- shExec "RunT4"
                                                               shPipeWith testioargs exec ("atualiza":rts) (tempo,jogo,jogador,acao)
                                                    else testMessage $ "teste inválido: " ++ pretty (tempo,jogo,acao) ++ "\njogador: " ++ pretty jogador
                                let nota1 = "<b>Nota: O visualizador aplica a mesma ação a todos os jogadores.</b><br>"
                                    nota2 = "<b>Nota: O sistema de feedback testa a ação individualmente para cada jogador.</b><br>"
                                    nota3 = "<b>Nota: O sistema de feedback considera apenas os primeiros 100 testes de cada grupo. </b><br>"
                                let notes = unlines [nota1,nota2,nota3,pretty ghcT4res]
                                useSpec feedbackmodeT4 $ renderHaapSpec gt4html "Tarefa 4" notes specT4
                            return (specT4path,hpcT4path)
                            
                        -- T3 collision simulator
                        collisionsimulatorpath <- withHakyllP hp $ do
                            let ghcjsargs = def { ghcjsSafe = False, ghcjsArgs = ["-i"++dirToRoot gsrcpath++"/oracle"]}
                            let ioargs = def
                            let cwioargs = ioargs
                            let title = "Physics Viewer"
                            let collisionsimulator = CodeWorldArgs (Left $ gsrcpath </> "CollisionSimulator.hs") title (CWGame CWGameConsole) (ghcjsargs) (cwioargs) (ghtml </> "collisionsimulator") cwImgs []
                            useAndRunCodeWorld collisionsimulator
                        
                        -- haddock
                        haddockpaths <- withHakyllP hp $ do
                                let haddock = HaddockArgs (Just sandboxcfg) (projname ++ " - Documentação do Grupo " ++ show (plabGroupId g)) [] (gsrcpath) gsrcfiles (ghtml </> "doc")
                                useAndRunHaddock haddock
                        
                        -- hlint
                        hlintpath <-  withHakyllP hp $ do
                                let hlint = HLintArgs (Just sandboxcfg) [] gsrcpath gsrcfiles (ghtml </> "hlint.html")
                                useAndRunHLint hlint
                        
                        -- homplexity
                        homplexitypath <- withHakyllP hp $ do
                                let hSandox = Nothing
                                let homplexity = HomplexityArgs hSandox [] gsrcpath gsrcfiles (ghtml </> "homplexity.html")
                                useAndRunHomplexity homplexity
                        
                        -- T3 ranking
                        t3rank <- do
                                forM T3.solutionsT3 $ \((tab,tempo,carro),solution) -> do
                                    mbsol <- orMaybe $ runBaseSh $ do
                                        shCd gsrcpath
                                        exec <- shExec "RunT3"
                                        shPipeWith testioargs exec ("movimenta":rts) (tab,tempo,carro)
                                        
                                    case mbsol of
                                        Nothing -> return $ PercentageScore 0
                                        Just groupsol -> runBaseIOWithTimeout (5 * 60) $ do
                                            idealranks <- traceCarros 20 tab (carTracing) carro $ \carro' -> do
                                                let idealsol = T3.colide tab tempo carro'
                                                let maxdistance = dist (posicao carro') (posicao carro' .+. (tempo .*. velocidade carro'))
                                                let rank = T3.compareT3Solutions maxdistance groupsol idealsol
                                                return (idealsol,rank)
                                            let (idealsol,idealrank) = headNote "no solution" $ sortBy (flip compareSnd) idealranks
                                            return $ PercentageScore idealrank
                        
                        -- finalization
                        time <- runBaseIO' getZonedTime
                        hp0 <- readHakyllP
                        hakyllRules $ create [fromFilePath gpage] $ do
                            route $ idRoute `composeRoutes` funRoute (hakyllRoute hp0)
                            compile $ do
                                let pageCtx = constField "group" (show $ plabGroupId g)
                                            `mappend` constField "projectname" (projname)
                                            `mappend` constField "projectpath" (".." </> projpath)
                                            `mappend` constField "date" (show time)
                                            `mappend` listField "haddocks" (field "haddock" (return . makeRelative "grupos" . itemBody)) (mapM makeItem haddockpaths)
                                            `mappend` constField "hlint" (makeRelative "grupos" hlintpath)
                                            `mappend` constField "homplexity" (makeRelative "grupos" homplexitypath)
                                            `mappend` constField "specT1path" (makeRelative "grupos" specT1path)
                                            `mappend` constField "hpcT1path" (makeRelative "grupos" hpcT1path)
                                            `mappend` constField "mapviewerpath" (dirToRoot "grupos" </> mapviewerpath)
                                            `mappend` constField "specT2path" (makeRelative "grupos" specT2path)
                                            `mappend` constField "hpcT2path" (makeRelative "grupos" hpcT2path)
                                            `mappend` constField "specT3path" (makeRelative "grupos" specT3path)
                                            `mappend` constField "hpcT3path" (makeRelative "grupos" hpcT3path)
                                            `mappend` constField "specT4path" (makeRelative "grupos" specT4path)
                                            `mappend` constField "hpcT4path" (makeRelative "grupos" hpcT4path)
                                            `mappend` constField "collisionsimulatorpath" (dirToRoot "grupos" </> collisionsimulatorpath)
                                            `mappend` constField "collisionviewerpath" (dirToRoot "grupos" </> collisionviewerpath)
                                            `mappend` constField "moveviewerpath" (dirToRoot "grupos" </> moveviewerpath)
                                            
                                makeItem "" >>= loadAndApplyHTMLTemplate "templates/grupo.html" pageCtx >>= hakyllCompile hp0
                        updateDB $ AcidDBUpdate $ UpdateGroupT3Rank g t3rank
                        return (gpage,t3rank) 

traceCarros n tab 0 carro f = liftM (:[]) (f carro)
traceCarros n tab delta carro f = forAllIO n (randomizeCarro tab delta carro) f

-- generate groups page
groupsPage :: HasPlugin Hakyll t m => [(Group,FilePath)] -> Haap t m FilePath
groupsPage groupsfeed = do
    projname <- getProjectName
    let makeRow (g,path) = [show (plabGroupId g),pretty $ H.a ! A.href (fromString path) $ "ver"]
    let groupstable = HaapTable
                        (projname ++ " - Feedback por Grupo")
                        ["Grupo","Feedback"]
                        (map makeRow groupsfeed)
                        "grupos.html"
    renderHaapTable groupstable

carTracing :: Double
carTracing = 0.05


