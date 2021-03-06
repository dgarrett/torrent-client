{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns    #-}

import           Control.Applicative
import           Control.Concurrent
import           Control.Exception      (finally, try, SomeException)
import           Control.Lens
import           Control.Monad
import           Crypto.Hash.SHA1       as SHA1
import           Data.BEncode
import           Data.Bits
import qualified Data.ByteString        as B
import qualified Data.ByteString.Char8  as BE
import qualified Data.ByteString.Lazy   as BL
import qualified Data.ByteString.UTF8   as BU
import qualified Data.Foldable          as F
import qualified Data.Map               as M
import qualified Data.Set               as S
import           Data.Word
import           Debug.Trace
import           Network.BSD
import           Network.HTTP
import qualified Network.HTTP.Types.URI as HU
import           Network.Socket
import           Network.URL
import           System.Environment
import           System.IO

type BlockSize = Int

data Block = Block { _bOffset :: Int
						, _bSize :: BlockSize
						} deriving (Eq, Show, Ord)

type PieceNum = Integer
type PieceSize = Integer

data PieceInfo = PieceInfo { _pOffset :: Integer
								, _pLength :: Integer
								, _pDigest :: BL.ByteString
								, _pState :: PieceState
								} deriving (Eq, Show)

data PieceState = Pending
				| Done
				| InProgress { _totalBlocks :: Int
								, _haveBlocks :: S.Set Block
								, _unrequestedBlocks :: S.Set Block
								, _requestedBlocks :: S.Set Block -- TODO add timeout timestamp
								} deriving (Eq, Show)

type PieceMap = M.Map PieceNum PieceInfo
type PieceHashesMap = M.Map PieceNum BL.ByteString

makeLenses ''Block
makeLenses ''PieceInfo
makeLenses ''PieceState

defaultBlockSize = 2^14

-- Message types
choke = '\0'
unchoke = '\1'
interested = '\2'
notInterested = '\3'
have = '\4'
bitfield = '\5'
request = '\6'
piece = '\7'
cancel = '\8'
port = '\9'

hGetCharSafe handle = do
	eof <- hIsEOF handle
	if eof then return '\0' else hGetChar handle

resumePieceMap :: PieceMap -> Handle -> IO (PieceMap)
resumePieceMap pieceMap hFile = do
	let Just firstPiece = M.lookup 0 pieceMap
	let pieceLength = view pLength firstPiece
	fileSize <- hFileSize hFile
	fileContents <- hGetContents hFile
	(result, _) <- F.foldlM (checkPiece pieceLength fileSize) (pieceMap, fileContents) pieceMap
	return result
	where
		checkPiece _ _ (pieceMap, []) _ = return (pieceMap, [])
		checkPiece pieceLength fileSize (!pieceMap, fileContents) piece = do
			let percentChecked = (fromIntegral $ (view pOffset piece)*pieceLength) / (fromIntegral fileSize)
			when (percentChecked <= 1) $ putStrLn $ "Resuming: " ++ (show $ 100 * percentChecked) ++ "%"
			let (pieceContents, remainingContents) = splitAt (fromIntegral $ view pLength piece) fileContents
			let hash = SHA1.hash $ BE.pack pieceContents
			let correctHash = view pDigest piece
			let matches = (BL.toStrict correctHash) == hash
			remainingContents `seq` return () -- strictness bug

			let updateAction = if matches
				then
					M.update (\p -> Just $ set pState Main.Done p) (view pOffset piece) pieceMap
				else
					M.update (\p -> Just $ set pState Main.Pending p) (view pOffset piece) pieceMap
			return (updateAction, remainingContents)

mkBitfield :: PieceMap -> String
mkBitfield pieceMap = reverse $ f (reverse _bits) []
	where
		f [] current = current
		f bits current = f rest ((toEnum (pack byte 0) :: Char):current)
			where
				(byte,_rest) = splitAt 8 bits
				rest = if ((length _rest) < 8 && (length _rest) > 0) then _rest ++ (replicate (8 - (length _rest)) False) else _rest
		pack :: [Bool] -> Int -> Int
		pack [] char = char
		pack (b:bits) char = pack bits $ (char `shift` 1) .|. (fromBool b)
		_bits = M.foldl (\x p -> ((view pState p) == Main.Done):x) [] pieceMap
		fromBool :: Bool -> Int
		fromBool True = 1
		fromBool False = 0

mkPieceMap :: M.Map String BEncode -> Maybe PieceMap
mkPieceMap metainfo = do
	BDict info <- M.lookup "info" metainfo
	BString pieces <- M.lookup "pieces" info
	BInt pieceLength <- M.lookup "piece length" info
	BInt fileLength <- M.lookup "length" info
	let hashes = splitHashes pieces
	let (_, _, _, pieceMap) = pieceMapList (pieceLength, fileLength, 0, M.fromList []) hashes
	return pieceMap
	where pieceMapList d@(pieceLength, fileLength, index, map) hashes = foldl f d hashes
		where
			f (pieceLength, remainingFileLength, index, map) hash = (nextPieceLength, remainingFileLength - pieceLength, index + 1, M.insert index (PieceInfo index pieceLength hash Pending) map)
				where nextPieceLength = if pieceLength < (remainingFileLength - pieceLength) then pieceLength else (remainingFileLength - pieceLength)

getUnrequestedOrRequestedBlock :: PieceMap -> Maybe (Block, PieceNum, PieceMap)
getUnrequestedOrRequestedBlock pieceMap = do
	case getUnrequestedBlock pieceMap of
		Just a -> return a
		Nothing -> trace "End Game: getRequestedBlock" $ getRequestedBlock pieceMap

getRequestedBlock :: PieceMap -> Maybe (Block, PieceNum, PieceMap)
getRequestedBlock pieceMap = do
	(block, key) <- findBlock pieceMap (M.keys pieceMap)
	--let newMap = M.update (\_ -> Just $ over (pState . requestedBlocks) (S.insert block) newPiece) key pieceMap
	return (block, key, pieceMap)
	where
		findBlock _ [] = Nothing
		findBlock pieceMap (k:keys) = case do
			piece <- M.lookup k pieceMap
			let pieceSize = view pLength piece
			--let newPiece = over pState (\ps -> if ps == Pending then InProgress (ceiling $ (fromIntegral pieceSize) / (fromIntegral defaultBlockSize)) (S.fromList []) (mkBlockSet pieceSize defaultBlockSize) (S.fromList []) else ps) piece
			let req = view (pState . requestedBlocks) piece
			(block, newUnrequested) <- S.minView req
			--let newNewPiece = over (pState . unrequestedBlocks) (\_ -> newUnrequested) newPiece
			return (block, k)--, newNewPiece)
			of
				Nothing -> findBlock pieceMap keys
				x -> x

getUnrequestedBlock :: PieceMap -> Maybe (Block, PieceNum, PieceMap)
getUnrequestedBlock pieceMap = do
	(block, key, newPiece) <- findBlock pieceMap (M.keys pieceMap)
	let newMap = M.update (\_ -> Just $ over (pState . requestedBlocks) (S.insert block) newPiece) key pieceMap
	return (block, key, newMap)
	where
		findBlock _ [] = Nothing
		findBlock pieceMap (k:keys) = case do
			piece <- M.lookup k pieceMap
			let pieceSize = view pLength piece
			let newPiece = over pState (\ps -> if ps == Pending then InProgress (ceiling $ (fromIntegral pieceSize) / (fromIntegral defaultBlockSize)) (S.fromList []) (mkBlockSet pieceSize defaultBlockSize) (S.fromList []) else ps) piece
			let unreq = view (pState . unrequestedBlocks) newPiece
			(block, newUnrequested) <- S.minView unreq
			let newNewPiece = over (pState . unrequestedBlocks) (\_ -> newUnrequested) newPiece
			return (block, k, newNewPiece)
			of
				Nothing -> findBlock pieceMap keys
				x -> x

receiveBlock :: PieceMap -> PieceNum -> Block -> (PieceMap, Bool)
receiveBlock pieceMap pieceNum block = (updatedPieceMap, needToCheckHash)
	where
		updatedPieceMap = M.update updateFunc pieceNum pieceMap
		updateFunc = moveBlockToHave block
		moveBlockToHave block piece = do
			let piece' = over (pState . requestedBlocks) (S.delete block) piece
			Just $ over (pState . haveBlocks) (S.insert block) piece'
		Just updatingBlock = M.lookup pieceNum updatedPieceMap
		needToCheckHash = (S.null $ view (pState . requestedBlocks) updatingBlock) && (S.null $ view (pState . unrequestedBlocks) updatingBlock)

bytesTotal pieceMap = M.foldl (\x pi -> x + (view pLength pi)) 0 pieceMap

bytesComplete pieceMap = M.foldl (\x pi -> if ((view pState pi) == Main.Done) then x + (view pLength pi) else x) 0 pieceMap

bytesUnverified pieceMap = M.foldl f 0 pieceMap
	where
		f x pi = if (isInProgress $ view pState pi) then x + (sumBlocksSize (view (pState . haveBlocks) pi)) else x
		isInProgress (InProgress _ _ _ _) = True
		isInProgress _ = False
		sumBlocksSize blocks = S.foldl (\x b -> x + (view bSize b)) 0 blocks

pieceSize piece = (S.foldl sum 0 $ view (pState . haveBlocks) piece) + (S.foldl sum 0 $ view (pState . unrequestedBlocks) piece) + (S.foldl sum 0 $ view (pState . requestedBlocks) piece)
	where
		sum s b = s + (view bSize b)

mkBlockSet :: PieceSize -> BlockSize -> S.Set Block
mkBlockSet pieceSize blockSize = _mkBlockSet pieceSize blockSize 0
	where
		_mkBlockSet pieceSize blockSize offset
			| pieceSize > 0 = S.union (S.fromList [Block (offset*defaultBlockSize) (if blockSize < (fromIntegral pieceSize) then blockSize else (fromIntegral pieceSize))]) (_mkBlockSet (pieceSize - fromIntegral(blockSize)) blockSize (offset + 1))
			| otherwise = S.fromList []

openTorrent filename = do
	fileContents <- BL.readFile filename
	let torrentFile = bRead fileContents
	let metainfo = case torrentFile of
		Just (BDict map) -> map
		_ -> M.fromList []
	return metainfo

connectTracker tracker info_hash = do
	let url = case importURL tracker of
		Just url -> (exportURL $ foldl add_param url [
														--("info_hash", info_hash),
														--("peer_id", info_hash),--BU.toString $ SHA1.hash $ BU.fromString "test"),
														("port", "4243"),--"6881"),
														("uploaded", "0"),
														("downloaded", "0"),
														("left", "0"),
														("compact", "1")
													]) ++ "&info_hash=" ++ info_hash ++ "&peer_id=" ++ info_hash
		Nothing -> tracker
	rsp <- Network.HTTP.simpleHTTP (getRequest (url))
	return (url, rsp)

trackerResponse string =
	bRead $ BL.fromStrict $ BE.pack string

splitHashes (BL.uncons -> Nothing) = []
splitHashes xs = y : splitHashes ys
	where
		(y, ys) = BL.splitAt 20 xs

splitPeers (BL.uncons -> Nothing) = []
splitPeers xs = (BL.splitAt 4 y) : splitPeers ys
	where
		(y, ys) = BL.splitAt 6 xs

toWord16 = BL.foldl' (\x y -> x * 256 + fromIntegral y) 0

toWord32 = BL.foldl' (\x y -> x * 256 + fromIntegral y) 0

--to4Byte :: Int -> String
to4Byte int = [ toEnum $ int `shift` (-8 * 3) .&. 0xff, toEnum $ int `shift` (-8 * 2) .&. 0xff, toEnum $ int `shift` (-8) .&. 0xff, toEnum $ int .&. 0xff ]

from4Byte :: (Enum a, Num b) => [a] -> b
from4Byte str = foldl (\x y -> x * 256 + (fromIntegral . fromEnum) y) 0 str

peerToAddrInfo (addrBS, portBS) =
	AddrInfo [] AF_INET Stream defaultProtocol (SockAddrInet port addr) Nothing
	where
		addr = toWord32 $ BL.reverse addrBS
		port = toWord16 $ portBS

connectPeer :: AddrInfo -> [Char] -> IO (Either SomeException (Handle, Socket))
connectPeer serveraddr info_hash = do
	--Right (handle, sock) <- f
	f
	where f = try $ do
		sock <- socket (addrFamily serveraddr) Stream defaultProtocol -- (getProtocolNumber "tcp")
		connect sock (addrAddress serveraddr)
		handle <- socketToHandle sock ReadWriteMode
		hSetBuffering handle LineBuffering
		--hPutStr handle $"BAL mySavings" ++ [toEnum 0]
		hPutStr handle $ [toEnum 19] ++ "BitTorrent protocol" ++ (replicate 8 $ toEnum 0) ++ info_hash ++ "Dylan" ++ (replicate 15 $ toEnum 0)
		--hPutStr handle $ "BitTorrent protocol" ++ info_hash ++ info_hash
		hFlush handle
		--line <- hGetLine handle
		line <- mapM hGetChar (replicate 68 $ handle)
		putStr "handshake: "
		putStrLn $ show line
		return (handle, sock)

bitfieldMsg handle bitfield = do
	--let length = 221
	--hPutStr handle $ (to4Byte (1 + length)) ++ [toEnum 5] ++ (replicate length '\x0')
	let msgLength = length bitfield
	hPutStr handle $ (to4Byte (1 + msgLength)) ++ [toEnum 5] ++ bitfield
	putStr "sending: bitfieldMsg - "
	putStrLn $ show bitfield
	hFlush handle

requestMsg handle piece offset length = do
	hPutStr handle $ (to4Byte 13) ++ [toEnum 6] ++ (to4Byte piece) ++ (to4Byte offset) ++ (to4Byte length) -- 2^14
	hFlush handle
	putStrLn "sending: requestMsg"

keepAliveMsg handle = do
	hPutStr handle (to4Byte 0)
	hFlush handle
	putStrLn "sending: keep-alive"

chokeMsg handle = do
	hPutStr handle $ (to4Byte 1) ++ [toEnum 0]
	hFlush handle
	putStrLn "sending: chokeMsg"

unchokeMsg handle = do
	hPutStr handle $ (to4Byte 1) ++ [toEnum 1]
	hFlush handle
	putStrLn "sending: unchokeMsg"

interestedMsg handle = do
	hPutStr handle $ (to4Byte 1) ++ [toEnum 2]
	hFlush handle
	putStrLn "sending: interestedMsg"

notinterestedMsg handle = do
	hPutStr handle $ (to4Byte 1) ++ [toEnum 3]
	hFlush handle
	putStrLn "sending: notinterestedMsg"

haveMsg handle index = do
	hPutStr handle $ (to4Byte 5) ++ [toEnum 4] ++ (to4Byte index)
	hFlush handle
	putStrLn "sending: haveMsg"

--handleMessage_ :: Handle -> Handle -> String -> PieceMap -> IO ()
handleMessage_ handle hFile [] pieceMap = do
	putStrLn "received: keep-alive"
	keepAliveMsg handle

handleMessage_ handle hFile (msg:xs) pieceMap
	| msg == choke = do
		putStr "received: choke - "
		putStrLn xs
	| msg == unchoke = do
		putStr "received: unchoke - "
		putStrLn xs
		startDownload handle pieceMap 10
	| msg == interested = do
		putStr "received: interested - "
		putStrLn xs
	| msg == notInterested = do
		putStr "received: not interested - "
		putStrLn xs
	| msg == have = do
		putStr "received: have - "
		putStrLn $ show xs
	| msg == bitfield = do
		putStr "received: bitfield "
		putStrLn $ show xs
		_bitfield <- withMVar pieceMap $ (return . mkBitfield)
		bitfieldMsg handle _bitfield
		interestedMsg handle
	| msg == request = do
		putStr "received: request - "
		putStrLn xs
	| msg == piece = modifyMVar_ pieceMap $ \_pieceMap -> do
		putStrLn "received: piece------------------------------------"
		let (index, rest) = splitAt 4 xs
		let (begin, block) = splitAt 4 rest
		putStr $ "index: " ++ show (from4Byte index)
		putStr $ " begin: " ++ show (from4Byte begin)
		putStrLn $ " length: " ++ show (length block)
		let Just piece = M.lookup (from4Byte index) _pieceMap
		result <- if ((view pState piece == Main.Done)) || (S.member (Block (from4Byte begin) (length block)) (view (pState . haveBlocks) piece))
			then do
				putStrLn "Stale block being received"
				let unreqBlock = getUnrequestedOrRequestedBlock _pieceMap
				--let Just (newBlock, newPieceNum, newPieceMap) = getUnrequestedBlock _pieceMap
				newPieceMap <- case unreqBlock of
					Just (newBlock, newPieceNum, newPieceMap) -> do
						let newOffset = view bOffset newBlock
						let newLength = view bSize newBlock
						trace ("Now requesting - pn: " ++ (show newPieceNum) ++ " off: " ++ (show newOffset)) (return ())
						requestMsg handle (fromIntegral newPieceNum) newOffset newLength
						return newPieceMap
					_ -> do
						putStrLn "No more pieces to request"
						return _pieceMap
				return newPieceMap
			else do
				let Just firstPiece = M.lookup 0 _pieceMap --M.lookup (from4Byte index) _pieceMap
				let pieceLength = view pLength firstPiece
				--putStrLn $ show block
				hSeek hFile AbsoluteSeek ((from4Byte index)*pieceLength + from4Byte begin)
				--putStrLn $ "Seek to: " ++ (show ((from4Byte index)*pieceLength + from4Byte begin))
				hPutStr hFile block
				hFlush hFile
				--let Just updated = putPieceMap _pieceMap (from4Byte index) (from4Byte begin) block

				-- Check piece if necessary
				let (__pieceMap, needToCheckHash) = receiveBlock _pieceMap (from4Byte index) (Block (from4Byte begin) (length block))
				putStrLn $ "needToCheckHash: " ++ (show needToCheckHash)
				___pieceMap <- if needToCheckHash
					then do
						hSeek hFile AbsoluteSeek ((from4Byte index)*pieceLength)
						let Just thisPiece = M.lookup (from4Byte index) __pieceMap
						pieceContents <- mapM hGetChar $ replicate (pieceSize thisPiece) hFile
						let hash = SHA1.hash $ BE.pack pieceContents
						--putStrLn $ "=======hash: " ++ (BE.unpack $ HU.urlEncode False hash)
						let correctHash = view pDigest thisPiece
						let matches = (BL.toStrict correctHash) == hash
						--putStrLn $ "========= correct: " ++ (show matches)

						if matches
							then do
								haveMsg handle (from4Byte index)
								return $ M.update (\p -> Just $ set pState Main.Done p) (from4Byte index) __pieceMap
							else
								return $ M.update (\p -> Just $ set pState Main.Pending p) (from4Byte index) __pieceMap
						--return __pieceMap
					else
						return __pieceMap

				--putStrLn $ "Percent complete: " ++ (show (100 * (fromIntegral $ bytesComplete ___pieceMap) / (fromIntegral $ bytesTotal ___pieceMap))) ++ "%"
				--putStrLn $ "Percent complete (with unverified): " ++ (show (100 * ((fromIntegral $ bytesComplete ___pieceMap) + (fromIntegral $ bytesUnverified ___pieceMap)) / (fromIntegral $ bytesTotal ___pieceMap))) ++ "%"

				let unreqBlock = getUnrequestedOrRequestedBlock ___pieceMap
				--let Just (newBlock, newPieceNum, newPieceMap) = getUnrequestedBlock _pieceMap
				newPieceMap <- case unreqBlock of
					Just (newBlock, newPieceNum, newPieceMap) -> do
						let newOffset = view bOffset newBlock
						let newLength = view bSize newBlock
						trace ("Now requesting - pn: " ++ (show newPieceNum) ++ " off: " ++ (show newOffset)) (return ())
						requestMsg handle (fromIntegral newPieceNum) newOffset newLength
						return newPieceMap
					_ -> return ___pieceMap
				return newPieceMap
				-- >> do
				--return ()
		putStrLn $ "Percent complete (verified): " ++ (show (100 * (fromIntegral $ bytesComplete result) / (fromIntegral $ bytesTotal result))) ++ "%"
		putStrLn $ "Percent complete (including unverified blocks): " ++ (show (100 * ((fromIntegral $ bytesComplete result) + (fromIntegral $ bytesUnverified result)) / (fromIntegral $ bytesTotal result))) ++ "%"
		return result
	| msg == cancel = do
		putStr "received: cancel - "
		putStrLn xs
	| otherwise = do
		putStrLn $ "received: unknown message type: " ++ show msg
		putStrLn xs

handleMessage handle hFile pieceMap = do
	eof <- hIsEOF handle
	case eof of
		False -> do
			sizeStr <- mapM hGetChar (replicate 4 handle)
			let size = foldl (\x y -> x * 256 + fromEnum y) 0 sizeStr
			msg <- mapM hGetChar (replicate size handle)
			--putStrLn msg
			handleMessage_ handle hFile msg pieceMap
			return True
			--return (size, msg)
		_ -> do
			hFlush hFile
			--hClose hFile
			--putStrLn "=====================================EOF"
			return False


listenWith handle hFile pieceMap masterThread = do
	takeMVar masterThread
	listenWith_ handle hFile pieceMap masterThread

listenWith_ handle hFile pieceMap masterThread = do
	cont <- handleMessage handle hFile pieceMap
	if cont
	then listenWith_ handle hFile pieceMap masterThread
	else do
		putStrLn "========================= listenWith done - Peer disconnected"
		return ()

startDownload :: Handle -> MVar PieceMap -> Int -> IO ()
startDownload handle pieceMap parallelRequests = modifyMVar_ pieceMap $ \_pieceMap -> do
	foldM f (_pieceMap) [1..parallelRequests]
	where
		f _pieceMap _ = do
			--_pieceMap <- pieceMap
			let unreqBlock = getUnrequestedOrRequestedBlock _pieceMap
			newPieceMap <- case unreqBlock of
				Just (newBlock, newPieceNum, newPieceMap) -> do
					let newOffset = view bOffset newBlock
					let newLength = view bSize newBlock
					trace ("pn: " ++ (show newPieceNum) ++ " off: " ++ (show newOffset)) (return ())
					requestMsg handle (fromIntegral newPieceNum) newOffset newLength
					return newPieceMap
				_ -> return _pieceMap
			return newPieceMap

testAddressIndex i = do
	(torrent, url, rsp, trackerResp, peers) <- test
	--torrent <- test
	let Just _pieceMap = mkPieceMap torrent
	pieceMap <- newMVar _pieceMap
	
	let info_hash = SHA1.hash $ BL.toStrict $ bPack $ torrent M.! "info"
	--addrinfos <- getAddrInfo Nothing (Just "localhost") (Just port)
	--let serveraddr = addrinfos !! 3
	putStrLn $ "connectPeer: " ++ (show (peers !! i))
	Right (handle, sock) <- connectPeer (peers !! i) $ BE.unpack info_hash
	hFile <- openBinaryFile "torrent" ReadWriteMode
	putStrLn "fork listen"
	masterThread <- newEmptyMVar
	forkIO $ listenWith handle hFile pieceMap masterThread
	--putStrLn "bitfieldMsg"
	--bitfieldMsg handle
	putStrLn "interestedMsg"
	interestedMsg handle
	--putStrLn "requestMsg"
	--requestMsg handle
	return (handle, pieceMap, torrent, hFile, peers)

testLocalhost port = do
	(torrent, url, rsp, trackerResp, peers) <- test
	--torrent <- test
	let Just _pieceMap = mkPieceMap torrent
	pieceMap <- newMVar _pieceMap
	hFile <- openBinaryFile "torrent" ReadWriteMode
	let info_hash = SHA1.hash $ BL.toStrict $ bPack $ torrent M.! "info"
	addrinfos <- getAddrInfo Nothing (Just "localhost") (Just port)
	let serveraddr = addrinfos !! 3
	putStrLn "connectPeer"
	Right (handle, sock) <- connectPeer serveraddr $ BE.unpack info_hash
	putStrLn "fork listen"
	masterThread <- newEmptyMVar
	forkIO $ listenWith handle hFile pieceMap masterThread
	--putStrLn "bitfieldMsg"
	--bitfieldMsg handle
	putStrLn "interestedMsg"
	interestedMsg handle
	--putStrLn "requestMsg"
	--requestMsg handle
	return (handle, pieceMap, torrent, hFile)

test = do
	torrent <- openTorrent "Interview_Franziska_Heine.ogg.torrent"
	let tracker = BU.toString $ BL.toStrict packed
		where BString packed = torrent M.! "announce"
	let info_hash = BE.unpack $ HU.urlEncode False $ SHA1.hash $ BL.toStrict $ bPack $ torrent M.! "info"
	(url, rsp) <- connectTracker tracker info_hash
	body <- getResponseBody rsp
	let Just (BDict trackerResp) = trackerResponse body
	let Just (BString peers) = M.lookup "peers" trackerResp
	let peersAddrInfo = map peerToAddrInfo $ splitPeers peers
	return (torrent, url, rsp, trackerResp, peersAddrInfo)

preExecTorrent fileName = do
	torrent <- openTorrent fileName
	let Just bInfo = M.lookup "info" torrent
	let BDict info = bInfo
	let tracker = BU.toString $ BL.toStrict packed
		where BString packed = torrent M.! "announce"
	let info_hash = SHA1.hash $ BL.toStrict $ bPack $ bInfo
	let info_url_hash = BE.unpack $ HU.urlEncode False info_hash
	let Just (BString _resultFile) = M.lookup "name" info
	let resultFile = BU.toString $ BL.toStrict _resultFile
	putStrLn $ "Downloading file: " ++ resultFile
	let Just _pieceMap = mkPieceMap torrent
	putStrLn "Checking progress..."
	resumedPieceMap <- withBinaryFile resultFile ReadWriteMode $ \hFile -> resumePieceMap _pieceMap hFile
	putStrLn $ "Current progress: " ++ (show (100 * (fromIntegral $ bytesComplete resumedPieceMap) / (fromIntegral $ bytesTotal resumedPieceMap)))
	(url, rsp) <- connectTracker tracker info_url_hash
	body <- getResponseBody rsp
	let Just (BDict trackerResp) = trackerResponse body
	let Just (BString peers) = M.lookup "peers" trackerResp
	let peersAddrInfo = map peerToAddrInfo $ splitPeers peers
	hFile <- openBinaryFile resultFile ReadWriteMode
	return (torrent, url, rsp, trackerResp, peersAddrInfo, info_hash, resultFile, resumedPieceMap, hFile)

myForkIO io = do
	mvar <- newEmptyMVar
	forkFinally io (\_ -> putMVar mvar ())
	return mvar

parallelExec peers pieceMap info_hash hFile = do
	putStrLn $ "Connecting to " ++ (show $ length peers) ++ " peers"
	pauses <- mapM myF peers
	mapM (takeMVar) pauses
	--forever $ sequence pauses
	where
		myF peer = do
			myForkIO $ do
				thread <- newMVar "test"
				res <- connectPeer peer $ BE.unpack info_hash
				case res of
					Right (handle, sock) -> do
						putStrLn $ "connectPeer: " ++ (show $ addrAddress peer)
						listenWith handle hFile pieceMap thread
						return ()
					Left e -> return ()

execTorrent (fileName:[]) = do
	(torrent, url, rsp, trackerResp, peers, info_hash, resultFile, _pieceMap, hFile) <- preExecTorrent fileName
	pieceMap <- newMVar _pieceMap
	let i = 0
	let info_hash = SHA1.hash $ BL.toStrict $ bPack $ torrent M.! "info"
	parallelExec (take 50 peers) pieceMap info_hash hFile
	{-- uncomment block for non-parallel, single peer
	putStrLn $ "connectPeer: " ++ (show (peers !! i))
	-- TODO: fix below pattern match, for errors
	Right (handle, sock) <- connectPeer (peers !! i) $ BE.unpack info_hash
	putStrLn "fork listen"
	thread1 <- newMVar "test"
	
	listenWith handle hFile pieceMap thread1
	--}
	putStrLn "All peers disconnected"
	--fClose hFile
	return ()
	--putStrLn "bitfieldMsg"
	--bitfieldMsg handle
	--putStrLn "interestedMsg"
	--interestedMsg handle

-- Localhost
execTorrent (fileName:port:[]) = do
	(torrent, url, rsp, trackerResp, peers, info_hash, resultFile, _pieceMap, hFile) <- preExecTorrent fileName
	pieceMap <- newMVar _pieceMap
	addrinfos <- getAddrInfo Nothing (Just "localhost") (Just port)
	let serveraddr = addrinfos !! 3
	putStrLn "connectPeer"
	connectRes <- connectPeer serveraddr $ BE.unpack info_hash
	case connectRes of
		Right (handle, sock) -> do
			putStrLn "fork listen"
			masterThread <- newMVar "test"
			listenWith handle hFile pieceMap masterThread -- forkIO $ 
			putStrLn "All peers disconnected"
		Left err -> putStrLn $ show err
	--putStrLn "bitfieldMsg"
	--bitfieldMsg handle
	--putStrLn "interestedMsg"
	--interestedMsg handle
	--putStrLn "requestMsg"
	--requestMsg handle
	--return (handle, pieceMap, torrent, hFile)
	-- TODO open server handle
	--takeMVar masterThread

execTorrent _ = do
	putStrLn "./torrent-client file.torrent [local peer's port]"
	putStrLn "(Providing [local peer's port] will cause the client to only connect to a local peer)"

main = do
	args <- getArgs
	execTorrent args
