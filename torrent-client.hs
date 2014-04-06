{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns    #-}

import           Control.Applicative
import           Control.Concurrent
import           Control.Exception      (finally)
import           Control.Lens
import           Control.Monad
import           Crypto.Hash.SHA1       as SHA1
import           Data.BEncode
import           Data.Bits
import qualified Data.ByteString        as B
import qualified Data.ByteString.Char8  as BE
import qualified Data.ByteString.Lazy   as BL
import qualified Data.ByteString.UTF8   as BU
import qualified Data.Map               as M
import qualified Data.Set               as S
import           Data.Word
import           Debug.Trace
import           Network.BSD
import           Network.HTTP
import qualified Network.HTTP.Types.URI as HU
import           Network.Socket
import           Network.URL
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

makeLenses ''Block
makeLenses ''PieceInfo
makeLenses ''PieceState

blockSize = 2^14

--type PieceDoneMap = M.Map PieceNum Bool

{--data Torrent = Torrent
	{ pieces :: M.Map PieceNum piece
	}
	--}

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

putPieceMap pieceMap pieceIndex offset block = do
	let updated = M.update replacePiece pieceIndex pieceMap
	return updated
	where replacePiece piece = do
		let piece' = over (pState . requestedBlocks) (S.delete (Block offset $ length block)) piece
		Just $ over (pState . haveBlocks) (S.insert (Block offset $ length block)) piece

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
			let newPiece = over pState (\ps -> if ps == Pending then InProgress (ceiling $ (fromIntegral pieceSize) / (fromIntegral blockSize)) (S.fromList []) (mkBlockSet pieceSize blockSize) (S.fromList []) else ps) piece
			let unreq = view (pState . unrequestedBlocks) newPiece
			(block, newUnrequested) <- S.minView unreq
			let newNewPiece = over (pState . unrequestedBlocks) (\_ -> newUnrequested) newPiece
			return (block, k, newNewPiece)
			of
				Nothing -> findBlock pieceMap keys
				x -> x
			{--if not $ S.null (view (pState . unrequestedBlocks) piece) then (blockToRequest piece, k) else findBlock pieceMap keys
		(block, )
			where
				Just piece = M.lookup k pieceMap
				blockToRequest piece = S.minView $ view (pState. unrequestedBlocks) piece--}

mkBlockSet :: PieceSize -> BlockSize -> S.Set Block
mkBlockSet pieceSize blockSize = _mkBlockSet pieceSize blockSize 0
	where
		_mkBlockSet pieceSize blockSize offset
			| pieceSize > 0 = S.union (S.fromList [Block offset (if blockSize < (fromIntegral pieceSize) then blockSize else (fromIntegral pieceSize))]) (_mkBlockSet (pieceSize - fromIntegral(blockSize)) blockSize (offset + 1))
			| otherwise = S.fromList []
--S.fromList $ map (\offset -> Block offset blockSize) [0..(totalBlocks - 1)]

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
		addr = toWord32 addrBS
		port = toWord16 portBS

connectPeer serveraddr info_hash = do
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
	putStrLn "handshake"
	putStrLn $ show line
	return (handle, sock)

bitfieldMsg handle = do
	let length = 221
	hPutStr handle $ (to4Byte (1 + length)) ++ [toEnum 5] ++ (replicate length '\x0')
	hFlush handle
	putStrLn "bitfieldMsg"

requestMsg handle piece offset length = do
	hPutStr handle $ (to4Byte 13) ++ [toEnum 6] ++ (to4Byte piece) ++ (to4Byte offset) ++ (to4Byte length) -- 2^14
	hFlush handle
	putStrLn "requestMsg waiting response"
	--line <- hGetLine handle
	--line <- mapM hGetChar (take 6 $ repeat handle)
	--(size, msg) <- handleMessage handle
	--mapM handleMessage (take 40 $ repeat handle)
	--return msg

chokeMsg handle = do
	hPutStr handle $ (to4Byte 1) ++ [toEnum 0]
	hFlush handle
	putStrLn "interestedMsg waiting response"

unchokeMsg handle = do
	hPutStr handle $ (to4Byte 1) ++ [toEnum 1]
	hFlush handle
	putStrLn "interestedMsg waiting response"

interestedMsg handle = do
	hPutStr handle $ (to4Byte 1) ++ [toEnum 2]
	hFlush handle
	putStrLn "interestedMsg waiting response"

notinterestedMsg handle = do
	hPutStr handle $ (to4Byte 1) ++ [toEnum 3]
	hFlush handle
	putStrLn "interestedMsg waiting response"

--handleMessage_ :: Handle -> Handle -> String -> PieceMap -> IO ()
handleMessage_ handle hFile [] pieceMap = do
	putStrLn "keep alive"

handleMessage_ handle hFile (msg:xs) pieceMap
	| msg == choke = do
		putStrLn "choke"
		putStrLn xs
	| msg == unchoke = do
		putStrLn "unchoke"
		putStrLn xs
	| msg == interested = do
		putStrLn "interested"
		putStrLn xs
	| msg == notInterested = do
		putStrLn "not interested"
		putStrLn xs
	| msg == have = do
		putStrLn "have"
		putStrLn $ show xs
	| msg == bitfield = do
		putStrLn "bitfield"
		putStrLn $ show xs
		putStrLn $ "length: " ++ (show $ length xs)
	| msg == request = do
		putStrLn "request"
		putStrLn xs
	| msg == piece = modifyMVar pieceMap $ \_pieceMap -> do
		putStrLn "piece"
		let (index, rest) = splitAt 4 xs
		let (begin, block) = splitAt 4 rest
		putStrLn $ "index: " ++ show index
		putStrLn $ "begin: " ++ show begin
		let Just firstPiece = M.lookup 0 _pieceMap --M.lookup (from4Byte index) _pieceMap
		let pieceLength = view pLength firstPiece
		--putStrLn $ show block
		hSeek hFile AbsoluteSeek ((from4Byte index)*pieceLength + from4Byte begin)
		hPutStr hFile block
		hFlush hFile
		--let Just updated = putPieceMap _pieceMap (from4Byte index) (from4Byte begin) block

		let unreqBlock = getUnrequestedBlock _pieceMap
		--let Just (newBlock, newPieceNum, newPieceMap) = getUnrequestedBlock _pieceMap
		newPieceMap <- case unreqBlock of
			Just (newBlock, newPieceNum, newPieceMap) -> do
				let newOffset = view bOffset newBlock
				let newLength = view bSize newBlock
				trace ("pn: " ++ (show newPieceNum) ++ " off: " ++ (show newOffset)) (return ())
				requestMsg handle (fromIntegral newPieceNum) newOffset newLength
				return newPieceMap
			_ -> return _pieceMap
		return (newPieceMap, ())
		-- >> do
		--return ()
	| msg == cancel = do
		putStrLn "cancel"
		putStrLn xs
	| otherwise = do
		putStrLn $ "message type: " ++ show msg
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
			hClose hFile
			putStrLn "=====================================EOF"
			return False

{--listenAt port_ = do
	let port = toEnum port_
	lsock <- socket AF_INET Stream 0
	bindSocket lsock $ SockAddrInet port iNADDR_ANY
--	listenWith lsock
--
--listenWith lsock = do
	listen lsock sOMAXCONN
	loop lsock `finally` sClose lsock
	where
		loop lsock = do
			(sock,SockAddrInet _ _) <- accept lsock
			handle <- socketToHandle sock ReadWriteMode
			--f handle
			handleMessage handle
			--line <- hGetLine handle
			--putStrLn $ show $ BE.pack line
			loop lsock
--}

listenWith handle hFile pieceMap = do
	cont <- handleMessage handle hFile pieceMap
	if cont
	then listenWith handle hFile pieceMap
	else do
		putStrLn "========================= listenWith done"
		return ()

testLocalhost port = do
	--(torrent, url, rsp, trackerResp, peers) <- test
	torrent <- test
	let Just _pieceMap = mkPieceMap torrent
	pieceMap <- newMVar _pieceMap
	hFile <- openBinaryFile "torrent" ReadWriteMode
	let info_hash = SHA1.hash $ BL.toStrict $ bPack $ torrent M.! "info"
	addrinfos <- getAddrInfo Nothing (Just "localhost") (Just port)
	let serveraddr = addrinfos !! 3
	putStrLn "connectPeer"
	(handle, sock) <- connectPeer serveraddr $ BE.unpack info_hash
	putStrLn "fork listen"
	forkIO $ listenWith handle hFile pieceMap
	--putStrLn "bitfieldMsg"
	--bitfieldMsg handle
	putStrLn "interestedMsg"
	interestedMsg handle
	--putStrLn "requestMsg"
	--requestMsg handle
	return (handle, pieceMap, torrent, hFile)

test = do
	torrent <- openTorrent "ubuntu-13.10-desktop-amd64.iso.torrent"
	let tracker = BU.toString $ BL.toStrict packed
		where BString packed = torrent M.! "announce"
	let info_hash = BE.unpack $ HU.urlEncode False $ SHA1.hash $ BL.toStrict $ bPack $ torrent M.! "info"
	{--(url, rsp) <- connectTracker tracker info_hash
	body <- getResponseBody rsp
	let Just (BDict trackerResp) = trackerResponse body
	let Just (BString peers) = M.lookup "peers" trackerResp
	let peersAddrInfo = map peerToAddrInfo $ splitPeers peers--}
	return (torrent)--, url, rsp, trackerResp, peersAddrInfo)
