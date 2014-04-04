{-# LANGUAGE ViewPatterns #-}

import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BU
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BE
import Data.BEncode
import Data.Word
import Data.Bits
import qualified Data.Map as M
import qualified Data.Set as S
import Network.HTTP
import Network.URL
import Network.Socket
import Network.BSD
import Crypto.Hash.SHA1 as SHA1
import qualified Network.HTTP.Types.URI as HU
import Control.Applicative
import System.IO
import Control.Exception (finally)
import Control.Concurrent --(forkIO)
import Control.Lens
import Debug.Trace

type BlockSize = Int

data Block = Block { bOffset :: Int
						, bSize :: BlockSize
						} deriving (Eq, Show)

type PieceNum = Integer
type PieceSize = Integer

data PieceInfo = PieceInfo { pOffset :: Integer
								, pLength :: Integer
								, pDigest :: BL.ByteString
								, pState :: PieceState
								} deriving (Eq, Show)

data PieceState = Pending
				| Done
				| InProgress { totalBlocks :: Int
								, haveBlocks :: S.Set Block
								, pendingBlocks :: [Block]
								} deriving (Eq, Show)

type PieceMap = M.Map PieceNum PieceInfo
--type PieceDoneMap = M.Map PieceNum Bool

{--data Torrent = Torrent
	{ pieces :: M.Map PieceNum piece
	}
	--}

createPieceMap :: M.Map String BEncode -> Maybe PieceMap
createPieceMap metainfo = do
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
				where nextPieceLength = if pieceLength < remainingFileLength then pieceLength else remainingFileLength

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

requestMsg handle = do
	hPutStr handle $ (to4Byte 13) ++ [toEnum 6] ++ (to4Byte 0) ++ (to4Byte 0) ++ (to4Byte (2^14))
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

handleMessage_ handle [] = do
	putStrLn "keep alive"

handleMessage_ handle ('\0':xs) = do
	putStrLn "choke"
	putStrLn xs

handleMessage_ handle ('\1':xs) = do
	putStrLn "unchoke"
	putStrLn xs

handleMessage_ handle ('\2':xs) = do
	putStrLn "interested"
	putStrLn xs

handleMessage_ handle ('\3':xs) = do
	putStrLn "not interested"
	putStrLn xs

handleMessage_ handle ('\4':xs) = do
	putStrLn "have"
	putStrLn $ show xs

handleMessage_ handle ('\5':xs) = do
	putStrLn "bitfield"
	putStrLn $ show xs
	putStrLn $ "length: " ++ (show $ length xs)

handleMessage_ handle ('\6':xs) = do
	putStrLn "request"
	putStrLn xs

handleMessage_ handle ('\7':xs) = do
	putStrLn "piece"
	let (index, rest) = splitAt 4 xs
	let (begin, block) = splitAt 4 rest
	putStrLn $ "index: " ++ show index
	putStrLn $ "begin: " ++ show begin
	putStrLn $ show block

handleMessage_ handle ('\8':xs) = do
	putStrLn "cancel"
	putStrLn xs

handleMessage_ handle (msgtype:xs) = do
	putStrLn $ "message type: " ++ show msgtype
	putStrLn xs

handleMessage handle = do
	sizeStr <- mapM hGetChar (replicate 4 handle)
	let size = foldl (\x y -> x * 256 + fromEnum y) 0 sizeStr
	msg <- mapM hGetChar (replicate size handle)
	--putStrLn msg
	handleMessage_ handle msg
	return (size, msg)

listenAt port_ = do
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

listenWith handle = do
	handleMessage handle
	listenWith handle

testLocalhost port = do
	(torrent, url, rsp, trackerResp, peers) <- test
	let info_hash = SHA1.hash $ BL.toStrict $ bPack $ torrent M.! "info"
	addrinfos <- getAddrInfo Nothing (Just "localhost") (Just port)
	let serveraddr = addrinfos !! 3
	putStrLn "connectPeer"
	(handle, sock) <- connectPeer serveraddr $ BE.unpack info_hash
	putStrLn "fork listen"
	forkIO $ listenWith handle
	putStrLn "bitfieldMsg"
	bitfieldMsg handle
	putStrLn "interestedMsg"
	interestedMsg handle
	--putStrLn "requestMsg"
	--requestMsg handle
	return handle

test = do
	torrent <- openTorrent "ubuntu-13.10-desktop-amd64.iso.torrent"
	let tracker = BU.toString $ BL.toStrict packed
		where BString packed = torrent M.! "announce"
	let info_hash = BE.unpack $ HU.urlEncode False $ SHA1.hash $ BL.toStrict $ bPack $ torrent M.! "info"
	(url, rsp) <- connectTracker tracker info_hash
	body <- getResponseBody rsp
	let Just (BDict trackerResp) = trackerResponse body
	let Just (BString peers) = M.lookup "peers" trackerResp
	let peersAddrInfo = map peerToAddrInfo $ splitPeers peers
	return (torrent, url, rsp, trackerResp, peersAddrInfo)
