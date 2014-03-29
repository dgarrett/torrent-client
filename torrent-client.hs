import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BU
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BE
import Data.BEncode
import qualified Data.Map as M
import Network.HTTP
import Network.URL
import Crypto.Hash.SHA1 as SHA1
import qualified Network.HTTP.Types.URI as HU
import Control.Applicative

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
														("port", "6881"),
														("uploaded", "0"),
														("downloaded", "0"),
														("left", "0"),
														("compact", "0")
													]) ++ "&info_hash=" ++ info_hash ++ "&peer_id=" ++ info_hash
		Nothing -> tracker
	rsp <- Network.HTTP.simpleHTTP (getRequest (url))
	return (url, rsp)

trackerResponse string =
	bRead $ BL.fromStrict $ BE.pack string

test = do
	torrent <- openTorrent "ubuntu-13.10-desktop-amd64.iso.torrent"
	let tracker = BU.toString $ BL.toStrict packed
		where BString packed = torrent M.! "announce"
	--let info_hash = BU.toString $ SHA1.hash $ BL.toStrict $ bPack $ torrent M.! "info"
	--let info_hash = BU.toString $ HU.urlEncode False $ SHA1.hash $ BU.fromString $ (bShow $ torrent M.! "info") ""
	let info_hash = BE.unpack $ HU.urlEncode False $ SHA1.hash $ BL.toStrict $ bPack $ torrent M.! "info"
	--let info_hash = BE.unpack $ SHA1.hash $ BL.toStrict $ bPack $ torrent M.! "info"
	(url, rsp) <- connectTracker tracker info_hash
	body <- getResponseBody rsp
	let Just (BDict trackerResp) = trackerResponse body
	return (torrent, url, rsp, trackerResp)
