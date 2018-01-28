module StaticServe.ContentType (contentTypeFromPath) where

import Prelude

import Data.Maybe (Maybe(..), maybe)
import Data.StrMap (StrMap, fromFoldable, lookup)
import Data.String (drop)
import Data.Tuple (Tuple(..))
import Node.Path (extname)



type Mime =
  { contentType :: String
  , charset :: Maybe String
  }



contentTypeFromPath :: String -> String
contentTypeFromPath path =
  case lookup (drop 1 $ extname path) mimes of
    Nothing -> ""
    Just mime ->
      mime.contentType <> maybe "" (append "; charset=") mime.charset



mimes :: StrMap Mime
mimes = fromFoldable
  [ Tuple "ez" { contentType: "application/andrew-inset", charset: Nothing }
  , Tuple "aw" { contentType: "application/applixware", charset: Nothing }
  , Tuple "atom" { contentType: "application/atom+xml", charset: Nothing }
  , Tuple "atomcat" { contentType: "application/atomcat+xml", charset: Nothing }
  , Tuple "atomsvc" { contentType: "application/atomsvc+xml", charset: Nothing }
  , Tuple "ccxml" { contentType: "application/ccxml+xml", charset: Nothing }
  , Tuple "cdmia" { contentType: "application/cdmi-capability", charset: Nothing }
  , Tuple "cdmic" { contentType: "application/cdmi-container", charset: Nothing }
  , Tuple "cdmid" { contentType: "application/cdmi-domain", charset: Nothing }
  , Tuple "cdmio" { contentType: "application/cdmi-object", charset: Nothing }
  , Tuple "cdmiq" { contentType: "application/cdmi-queue", charset: Nothing }
  , Tuple "cu" { contentType: "application/cu-seeme", charset: Nothing }
  , Tuple "mpd" { contentType: "application/dash+xml", charset: Nothing }
  , Tuple "davmount" { contentType: "application/davmount+xml", charset: Nothing }
  , Tuple "dbk" { contentType: "application/docbook+xml", charset: Nothing }
  , Tuple "dssc" { contentType: "application/dssc+der", charset: Nothing }
  , Tuple "xdssc" { contentType: "application/dssc+xml", charset: Nothing }
  , Tuple "ecma" { contentType: "application/ecmascript", charset: Nothing }
  , Tuple "emma" { contentType: "application/emma+xml", charset: Nothing }
  , Tuple "epub" { contentType: "application/epub+zip", charset: Nothing }
  , Tuple "exi" { contentType: "application/exi", charset: Nothing }
  , Tuple "pfr" { contentType: "application/font-tdpfr", charset: Nothing }
  , Tuple "woff" { contentType: "application/font-woff", charset: Nothing }
  , Tuple "geojson" { contentType: "application/geo+json", charset: Nothing }
  , Tuple "gml" { contentType: "application/gml+xml", charset: Nothing }
  , Tuple "gpx" { contentType: "application/gpx+xml", charset: Nothing }
  , Tuple "gxf" { contentType: "application/gxf", charset: Nothing }
  , Tuple "gz" { contentType: "application/gzip", charset: Nothing }
  , Tuple "stk" { contentType: "application/hyperstudio", charset: Nothing }
  , Tuple "ink" { contentType: "application/inkml+xml", charset: Nothing }
  , Tuple "inkml" { contentType: "application/inkml+xml", charset: Nothing }
  , Tuple "ipfix" { contentType: "application/ipfix", charset: Nothing }
  , Tuple "jar" { contentType: "application/java-archive", charset: Nothing }
  , Tuple "war" { contentType: "application/java-archive", charset: Nothing }
  , Tuple "ear" { contentType: "application/java-archive", charset: Nothing }
  , Tuple "ser" { contentType: "application/java-serialized-object", charset: Nothing }
  , Tuple "class" { contentType: "application/java-vm", charset: Nothing }
  , Tuple "js" { contentType: "application/javascript", charset: Just "utf-8" }
  , Tuple "mjs" { contentType: "application/javascript", charset: Just "utf-8" }
  , Tuple "json" { contentType: "application/json", charset: Just "utf-8" }
  , Tuple "map" { contentType: "application/json", charset: Just "utf-8" }
  , Tuple "jsonml" { contentType: "application/jsonml+json", charset: Nothing }
  , Tuple "jsonld" { contentType: "application/ld+json", charset: Nothing }
  , Tuple "lostxml" { contentType: "application/lost+xml", charset: Nothing }
  , Tuple "hqx" { contentType: "application/mac-binhex40", charset: Nothing }
  , Tuple "cpt" { contentType: "application/mac-compactpro", charset: Nothing }
  , Tuple "mads" { contentType: "application/mads+xml", charset: Nothing }
  , Tuple "mrc" { contentType: "application/marc", charset: Nothing }
  , Tuple "mrcx" { contentType: "application/marcxml+xml", charset: Nothing }
  , Tuple "ma" { contentType: "application/mathematica", charset: Nothing }
  , Tuple "nb" { contentType: "application/mathematica", charset: Nothing }
  , Tuple "mb" { contentType: "application/mathematica", charset: Nothing }
  , Tuple "mathml" { contentType: "application/mathml+xml", charset: Nothing }
  , Tuple "mbox" { contentType: "application/mbox", charset: Nothing }
  , Tuple "mscml" { contentType: "application/mediaservercontrol+xml", charset: Nothing }
  , Tuple "metalink" { contentType: "application/metalink+xml", charset: Nothing }
  , Tuple "meta4" { contentType: "application/metalink4+xml", charset: Nothing }
  , Tuple "mets" { contentType: "application/mets+xml", charset: Nothing }
  , Tuple "mods" { contentType: "application/mods+xml", charset: Nothing }
  , Tuple "m21" { contentType: "application/mp21", charset: Nothing }
  , Tuple "mp21" { contentType: "application/mp21", charset: Nothing }
  , Tuple "mp4s" { contentType: "application/mp4", charset: Nothing }
  , Tuple "m4p" { contentType: "application/mp4", charset: Nothing }
  , Tuple "doc" { contentType: "application/msword", charset: Nothing }
  , Tuple "dot" { contentType: "application/msword", charset: Nothing }
  , Tuple "mxf" { contentType: "application/mxf", charset: Nothing }
  , Tuple "bin" { contentType: "application/octet-stream", charset: Nothing }
  , Tuple "dms" { contentType: "application/octet-stream", charset: Nothing }
  , Tuple "lrf" { contentType: "application/octet-stream", charset: Nothing }
  , Tuple "mar" { contentType: "application/octet-stream", charset: Nothing }
  , Tuple "so" { contentType: "application/octet-stream", charset: Nothing }
  , Tuple "dist" { contentType: "application/octet-stream", charset: Nothing }
  , Tuple "distz" { contentType: "application/octet-stream", charset: Nothing }
  , Tuple "pkg" { contentType: "application/octet-stream", charset: Nothing }
  , Tuple "bpk" { contentType: "application/octet-stream", charset: Nothing }
  , Tuple "dump" { contentType: "application/octet-stream", charset: Nothing }
  , Tuple "elc" { contentType: "application/octet-stream", charset: Nothing }
  , Tuple "deploy" { contentType: "application/octet-stream", charset: Nothing }
  , Tuple "exe" { contentType: "application/octet-stream", charset: Nothing }
  , Tuple "dll" { contentType: "application/octet-stream", charset: Nothing }
  , Tuple "deb" { contentType: "application/octet-stream", charset: Nothing }
  , Tuple "dmg" { contentType: "application/octet-stream", charset: Nothing }
  , Tuple "iso" { contentType: "application/octet-stream", charset: Nothing }
  , Tuple "img" { contentType: "application/octet-stream", charset: Nothing }
  , Tuple "msi" { contentType: "application/octet-stream", charset: Nothing }
  , Tuple "msp" { contentType: "application/octet-stream", charset: Nothing }
  , Tuple "msm" { contentType: "application/octet-stream", charset: Nothing }
  , Tuple "buffer" { contentType: "application/octet-stream", charset: Nothing }
  , Tuple "oda" { contentType: "application/oda", charset: Nothing }
  , Tuple "opf" { contentType: "application/oebps-package+xml", charset: Nothing }
  , Tuple "ogx" { contentType: "application/ogg", charset: Nothing }
  , Tuple "omdoc" { contentType: "application/omdoc+xml", charset: Nothing }
  , Tuple "onetoc" { contentType: "application/onenote", charset: Nothing }
  , Tuple "onetoc2" { contentType: "application/onenote", charset: Nothing }
  , Tuple "onetmp" { contentType: "application/onenote", charset: Nothing }
  , Tuple "onepkg" { contentType: "application/onenote", charset: Nothing }
  , Tuple "oxps" { contentType: "application/oxps", charset: Nothing }
  , Tuple "xer" { contentType: "application/patch-ops-error+xml", charset: Nothing }
  , Tuple "pdf" { contentType: "application/pdf", charset: Nothing }
  , Tuple "pgp" { contentType: "application/pgp-encrypted", charset: Nothing }
  , Tuple "asc" { contentType: "application/pgp-signature", charset: Nothing }
  , Tuple "sig" { contentType: "application/pgp-signature", charset: Nothing }
  , Tuple "prf" { contentType: "application/pics-rules", charset: Nothing }
  , Tuple "p10" { contentType: "application/pkcs10", charset: Nothing }
  , Tuple "p7m" { contentType: "application/pkcs7-mime", charset: Nothing }
  , Tuple "p7c" { contentType: "application/pkcs7-mime", charset: Nothing }
  , Tuple "p7s" { contentType: "application/pkcs7-signature", charset: Nothing }
  , Tuple "p8" { contentType: "application/pkcs8", charset: Nothing }
  , Tuple "ac" { contentType: "application/pkix-attr-cert", charset: Nothing }
  , Tuple "cer" { contentType: "application/pkix-cert", charset: Nothing }
  , Tuple "crl" { contentType: "application/pkix-crl", charset: Nothing }
  , Tuple "pkipath" { contentType: "application/pkix-pkipath", charset: Nothing }
  , Tuple "pki" { contentType: "application/pkixcmp", charset: Nothing }
  , Tuple "pls" { contentType: "application/pls+xml", charset: Nothing }
  , Tuple "ai" { contentType: "application/postscript", charset: Nothing }
  , Tuple "eps" { contentType: "application/postscript", charset: Nothing }
  , Tuple "ps" { contentType: "application/postscript", charset: Nothing }
  , Tuple "cww" { contentType: "application/prs.cww", charset: Nothing }
  , Tuple "pskcxml" { contentType: "application/pskc+xml", charset: Nothing }
  , Tuple "rdf" { contentType: "application/rdf+xml", charset: Nothing }
  , Tuple "rif" { contentType: "application/reginfo+xml", charset: Nothing }
  , Tuple "rnc" { contentType: "application/relax-ng-compact-syntax", charset: Nothing }
  , Tuple "rl" { contentType: "application/resource-lists+xml", charset: Nothing }
  , Tuple "rld" { contentType: "application/resource-lists-diff+xml", charset: Nothing }
  , Tuple "rs" { contentType: "application/rls-services+xml", charset: Nothing }
  , Tuple "gbr" { contentType: "application/rpki-ghostbusters", charset: Nothing }
  , Tuple "mft" { contentType: "application/rpki-manifest", charset: Nothing }
  , Tuple "roa" { contentType: "application/rpki-roa", charset: Nothing }
  , Tuple "rsd" { contentType: "application/rsd+xml", charset: Nothing }
  , Tuple "rss" { contentType: "application/rss+xml", charset: Nothing }
  , Tuple "rtf" { contentType: "application/rtf", charset: Nothing }
  , Tuple "sbml" { contentType: "application/sbml+xml", charset: Nothing }
  , Tuple "scq" { contentType: "application/scvp-cv-request", charset: Nothing }
  , Tuple "scs" { contentType: "application/scvp-cv-response", charset: Nothing }
  , Tuple "spq" { contentType: "application/scvp-vp-request", charset: Nothing }
  , Tuple "spp" { contentType: "application/scvp-vp-response", charset: Nothing }
  , Tuple "sdp" { contentType: "application/sdp", charset: Nothing }
  , Tuple "setpay" { contentType: "application/set-payment-initiation", charset: Nothing }
  , Tuple "setreg" { contentType: "application/set-registration-initiation", charset: Nothing }
  , Tuple "shf" { contentType: "application/shf+xml", charset: Nothing }
  , Tuple "smi" { contentType: "application/smil+xml", charset: Nothing }
  , Tuple "smil" { contentType: "application/smil+xml", charset: Nothing }
  , Tuple "rq" { contentType: "application/sparql-query", charset: Nothing }
  , Tuple "srx" { contentType: "application/sparql-results+xml", charset: Nothing }
  , Tuple "gram" { contentType: "application/srgs", charset: Nothing }
  , Tuple "grxml" { contentType: "application/srgs+xml", charset: Nothing }
  , Tuple "sru" { contentType: "application/sru+xml", charset: Nothing }
  , Tuple "ssdl" { contentType: "application/ssdl+xml", charset: Nothing }
  , Tuple "ssml" { contentType: "application/ssml+xml", charset: Nothing }
  , Tuple "tei" { contentType: "application/tei+xml", charset: Nothing }
  , Tuple "teicorpus" { contentType: "application/tei+xml", charset: Nothing }
  , Tuple "tfi" { contentType: "application/thraud+xml", charset: Nothing }
  , Tuple "tsd" { contentType: "application/timestamped-data", charset: Nothing }
  , Tuple "vxml" { contentType: "application/voicexml+xml", charset: Nothing }
  , Tuple "wgt" { contentType: "application/widget", charset: Nothing }
  , Tuple "hlp" { contentType: "application/winhlp", charset: Nothing }
  , Tuple "wsdl" { contentType: "application/wsdl+xml", charset: Nothing }
  , Tuple "wspolicy" { contentType: "application/wspolicy+xml", charset: Nothing }
  , Tuple "xaml" { contentType: "application/xaml+xml", charset: Nothing }
  , Tuple "xdf" { contentType: "application/xcap-diff+xml", charset: Nothing }
  , Tuple "xenc" { contentType: "application/xenc+xml", charset: Nothing }
  , Tuple "xhtml" { contentType: "application/xhtml+xml", charset: Nothing }
  , Tuple "xht" { contentType: "application/xhtml+xml", charset: Nothing }
  , Tuple "xml" { contentType: "application/xml", charset: Nothing }
  , Tuple "xsl" { contentType: "application/xml", charset: Nothing }
  , Tuple "xsd" { contentType: "application/xml", charset: Nothing }
  , Tuple "rng" { contentType: "application/xml", charset: Nothing }
  , Tuple "dtd" { contentType: "application/xml-dtd", charset: Nothing }
  , Tuple "xop" { contentType: "application/xop+xml", charset: Nothing }
  , Tuple "xpl" { contentType: "application/xproc+xml", charset: Nothing }
  , Tuple "xslt" { contentType: "application/xslt+xml", charset: Nothing }
  , Tuple "xspf" { contentType: "application/xspf+xml", charset: Nothing }
  , Tuple "mxml" { contentType: "application/xv+xml", charset: Nothing }
  , Tuple "xhvml" { contentType: "application/xv+xml", charset: Nothing }
  , Tuple "xvml" { contentType: "application/xv+xml", charset: Nothing }
  , Tuple "xvm" { contentType: "application/xv+xml", charset: Nothing }
  , Tuple "yang" { contentType: "application/yang", charset: Nothing }
  , Tuple "yin" { contentType: "application/yin+xml", charset: Nothing }
  , Tuple "zip" { contentType: "application/zip", charset: Nothing }
  , Tuple "3gpp" { contentType: "audio/3gpp", charset: Nothing }
  , Tuple "adp" { contentType: "audio/adpcm", charset: Nothing }
  , Tuple "au" { contentType: "audio/basic", charset: Nothing }
  , Tuple "snd" { contentType: "audio/basic", charset: Nothing }
  , Tuple "mid" { contentType: "audio/midi", charset: Nothing }
  , Tuple "midi" { contentType: "audio/midi", charset: Nothing }
  , Tuple "kar" { contentType: "audio/midi", charset: Nothing }
  , Tuple "rmi" { contentType: "audio/midi", charset: Nothing }
  , Tuple "m4a" { contentType: "audio/mp4", charset: Nothing }
  , Tuple "mp4a" { contentType: "audio/mp4", charset: Nothing }
  , Tuple "mpga" { contentType: "audio/mpeg", charset: Nothing }
  , Tuple "mp2" { contentType: "audio/mpeg", charset: Nothing }
  , Tuple "mp2a" { contentType: "audio/mpeg", charset: Nothing }
  , Tuple "mp3" { contentType: "audio/mpeg", charset: Nothing }
  , Tuple "m2a" { contentType: "audio/mpeg", charset: Nothing }
  , Tuple "m3a" { contentType: "audio/mpeg", charset: Nothing }
  , Tuple "oga" { contentType: "audio/ogg", charset: Nothing }
  , Tuple "ogg" { contentType: "audio/ogg", charset: Nothing }
  , Tuple "spx" { contentType: "audio/ogg", charset: Nothing }
  , Tuple "s3m" { contentType: "audio/s3m", charset: Nothing }
  , Tuple "sil" { contentType: "audio/silk", charset: Nothing }
  , Tuple "weba" { contentType: "audio/webm", charset: Nothing }
  , Tuple "xm" { contentType: "audio/xm", charset: Nothing }
  , Tuple "ttc" { contentType: "font/collection", charset: Nothing }
  , Tuple "otf" { contentType: "font/otf", charset: Nothing }
  , Tuple "ttf" { contentType: "font/ttf", charset: Nothing }
  , Tuple "woff" { contentType: "font/woff", charset: Nothing }
  , Tuple "woff2" { contentType: "font/woff2", charset: Nothing }
  , Tuple "ico" { contentType: "image/x-icon", charset: Nothing }
  , Tuple "bmp" { contentType: "image/bmp", charset: Nothing }
  , Tuple "cgm" { contentType: "image/cgm", charset: Nothing }
  , Tuple "g3" { contentType: "image/g3fax", charset: Nothing }
  , Tuple "gif" { contentType: "image/gif", charset: Nothing }
  , Tuple "ief" { contentType: "image/ief", charset: Nothing }
  , Tuple "jp2" { contentType: "image/jp2", charset: Nothing }
  , Tuple "jpg2" { contentType: "image/jp2", charset: Nothing }
  , Tuple "jpeg" { contentType: "image/jpeg", charset: Nothing }
  , Tuple "jpg" { contentType: "image/jpeg", charset: Nothing }
  , Tuple "jpe" { contentType: "image/jpeg", charset: Nothing }
  , Tuple "jpm" { contentType: "image/jpm", charset: Nothing }
  , Tuple "jpx" { contentType: "image/jpx", charset: Nothing }
  , Tuple "jpf" { contentType: "image/jpx", charset: Nothing }
  , Tuple "ktx" { contentType: "image/ktx", charset: Nothing }
  , Tuple "png" { contentType: "image/png", charset: Nothing }
  , Tuple "btif" { contentType: "image/prs.btif", charset: Nothing }
  , Tuple "sgi" { contentType: "image/sgi", charset: Nothing }
  , Tuple "svg" { contentType: "image/svg+xml", charset: Nothing }
  , Tuple "svgz" { contentType: "image/svg+xml", charset: Nothing }
  , Tuple "tiff" { contentType: "image/tiff", charset: Nothing }
  , Tuple "tif" { contentType: "image/tiff", charset: Nothing }
  , Tuple "webp" { contentType: "image/webp", charset: Nothing }
  , Tuple "eml" { contentType: "message/rfc822", charset: Nothing }
  , Tuple "mime" { contentType: "message/rfc822", charset: Nothing }
  , Tuple "gltf" { contentType: "model/gltf+json", charset: Nothing }
  , Tuple "glb" { contentType: "model/gltf-binary", charset: Nothing }
  , Tuple "igs" { contentType: "model/iges", charset: Nothing }
  , Tuple "iges" { contentType: "model/iges", charset: Nothing }
  , Tuple "msh" { contentType: "model/mesh", charset: Nothing }
  , Tuple "mesh" { contentType: "model/mesh", charset: Nothing }
  , Tuple "silo" { contentType: "model/mesh", charset: Nothing }
  , Tuple "wrl" { contentType: "model/vrml", charset: Nothing }
  , Tuple "vrml" { contentType: "model/vrml", charset: Nothing }
  , Tuple "x3db" { contentType: "model/x3d+binary", charset: Nothing }
  , Tuple "x3dbz" { contentType: "model/x3d+binary", charset: Nothing }
  , Tuple "x3dv" { contentType: "model/x3d+vrml", charset: Nothing }
  , Tuple "x3dvz" { contentType: "model/x3d+vrml", charset: Nothing }
  , Tuple "x3d" { contentType: "model/x3d+xml", charset: Nothing }
  , Tuple "x3dz" { contentType: "model/x3d+xml", charset: Nothing }
  , Tuple "appcache" { contentType: "text/cache-manifest", charset: Just "utf-8" }
  , Tuple "manifest" { contentType: "text/cache-manifest", charset: Just "utf-8" }
  , Tuple "ics" { contentType: "text/calendar", charset: Just "utf-8" }
  , Tuple "ifb" { contentType: "text/calendar", charset: Just "utf-8" }
  , Tuple "css" { contentType: "text/css", charset: Just "utf-8" }
  , Tuple "csv" { contentType: "text/csv", charset: Just "utf-8" }
  , Tuple "html" { contentType: "text/html", charset: Just "utf-8" }
  , Tuple "htm" { contentType: "text/html", charset: Just "utf-8" }
  , Tuple "shtml" { contentType: "text/html", charset: Just "utf-8" }
  , Tuple "markdown" { contentType: "text/markdown", charset: Just "utf-8" }
  , Tuple "md" { contentType: "text/markdown", charset: Just "utf-8" }
  , Tuple "mml" { contentType: "text/mathml", charset: Just "utf-8" }
  , Tuple "n3" { contentType: "text/n3", charset: Just "utf-8" }
  , Tuple "txt" { contentType: "text/plain", charset: Just "utf-8" }
  , Tuple "text" { contentType: "text/plain", charset: Just "utf-8" }
  , Tuple "conf" { contentType: "text/plain", charset: Just "utf-8" }
  , Tuple "def" { contentType: "text/plain", charset: Just "utf-8" }
  , Tuple "list" { contentType: "text/plain", charset: Just "utf-8" }
  , Tuple "log" { contentType: "text/plain", charset: Just "utf-8" }
  , Tuple "in" { contentType: "text/plain", charset: Just "utf-8" }
  , Tuple "ini" { contentType: "text/plain", charset: Just "utf-8" }
  , Tuple "dsc" { contentType: "text/prs.lines.tag", charset: Just "utf-8" }
  , Tuple "rtx" { contentType: "text/richtext", charset: Just "utf-8" }
  , Tuple "rtf" { contentType: "text/rtf", charset: Just "utf-8" }
  , Tuple "sgml" { contentType: "text/sgml", charset: Just "utf-8" }
  , Tuple "sgm" { contentType: "text/sgml", charset: Just "utf-8" }
  , Tuple "tsv" { contentType: "text/tab-separated-values", charset: Just "utf-8" }
  , Tuple "t" { contentType: "text/troff", charset: Just "utf-8" }
  , Tuple "tr" { contentType: "text/troff", charset: Just "utf-8" }
  , Tuple "roff" { contentType: "text/troff", charset: Just "utf-8" }
  , Tuple "man" { contentType: "text/troff", charset: Just "utf-8" }
  , Tuple "me" { contentType: "text/troff", charset: Just "utf-8" }
  , Tuple "ms" { contentType: "text/troff", charset: Just "utf-8" }
  , Tuple "ttl" { contentType: "text/turtle", charset: Just "utf-8" }
  , Tuple "uri" { contentType: "text/uri-list", charset: Just "utf-8" }
  , Tuple "uris" { contentType: "text/uri-list", charset: Just "utf-8" }
  , Tuple "urls" { contentType: "text/uri-list", charset: Just "utf-8" }
  , Tuple "vcard" { contentType: "text/vcard", charset: Just "utf-8" }
  , Tuple "xml" { contentType: "text/xml", charset: Just "utf-8" }
  , Tuple "3gp" { contentType: "video/3gpp", charset: Nothing }
  , Tuple "3gpp" { contentType: "video/3gpp", charset: Nothing }
  , Tuple "3g2" { contentType: "video/3gpp2", charset: Nothing }
  , Tuple "h261" { contentType: "video/h261", charset: Nothing }
  , Tuple "h263" { contentType: "video/h263", charset: Nothing }
  , Tuple "h264" { contentType: "video/h264", charset: Nothing }
  , Tuple "jpgv" { contentType: "video/jpeg", charset: Nothing }
  , Tuple "jpm" { contentType: "video/jpm", charset: Nothing }
  , Tuple "jpgm" { contentType: "video/jpm", charset: Nothing }
  , Tuple "mj2" { contentType: "video/mj2", charset: Nothing }
  , Tuple "mjp2" { contentType: "video/mj2", charset: Nothing }
  , Tuple "ts" { contentType: "video/mp2t", charset: Nothing }
  , Tuple "mp4" { contentType: "video/mp4", charset: Nothing }
  , Tuple "mp4v" { contentType: "video/mp4", charset: Nothing }
  , Tuple "mpg4" { contentType: "video/mp4", charset: Nothing }
  , Tuple "mpeg" { contentType: "video/mpeg", charset: Nothing }
  , Tuple "mpg" { contentType: "video/mpeg", charset: Nothing }
  , Tuple "mpe" { contentType: "video/mpeg", charset: Nothing }
  , Tuple "m1v" { contentType: "video/mpeg", charset: Nothing }
  , Tuple "m2v" { contentType: "video/mpeg", charset: Nothing }
  , Tuple "ogv" { contentType: "video/ogg", charset: Nothing }
  , Tuple "qt" { contentType: "video/quicktime", charset: Nothing }
  , Tuple "mov" { contentType: "video/quicktime", charset: Nothing }
  , Tuple "webm" { contentType: "video/webm", charset: Nothing }
  ]
