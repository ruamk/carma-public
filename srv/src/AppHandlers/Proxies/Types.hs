-- |AppHandlers.Proxies.Types
--
-- Types that must be shared with proxies and other parts of program.
--
-- Copyright (C) ...

module AppHandlers.Proxies.Types where

-- |Configuration for POST request proxy.
data PostProxyCfg = PostProxyCfg
     { postProxyHeaders          :: [(String, String)] -- ^ Header name and header value.
     , postProxyURI              :: String             -- ^ URI to redirect requests to.
     }
     deriving Show
