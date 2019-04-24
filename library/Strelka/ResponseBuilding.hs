module Strelka.ResponseBuilding where

import Strelka.Prelude
import Strelka.Core.Model
import qualified Strelka.ResponseBodyBuilding.Builder as A
import qualified Strelka.Core.ResponseBuilder as B


{- |
A composable abstraction for building an HTTP response.
-}
type Builder =
  B.ResponseBuilder


-- * Headers
-------------------------

{- |
Add a header by name and value.
-}
header :: ByteString -> ByteString -> Builder
header name value =
  B.ResponseBuilder (\(Response status headers body) -> Response status (Header (HeaderName name) (HeaderValue value) : headers) body)

{- |
Add a @Content-type@ header.
-}
contentTypeHeader :: ByteString -> Builder
contentTypeHeader x =
  header "content-type" x

{- |
Add a @Location@ header.
-}
locationHeader :: ByteString -> Builder
locationHeader x =
  header "location" x


-- * Statuses
-------------------------

{- |
Set the status code.
-}
status :: Int -> Builder
status x =
  B.ResponseBuilder (\(Response _ headers body) -> Response (Status x) headers body)

-- ** 2xx Successful Statuses
-------------------------

{- |
Set the status code to @200@. Following is the description of this status.

The request has succeeded. The information returned with the response is dependent on the method used in the request, for example:

GET an entity corresponding to the requested resource is sent in the response;

HEAD the entity-header fields corresponding to the requested resource are sent in the response without any message-body;

POST an entity describing or containing the result of the action;

TRACE an entity containing the request message as received by the end server.
-}
okayStatus :: Builder
okayStatus =
  status 200

-- ** 3xx Redirection Statuses
-------------------------

{- |
Set the status code to @301@. Following is the description of this status.

The requested resource has been assigned a new permanent URI and any future references to this resource SHOULD use one of the returned URIs. Clients with link editing capabilities ought to automatically re-link references to the Request-URI to one or more of the new references returned by the server, where possible. This response is cacheable unless indicated otherwise.

The new permanent URI SHOULD be given by the Location field in the response. Unless the request method was HEAD, the entity of the response SHOULD contain a short hypertext note with a hyperlink to the new URI(s).

If the 301 status code is received in response to a request other than GET or HEAD, the user agent MUST NOT automatically redirect the request unless it can be confirmed by the user, since this might change the conditions under which the request was issued.

      Note: When automatically redirecting a POST request after
      receiving a 301 status code, some existing HTTP/1.0 user agents
      will erroneously change it into a GET request.
-}
movedPermanentlyStatus :: Builder
movedPermanentlyStatus =
  status 301

{- |
Set the status code to @307@. Following is the description of this status.

The request should be repeated with another URI; however, future requests should still use the original URI. In contrast to how 302 was historically implemented, the request method is not allowed to be changed when reissuing the original request. For example, a POST request should be repeated using another POST request.

The requested resource has been assigned a new permanent URI and any future references to this resource SHOULD use one of the returned URIs. Clients with link editing capabilities ought to automatically re-link references to the Request-URI to one or more of the new references returned by the server, where possible. This response is cacheable unless indicated otherwise.
-}
temporaryRedirectStatus :: Builder
temporaryRedirectStatus =
  status 307

-- ** 4xx Client Error Statuses
-------------------------

{- |
Set the status code to @400@. Following is the description of this status.

The request could not be understood by the server due to malformed syntax. The client SHOULD NOT repeat the request without modifications.
-}
badRequestStatus :: Builder
badRequestStatus =
  status 400

{- |
Set the status code to @401@. Following is the description of this status.

The request requires user authentication. The response MUST include a WWW-Authenticate header field (section 14.47) containing a challenge applicable to the requested resource. The client MAY repeat the request with a suitable Authorization header field (section 14.8). If the request already included Authorization credentials, then the 401 response indicates that authorization has been refused for those credentials. If the 401 response contains the same challenge as the prior response, and the user agent has already attempted authentication at least once, then the user SHOULD be presented the entity that was given in the response, since that entity might include relevant diagnostic information. HTTP access authentication is explained in "HTTP Authentication: Basic and Digest Access Authentication".
-}
unauthorizedStatus :: Builder
unauthorizedStatus =
  status 401

{- |
Set the status code to @403@. Following is the description of this status.

The server understood the request, but is refusing to fulfill it. Authorization will not help and the request SHOULD NOT be repeated. If the request method was not HEAD and the server wishes to make public why the request has not been fulfilled, it SHOULD describe the reason for the refusal in the entity. If the server does not wish to make this information available to the client, the status code 404 (Not Found) can be used instead.
-}
forbiddenStatus :: Builder
forbiddenStatus =
  status 403

{- |
Set the status code to @404@. Following is the description of this status.

The server has not found anything matching the Request-URI. No indication is given of whether the condition is temporary or permanent. The 410 (Gone) status code SHOULD be used if the server knows, through some internally configurable mechanism, that an old resource is permanently unavailable and has no forwarding address. This status code is commonly used when the server does not wish to reveal exactly why the request has been refused, or when no other response is applicable.
-}
notFoundStatus :: Builder
notFoundStatus =
  status 404

{- |
Set the status code to @405@. Following is the description of this status.

The method specified in the Request-Line is not allowed for the resource identified by the Request-URI. The response MUST include an Allow header containing a list of valid methods for the requested resource.
-}
methodNotAllowedStatus :: Builder
methodNotAllowedStatus =
  status 405

{- |
Set the status code to @406@. Following is the description of this status.

The resource identified by the request is only capable of generating response entities which have content characteristics not acceptable according to the accept headers sent in the request.

Unless it was a HEAD request, the response SHOULD include an entity containing a list of available entity characteristics and location(s) from which the user or user agent can choose the one most appropriate. The entity format is specified by the media type given in the Content-Type header field. Depending upon the format and the capabilities of the user agent, selection of the most appropriate choice MAY be performed automatically. However, this specification does not define any standard for such automatic selection.

      Note: HTTP/1.1 servers are allowed to return responses which are
      not acceptable according to the accept headers sent in the
      request. In some cases, this may even be preferable to sending a
      406 response. User agents are encouraged to inspect the headers of
      an incoming response to determine if it is acceptable.

If the response could be unacceptable, a user agent SHOULD temporarily stop receipt of more data and query the user for a decision on further actions.
-}
notAcceptableStatus :: Builder
notAcceptableStatus =
  status 406

-- ** 5xx Server Error Statuses
-------------------------

{- |
Set the status code to @500@. Following is the description of this status.

The server encountered an unexpected condition which prevented it from fulfilling the request.
-}
internalErrorStatus :: Builder
internalErrorStatus =
  status 500


-- * Bodies
-------------------------

{- |
Set the body.
-}
body :: A.Builder -> Builder
body (A.Builder x) =
  B.ResponseBuilder (\(Response status headers _) -> Response status headers (OutputStream x)) 

{- |
Add a @Content-type@ header with the value of @text/plain@ and set the body.
-}
text :: A.Builder -> Builder
text x =
  contentTypeHeader "text/plain" <> body x

{- |
Add a @Content-type@ header with the value of @text/html@ and set the body.
-}
html :: A.Builder -> Builder
html x =
  contentTypeHeader "text/html" <> body x

{- |
Add a @Content-type@ header with the value of @application/json@ and set the body.
-}
json :: A.Builder -> Builder
json x =
  contentTypeHeader "application/json" <> body x


-- * Misc
-------------------------

{- |
Set the status code to 401, adding a @WWW-Authenticate@ header with specified Realm.
-}
unauthorized :: ByteString -> Builder
unauthorized realm =
  unauthorizedStatus <> header "WWW-Authenticate" ("Basic realm=\"" <> realm <> "\"")

{- |
Set the status code to 301, adding a @Location@ header with the specified URL.
-}
redirect :: ByteString -> Builder
redirect url =
  movedPermanentlyStatus <> locationHeader url

{- |
Set the status code to 307, adding a @Location@ header with the specified URL.
-}
temporaryRedirect :: ByteString -> Builder
temporaryRedirect url =
  temporaryRedirectStatus <> locationHeader url
