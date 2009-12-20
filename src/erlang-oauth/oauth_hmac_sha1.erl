-module(oauth_hmac_sha1).

-export([signature/3, verify/4]).


signature(BaseString, CS, TS) ->
  Key = oauth_uri:calate("&", [CS, TS]),
  io:format("Key: '~p'~n", [Key]),
  base64:encode_to_string(crypto:sha_mac(Key, BaseString)).

verify(Signature, BaseString, CS, TS) ->
  io:format("BaseString: '~p'~n", [BaseString]),
  ComputedSignature = signature(BaseString, CS, TS),
  io:format("ComputedSignature: '~p'~n", [ComputedSignature]),
  Signature =:= ComputedSignature.
