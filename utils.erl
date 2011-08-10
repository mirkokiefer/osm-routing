-module(utils).

-export([float_to_string/1, deg2rad/1, rad2deg/1, intersection/2]).

float_to_string(Float) ->
  [String] = io_lib:format("~p",[trunc(Float)]),
  String.
  
deg2rad(Deg) -> Deg*math:pi()/180.

rad2deg(Rad) -> Rad*180/math:pi().
  
intersection(L1,L2) -> lists:filter(fun(X) -> lists:member(X,L1) end, L2).