-module(main).
-export([start/0, stop/0]).

start() ->
  sound_generate:playMelody(user_case).

stop() ->
  sound_generate:stop().
