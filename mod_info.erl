-module(mod_info).
-export([most_functions/0, total_functions/1, list_functions/1, list_functions/0, unambiguous_functions/0, most_common_function_name/0, ambiguous_functions/0]).

most_functions() ->
    most_functions(code:all_loaded(), {}, 0).

most_functions([{M, _Src}|T], Champion, Max) ->
    case total_functions(M) of
        Total when Total > Max ->
            most_functions(T, M, Max+1);
        Total ->
            most_functions(T, Champion, Max)
    end;
most_functions([], Champion, Max) ->
    {Champion, Max}.
        
total_functions([{module, _}, {exports, L}|T]) ->
    lib_misc:size_of(L);
total_functions(Module) ->
    lib_misc:size_of(list_functions(Module)).

list_functions() ->
    lib_misc:map_joining(code:all_loaded(), fun({M, _}) -> list_functions(M) end).
                                                   
list_functions([{module, _}, {exports, L}|T]) ->
    L;
list_functions(Module) ->
    list_functions(Module:module_info()).

unambiguous_functions() ->
    lib_misc:unique(list_functions()).

most_common_function_name() ->
    most_common_function_name(maps:to_list(lib_misc:count(list_functions())), {void, 0}).

most_common_function_name([{_, N}=Current|T], {_, Max}=Champion) ->
    if N > Max -> most_common_function_name(T, Current);
       true -> most_common_function_name(T, Champion)
    end;
most_common_function_name([], Champion) ->
    Champion.

ambiguous_functions() ->
    lists:filter(
      fun({X, N}) -> N > 1 end,
      maps:to_list(lib_misc:count(list_functions()))
     ).
                                                                       
                                                           
