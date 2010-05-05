all_females() ->
  F = fun() ->
      Female = #employee{sex = female, name = '$1', _ = '_'},
      mnesia:select(employee, [{Female, [], ['$1']}])
      end,
  mnesia:transaction(F).