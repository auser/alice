-module (assets).
-export ([get/1]).

get(["css", Path]) -> {"text/css", read_css(Path)};
get(["js", Path]) -> {"application/javascript", read_js(Path)};
get(_Path) -> {"text/html", "Box"}.

read_css(Name) ->
  {ok, Cwd} = file:get_cwd(),
  {ok, Contents} = file:read_file(Cwd ++ "/web/assets/css/" ++ Name),
  Contents.

read_js(Name) ->
  {ok, Cwd} = file:get_cwd(),
  {ok, Contents} = file:read_file(Cwd ++ "/web/assets/js/" ++ Name),
  Contents.