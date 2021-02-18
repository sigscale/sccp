%%% sccp.erl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2021 SigScale Global Inc.
%%% @author Vance Shipley <vances@sigscale.org> [http://www.sigscale.org]
%%% @end
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc This library module implements a public API for the
%%% 	{@link //sccp. sccp} application.
%%%
-module(sccp).
-copyright('Copyright (c) 2021 SigScale Global Inc.').
-author('vances@sigscale.org').

-export([point_code/1, point_code/2]).

-include("sccp.hrl").

-spec point_code(PC) -> PC
	when
		PC :: integer() | string().
%% @equiv point_code(itu, PC)
point_code(PC) ->
	point_code(itu, PC).

-spec point_code(Format, PC) -> PC
	when
		Format :: itu | ansi,
		PC :: binary() | integer() | string().
%% @doc Conversion of common display format of point codes. 
%%
%% 	Formats or parses the representation of point codes
%% 	using either the ITU (3-8-3) or ANSI (8-8-8) display
%% 	format (e.g. ITU point code 2067 = "1-2-3").
%% 	
point_code(itu = _Format, PC) when is_integer(PC) ->
	<<0:2, A:3, B:8, C:3>> = <<PC:16>>,
	lists:flatten(io_lib:fwrite("~b-~b-~b", [A, B, C]));
point_code(ansi, PC) when is_integer(PC) ->
	<< A:8, B:8, C:3>> = <<PC:24>>,
	lists:flatten(io_lib:fwrite("~b-~b-~b", [A, B, C]));
point_code(itu, PC) when is_list(PC) ->
	[A, B, C] = [list_to_integer(C) || C <- string:tokens(PC, [$-])],
	<<I:16>> = <<0:2, A:3, B:8, C:3>>,
	I;
point_code(ansi, PC) when is_list(PC) ->
	[A, B, C] = [list_to_integer(C) || C <- string:tokens(PC, [$-])],
	<<I:24>> = <<A:8, B:8, C:8>>,
	I.

