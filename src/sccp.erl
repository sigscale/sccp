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
%% 	using either the ITU or ANSI display format,
%%
%% 	<dl>
%% 		<dt>ANSI</dt>
%% 			<dd>A 24 bit value: `<<Network, Cluster, Member>>'
%% 			(e.g. 66051 = `"1-2-3"').</dd>
%% 		<dt>ITU</dt>
%% 			<dd>A 14 bit value: `<<Zone:3, Region:8, SP:3>>'
%% 			(e.g. ITU point code 2067 = `"1-2-3"').</dd>
%% 	</dl>
%% 	
point_code(itu = _Format, PC) when is_integer(PC) ->
	<<0:2, Zone:3, Region:8, SP:3>> = <<PC:16>>,
	lists:flatten(io_lib:fwrite("~b-~b-~b", [Zone, Region, SP]));
point_code(ansi, PC) when is_integer(PC) ->
	<<Network:8, Cluster:8, Member:8>> = <<PC:24>>,
	lists:flatten(io_lib:fwrite("~b-~b-~b", [Network, Cluster, Member]));
point_code(itu, PC) when is_list(PC) ->
	[Zone, Region, SP] = [list_to_integer(C)
			|| C <- string:tokens(PC, [$-])],
	<<I:16>> = <<0:2, Zone:3, Region:8, SP:3>>,
	I;
point_code(ansi, PC) when is_list(PC) ->
	[Network, Cluster, Member] = [list_to_integer(C)
			|| C <- string:tokens(PC, [$-])],
	<<I:24>> = <<Network:8, Cluster:8, Member:8>>,
	I.

