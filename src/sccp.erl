%%% sccp.erl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2021-2025 SigScale Global Inc.
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
-copyright('Copyright (c) 2021-2025 SigScale Global Inc.').
-author('vances@sigscale.org').

-export([point_code/1, point_code/2, party_address/1]).
-export([sequence_selection/2]).

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

-spec party_address(Address) -> String
  when
      Address :: sccp_codec:party_address(),
		String :: string().
%% @doc Pretty print sccp party address.
party_address(#party_address{pc = PC, ssn = SSN, gt = undefined})
		when is_integer(PC), is_integer(SSN) ->
	"PC: " ++ point_code(PC) ++ ", " ++ "SSN: " ++ integer_to_list(SSN);
party_address(#party_address{pc = PC, ssn = SSN, gt = GT})
		when is_integer(PC), is_integer(SSN), is_list(GT) ->
	"PC: " ++ point_code(PC) ++ ", " ++"SSN: " ++ integer_to_list(SSN)
		 ++ ", " ++ "GT: " ++ sccp_codec:global_title(GT);
party_address(#party_address{pc = undefined, ssn = SSN, gt = undefined})
		when is_integer(SSN) ->
	"SSN: " ++ integer_to_list(SSN);
party_address(#party_address{pc = undefined, ssn = undefined, gt = GT})
		when is_list(GT) ->
	"GT: " ++ sccp_codec:global_title(GT).

-spec sequence_selection(CallingParty, CalledParty) -> SLS 
	when
		CallingParty :: sccp_codec:party_address(),
		CalledParty :: sccp_codec:party_address(),
		SLS :: byte().
%% @doc Calculate a signaling link selection (SLS) value.
%%
%% 	An SLS is used in the routing label of MTP messages
%% 	to ensure in sequence delivery of network service
%% 	data units (NSDU) related to the same endpoints.
%%
sequence_selection(#party_address{pc = OPC,
				ssn = SSN1, gt = GT1} = _CallingParty,
		#party_address{pc = DPC,
				ssn = SSN2, gt = GT2} = _CalledParty) ->
	sequence_selection1([OPC, SSN1, GT1, DPC, SSN2, GT2], 0).
%% @hidden
sequence_selection1([H | T], Acc) when is_integer(H) ->
	sequence_selection1(T, Acc + H);
sequence_selection1([H | T], Acc) when is_list(H) ->
	sequence_selection1(H ++ T, Acc);
sequence_selection1([_ | T], Acc) ->
	sequence_selection1(T, Acc);
sequence_selection1([], Acc) ->
	Acc rem 255.

