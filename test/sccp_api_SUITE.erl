%%% sccp_api_SUITE.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2022-2025 SigScale Global Inc.
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
%%%  @doc Test suite for public API of the {@link //sccp. sccp} application.
%%%
-module(sccp_api_SUITE).
-copyright('Copyright 2022-2025 SigScale Global Inc.').
-author('vances@sigscale.org').

%% common_test required callbacks
-export([suite/0, sequences/0, all/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include("sccp.hrl").
-include_lib("common_test/include/ct.hrl").

%%---------------------------------------------------------------------
%%  Test server callback functions
%%---------------------------------------------------------------------

-spec suite() -> DefaultData :: [tuple()].
%% Require variables and set default values for the suite.
%%
suite() ->
	[{userdata, [{doc, "Test suite for public API in SCCP"}]}].

-spec init_per_suite(Config :: [tuple()]) -> Config :: [tuple()].
%% Initialization before the whole suite.
%%
init_per_suite(Config) ->
	Config.

-spec end_per_suite(Config :: [tuple()]) -> any().
%% Cleanup after the whole suite.
%%
end_per_suite(Config) ->
	Config.

-spec init_per_testcase(TestCase :: atom(), Config :: [tuple()]) -> Config :: [tuple()].
%% Initialization before each test case.
%%
init_per_testcase(_TestCase, Config) ->
	Config.

-spec end_per_testcase(TestCase :: atom(), Config :: [tuple()]) -> any().
%% Cleanup after each test case.
%%
end_per_testcase(_TestCase, _Config) ->
	ok.

-spec sequences() -> Sequences :: [{SeqName :: atom(), Testcases :: [atom()]}].
%% Group test cases into a test sequence.
%%
sequences() -> 
	[].

-spec all() -> TestCases :: [Case :: atom()].
%% Returns a list of all test cases in this test suite.
%%
all() -> 
	[party_address_pc_ssn_gt, party_address_gt, party_address_pc_ssn,
			party_address_ssn, sequence_control].


%%---------------------------------------------------------------------
%%  Test cases
%%---------------------------------------------------------------------

party_address_pc_ssn_gt() ->
	[{userdata, [{doc, "Pretty print party address with PC, SSN and GT"}]}].

party_address_pc_ssn_gt(_Config) ->
	Address = #party_address{pc = 4, ssn = 4, gt = [1,2,3]},
	"PC: 0-0-4, SSN: 4, GT: 123" = sccp:party_address(Address).

party_address_gt() ->
	[{userdata, [{doc, "Pretty print party address with GT"}]}].

party_address_gt(_Config) ->
	Address = #party_address{pc = undefined, ssn = undefined, gt = [1,2,3]},
	"GT: 123" = sccp:party_address(Address).

party_address_pc_ssn() ->
	[{userdata, [{doc, "Pretty print party address with PC and SSN"}]}].

party_address_pc_ssn(_Config) ->
	Address = #party_address{pc = 42, ssn = 51, gt = undefined},
	"PC: 0-5-2, SSN: 51" = sccp:party_address(Address).

party_address_ssn() ->
	[{userdata, [{doc, "Pretty print party address with SSN"}]}].

party_address_ssn(_Config) ->
	Address = #party_address{pc = undefined, ssn = 322, gt = undefined},
	"SSN: 322" = sccp:party_address(Address).

sequence_control() ->
	[{userdata, [{doc, "Get hash of party addresses."}]}].

sequence_control(_Config) ->
	CP1 = #party_address{ri = route_on_ssn, ssn = rand:uniform(255)},
	CP2 = #party_address{ri = route_on_ssn, pc = rand:uniform(16383),
			ssn = rand:uniform(255)},
	CP3 = #party_address{ri = route_on_gt, gt = random_gt()},
	CP4 = #party_address{ri = route_on_gt, pc = rand:uniform(16383),
			gt = random_gt()},
	SLS1 = sccp:sequence_selection(CP1, CP2),
	SLS1 = sccp:sequence_selection(CP2, CP1),
	true = SLS1 >= 0,
	true = SLS1 =< 255,
	SLS2 = sccp:sequence_selection(CP3, CP4),
	SLS2 = sccp:sequence_selection(CP4, CP3),
	true = SLS2 >= 0,
	true = SLS2 =< 255.

%%---------------------------------------------------------------------
%%  Internal functions
%%---------------------------------------------------------------------

random_gt() ->
	random_gt(rand:uniform(20)).
random_gt(N) ->
	random_gt(N, []).
random_gt(0, Acc) ->
	Acc;
random_gt(N, Acc) ->
	BCD = rand:uniform(16) - 1,
	random_gt(N - 1, 	[BCD | Acc]).

