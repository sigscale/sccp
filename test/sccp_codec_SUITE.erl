%%% sccp_codec_SUITE.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2018 SigScale Global Inc.
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
%%%  @doc Test suite for public API of the {@link //ocs. ocs} application.
%%%
-module(sccp_codec_SUITE).
-copyright('Copyright (c) 2018 SigScale Global Inc.').

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
	[translation_type, numbering_plan, encoding_scheme, importance, refusal_cause,
		release_cause, return_cause, segmentation, point_code, ssn, bcd].

%%---------------------------------------------------------------------
%%  Test cases
%%---------------------------------------------------------------------

translation_type() ->
	[{userdata, [{doc, "encode and decode translation type"}]}].

translation_type(_Config) ->
	F = fun(_, 256) ->
				ok;
		(F, N) ->
			TT = sccp_codec:tt(N),
			true = is_atom(TT),
			N = sccp_codec:tt(TT),
			F(F, N+1)
	end,
	ok = F(F, 0).

numbering_plan() ->
	[{userdata, [{doc, "encode and decode numbering plan"}]}].

numbering_plan(_Config) ->
	F = fun(_, 16) ->
				ok;
		(F, N) ->
			NP = sccp_codec:numbering_plan(N),
			true = is_atom(NP),
			N = sccp_codec:encoding_scheme(NP),
			F(F, N+1)
	end,
	ok = F(F, 0).

encoding_scheme() ->
	[{userdata, [{doc, "encode and decode encoding_scheme parameter"}]}].

encoding_scheme(_Config) ->
	F = fun(_, 16) ->
				ok;
		(F, N) ->
			Scheme = sccp_codec:encoding_scheme(N),
			true = is_atom(Scheme),
			N = sccp_codec:encoding_scheme(Scheme),
			F(F, N+1)
	end,
	ok = F(F, 0).

importance() ->
	[{userdata, [{doc, "encode and decode importance parameter"}]}].

importance(_Config) ->
	F = fun(_, 8) ->
				ok;
		(F, N) ->
			Importance = sccp_codec:importance(N),
			1 = size(Importance),
			true = is_binary(Importance),
			<<_:5, N:3/integer>> = Importance,
			F(F, N+1)
	end,
	ok = F(F, 0).


refusal_cause() ->
	[{userdata, [{doc, "encode and decode refusal cause parameter"}]}].

refusal_cause(_Config) ->
	F = fun(F, 256) ->
				ok;
		(F, N) ->
			RC = sccp_codec:refusal_cause(N),
			true = is_atom(RC),
			N = sccp_codec:refusal_cause(RC),
			F(F, N+1)
	end,
	ok = F(F, 0).

release_cause() ->
	[{userdata, [{doc, "encode and decode release cause parameter"}]}].

release_cause(_Config) ->
	F = fun(F, 256) ->
				ok;
		(F, N) ->
			RC = sccp_codec:release_cause(N),
			true = is_atom(RC),
			N = sccp_codec:release_cause(RC),
			F(F, N+1)
	end,
	ok = F(F, 0).

return_cause() ->
	[{userdata, [{doc, "encode and decode return_cause parameter"}]}].

return_cause(_Config) ->
	F = fun(F, 256) ->
				ok;
		(F, N) ->
			RC = sccp_codec:return_cause(N),
			true = is_atom(RC),
			N = sccp_codec:return_cause(RC),
			F(F, N+1)
	end,
	ok = F(F, 0).

segmentation() ->
	[{userdata, [{doc, "encode and decode segmentation parameter"}]}].

segmentation(_Config) ->
	First = case rand:uniform(2) of
		2 ->
			true;
		1 ->
			false
	end,
	C = rand:uniform(2) - 1,
	RemSeg = rand:uniform(16) - 1,
	S1 = #segmentation{first = First, class = C, remaning_seg = RemSeg},
	S2 = sccp_codec:segmentation(S1),
	true = is_binary(S2),
	S2 = sccp_codec:segmentation(S1).

point_code() ->
	[{userdata, [{doc, "encode and decode signalling point code"}]}].

point_code(_Config) ->
	F = fun(_, 256) ->
				ok;
		(F, N) ->
			PC = sccp_codec:point_code(N),
			2 = size(PC),
			true = is_binary(PC),
			N = sccp_codec:point_code(PC),
			F(F, N+1)
	end,
	ok = F(F, 0).

ssn() ->
	[{userdata, [{doc, "encode and decode sub system number"}]}].

ssn(_Config) ->
	F = fun(_, 256) ->
				ok;
		(F, N) ->
			SSN = sccp_codec:ssn(N),
			1 = size(SSN),
			true = is_binary(SSN),
			N = sccp_codec:ssn(SSN),
			F(F, N+1)
	end,
	ok = F(F, 0).

bcd() ->
	[{userdata, [{doc, "encode and decode binary coded decimals"}]}].

bcd(Config) ->
	Dec = [2,4,8,3,2,3,5,5,5,1,2,3,4,1],
	Bin = sccp_codec:bcd(Dec),
	true = is_binary(Bin),
	Dec = sccp_codec:bcd(Bin).

%%---------------------------------------------------------------------
%%  Internal functions
%%---------------------------------------------------------------------

