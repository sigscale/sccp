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
	[party_address_git_0, party_address_git_1, party_address_git_2, party_address_git_3,
		party_address_git_4, nai, translation_type, numbering_plan, encoding_scheme, importance,
		refusal_cause, release_cause, return_cause, segmentation, point_code, ssn, bcd,
		sccp_connection_req, sccp_connection_confirm, sccp_connection_refused, sccp_released].

%%---------------------------------------------------------------------
%%  Test cases
%%---------------------------------------------------------------------

party_address_git_4() ->
	[{userdata, [{doc, "encode and decode called/calling party address
		when global title indicator = 4"}]}].

party_address_git_4(_Config) ->
	RI = case rand:uniform(2) of
		2 ->
			true;
		1 ->
			false
	end,
	PC = rand:uniform(256) - 1,
	SSN = rand:uniform(16384) - 1,
	TT = sccp_codec:tt(rand:uniform(256) - 1),
	NP = sccp_codec:numbering_plan(rand:uniform(16) - 1),
	ES = sccp_codec:encoding_scheme(rand:uniform(16) - 1),
	NAI = sccp_codec:nai(rand:uniform(128) - 1),
	GT = [9, 4, 7, 7, 1, 2, 3, 4, 5, 6, 7],
	P1 = #party_address{ri = RI, pc = PC, ssn = SSN, translation_type = TT,
			numbering_plan = NP, encoding_scheme = ES, nai = NAI, gt = GT},
	P2 = sccp_codec:party_address(P1),
	true = is_binary(P2),
	P1 = sccp_codec:party_address(P2).

party_address_git_3() ->
	[{userdata, [{doc, "encode and decode called/calling party address
		when global title indicator = 3"}]}].

party_address_git_3(_Config) ->
	RI = case rand:uniform(2) of
		2 ->
			true;
		1 ->
			false
	end,
	PC = rand:uniform(256) - 1,
	SSN = rand:uniform(16384) - 1,
	TT = sccp_codec:tt(rand:uniform(256) - 1),
	NP = sccp_codec:numbering_plan(rand:uniform(16) - 1),
	ES = sccp_codec:encoding_scheme(rand:uniform(16) - 1),
	GT = [9, 4, 7, 7, 1, 2, 3, 4, 5, 6, 7],
	P1 = #party_address{ri = RI, pc = PC, ssn = SSN, translation_type = TT,
			numbering_plan = NP, encoding_scheme = ES, gt = GT},
	P2 = sccp_codec:party_address(P1),
	true = is_binary(P2),
	P1 = sccp_codec:party_address(P2).

party_address_git_2() ->
	[{userdata, [{doc, "encode and decode called/calling party address
		when global title indicator = 2"}]}].

party_address_git_2(_Config) ->
	RI = case rand:uniform(2) of
		2 ->
			true;
		1 ->
			false
	end,
	PC = rand:uniform(256) - 1,
	SSN = rand:uniform(16384) - 1,
	TT = sccp_codec:tt(rand:uniform(256) - 1),
	GT = [9, 4, 7, 7, 1, 2, 3, 4, 5, 6, 7],
	P1 = #party_address{ri = RI, pc = PC, ssn = SSN, translation_type = TT, gt = GT},
	P2 = sccp_codec:party_address(P1),
	true = is_binary(P2),
	P1 = sccp_codec:party_address(P2).

party_address_git_1() ->
	[{userdata, [{doc, "encode and decode called/calling party address
		when global title indicator = 1"}]}].

party_address_git_1(_Config) ->
	RI = case rand:uniform(2) of
		2 ->
			true;
		1 ->
			false
	end,
	PC = rand:uniform(256) - 1,
	SSN = rand:uniform(16384) - 1,
	NAI = sccp_codec:nai(rand:uniform(128) - 1),
	GT = [9, 4, 7, 7, 1, 2, 3, 4, 5, 6, 7],
	P1 = #party_address{ri = RI, pc = PC, ssn = SSN, nai = NAI, gt = GT},
	P2 = sccp_codec:party_address(P1),
	true = is_binary(P2),
	P1 = sccp_codec:party_address(P2).

party_address_git_0() ->
	[{userdata, [{doc, "encode and decode called/calling party address
		when global title indicator = 0"}]}].

party_address_git_0(_Config) ->
	RI = case rand:uniform(2) of
		2 ->
			true;
		1 ->
			false
	end,
	P1 = #party_address{ri = RI},
	P2 = sccp_codec:party_address(P1),
	true = is_binary(P2),
	P1 = sccp_codec:party_address(P2).

nai() ->
	[{userdata, [{doc, "encode and decode network address indicator"}]}].

nai(_Config) ->
	F = fun(_, 128) ->
				ok;
		(F, N) ->
			NAI = sccp_codec:nai(N),
			true = is_atom(NAI),
			case sccp_codec:nai(NAI) of
				_M when NAI == spare andalso (N >= 5 andalso N =< 111) ->
					F(F, N+1);
				_M when NAI == reserved_for_national  andalso (N >= 112 andalso N =< 126) ->
					F(F, N+1);
				N ->
					F(F, N+1)
			end
	end,
	ok = F(F, 0).

translation_type() ->
	[{userdata, [{doc, "encode and decode translation type"}]}].

translation_type(_Config) ->
	F = fun(_, 256) ->
				ok;
		(F, N) ->
			TT = sccp_codec:tt(N),
			true = is_atom(TT),
			case sccp_codec:tt(TT) of
				_M when TT == network_specific andalso ((N >= 2 andalso N =< 254)) ->
					F(F, N+1);
				N ->
					F(F, N+1)
			end
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
			case sccp_codec:numbering_plan(NP) of
				M when NP == spare andalso (M >=8 andalso M =< 13) ->
					F(F, N+1);
				N ->
					F(F, N+1)
			end
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
			case sccp_codec:encoding_scheme(Scheme) of
				M when Scheme == spare andalso (M >=4 andalso M =< 14) ->
					F(F, N+1);
				N ->
					F(F, N+1)
			end
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
			case sccp_codec:refusal_cause(RC) of
				M when RC == reserved andalso (M > 19 andalso 255 =< M) ->
					F(F, N+1);
				N ->
					F(F, N+1)
			end
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
			case sccp_codec:release_cause(RC) of
				M when RC == reserved andalso (M > 16 andalso 255 =< M) ->
					F(F, N+1);
				N ->
					F(F, N+1)
			end
	end,
	ok = F(F, 0).

return_cause() ->
	[{userdata, [{doc, "encode and decode return_cause parameter"}]}].

return_cause(_Config) ->
	F = fun(F, 256) ->
				ok;
		(F, N)  ->
			RC = sccp_codec:return_cause(N),
			true = is_atom(RC),
			case sccp_codec:return_cause(RC) of
				M when RC == no_translation andalso (M == 0 orelse M == 1) ->
					F(F, N+1);
				M  when RC == reserved andalso (M >= 15 andalso M =< 255) ->
					F(F, N+1);
				N ->
					F(F, N+1)
			end
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
	F = fun(_, 16384) ->
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

sccp_connection_req() ->
	[{userdata, [{doc, "encode and decode SCCP connection request message"}]}].

sccp_connection_req(_Config) ->
	SrcLocalRef = rand:uniform(256) - 1,
	Class = rand:uniform(5) - 1,
	CalledParty = gen_party_address(),
	Credit = <<123:24>>,
	CallingParty = gen_party_address(),
	Data = <<123:24>>,
	Hops = rand:uniform(15),
	Importance = rand:uniform(5) - 1,
	Rec = #sccp_connection_req{src_local_ref = SrcLocalRef, class = Class,
			called_party = CalledParty, credit = Credit, calling_party = CallingParty,
			data = Data, hop_counter = Hops, importance = Importance},
	Bin = sccp_codec:sccp(Rec),
	true = is_binary(Bin),
	Rec = sccp_codec:sccp(Bin).

sccp_connection_confirm() ->
	[{userdata, [{doc, "encode and decode SCCP connection confirm message"}]}].

sccp_connection_confirm(_Config) ->
	SrcLocalRef = rand:uniform(256) - 1,
	DestLocalRef = rand:uniform(256) - 1,
	Class = rand:uniform(5) - 1,
	Credit = <<123:24>>,
	CalledParty = gen_party_address(),
	Data = <<123:24>>,
	Importance = rand:uniform(5) - 1,
	Rec = #sccp_connection_confirm{dest_local_ref = DestLocalRef, src_local_ref = SrcLocalRef,
			class = Class, called_party = CalledParty, credit = Credit, data = Data,
			importance = Importance},
	Bin = sccp_codec:sccp(Rec),
	true = is_binary(Bin),
	Rec = sccp_codec:sccp(Bin).

sccp_connection_refused() ->
	[{userdata, [{doc, "encode and decode SCCP connection refused message"}]}].

sccp_connection_refused(_Config) ->
	DestLocalRef = rand:uniform(256) - 1,
	Cause = sccp_codec:refusal_cause(rand:uniform(256) - 1),
	CalledParty = gen_party_address(),
	Data = <<123:24>>,
	Importance = rand:uniform(5) - 1,
	Rec = #sccp_connection_refused{dest_local_ref = DestLocalRef, refusal_cause = Cause,
			called_party = CalledParty, data = Data, importance = Importance},
	Bin = sccp_codec:sccp(Rec),
	true = is_binary(Bin),
	Rec = sccp_codec:sccp(Bin).

sccp_released() ->
	[{userdata, [{doc, "encode and decode SCCP released message"}]}].

sccp_released(_Config) ->
	SrcLocalRef = rand:uniform(256) - 1,
	DestLocalRef = rand:uniform(256) - 1,
	Cause = sccp_codec:release_cause(rand:uniform(256) - 1),
	CalledParty = gen_party_address(),
	Data = <<123:24>>,
	Importance = rand:uniform(5) - 1,
	Rec = #sccp_released{dest_local_ref = DestLocalRef, src_local_ref = SrcLocalRef,
			release_cause = Cause, data = Data, importance = Importance},
	Bin = sccp_codec:sccp(Rec),
	true = is_binary(Bin),
	Rec = sccp_codec:sccp(Bin).


%%---------------------------------------------------------------------
%%  Internal functions
%%---------------------------------------------------------------------

%% @hidden
gen_party_address() ->
	RI = case rand:uniform(2) of
		2 ->
			true;
		1 ->
			false
	end,
	PC = rand:uniform(256) - 1,
	SSN = rand:uniform(16384) - 1,
	TT = sccp_codec:tt(rand:uniform(256) - 1),
	NP = sccp_codec:numbering_plan(rand:uniform(16) - 1),
	ES = sccp_codec:encoding_scheme(rand:uniform(16) - 1),
	NAI = sccp_codec:nai(rand:uniform(128) - 1),
	GT = [9, 4, 7, 7, 1, 2, 3, 4, 5, 6, 7],
	#party_address{ri = RI, pc = PC, ssn = SSN, translation_type = TT,
			numbering_plan = NP, encoding_scheme = ES, nai = NAI, gt = GT}.
