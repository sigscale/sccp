%%% sccp_codec_SUITE.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2018-2025 SigScale Global Inc.
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
%%%  @doc Test suite for public API of the {@link //ocs. ocs} application.
%%%
-module(sccp_codec_SUITE).
-copyright('Copyright (c) 2018-2025 SigScale Global Inc.').
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
	[party_address_gti_0, party_address_gti_1, party_address_gti_2,
		party_address_gti_3, party_address_gti_4, nai, numbering_plan,
		importance, refusal_cause, release_cause, return_cause,
		segmentation, point_code, global_title, ssn, bcd_even, bcd_odd,
		sccp_connection_req, sccp_connection_confirm,
		sccp_connection_refused, sccp_released, sccp_release_complete,
		sccp_data_form1, sccp_data_form2, sccp_data_ack, sccp_unitdata,
		sccp_unitdata_service, sccp_expedited_data, sccp_expedited_ack,
		sccp_reset_request, sccp_reset_confirmation,
		sccp_protocol_data_unit_error, sccp_inactivity_test,
		sccp_extended_unitdata, sccp_extended_unitdata_service,
		sccp_long_unitdata, sccp_long_unitdata_service,
		ws_cr, ws_cc, ws_df1, ws_ud, ws_rlsd, ws_rlc,
		ws_xudt1, ws_xudt2, ws_xudts1, ws_xudts2, ws_xudts3].

%%---------------------------------------------------------------------
%%  Test cases
%%---------------------------------------------------------------------

party_address_gti_4() ->
	[{userdata, [{doc, "encode and decode called/calling party address
		when global title indicator = 4"}]}].

party_address_gti_4(_Config) ->
	RI = case rand:uniform(2) of
		2 ->
			route_on_ssn;
		1 ->
			route_on_gt
	end,
	PC = rand:uniform(16384) - 1,
	SSN = rand:uniform(255) - 1,
	TT = rand:uniform(254),
	NP = sccp_codec:numbering_plan(rand:uniform(16) - 1),
	NAI = sccp_codec:nai(rand:uniform(5) - 1),
	GT = gen_title(),
	P1 = #party_address{ri = RI, pc = PC, ssn = SSN, translation_type = TT,
			numbering_plan = NP, nai = NAI, gt = GT},
	P2 = sccp_codec:party_address(P1),
	true = is_binary(P2),
	P1 = sccp_codec:party_address(P2).

party_address_gti_3() ->
	[{userdata, [{doc, "encode and decode called/calling party address
		when global title indicator = 3"}]}].

party_address_gti_3(_Config) ->
	RI = case rand:uniform(2) of
		2 ->
			route_on_ssn;
		1 ->
			route_on_gt
	end,
	PC = rand:uniform(16384) - 1,
	SSN = rand:uniform(255) - 1,
	TT = rand:uniform(254),
	NP = sccp_codec:numbering_plan(rand:uniform(16) - 1),
	GT = gen_title(),
	P1 = #party_address{ri = RI, pc = PC, ssn = SSN, translation_type = TT,
			numbering_plan = NP, gt = GT},
	P2 = sccp_codec:party_address(P1),
	true = is_binary(P2),
	P1 = sccp_codec:party_address(P2).

party_address_gti_2() ->
	[{userdata, [{doc, "encode and decode called/calling party address
		when global title indicator = 2"}]}].

party_address_gti_2(_Config) ->
	RI = case rand:uniform(2) of
		2 ->
			route_on_ssn;
		1 ->
			route_on_gt
	end,
	PC = rand:uniform(16384) - 1,
	SSN = rand:uniform(255) - 1,
	TT = rand:uniform(254),
	GT = gen_title(12),
	P1 = #party_address{ri = RI, pc = PC, ssn = SSN, translation_type = TT, gt = GT},
	P2 = sccp_codec:party_address(P1),
	true = is_binary(P2),
	P1 = sccp_codec:party_address(P2).

party_address_gti_1() ->
	[{userdata, [{doc, "encode and decode called/calling party address
		when global title indicator = 1"}]}].

party_address_gti_1(_Config) ->
	RI = case rand:uniform(2) of
		2 ->
			route_on_ssn;
		1 ->
			route_on_gt
	end,
	PC = rand:uniform(16384) - 1,
	SSN = rand:uniform(255) - 1,
	NAI = sccp_codec:nai(rand:uniform(5) - 1),
	GT = gen_title(),
	P1 = #party_address{ri = RI, pc = PC, ssn = SSN, nai = NAI, gt = GT},
	P2 = sccp_codec:party_address(P1),
	true = is_binary(P2),
	P1 = sccp_codec:party_address(P2).

party_address_gti_0() ->
	[{userdata, [{doc, "encode and decode called/calling party address
		when global title indicator = 0"}]}].

party_address_gti_0(_Config) ->
	RI = case rand:uniform(2) of
		2 ->
			route_on_ssn;
		1 ->
			route_on_gt
	end,
	PC = rand:uniform(16384) - 1,
	SSN = rand:uniform(255) - 1,
	P1 = #party_address{ri = RI, pc = PC, ssn = SSN},
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
	F = fun(_F, 256) ->
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
	F = fun(_F, 256) ->
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
	F = fun(_F, 256) ->
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
	SegLocalRef = rand:uniform(16777216) - 1,
	R = #segmentation{first = First, class = C,
			remaining_seg = RemSeg, seg_local_ref = SegLocalRef},
	B = sccp_codec:segmentation(R),
	true = is_binary(B),
	R = sccp_codec:segmentation(B).

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

global_title() ->
	[{userdata, [{doc, "Global title address signals CODEC"}]}].

global_title(_Config) ->
	Digits = [0, 1, 2, 3, 4,5 ,6 ,7, 8, 9],
	NumChars = "0123456789",
	NumChars = sccp_codec:global_title(Digits),
	Digits = sccp_codec:global_title(NumChars).

bcd_even() ->
	[{userdata, [{doc, "encode and decode binary coded decimals"}]}].

bcd_even(_Config) ->
	Dec = [2,4,8,3,2,3,5,5,5,1,2,3,4,1],
	Bin = sccp_codec:bcd(Dec),
	true = is_binary(Bin),
	Dec = sccp_codec:bcd(Bin, 0).

bcd_odd() ->
	[{userdata, [{doc, "encode and decode odd binary coded decimals"}]}].

bcd_odd(_Config) ->
	Dec = [2,4,8,3,2,3,5,5,5,1,2,3,4],
	Bin = sccp_codec:bcd(Dec),
	true = is_binary(Bin),
	Dec = sccp_codec:bcd(Bin, 1).


sccp_connection_req() ->
	[{userdata, [{doc, "encode and decode SCCP connection request message"}]}].

sccp_connection_req(_Config) ->
	SrcLocalRef = rand:uniform(16777216) - 1,
	Class = rand:uniform(4) - 1,
	CalledParty = gen_party_address(),
	Credit = rand:uniform(256) - 1,
	CallingParty = gen_party_address(),
	Data = crypto:strong_rand_bytes(rand:uniform(128) + 2),
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
	SrcLocalRef = rand:uniform(16777216) - 1,
	DestLocalRef = rand:uniform(16777216) - 1,
	Class = rand:uniform(4) - 1,
	Credit = rand:uniform(256) - 1,
	CalledParty = gen_party_address(),
	Data = crypto:strong_rand_bytes(rand:uniform(128) + 2),
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
	DestLocalRef = rand:uniform(16777216) - 1,
	Cause = sccp_codec:refusal_cause(rand:uniform(256) - 1),
	CalledParty = gen_party_address(),
	Data = crypto:strong_rand_bytes(rand:uniform(128) + 2),
	Importance = rand:uniform(5) - 1,
	Rec = #sccp_connection_refused{dest_local_ref = DestLocalRef, refusal_cause = Cause,
			called_party = CalledParty, data = Data, importance = Importance},
	Bin = sccp_codec:sccp(Rec),
	true = is_binary(Bin),
	Rec = sccp_codec:sccp(Bin).

sccp_released() ->
	[{userdata, [{doc, "encode and decode SCCP released message"}]}].

sccp_released(_Config) ->
	SrcLocalRef = rand:uniform(16777216) - 1,
	DestLocalRef = rand:uniform(16777216) - 1,
	Cause = sccp_codec:release_cause(rand:uniform(256) - 1),
	Data = crypto:strong_rand_bytes(rand:uniform(128) + 2),
	Importance = rand:uniform(5) - 1,
	Rec = #sccp_released{dest_local_ref = DestLocalRef, src_local_ref = SrcLocalRef,
			release_cause = Cause, data = Data, importance = Importance},
	Bin = sccp_codec:sccp(Rec),
	true = is_binary(Bin),
	Rec = sccp_codec:sccp(Bin).

sccp_release_complete() ->
	[{userdata, [{doc, "encode and decode SCCP release complete  message"}]}].

sccp_release_complete(_Config) ->
	SrcLocalRef = rand:uniform(16777216) - 1,
	DestLocalRef = rand:uniform(16777216) - 1,
	Rec = #sccp_release_complete{dest_local_ref = DestLocalRef, src_local_ref = SrcLocalRef},
	Bin = sccp_codec:sccp(Rec),
	true = is_binary(Bin),
	Rec = sccp_codec:sccp(Bin).

sccp_data_form1() ->
	[{userdata, [{doc, "encode and decode SCCP data form1 message"}]}].

sccp_data_form1(_Config) ->
	DestLocalRef = rand:uniform(16777216) - 1,
	Seg = case rand:uniform(2) of
		1 ->
			false;
		2 ->
			true
	end,
	Data = crypto:strong_rand_bytes(rand:uniform(255) + 1),
	Rec = #sccp_data_form1{dest_local_ref = DestLocalRef, segmenting = Seg, data = Data},
	Bin = sccp_codec:sccp(Rec),
	true = is_binary(Bin),
	Rec = sccp_codec:sccp(Bin).

sccp_data_form2() ->
	[{userdata, [{doc, "encode and decode SCCP data form2 message"}]}].

sccp_data_form2(_Config) ->
	DestLocalRef = rand:uniform(16777216) - 1,
	Sequencing = #sequencing{send_seq_num = rand:uniform(128) - 1,
			receive_seq_num = rand:uniform(128) - 1,
			more_data = case rand:uniform(2) of
						1 ->
							false;
						2 ->
							true
					end},
	Data = crypto:strong_rand_bytes(rand:uniform(255) + 1),
	Rec = #sccp_data_form2{dest_local_ref = DestLocalRef,
			sequencing = Sequencing, data = Data},
	Bin = sccp_codec:sccp(Rec),
	true = is_binary(Bin),
	Rec = sccp_codec:sccp(Bin).

sccp_data_ack() ->
	[{userdata, [{doc, "encode and decode SCCP data ack message"}]}].

sccp_data_ack(_Config) ->
	DestLocalRef = rand:uniform(16777216) - 1,
	Sequencing = #sequencing{send_seq_num = rand:uniform(128) - 1,
			receive_seq_num = rand:uniform(128) - 1,
			more_data = case rand:uniform(2) of
						1 ->
							false;
						2 ->
							true
					end},
	Credit = rand:uniform(256) - 1,
	Rec = #sccp_data_ack{dest_local_ref = DestLocalRef,
			sequencing = Sequencing, credit = Credit},
	Bin = sccp_codec:sccp(Rec),
	true = is_binary(Bin),
	Rec = sccp_codec:sccp(Bin).

sccp_unitdata() ->
	[{userdata, [{doc, "encode and decode SCCP unit data message"}]}].

sccp_unitdata(_Config) ->
	Class = rand:uniform(4) - 1,
	CalledParty = gen_party_address(),
	CallingParty = gen_party_address(),
	Data = crypto:strong_rand_bytes(rand:uniform(254) + 1),
	Rec = #sccp_unitdata{class = Class, called_party = CalledParty,
			calling_party = CallingParty, data = Data},
	Bin = sccp_codec:sccp(Rec),
	true = is_binary(Bin),
	Rec = sccp_codec:sccp(Bin).

sccp_unitdata_service() ->
	[{userdata, [{doc, "encode and decode SCCP unit data service message"}]}].

sccp_unitdata_service(_Config) ->
	Cause = sccp_codec:return_cause(rand:uniform(256) - 1),
	CalledParty = gen_party_address(),
	CallingParty = gen_party_address(),
	Data = crypto:strong_rand_bytes(rand:uniform(254) + 1),
	Rec = #sccp_unitdata_service{return_cause = Cause, called_party = CalledParty,
			calling_party = CallingParty, data = Data},
	Bin = sccp_codec:sccp(Rec),
	true = is_binary(Bin),
	Rec = sccp_codec:sccp(Bin).

sccp_expedited_data() ->
	[{userdata, [{doc, "encode and decode SCCP expedited data message"}]}].

sccp_expedited_data(_Config) ->
	DestLocalRef = rand:uniform(16777216) - 1,
	Data = crypto:strong_rand_bytes(rand:uniform(32) + 1),
	Rec = #sccp_expedited_data{dest_local_ref = DestLocalRef, data = Data},
	Bin = sccp_codec:sccp(Rec),
	true = is_binary(Bin),
	Rec = sccp_codec:sccp(Bin).

sccp_expedited_ack() ->
	[{userdata, [{doc, "encode and decode SCCP expedited ack message"}]}].

sccp_expedited_ack(_Config) ->
	DestLocalRef = rand:uniform(16777216) - 1,
	Rec = #sccp_expedited_ack{dest_local_ref = DestLocalRef},
	Bin = sccp_codec:sccp(Rec),
	true = is_binary(Bin),
	Rec = sccp_codec:sccp(Bin).

sccp_reset_request() ->
	[{userdata, [{doc, "encode and decode SCCP reset request message"}]}].

sccp_reset_request(_Config) ->
	DestLocalRef = rand:uniform(16777216) - 1,
	SrcLocalRef = rand:uniform(16777216) - 1,
	Cause = sccp_codec:reset_cause(rand:uniform(256) - 1),
	Rec = #sccp_reset_request{dest_local_ref = DestLocalRef, src_local_ref = SrcLocalRef,
			reset_cause = Cause},
	Bin = sccp_codec:sccp(Rec),
	true = is_binary(Bin),
	Rec = sccp_codec:sccp(Bin).

sccp_reset_confirmation() ->
	[{userdata, [{doc, "encode and decode SCCP reset confirmation message"}]}].

sccp_reset_confirmation(_Config) ->
	DestLocalRef = rand:uniform(16777216) - 1,
	SrcLocalRef = rand:uniform(16777216) - 1,
	Rec = #sccp_reset_confirmation{dest_local_ref = DestLocalRef, src_local_ref = SrcLocalRef},
	Bin = sccp_codec:sccp(Rec),
	true = is_binary(Bin),
	Rec = sccp_codec:sccp(Bin).

sccp_protocol_data_unit_error() ->
	[{userdata, [{doc, "encode and decode SCCP protocol data unit error message"}]}].

sccp_protocol_data_unit_error(_Config) ->
	DestLocalRef = rand:uniform(16777216) - 1,
	Error = rand:uniform(256) - 1,
	Rec = #sccp_protocol_data_unit_error{dest_local_ref = DestLocalRef, error_cause = Error},
	Bin = sccp_codec:sccp(Rec),
	true = is_binary(Bin),
	Rec = sccp_codec:sccp(Bin).

sccp_inactivity_test() ->
	[{userdata, [{doc, "encode and decode SCCP inactivity test message"}]}].

sccp_inactivity_test(_Config) ->
	DestLocalRef = rand:uniform(16777216) - 1,
	SrcLocalRef = rand:uniform(16777216) - 1,
	Class = rand:uniform(4) - 1,
	Sequencing = #sequencing{send_seq_num = rand:uniform(128) - 1,
			receive_seq_num = rand:uniform(128) - 1,
			more_data = case rand:uniform(2) of
						1 ->
							false;
						2 ->
							true
					end},
	Credit = rand:uniform(256) - 1,
	Rec = #sccp_inactivity_test{dest_local_ref = DestLocalRef,
			src_local_ref = SrcLocalRef, class = Class,
			sequencing = Sequencing, credit = Credit},
	Bin = sccp_codec:sccp(Rec),
	true = is_binary(Bin),
	Rec = sccp_codec:sccp(Bin).

sccp_extended_unitdata() ->
	[{userdata, [{doc, "encode and decode SCCP extended unit data message"}]}].

sccp_extended_unitdata(_Config) ->
	Class = rand:uniform(4) - 1,
	Hops = rand:uniform(15),
	CalledParty = gen_party_address(),
	CallingParty = gen_party_address(),
	Data = crypto:strong_rand_bytes(rand:uniform(159) + 1),
	Segmentation = #segmentation{first = case rand:uniform(2) of
						1 ->
							false;
						2 ->
							true
					end,
			class = rand:uniform(2) - 1,
			remaining_seg = rand:uniform(16) - 1,
			seg_local_ref = rand:uniform(16777216) - 1},
	Importance = rand:uniform(5) - 1,
	Rec = #sccp_extended_unitdata{class = Class, hop_counter = Hops,
			called_party = CalledParty, calling_party = CallingParty, data = Data,
			segmentation = Segmentation, importance = Importance},
	Bin = sccp_codec:sccp(Rec),
	true = is_binary(Bin),
	Rec = sccp_codec:sccp(Bin).

sccp_extended_unitdata_service() ->
	[{userdata, [{doc, "encode and decode SCCP extended unit data service message"}]}].

sccp_extended_unitdata_service(_Config) ->
	Cause = sccp_codec:return_cause(rand:uniform(256) - 1),
	Hops = rand:uniform(15),
	CalledParty = gen_party_address(),
	CallingParty = gen_party_address(),
	Data = crypto:strong_rand_bytes(rand:uniform(159) + 1),
	Segmentation = #segmentation{first = case rand:uniform(2) of
						1 ->
							false;
						2 ->
							true
					end,
			class = rand:uniform(2) - 1,
			remaining_seg = rand:uniform(16) - 1,
			seg_local_ref = rand:uniform(16777216) - 1},
	Importance = rand:uniform(5) - 1,
	Rec = #sccp_extended_unitdata_service{return_cause = Cause, hop_counter = Hops,
			called_party = CalledParty, calling_party = CallingParty, data = Data,
			segmentation = Segmentation, importance = Importance},
	Bin = sccp_codec:sccp(Rec),
	true = is_binary(Bin),
	Rec = sccp_codec:sccp(Bin).

sccp_long_unitdata() ->
	[{userdata, [{doc, "encode and decode SCCP long unit data service message"}]}].

sccp_long_unitdata(_Config) ->
	Class = rand:uniform(4) - 1,
	Hops = rand:uniform(15),
	CalledParty = gen_party_address(),
	CallingParty = gen_party_address(),
	Data = crypto:strong_rand_bytes(rand:uniform(3952) + 2),
	Segmentation = #segmentation{first = case rand:uniform(2) of
						1 ->
							false;
						2 ->
							true
					end,
			class = rand:uniform(2) - 1,
			remaining_seg = rand:uniform(16) - 1,
			seg_local_ref = rand:uniform(16777216) - 1},
	Importance = rand:uniform(5) - 1,
	Rec = #sccp_long_unitdata{class = Class, hop_counter = Hops,
			called_party = CalledParty, calling_party = CallingParty, data = Data,
			segmentation = Segmentation, importance = Importance},
	Bin = sccp_codec:sccp(Rec),
	true = is_binary(Bin),
	Rec = sccp_codec:sccp(Bin).

sccp_long_unitdata_service() ->
	[{userdata, [{doc, "encode and decode SCCP long unit data service message"}]}].

sccp_long_unitdata_service(_Config) ->
	Cause = sccp_codec:return_cause(rand:uniform(256) - 1),
	Hops = rand:uniform(15),
	CalledParty = gen_party_address(),
	CallingParty = gen_party_address(),
	Data = crypto:strong_rand_bytes(rand:uniform(3952) + 2),
	Segmentation = #segmentation{first = case rand:uniform(2) of
						1 ->
							false;
						2 ->
							true
					end,
			class = rand:uniform(2) - 1,
			remaining_seg = rand:uniform(16) - 1,
			seg_local_ref = rand:uniform(16777216) - 1},
	Importance = rand:uniform(5) - 1,
	Rec = #sccp_long_unitdata_service{return_cause = Cause, hop_counter = Hops,
			called_party = CalledParty, calling_party = CallingParty, data = Data,
			segmentation = Segmentation, importance = Importance},
	Bin = sccp_codec:sccp(Rec),
	true = is_binary(Bin),
	Rec = sccp_codec:sccp(Bin).

ws_cr() ->
	[{userdata, [{doc, "Wireshark sample Connection Request (Mobile Originating Call (AMR))"}]}].

ws_cr(_Config) ->
	Bin = <<1,2,7,32,2,2,7,5,195,142,0,32,0,4,5,195,142,0,16,0,15,71,
			0,19,64,67,0,0,6,0,3,64,1,0,0,15,64,6,0,98,241,16,64,1,0,58,
			64,8,0,98,241,16,64,1,129,149,0,16,64,17,16,6,39,0,3,64,16,
			0,8,25,50,84,118,8,32,0,0,0,79,64,3,32,7,2,0,86,64,5,98,241,
			16,0,1,0>>,
	#sccp_connection_req{src_local_ref = 2098946,
			class = 2,
			called_party = #party_address{ri = route_on_ssn,
					pc = 142, ssn = 32, _ = undefined},
			calling_party = #party_address{ri = route_on_ssn,
					pc = 142, ssn = 16, _ = undefined},
			data = <<0,19,64,67,0,0,6,0,3,64,1,0,0,15,64,6,0,98,241,16,
					64,1,0,58,64,8,0,98,241,16,64,1,129,149,0,16,64,17,16,
					6,39,0,3,64,16,0,8,25,50,84,118,8,32,0,0,0,79,64,3,32,
					7,2,0,86,64,5,98,241,16,0,1>>,
			_ = undefined} = sccp_codec:sccp(Bin).

ws_cc() ->
	[{userdata, [{doc, "Wireshark sample Connection Confirm (UMTS_FP_MAC_RLC_RRC_NBAP)"}]}].

ws_cc(_Config) ->
	Bin = <<2,146,124,48,39,43,24,2,1,3,4,67,192,168,142,0>>,
	#sccp_connection_confirm{dest_local_ref = 3177618,
			src_local_ref = 1583911,
			class = 2,
			called_party = #party_address{ri = route_on_ssn,
					pc = 10432, ssn = 142, _ = undefined},
			_ = undefined} = sccp_codec:sccp(Bin).

ws_df1() ->
	[{userdata, [{doc, "Wireshark sample Data Form 1 (UMTS_FP_MAC_RLC_RRC_NBAP)"}]}].

ws_df1(_Config) ->
	Bin = <<6,146,124,48,0,1,12,0,17,64,8,0,0,1,0,57,64,1,16>>,
	#sccp_data_form1{dest_local_ref = 3177618,
			segmenting = false,
			data = <<0,17,64,8,0,0,1,0,57,64,1,16>>} = sccp_codec:sccp(Bin).

ws_ud() ->
	[{userdata, [{doc, "Wireshark sample Unitdata (UMTS_FP_MAC_RLC_RRC_NBAP)"}]}].

ws_ud(_Config) ->
	Bin = <<9,128,48,2,4,2,66,142,41,0,14,64,37,64,0,3,0,3,
			64,1,0,0,23,64,9,80,36,4,18,100,48,84,50,244,0,22,
			64,1,0,0,0,0,96,64,5,4,244,17,0,2,2,66,142>>,
	#sccp_unitdata{class = 128,
			called_party = #party_address{ri = route_on_ssn,
					ssn = 142, _ = undefined},
			calling_party = #party_address{ri = route_on_ssn,
					ssn = 142, _ = undefined},
			data = <<0,14,64,37,64,0,3,0,3,64,1,0,0,23,64,9,
					80,36,4,18,100,48,84,50,244,0,22,64,1,0,0,
					0,0,96,64,5,4,244,17,0,2>>} = sccp_codec:sccp(Bin).

ws_rlsd() ->
	[{userdata, [{doc, "Wireshark sample Released (UMTS_FP_MAC_RLC_RRC_NBAP)"}]}].

ws_rlsd(_Config) ->
	Bin = <<4,146,124,48,39,43,24,3,0,0>>,
	#sccp_released{dest_local_ref = 3177618,
			src_local_ref = 1583911,
			release_cause = sccp_user_originated,
			_ = undefined} = sccp_codec:sccp(Bin).

ws_rlc() ->
	[{userdata, [{doc, "Wireshark sample Release Complete (Mobile Originating Call (AMR))"}]}].

ws_rlc(_Config) ->
	Bin = <<5,3,6,16,3,6,32>>,
	#sccp_release_complete{dest_local_ref = 1050115,
			src_local_ref = 2098691} = sccp_codec:sccp(Bin).

ws_xudt1() ->
	[{userdata, [{doc, "Wireshark sample Extended Unit Data Segmented (1 of 3) (from Bug 2059)"}]}].

ws_xudt1(_Config) ->
	Bin = <<17,129,4,4,14,16,255,10,18,6,0,18,4,121,82,68,51,34,2,
			66,11,239,98,130,2,115,72,3,18,0,255,107,47,40,45,6,7,0,
			17,134,5,1,1,1,160,34,97,32,128,2,7,128,161,14,6,12,42,134,
			58,0,137,97,51,1,1,1,0,1,162,3,2,1,0,163,5,161,3,2,1,0,108,
			128,161,36,2,1,1,2,1,23,48,28,160,26,48,11,128,1,18,129,1,
			0,162,3,128,1,1,48,11,128,1,17,129,1,0,162,3,128,1,1,161,
			18,2,1,2,2,1,19,48,10,131,0,190,6,130,1,1,132,1,2,161,21,
			2,1,3,2,1,47,48,13,160,11,160,9,160,4,128,2,16,103,129,1,
			127,161,67,2,1,4,2,1,32,48,59,193,7,2,144,80,68,4,0,32,226,
			3,128,1,2,196,7,2,144,80,68,4,0,32,197,2,4,2,160,9,4,7,1,
			16,80,68,4,0,32,164,10,48,8,2,1,5,161,3,2,1,1,158,7,131,19,
			69,20,6,66,6,161,29,2,1,5,2,1,46,48,21,160,14,161,12,129,1,
			0,130,4,0,0,0,0,131,1,0,161,3,128,16,4,194,1,0,0,0>>,
	#sccp_extended_unitdata{class = 129,
			hop_counter = 4,
			called_party = #party_address{ri = route_on_gt,
					translation_type = 0,
					numbering_plan = isdn_tele,
					nai = international,
					gt = [9,7,2,5,4,4,3,3,2,2],
					ssn = 6, _ = undefined},
			calling_party = #party_address{ri = route_on_ssn,
					ssn = 11, _ = undefined},
			data = <<98,130,2,115,72,3,18,0,255,107,47,40,45,6,7,0,
					17,134,5,1,1,1,160,34,97,32,128,2,7,128,161,14,6,12,42,134,
					58,0,137,97,51,1,1,1,0,1,162,3,2,1,0,163,5,161,3,2,1,0,108,
					128,161,36,2,1,1,2,1,23,48,28,160,26,48,11,128,1,18,129,1,
					0,162,3,128,1,1,48,11,128,1,17,129,1,0,162,3,128,1,1,161,
					18,2,1,2,2,1,19,48,10,131,0,190,6,130,1,1,132,1,2,161,21,
					2,1,3,2,1,47,48,13,160,11,160,9,160,4,128,2,16,103,129,1,
					127,161,67,2,1,4,2,1,32,48,59,193,7,2,144,80,68,4,0,32,226,
					3,128,1,2,196,7,2,144,80,68,4,0,32,197,2,4,2,160,9,4,7,1,
					16,80,68,4,0,32,164,10,48,8,2,1,5,161,3,2,1,1,158,7,131,19,
					69,20,6,66,6,161,29,2,1,5,2,1,46,48,21,160,14,161,12,129,1,
					0,130,4,0,0,0,0,131,1,0,161,3,128>>,
			segmentation = #segmentation{first = true,
					class = 1,
					remaining_seg = 2,
					seg_local_ref = 1},
			_ = undefined} = sccp_codec:sccp(Bin).

ws_xudt2() ->
	[{userdata, [{doc, "Wireshark sample Extended Unit Data Segmented (2 of 3)  (from Bug 2059)"}]}].

ws_xudt2(_Config) ->
	Bin = <<17,129,4,4,14,16,255,10,18,6,0,18,4,121,82,68,51,34,2,
			66,11,239,1,2,161,32,2,1,6,2,1,34,4,24,161,3,128,1,2,163,
			17,160,3,128,1,3,164,10,4,8,1,1,16,80,68,4,0,32,161,67,2,
			1,7,2,1,32,48,59,193,7,2,144,80,68,4,0,32,226,3,128,1,3,
			196,7,2,144,80,68,4,0,32,197,2,4,2,160,9,4,7,1,16,80,68,
			97,64,0,164,10,48,8,2,1,5,161,3,2,1,1,158,7,131,19,69,20,
			6,66,6,161,29,2,1,8,2,1,46,48,21,160,14,161,12,129,1,0,
			130,4,0,0,0,0,131,1,0,161,3,128,1,3,161,32,2,1,9,2,1,34,
			4,24,161,3,128,1,3,163,17,160,3,128,1,3,164,10,4,8,1,1,
			16,80,68,4,0,32,161,93,2,1,10,2,1,23,48,85,160,83,48,11,
			128,1,249,129,1,0,162,3,128,1,3,48,11,128,1,13,129,1,0,
			162,3,128,1,3,48,11,128,1,251,129,1,0,162,3,128,1,3,48,
			16,128,1,14,129,1,0,162,3,128,1,3,190,3,129,1,20,16,4,
			65,1,0,0,0>>,
	#sccp_extended_unitdata{class = 129,
			hop_counter = 4,
			called_party = #party_address{ri = route_on_gt,
					translation_type = 0,
					numbering_plan = isdn_tele,
					nai = international,
					gt = [9,7,2,5,4,4,3,3,2,2],
					ssn = 6, _ = undefined},
			calling_party = #party_address{ri = route_on_ssn,
					ssn = 11, _ = undefined},
			data = <<1,2,161,32,2,1,6,2,1,34,4,24,161,3,128,1,2,163,
					17,160,3,128,1,3,164,10,4,8,1,1,16,80,68,4,0,32,161,67,2,
					1,7,2,1,32,48,59,193,7,2,144,80,68,4,0,32,226,3,128,1,3,
					196,7,2,144,80,68,4,0,32,197,2,4,2,160,9,4,7,1,16,80,68,
					97,64,0,164,10,48,8,2,1,5,161,3,2,1,1,158,7,131,19,69,20,
					6,66,6,161,29,2,1,8,2,1,46,48,21,160,14,161,12,129,1,0,
					130,4,0,0,0,0,131,1,0,161,3,128,1,3,161,32,2,1,9,2,1,34,
					4,24,161,3,128,1,3,163,17,160,3,128,1,3,164,10,4,8,1,1,
					16,80,68,4,0,32,161,93,2,1,10,2,1,23,48,85,160,83,48,11,
					128,1,249,129,1,0,162,3,128,1,3,48,11,128,1,13,129,1,0,
					162,3,128,1,3,48,11,128,1,251,129,1,0,162,3,128,1,3,48,
					16,128,1,14,129,1,0,162,3,128,1,3,190,3,129,1,20>>,
			segmentation = #segmentation{first = false,
					class = 1,
					remaining_seg = 1,
					seg_local_ref = 1},
			_ = undefined} = sccp_codec:sccp(Bin).

ws_xudt3() ->
	[{userdata, [{doc, "Wireshark sample Extended Unit Data Segmented (3 of 3) (from Bug 2059)"}]}].

ws_xudt3(_Config) ->
	Bin = <<17,129,4,4,14,16,169,10,18,6,0,18,4,121,82,68,51,34,
			2,66,11,153,48,11,128,1,15,129,1,0,162,3,128,1,3,48,11,
			128,1,250,129,1,1,162,3,128,1,3,161,93,2,1,11,2,1,23,48,
			85,160,83,48,11,128,1,249,129,1,0,162,3,128,1,2,48,11,
			128,1,13,129,1,0,162,3,128,1,2,48,11,128,1,251,129,1,0,
			162,3,128,1,2,48,16,128,1,14,129,1,0,162,3,128,1,2,190,
			3,129,1,20,48,11,128,1,15,129,1,0,162,3,128,1,2,48,11,
			128,1,250,129,1,1,162,3,128,1,2,161,13,2,1,12,2,1,31,
			225,5,161,3,128,1,2,161,13,2,1,13,2,1,31,225,5,161,3,
			128,1,3,0,0,16,4,64,1,0,0,0>>,
	#sccp_extended_unitdata{class = 129,
			hop_counter = 4,
			called_party = #party_address{ri = route_on_gt,
					translation_type = 0,
					numbering_plan = isdn_tele,
					nai = international,
					gt = [9,7,2,5,4,4,3,3,2,2],
					ssn = 6, _ = undefined},
			calling_party = #party_address{ri = route_on_ssn,
					ssn = 11, _ = undefined},
			data = <<48,11,128,1,15,129,1,0,162,3,128,1,3,48,11,
					128,1,250,129,1,1,162,3,128,1,3,161,93,2,1,11,2,1,23,48,
					85,160,83,48,11,128,1,249,129,1,0,162,3,128,1,2,48,11,
					128,1,13,129,1,0,162,3,128,1,2,48,11,128,1,251,129,1,0,
					162,3,128,1,2,48,16,128,1,14,129,1,0,162,3,128,1,2,190,
					3,129,1,20,48,11,128,1,15,129,1,0,162,3,128,1,2,48,11,
					128,1,250,129,1,1,162,3,128,1,2,161,13,2,1,12,2,1,31,
					225,5,161,3,128,1,2,161,13,2,1,13,2,1,31,225,5,161,3,
					128,1,3,0,0>>,
			segmentation = #segmentation{first = false,
					class = 1,
					remaining_seg = 0,
					seg_local_ref = 1},
			_ = undefined} = sccp_codec:sccp(Bin).

ws_xudts1() ->
	[{userdata, [{doc, "Wireshark sample Extended Unit Data Service Segmented (1 of 3) (from Bug 2059)"}]}].

ws_xudts1(_Config) ->
	Bin = <<18,0,3,4,6,10,249,2,66,11,4,67,134,3,0,239,
			98,130,2,115,72,3,18,0,255,107,47,40,45,6,7,0,17,
			134,5,1,1,1,160,34,97,32,128,2,7,128,161,14,6,12,
			42,134,58,0,137,97,51,1,1,1,0,1,162,3,2,1,0,163,
			5,161,3,2,1,0,108,128,161,36,2,1,1,2,1,23,48,28,
			160,26,48,11,128,1,18,129,1,0,162,3,128,1,1,48,11,
			128,1,17,129,1,0,162,3,128,1,1,161,18,2,1,2,2,1,19,
			48,10,131,0,190,6,130,1,1,132,1,2,161,21,2,1,3,2,1,
			47,48,13,160,11,160,9,160,4,128,2,16,103,129,1,127,
			161,67,2,1,4,2,1,32,48,59,193,7,2,144,80,68,4,0,32,
			226,3,128,1,2,196,7,2,144,80,68,4,0,32,197,2,4,2,
			160,9,4,7,1,16,80,68,4,0,32,164,10,48,8,2,1,5,161,
			3,2,1,1,158,7,131,19,69,20,6,66,6,161,29,2,1,5,2,1,
			46,48,21,160,14,161,12,129,1,0,130,4,0,0,0,0,131,1,
			0,161,3,128,16,4,194,2,0,0,0>>,
	#sccp_extended_unitdata_service{return_cause = no_translation,
			hop_counter = 3,
			called_party = #party_address{ri = route_on_ssn,
					ssn = 11, _ = undefined},
			calling_party = #party_address{ri = route_on_ssn,
					pc = 902,
					ssn = 0, _ = undefined},
			data = <<98,130,2,115,72,3,18,0,255,107,47,40,45,6,7,0,17,
					134,5,1,1,1,160,34,97,32,128,2,7,128,161,14,6,12,
					42,134,58,0,137,97,51,1,1,1,0,1,162,3,2,1,0,163,
					5,161,3,2,1,0,108,128,161,36,2,1,1,2,1,23,48,28,
					160,26,48,11,128,1,18,129,1,0,162,3,128,1,1,48,11,
					128,1,17,129,1,0,162,3,128,1,1,161,18,2,1,2,2,1,19,
					48,10,131,0,190,6,130,1,1,132,1,2,161,21,2,1,3,2,1,
					47,48,13,160,11,160,9,160,4,128,2,16,103,129,1,127,
					161,67,2,1,4,2,1,32,48,59,193,7,2,144,80,68,4,0,32,
					226,3,128,1,2,196,7,2,144,80,68,4,0,32,197,2,4,2,
					160,9,4,7,1,16,80,68,4,0,32,164,10,48,8,2,1,5,161,
					3,2,1,1,158,7,131,19,69,20,6,66,6,161,29,2,1,5,2,1,
					46,48,21,160,14,161,12,129,1,0,130,4,0,0,0,0,131,1,
					0,161,3,128>>,
			segmentation = #segmentation{first = true,
					class = 1,
					remaining_seg = 2,
					seg_local_ref = 2},
			_ = undefined} = sccp_codec:sccp(Bin).

ws_xudts2() ->
	[{userdata, [{doc, "Wireshark sample Extended Unit Data Service Segmented (2 of 3) (from Bug 2059)"}]}].

ws_xudts2(_Config) ->
	Bin = <<18,0,3,4,6,10,249,2,66,11,4,67,134,3,0,239,
			1,2,161,32,2,1,6,2,1,34,4,24,161,3,128,
			1,2,163,17,160,3,128,1,3,164,10,4,8,1,1,16,80,68,4,0,32,
			161,67,2,1,7,2,1,32,48,59,193,7,2,144,80,68,4,0,32,226,
			3,128,1,3,196,7,2,144,80,68,4,0,32,197,2,4,2,160,9,4,7,
			1,16,80,68,97,64,0,164,10,48,8,2,1,5,161,3,2,1,1,158,7,
			131,19,69,20,6,66,6,161,29,2,1,8,2,1,46,48,21,160,14,161,
			12,129,1,0,130,4,0,0,0,0,131,1,0,161,3,128,1,3,161,32,2,
			1,9,2,1,34,4,24,161,3,128,1,3,163,17,160,3,128,1,3,164,
			10,4,8,1,1,16,80,68,4,0,32,161,93,2,1,10,2,1,23,48,85,
			160,83,48,11,128,1,249,129,1,0,162,3,128,1,3,48,11,128,
			1,13,129,1,0,162,3,128,1,3,48,11,128,1,251,129,1,0,162,
			3,128,1,3,48,16,128,1,14,129,1,0,162,3,128,1,3,190,3,
			129,1,20,16,4,65,2,0,0,0>>,
	#sccp_extended_unitdata_service{return_cause = no_translation,
			hop_counter = 3,
			called_party = #party_address{ri = route_on_ssn,
					ssn = 11, _ = undefined},
			calling_party = #party_address{ri = route_on_ssn,
					pc = 902,
					ssn = 0, _ = undefined},
			data = <<1,2,161,32,2,1,6,2,1,34,4,24,161,3,128,
					1,2,163,17,160,3,128,1,3,164,10,4,8,1,1,16,80,68,4,0,32,
					161,67,2,1,7,2,1,32,48,59,193,7,2,144,80,68,4,0,32,226,
					3,128,1,3,196,7,2,144,80,68,4,0,32,197,2,4,2,160,9,4,7,
					1,16,80,68,97,64,0,164,10,48,8,2,1,5,161,3,2,1,1,158,7,
					131,19,69,20,6,66,6,161,29,2,1,8,2,1,46,48,21,160,14,161,
					12,129,1,0,130,4,0,0,0,0,131,1,0,161,3,128,1,3,161,32,2,
					1,9,2,1,34,4,24,161,3,128,1,3,163,17,160,3,128,1,3,164,
					10,4,8,1,1,16,80,68,4,0,32,161,93,2,1,10,2,1,23,48,85,
					160,83,48,11,128,1,249,129,1,0,162,3,128,1,3,48,11,128,
					1,13,129,1,0,162,3,128,1,3,48,11,128,1,251,129,1,0,162,
					3,128,1,3,48,16,128,1,14,129,1,0,162,3,128,1,3,190,3,
					129,1,20>>,
			segmentation = #segmentation{first = false,
					class = 1,
					remaining_seg = 1,
					seg_local_ref = 2},
			_ = undefined} = sccp_codec:sccp(Bin).

ws_xudts3() ->
	[{userdata, [{doc, "Wireshark sample Extended Unit Data Service Segmented (3 of 3) (from Bug 2059)"}]}].

ws_xudts3(_Config) ->
	Bin = <<18,0,3,4,6,10,163,2,66,11,4,67,134,3,0,153,
			48,11,128,1,15,129,1,0,162,3,128,1,3,48,11,128,1,250,
			129,1,1,162,3,128,1,3,161,93,2,1,11,2,1,23,48,85,160,
			83,48,11,128,1,249,129,1,0,162,3,128,1,2,48,11,128,1,
			13,129,1,0,162,3,128,1,2,48,11,128,1,251,129,1,0,162,
			3,128,1,2,48,16,128,1,14,129,1,0,162,3,128,1,2,190,3,
			129,1,20,48,11,128,1,15,129,1,0,162,3,128,1,2,48,11,
			128,1,250,129,1,1,162,3,128,1,2,161,13,2,1,12,2,1,31,
			225,5,161,3,128,1,2,161,13,2,1,13,2,1,31,225,5,161,3,
			128,1,3,0,0,16,4,64,2,0,0,0>>,
	#sccp_extended_unitdata_service{return_cause = no_translation,
			hop_counter = 3,
			called_party = #party_address{ri = route_on_ssn,
					ssn = 11, _ = undefined},
			calling_party = #party_address{ri = route_on_ssn,
					pc = 902,
					ssn = 0, _ = undefined},
			data = <<48,11,128,1,15,129,1,0,162,3,128,1,3,48,11,128,1,250,
					129,1,1,162,3,128,1,3,161,93,2,1,11,2,1,23,48,85,160,
					83,48,11,128,1,249,129,1,0,162,3,128,1,2,48,11,128,1,
					13,129,1,0,162,3,128,1,2,48,11,128,1,251,129,1,0,162,
					3,128,1,2,48,16,128,1,14,129,1,0,162,3,128,1,2,190,3,
					129,1,20,48,11,128,1,15,129,1,0,162,3,128,1,2,48,11,
					128,1,250,129,1,1,162,3,128,1,2,161,13,2,1,12,2,1,31,
					225,5,161,3,128,1,2,161,13,2,1,13,2,1,31,225,5,161,3,
					128,1,3,0,0>>,
			segmentation = #segmentation{first = false,
					class = 1,
					remaining_seg = 0,
					seg_local_ref = 2},
			_ = undefined} = sccp_codec:sccp(Bin).

%%---------------------------------------------------------------------
%%  Internal functions
%%---------------------------------------------------------------------

%% @hidden
gen_party_address() ->
	RI = case rand:uniform(2) of
		2 ->
			route_on_ssn;
		1 ->
			route_on_gt
	end,
	PC = rand:uniform(16384) - 1,
	SSN = rand:uniform(255) - 1,
	TT = rand:uniform(254),
	NP = sccp_codec:numbering_plan(rand:uniform(16) - 1),
	GT = gen_title(),
	NAI = sccp_codec:nai(rand:uniform(5) - 1),
	#party_address{ri = RI, pc = PC, ssn = SSN, translation_type = TT,
			numbering_plan = NP, nai = NAI, gt = GT}.

%% @hidden
gen_title() ->
	gen_title(rand:uniform(8) + 7).
%% @hidden
gen_title(N) ->
	gen_title(N, []).
%% @hidden
gen_title(0, Acc) ->
	Acc;
gen_title(N, Acc) ->
	gen_title(N - 1, [rand:uniform(10) - 1 | Acc]).

