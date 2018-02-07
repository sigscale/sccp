%%% sccp.erl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2015-2018 SigScale Global Inc.
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
%%% @doc This library module implements encoding and decoding (CODEC)
%%% 	functions for the M3UA protocol in the
%%% 	{@link //sccp. sccp} application.
%%%
%%% @reference <a href="http://www.itu.int/rec/T-REC-Q.713-200103-I/en">
%%%		ITU-T Recommendation Q.713 - SCCP formats and codes</a>
%%%

-module(sccp_codec).

%% SCCP message codec funcion
-export([sccp/1]).
%% SCCP options codec funcion
-export([party_address/1, nai/1, tt/1, numbering_plan/1, encoding_scheme/1,
		routing_indicator/1, importance/1, refusal_cause/1, release_cause/1,
		segmenting/1, return_cause/1, segmentation/1, point_code/1, bcd/1, ssn/1]).
%% functions to deal with SCCP optional part
-export([optional_part/1, get_option/2, set_option/2]).

-export_type([sccp_message/0]).

-include("sccp.hrl").

-type sccp_message() :: #sccp_connection_req{} | #sccp_connection_confirm{}
			| #sccp_connection_refused{} |  #sccp_released{}
			| #sccp_release_complete{} | #sccp_data_form1{} | #sccp_data_form2{}
			| #sccp_data_ack{} | #sccp_unitdata{} | #sccp_unitdata_service{}
			| #sccp_expedited_data{} | #sccp_expedited_ack{} | #sccp_reset_request{}
			| #sccp_reset_confirmation{} |#sccp_protocol_data_unit_error{}
			| #sccp_inactivity_test{} | #sccp_extended_unitdata{}
			| #sccp_extended_unitdata_service{} | #sccp_long_unitdata{}
			| #sccp_long_unitdata_service{}.

-spec sccp(Message) -> Message
	when
		Message :: binary() | sccp_message().
%% @doc codec for SCCP messages
%% ITU-T Recommendation Q.713, SCCP formats and codes
%%
sccp(<<?ConnectRequest, SrcLocalRef:24, Class, CalledPartyP, Rest/binary>>) ->
	CalledPartyL = binary:at(Rest, CalledPartyP -1), 
	CalledPartyB = binary:part(Rest, CalledPartyP, CalledPartyL),
	Address = party_address(CalledPartyB),
	Opts = optional_part(Rest), 
	#sccp_connection_req{src_local_ref = SrcLocalRef,
			class = Class, called_party = Address,
			credit = get_option(?Credit, Opts),
			calling_party  = get_option(?CallingPartyAddress, Opts),
			data = get_option(?DATA, Opts), hop_counter = get_option(?HopCounter, Opts),
			importance = get_option(?Importance, Opts)};
sccp(<<?ConnectionConfirm, DestLocalRef:24, SrcLocalRef:24, Class, Rest/binary>>) ->
	Opts = optional_part(Rest),
	#sccp_connection_confirm{dest_local_ref = DestLocalRef,
			src_local_ref = SrcLocalRef, class = Class,
			credit = get_option(?Credit, Opts),
			called_party = get_option(?CalledPartyAddress, Opts),
			data = get_option(?DATA, Opts),
			importance = get_option(?Importance, Opts)};
sccp(<<?ConnectionRefused, DestLocalRef:24, Refuse, Rest/binary>>) ->
	Opts = optional_part(Rest),
	#sccp_connection_refused{dest_local_ref = DestLocalRef,
			refusal_cause = refusal_cause(Refuse),
			called_party = get_option(?CalledPartyAddress, Opts),
			data = get_option(?DATA, Opts), importance = get_option(?Importance, Opts)};
sccp(<<?Released, DestLocalRef:24, SrcLocalRef:24, Release, Rest/binary>>) ->
	Opts = optional_part(Rest),
	#sccp_released{dest_local_ref = DestLocalRef,
			src_local_ref = SrcLocalRef, release_cause = release_cause(Release),
			data = get_option(?DATA, Opts), importance = get_option(?Importance, Opts)};
sccp(<<?ReleaseComplete, DestLocalRef:24, SrcLocalRef/integer>>) ->
	#sccp_release_complete{dest_local_ref = DestLocalRef,
			src_local_ref = SrcLocalRef};
sccp(<<?DataForm1, DestLocalRef:24, _:7, Seg:1, DataP, Rest/binary>>) ->
	DataL = binary:at(Rest, DataP - 1),
	Data = binary:part(Rest, DataP, DataL),
	#sccp_data_form1{dest_local_ref = DestLocalRef,
			segmenting = segmenting(Seg), data = Data};
sccp(<<?DataForm2, DestLocalRef:24, _:7, Seq:1, DataP, Rest/binary>>) ->
	DataL = binary:at(Rest, DataP - 1),
	Data = binary:part(Rest, DataP, DataL),
	#sccp_data_form2{dest_local_ref = DestLocalRef,
			sequencing = segmenting(Seq), data = Data};
sccp(<<?DataAck, DestLocalRef:24, RecvSeq:8/binary, Credit:8/binary>>) ->
	#sccp_data_ack{dest_local_ref = DestLocalRef,
			receive_seq_num = RecvSeq, credit = Credit};
sccp(<<?UnitData, Class, CalledPartyP, CallingPartyP, DataP, Rest/binary>>) ->
	CalledPartyL = binary:at(Rest, CalledPartyP - 3),
	CallingPartyL = binary:at(Rest, CallingPartyP - 2),
	DataL = binary:at(Rest, DataP - 1),
	CalledPartyB = binary:part(Rest, CalledPartyP - 2, CalledPartyL),
	CallingPartyB = binary:part(Rest, CallingPartyP - 1, CallingPartyL),
	Data = binary:part(Rest, DataP, DataL),
	#sccp_unitdata{class = Class,
			called_party = party_address(CalledPartyB),
			calling_party = party_address(CallingPartyB), data = Data};
sccp(<<?UnitDataService, RC/integer, CalledPartyP, CallingPartyP, DataP, Rest/binary>>) ->
	Return = return_cause(RC),
	CalledPartyL = binary:at(Rest, CalledPartyP - 3),
	CallingPartyL = binary:at(Rest, CallingPartyP - 2),
	DataL = binary:at(Rest, DataP - 1),
	CalledPartyB = binary:part(Rest, CalledPartyP - 2, CalledPartyL),
	CallingPartyB = binary:part(Rest, CallingPartyP - 1, CallingPartyL),
	Data = binary:part(Rest, DataP, DataL),
	#sccp_unitdata_service{return_cause = Return,
			called_party = party_address(CalledPartyB),
			calling_party = party_address(CallingPartyB), data = Data};
sccp(<<?ExpeditedData, DestLocalRef:24, DataP, Rest/binary>>) ->
	DataL = binary:at(Rest, DataP - 1),
	Data = binary:part(Rest, DataP, DataL),
	#sccp_expedited_data{dest_local_ref = DestLocalRef, data = Data };
sccp(<<?ExpeditedDataAck, DestLocalRef:24/integer>>) ->
	#sccp_expedited_ack{dest_local_ref = DestLocalRef};
sccp(<<?ResetRequest, DestLocalRef:24, SrcLocalRef:24, Reset/integer>>) ->
	#sccp_reset_request{dest_local_ref = DestLocalRef,
			src_local_ref = SrcLocalRef, reset_cause = Reset};
sccp(<<?ResetConfirmation, DestLocalRef:24, SrcLocalRef/integer>>) ->
	#sccp_reset_confirmation{dest_local_ref = DestLocalRef,
			src_local_ref = SrcLocalRef};
sccp(<<?ProtocolDataUnitError, DestLocalRef:24, Error/integer>>) ->
	#sccp_protocol_data_unit_error{dest_local_ref = DestLocalRef,
			error_cause =  Error};
sccp(<<?InactivityTest, DestLocalRef:24, SrcLocalRef:24, Class, _:15, Seq:1,
		Credit/binary>>) ->
	#sccp_inactivity_test{dest_local_ref = DestLocalRef,
			src_local_ref = SrcLocalRef, class = Class, sequencing = segmenting(Seq),
			credit = Credit};
sccp(<<?ExtendedUnitData, Class, Hops, CalledPartyP, CallingPartyP, DataP,
		Rest/binary>>) ->
	CalledPartyL = binary:at(Rest, CalledPartyP - 3),
	CallingPartyL = binary:at(Rest, CallingPartyP - 2),
	DataL = binary:at(Rest, DataP - 1),
	CalledPartyB = binary:part(Rest, CalledPartyP - 2, CalledPartyL),
	CallingPartyB = binary:part(Rest, CallingPartyP - 1, CallingPartyL),
	Data = binary:part(Rest, DataP, DataL),
	Opts = optional_part(Rest),
	#sccp_extended_unitdata{class = Class, hop_counter = Hops,
			called_party = party_address(CalledPartyB),
			calling_party = party_address(CallingPartyB), data = Data,
			segmentation = get_option(?Segmentation, Opts),
			importance = get_option(?Importance, Opts)};
sccp(<<?ExtendedUnitDataService, RC, Hops, CalledPartyP, CallingPartyP, DataP,
		Rest/binary>>) ->
	Return = return_cause(RC),
	CalledPartyL = binary:at(Rest, CalledPartyP - 3),
	CallingPartyL = binary:at(Rest, CallingPartyP - 2),
	DataL = binary:at(Rest, DataP - 1),
	CalledPartyB = binary:part(Rest, CalledPartyP - 2, CalledPartyL),
	CallingPartyB = binary:part(Rest, CallingPartyP - 1, CallingPartyL),
	Data = binary:part(Rest, DataP, DataL),
	Opts = optional_part(Rest),
	#sccp_extended_unitdata_service{return_cause = Return, hop_counter = Hops,
			called_party = party_address(CalledPartyB),
			calling_party = party_address(CallingPartyB), data = Data,
			segmentation = get_option(?Segmentation, Opts),
			importance = get_option(?Importance, Opts)};
sccp(<<?LongUnitData, Class, Hops, CalledPartyP, CallingPartyP, LongDataP,
		Rest/binary>>) ->
	CalledPartyL = binary:at(Rest, CalledPartyP - 3),
	CallingPartyL = binary:at(Rest, CallingPartyP - 2),
	LongDataL = binary:at(Rest, LongDataP - 1),
	CalledPartyB = binary:part(Rest, CalledPartyP - 2, CalledPartyL),
	CallingPartyB = binary:part(Rest, CallingPartyP - 1, CallingPartyL),
	LongData = binary:part(Rest, LongDataP, LongDataL),
	Opts = optional_part(Rest),
	#sccp_long_unitdata{class = Class, hop_counter = Hops,
			called_party = party_address(CalledPartyB),
			calling_party = party_address(CallingPartyB), 
			long_data = LongData, segmentation = get_option(?Segmentation, Opts),
			importance = get_option(?Importance, Opts)};
sccp(<<?LongUnitDataService, Class, Hops, CalledPartyP, CallingPartyP, LongDataP, 
		Rest/binary>>) ->
	CalledPartyL = binary:at(Rest, CalledPartyP - 3),
	CallingPartyL = binary:at(Rest, CallingPartyP - 1),
	LongDataL = binary:at(Rest, LongDataP - 1),
	CalledPartyB = binary:part(Rest, CalledPartyP - 2, CalledPartyL),
	CallingPartyB = binary:part(Rest, CallingPartyP - 1, CallingPartyL),
	LongData = binary:part(Rest, LongDataP, LongDataL),
	Opts = optional_part(Rest),
	#sccp_long_unitdata{class = Class, hop_counter = Hops,
			called_party = party_address(CalledPartyB),
			calling_party = party_address(CallingPartyB), 
			long_data = LongData, segmentation = get_option(?Segmentation, Opts),
			importance = get_option(?Importance, Opts)};
sccp(#sccp_connection_req{src_local_ref = Src, class = Class,
		called_party = CalledParty} = S) ->
	CalledPartyB = party_address(CalledParty),
	Credit = set_option(?Credit, S#sccp_connection_req.credit),
	CallingParty= set_option(?CallingPartyAddress, S#sccp_connection_req.calling_party),
	Data = set_option(?DATA, S#sccp_connection_req.data),
	Hops = set_option(?HopCounter, S#sccp_connection_req.hop_counter),
	Importance = set_option(?Importance, importance(S#sccp_connection_req.importance)),
	<<?ConnectRequest, Src:24, Class, CalledPartyB/binary, Credit/binary,
			CallingParty/binary, Data/binary, Hops/binary, Importance/binary>>;
sccp(#sccp_connection_confirm{dest_local_ref = Dest, src_local_ref = Src,
		class = Class} = S) ->
	Credit = set_option(?Credit, S#sccp_connection_confirm.credit),
	CalledParty= set_option(?CalledPartyAddress, S#sccp_connection_confirm.called_party),
	Data = set_option(?DATA, S#sccp_connection_confirm.data),
	Importance = set_option(?Importance, importance(S#sccp_connection_confirm.importance)),
	<<?ConnectionConfirm, Dest:24, Src:24, Class/integer, Credit/binary,
			CalledParty/binary, Data/binary, Importance/binary>>;
sccp(#sccp_connection_refused{dest_local_ref = Dest, refusal_cause = RC} = S) ->
	Refuse = refusal_cause(RC),
	CalledParty= set_option(?CalledPartyAddress, S#sccp_connection_refused.called_party),
	Data = set_option(?DATA, S#sccp_connection_refused.data),
	Importance = set_option(?Importance, importance(S#sccp_connection_refused.importance)),
	<<?ConnectionRefused, Dest:24, Refuse, CalledParty/binary, Data/binary,
			Importance/binary>>;
sccp(#sccp_released{dest_local_ref = Dest, src_local_ref = Src,
		release_cause = RC} = S) ->
	Release = release_cause(RC),
	Data = set_option(?DATA, S#sccp_released.data),
	Importance = set_option(?Importance, importance(S#sccp_released.importance)),
	<<?Released, Dest:24, Src:24, Release, Data/binary, Importance/binary>>;
sccp(#sccp_release_complete{dest_local_ref = Dest, src_local_ref = Src} = _S) ->
	<<?ReleaseComplete, Dest:24, Src:24>>;
sccp(#sccp_data_form1{dest_local_ref = Dest, segmenting = S, data = Data}) ->
	Seg = segmenting(S),
	<<?DataForm1, Dest:24, Seg, Data/binary>>;
sccp(#sccp_data_form2{dest_local_ref = Dest, sequencing = S, data = Data}) ->
	Seq = segmenting(S),
	<<?DataForm1, Dest:24, Seq, Data/binary>>;
sccp(#sccp_data_ack{dest_local_ref = Dest, receive_seq_num = Seq, credit = Credit}) ->
	<<?DataAck, Dest:24, Seq/binary, Credit/binary>>;
sccp(#sccp_unitdata{class = Class, called_party = CalledParty,
		calling_party = CallingParty, data = Data}) ->
	CalledPartyB = party_address(CalledParty),
	CallingPartyB = party_address(CallingParty),
	<<?UnitData, Class, CalledPartyB/binary, CallingPartyB/binary, Data/binary>>;
sccp(#sccp_unitdata_service{return_cause = RC, called_party = CalledParty,
		calling_party = CallingParty, data = Data}) ->
	Return = return_cause(RC),
	CalledPartyB = party_address(CalledParty),
	CallingPartyB = party_address(CallingParty),
	<<?UnitDataService, Return/integer, CalledPartyB/binary, CallingPartyB/binary,
			Data/binary>>;
sccp(#sccp_expedited_data{dest_local_ref  = Dest, data = Data}) ->
	<<?ExpeditedData, Dest:24, Data/binary>>;
sccp(#sccp_expedited_ack{dest_local_ref = Dest}) ->
	<<?ExpeditedDataAck, Dest/integer>>;
sccp(#sccp_reset_request{dest_local_ref = Dest, src_local_ref = Src, reset_cause = Cause}) ->
	<<?ResetRequest, Dest:24, Src:24, Cause/binary>>;
sccp(#sccp_reset_confirmation{dest_local_ref = Dest, src_local_ref = Src}) ->
	<<?ResetConfirmation, Dest:24, Src:24/integer>>;
sccp(#sccp_protocol_data_unit_error{dest_local_ref = Dest, error_cause = Error}) ->
	<<?ProtocolDataUnitError, Dest:24, Error/integer>>;
sccp(#sccp_inactivity_test{dest_local_ref = Dest, src_local_ref = Src, class = Class,
		sequencing = S, credit = Credit}) ->
	Seq = segmenting(S),
	<<?InactivityTest, Dest:24, Src:24, Class, 0:15, Seq/integer, Credit/binary>>;
sccp(#sccp_extended_unitdata{class = Class, hop_counter = Hops,
		called_party = CalledParty, calling_party = CallingParty, data = Data} = S) ->
	Segs = set_option(?Segmentation, S#sccp_extended_unitdata.segmentation),
	Imp = set_option(?Importance, importance(S#sccp_extended_unitdata.importance)),
	CalledPartyB = party_address(CalledParty),
	CallingPartyB = party_address(CallingParty),
	<<?ExtendedUnitData, Class, Hops, CalledPartyB/binary, CallingPartyB/binary,
			Data/binary, Segs:48/binary, Imp:24/binary >>;
sccp(#sccp_extended_unitdata_service{return_cause = RC, hop_counter = Hops,
		called_party = CalledParty, calling_party = CallingParty, data = Data} = S) ->
	Cause = return_cause(RC),
	Segs = set_option(?Segmentation, S#sccp_extended_unitdata_service.segmentation),
	Imp = set_option(?Importance, importance(S#sccp_extended_unitdata_service.importance)),
	CalledPartyB = party_address(CalledParty),
	CallingPartyB = party_address(CallingParty),
	<<?ExtendedUnitData, Cause/integer, Hops, CalledPartyB/binary, CallingPartyB/binary,
			Data/binary, Segs:48/binary, Imp:24/binary >>;
sccp(#sccp_long_unitdata{class = Class, hop_counter = Hops, called_party = CalledParty,
		calling_party = CallingParty, long_data = LongData} = S) ->
	Segs = set_option(?Segmentation, S#sccp_long_unitdata.segmentation),
	Importance = set_option(?Importance, importance(S#sccp_long_unitdata.importance)),
	CalledPartyB = party_address(CalledParty),
	CallingPartyB = party_address(CallingParty),
	<<?LongUnitData, Class, Hops, CalledPartyB/binary, CallingPartyB/binary,
		LongData/binary, Segs:48/binary, Importance:24/binary>>;
sccp(#sccp_long_unitdata_service{return_cause = RC, hop_counter = Hops,
		called_party = CalledParty, calling_party = CallingParty,
		long_data = LongData} = S) ->
	Cause = return_cause(RC),
	Segs = set_option(?Segmentation, S#sccp_long_unitdata_service.segmentation),
	Importance = set_option(?Importance, importance(S#sccp_long_unitdata_service.importance)),
	CalledPartyB = party_address(CalledParty),
	CallingPartyB = party_address(CallingParty),
	<<?LongUnitData, Cause/integer, Hops, CalledPartyB/binary, CallingPartyB/binary,
			LongData/binary,
		Segs:48/binary, Importance:24/binary>>.

-spec party_address(PA1) -> PA2
	when
		PA1 :: binary() | undefined | #party_address{},
		PA2 :: binary() | #party_address{}.
%% @doc Check address indicator values and extract relevant information.
party_address(<<_:7, 1:1, LSB, _:2, MSB:6, _/binary>> = B) ->
	party_address1(B, #party_address{pc = (MSB bsl 8) + LSB});
party_address(<<_:7, 0:1,  _/binary>> = B) ->
	party_address1(B, #party_address{});
party_address(#party_address{ri  = R, pc = P, ssn = S, gt = undefined}) ->
	RI = routing_indicator(R),
	PC = point_code(P),
	SSN = ssn(S),
	<<0:1, RI:1, 0:4, 1:1, 1:1, PC/binary, SSN/binary>>;
party_address(#party_address{ri  = R, pc = P, ssn = S, nai = N , gt = G})
		when N /= undefined, G /= undefined ->
	RI = routing_indicator(R),
	PC = point_code(P),
	SSN = ssn(S),
	NAI = nai(N),
	OE = length(G) rem 2,
	BCD = bcd(G),
	<<0:1, RI:1, 0:4, 1:1, 1:1, PC/binary, SSN/binary, OE:1, NAI/integer, BCD/binary>>;
party_address(#party_address{ri  = R, pc = P, ssn = S, translation_type = T,  gt = G})
		when T /= undefined, G /= undefined ->
	RI = routing_indicator(R),
	PC = point_code(P),
	SSN = ssn(S),
	TT = tt(T),
	BCD = bcd(G),
	<<0:1, RI:1, 0:4, 1:1, 1:1, PC/binary, SSN/binary, TT:8, BCD/binary>>;
party_address(#party_address{ri  = R, pc = P, ssn = S, translation_type = T,
		numbering_plan = NP, encoding_scheme = E, gt = G}) when T /= undefined, NP /= undefined,
		E /= undefined, G /= undefined ->
	RI = routing_indicator(R),
	PC = point_code(P),
	SSN = ssn(S),
	TT = tt(T),
	NPlan = numbering_plan(NP),
	ES = encoding_scheme(E),
	BCD = bcd(G),
	<<0:1, RI:1, 0:4, 1:1, 1:1, PC/binary, SSN/binary, TT:8, NPlan:4, ES:4, BCD/binary>>;
party_address(#party_address{ri  = R, pc = P, ssn = S, translation_type = T,
		numbering_plan = NP, encoding_scheme = E, nai = N, gt = G}) when T /= undefined,
		NP /= undefined, E /= undefined, G /= undefined, N /= undefined ->
	RI = routing_indicator(R),
	PC = point_code(P),
	SSN = ssn(S),
	TT = tt(T),
	NPlan = numbering_plan(NP),
	ES = encoding_scheme(E),
	NAI = nai(N),
	BCD = bcd(G),
	<<0:1, RI:1, 0:4, 1:1, 1:1, PC/binary, SSN/binary, TT:8, NPlan:4, ES:4, 0:0,
			NAI:7, BCD/binary>>;
party_address(undefined) ->
	<<>>.

%% @hidden
%% @doc Evaluate SSN indicator
party_address1(<<_:6, 1:1, _:1, SSN, _Rest/binary>> = B, P) ->
	party_address2(B, P#party_address{ssn = SSN});
party_address1(<<_:6, 0:1, _:1, _Rest/binary>> = B, P) ->
	party_address2(B, P).

%% @hidden
%% @doc Evaluate global Title indicator
party_address2(<<_:1, RI:1, 0:4, _:2, _Rest/binary>> = _B, P) -> % No global title included
	R = routing_indicator(RI),
	P#party_address{ri = R};
party_address2(<<_:1, RI:1, 1:4, _:2, _PC, _SSN, _:1, NAI:7, GT/binary>> = _B, P) -> % NAI only
	R = routing_indicator(RI),
	N = nai(NAI),
	G = bcd(GT),
	P#party_address{ri = R, nai = N, gt = G};
party_address2(<<_:1, RI:1, 2:4, _:2, _PC, _SSN, TT, GT/binary>> = _B, P) -> % Translation type only
	R = routing_indicator(RI),
	T = tt(TT),
	G = bcd(GT),
	P#party_address{ri = R, translation_type = T, gt = G};
party_address2(<<_:1, RI:1, 3:4, _:2, _PC, _SSN, TT, NPlan:4, Enc:4,
		GT/binary>> = _B, P) -> % Translation type, numbering plan and encoding scheme
	R = routing_indicator(RI),
	T = tt(TT),
	N = numbering_plan(NPlan),
	E = encoding_scheme(Enc),
	G = bcd(GT),
	P#party_address{ri = R, translation_type = T, numbering_plan = N,
			encoding_scheme = E, gt = G};
party_address2(<<_:1, RI:1, 4:4, _:2, _PC, _SSN, TT, NPlan:4, Enc:4, 0:1, NAI:7,
		GT/binary>> = _B, P) -> % Translation type, numbering plan and encoding scheme, NAI
	R = routing_indicator(RI),
	T = tt(TT),
	NP = numbering_plan(NPlan),
	E = encoding_scheme(Enc),
	N = nai(NAI),
	G = bcd(GT),
	P#party_address{ri = R, translation_type = T, numbering_plan = NP,
			encoding_scheme = E, nai = N, gt = G}.

-spec nai(N) -> N
	when
		N :: integer() | atom().
%% @doc Values for network address indicator as defined in
%% ITU-T Recommendation Q.713, section 3.4.2.3.1  
nai(0) -> unknown;
nai(1) -> subscriber;
nai(2) -> reserved_for_national;
nai(3) -> national;
nai(4) -> international;
nai(N) when (is_integer(N))
		andalso (N >= 5 andalso N =< 111) -> spare;
nai(N) when (is_integer(N))
		andalso (N >= 112 andalso N =< 126) -> reserved_for_national;
nai(127) -> reserved;
nai(unknown) -> 0;
nai(subscriber) -> 1;
nai(reserved_for_national) -> 2;
nai(national) -> 3; 
nai(international) -> 4;
nai(spare) -> 5;
nai(reserved) -> 127.

-spec tt(T) -> T
	when
		T :: integer() | atom().
%% @doc Values for translation type as defined in
%% ITU-T Recommendation Q.713, section 3.4.2.3.2
tt(0) -> not_used;
tt(1) -> internetwork;
tt(N) when (is_integer(N)) andalso
		(N >= 2 andalso N =< 254) -> network_specific;
tt(255) -> reserved;
tt(not_used) -> 0;
tt(internetwork) -> 1;
tt(network_specific) -> 2;
tt(reserved) -> 255.

-spec numbering_plan(P) -> P
	when
		P :: integer() | atom().
%% @doc Values for numbering plan as defined in
%% ITU-T Recommendation Q.713, section 3.4.2.3.3
numbering_plan(0) -> unknown;
numbering_plan(1) -> isdn_tele;
numbering_plan(2) -> generic;
numbering_plan(3) -> data;
numbering_plan(4) -> telex;
numbering_plan(5) -> maritime;
numbering_plan(6) -> land_mobile;
numbering_plan(7) -> isdn_mobile;
numbering_plan(N) when (is_integer(N)) andalso
		(N >= 8 andalso N =< 13) -> spare;
numbering_plan(14) -> private_net;
numbering_plan(15) -> reserved; 
numbering_plan(unknown) -> 0;
numbering_plan(isdn_tele) -> 1; 
numbering_plan(generic) -> 2; 
numbering_plan(data) -> 3; 
numbering_plan(telex) -> 4; 
numbering_plan(maritime) -> 5; 
numbering_plan(land_mobile) -> 6; 
numbering_plan(isdn_mobile) -> 7;
numbering_plan(private_net) -> 14; 
numbering_plan(reserved) -> 15;
numbering_plan(_) -> 13.

-spec encoding_scheme(E) -> E
	when
		E :: integer() | atom().
%% @doc Values for encoding scheme as defined in
%% ITU-T Recommendation Q.713, section 3.4.2.3.3
encoding_scheme(0) -> unknown;
encoding_scheme(1) -> bcd_odd;
encoding_scheme(2) -> bcd_even;
encoding_scheme(3) -> national;
encoding_scheme(N) when (is_integer(N)) andalso
		(N >=4 andalso N =< 14 ) -> spare;
encoding_scheme(15) -> reserved;
encoding_scheme(unknown) -> 0;
encoding_scheme(bcd_odd) -> 1;
encoding_scheme(bcd_even) -> 2;
encoding_scheme(national) -> 3; 
encoding_scheme(reserved) -> 15;
encoding_scheme(_) -> 14.

-spec routing_indicator(R) -> R
	when
		R :: integer() | boolean().
%% @doc Values for routing indicator as defined in
%% ITU-T Recommendation Q.713, section 3.4.1
routing_indicator(0) -> false;
routing_indicator(1) -> true;
routing_indicator(false) -> 0;
routing_indicator(true) -> 1.

-spec get_option(K, Options) -> V
	when
		K :: integer(),
		Options :: [{K, V}],
		V :: term().
%% @doc Get option `O' from SCCP optional parameters.
get_option(K, Options) when is_integer(K) ->
	case lists:keyfind(K, 1, Options) of
		false ->
			undefined;
		{_, V} ->
			V
	end.

-spec optional_part(P) -> Q
	when
		P :: binary(),
		Q :: [{K, V}],
		K :: byte(),
		V :: term().
%% @doc Extract optional part from a SCCP message.
optional_part(Part) ->
	optional_part1(Part, []).

%% @hidden
optional_part1(<<Name, Len, Rest/binary>>, Acc) ->
	V = binary:part(Rest, 16, Len),
	<<V:Len/binary, Rest1/binary>> = Rest,
	optional_part1(Rest1, [{Name, V} | Acc]);
optional_part1(<<>>, Acc) ->
	Acc.

-spec set_option(N, V) -> Option
	when
		N :: integer(),
		V :: term(),
		Option :: binary().
%% @doc Return an AVP binary.
set_option(_, undefined) ->
	<<>>;
set_option(N, V) ->
		VBin = term_to_binary(V),
		L = size(VBin),
		<<N, L, VBin/binary>>.

-spec importance(I) -> I
	when
		I :: integer() | binary().
%% @doc Values for importance as defined in
%% ITU-T Recommendation Q.713, section 3.19
importance(<<_:5, I/integer>>) ->
	I;
importance(I) when I >= 0; I =< 7 ->
	integer_to_binary(I).

-spec refusal_cause(C) -> C
	when
		C :: integer() | atom().
%% @doc Values for refusal cause  as defined in
%% ITU-T Recommendation Q.713, section 3.15
refusal_cause(0) -> enduser_orig;
refusal_cause(1) -> enduser_congestion;
refusal_cause(2) -> enduser_failure;
refusal_cause(3) -> sccp_user_oirg;
refusal_cause(4) -> dest_unknown;
refusal_cause(5) -> dest_inaccessible;
refusal_cause(6) -> qos_not_avail_non_transient;
refusal_cause(7) -> qos_not_avail_transient;
refusal_cause(8) -> access_failure;
refusal_cause(9) -> access_congestion;
refusal_cause(10) -> subsystem_failure;
refusal_cause(11) -> subsystem_congestion;
refusal_cause(12) -> connection_expire;
refusal_cause(13) -> incomp_userdata;
refusal_cause(C) when (is_integer(C)) andalso
		(C == 14 orelse (C > 19 andalso C =< 255))-> reserved;
refusal_cause(15) -> unqualified;
refusal_cause(16) -> hcounter_violation;
refusal_cause(17) -> sccp_failure;
refusal_cause(18) -> no_translation_addr;
refusal_cause(19) -> unequipped_error;
refusal_cause(enduser_orig) -> 0;
refusal_cause(enduser_congestion) -> 1;
refusal_cause(enduser_failure) -> 2;
refusal_cause(sccp_user_oirg) -> 3;
refusal_cause(dest_unknown) -> 4;
refusal_cause(dest_inaccessible) -> 5;
refusal_cause(qos_not_avail_non_transient) -> 6;
refusal_cause(qos_not_avail_transient) -> 7;
refusal_cause(access_failure) -> 8;
refusal_cause(access_congestion) -> 9;
refusal_cause(subsystem_failure) -> 10;
refusal_cause(subsystem_congestion) -> 11;
refusal_cause(connection_expire) -> 12;
refusal_cause(incomp_userdata) -> 13; 
refusal_cause(unqualified) -> 15;
refusal_cause(hcounter_violation) -> 16;
refusal_cause(sccp_failure) -> 17;
refusal_cause(no_translation_addr) -> 18;
refusal_cause(unequipped_error) -> 19;
refusal_cause(_) -> 255.

-spec release_cause(C) -> C
	when
		C :: integer() | atom().
%% @doc Values for release cause  as defined in
%% ITU-T Recommendation Q.713, section 3.11
release_cause(0) -> enduser_orig;
release_cause(1) -> enduser_congestion;
release_cause(2) -> enduser_failure;
release_cause(3) -> sccp_user_originated;
release_cause(4) -> rpc_error;
release_cause(5) -> inconsistant_data;
release_cause(6) -> access_failure;
release_cause(7) -> access_congestion;
release_cause(8) -> subsystem_failure;
release_cause(9) -> subsystem_congestion;
release_cause(10) -> mtp_failure;
release_cause(11) -> network_congestion;
release_cause(12) -> reset_timer_expire;
release_cause(13) -> inactivity_timer_expire;
release_cause(C) when (is_integer(C)) andalso
		(C == 14 orelse (C > 16 andalso C =< 255)) -> reserved;
release_cause(15) -> unqualified;
release_cause(16) -> sccp_failure;
release_cause(enduser_orig) -> 0;
release_cause(enduser_congestion) -> 1;
release_cause(enduser_failure) -> 2;
release_cause(sccp_user_originated) -> 3;
release_cause(rpc_error) -> 4;
release_cause(inconsistant_data) -> 5;
release_cause(access_failure) -> 6;
release_cause(access_congestion) -> 7;
release_cause(subsystem_failure) -> 8;
release_cause(subsystem_congestion) -> 9;
release_cause(mtp_failure) -> 10;
release_cause(network_congestion) -> 11;
release_cause(reset_timer_expire) -> 12;
release_cause(inactivity_timer_expire) -> 13;
release_cause(unqualified) -> 15;
release_cause(sccp_failure) -> 16;
release_cause(_) -> 255.

-spec segmenting(S) -> S
	when
		S :: integer() | boolean().
%% @doc Values for segmenting/reassembling  as defined in
%% ITU-T Recommendation Q.713, section 3.7
segmenting(0) -> false;
segmenting(1) -> true;
segmenting(false) -> 0;
segmenting(true) -> 1.

-spec return_cause(C) -> C
	when
		C :: integer() | atom().
%% @doc Values for return cause as defined in
%% ITU-T Recommendation Q.713, section 3.12
return_cause(N) when N == 0; N == 1 -> no_translation;
return_cause(2) -> subsystem_congestion;
return_cause(3) -> subsystem_failure;
return_cause(4) -> unequipped_error;
return_cause(5) -> mtp_failure;
return_cause(6) -> network_congestion;
return_cause(7) -> unqualified;
return_cause(8) -> transport_error;
return_cause(9) -> processing_error;
return_cause(10) -> reassembly_fail;
return_cause(11) -> sccp_failure;
return_cause(12) -> hcounter_violation;
return_cause(13) -> seg_not_supported;
return_cause(14) -> seg_failure;
return_cause(C) when is_integer(C), C >= 15, C =< 255 -> reserved;
return_cause(no_translation) -> 0;
return_cause(subsystem_congestion) -> 2;
return_cause(subsystem_failure) -> 3;
return_cause(unequipped_error) -> 4;
return_cause(mtp_failure) -> 5;
return_cause(network_congestion) -> 6;
return_cause(unqualified) -> 7;
return_cause(transport_error) -> 8;
return_cause(processing_error) -> 9;
return_cause(reassembly_fail) -> 10;
return_cause(sccp_failure) -> 11;
return_cause(hcounter_violation) -> 12;
return_cause(seg_not_supported) -> 13;
return_cause(seg_failure) -> 14;
return_cause(_) -> 255.

-spec segmentation(S) -> S
	when
		S :: #segmentation{} | binary().
%% @doc Values for segmentation as defined in
%% ITU-T Recommendation Q.713, section 3.17
segmentation(<<0:1, C:1, _:2, RemSeg:4, _Rest/binary>>) ->
	#segmentation{first = false, class = C, remaning_seg = RemSeg};
segmentation(<<1:1, C:1, _:2, RemSeg:4, _Rest/binary>>) ->
	#segmentation{first = true, class = C, remaning_seg = RemSeg};
segmentation(#segmentation{first = false, class = C, remaning_seg = R}) ->
	<<0:1, C:1, 0:2, R:4, 0:24>>;
segmentation(#segmentation{first = true, class = C, remaning_seg = R}) ->
	<<1:1, C:1, 0:2, R:4, 0:24>>.

-spec point_code(P) -> P
	when
		P :: undefined | integer() | binary().
%% @doc Values for signalling point code as defined in
%% ITU-T Recommendation Q.713, section 3.4.2.1
point_code(undefined) ->
	<<>>;
point_code(<<>>) ->
	undefined;
point_code(<<0:2, P:14/integer>>) ->
	P;
point_code(P) when is_integer(P) ->
	<<0:2, P:14>>.

-spec ssn(N) -> N
	when
		N :: undefined | integer() | binary().
%% @doc Values for subsystem number as defined in
%% ITU-T Recommendation Q.713, section 3.4.2.2
ssn(undefined) ->
	<<>>;
ssn(<<>>) ->
	undefined;
ssn(<<N:8/integer>>) ->
	N;
ssn(N) when is_integer(N) ->
	<<N:8>>.

-spec bcd(N) -> N
	when
		N :: binary() | [integer()].
%% @doc Encode/decode binary coded decimal values
bcd(Address) when is_binary(Address) ->
	bcd1(Address, []);
bcd(Address) when is_list(Address) ->
	bcd2(Address, <<>>).

%% @hidden
bcd1(<<X:4/integer, Y:4/integer, Rest/binary>>, Acc) when is_list(Acc) ->
	bcd1(Rest, [X, Y | Acc]);
bcd1(<<0:4/integer, Y:4/integer, Rest/binary>>, Acc) when is_list(Acc) ->
	bcd1(Rest, [Y | Acc]);
bcd1(<<>>, Acc) ->
	lists:reverse(Acc).

%% @hidden
bcd2([X | []], Acc) when is_binary(Acc) ->
	I = <<0:4/integer, X:4/integer>>,
	bcd2([], <<Acc/binary, I/binary>>);
bcd2([X | [Y | Rest]], Acc) when is_binary(Acc) ->
	I = <<Y:4/integer, X:4/integer>>,
	bcd2(Rest, <<Acc/binary, I/binary>>);
bcd2([], Acc) ->
	Acc.

