%%% sccp.erl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2015-2021 SigScale Global Inc.
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
%%% @doc This library module implements encoding and decoding (CODEC)
%%% 	functions for the Signalling Connection Control Part (SCCP) protocol
%%% 	in the {@link //sccp. sccp} application.
%%%
%%% @reference <a href="http://www.itu.int/rec/T-REC-Q.713-200103-I/en">
%%%		ITU-T Recommendation Q.713 - SCCP formats and codes</a>
%%%
-module(sccp_codec).
-copyright('Copyright (c) 2015-2021 SigScale Global Inc.').
-author('vances@sigscale.org').

%% SCCP message codec funcion
-export([sccp/1]).
%% SCCP options codec funcion
-export([party_address/1, nai/1, numbering_plan/1, encoding_scheme/1,
		routing_indicator/1, importance/1, refusal_cause/1, release_cause/1,
		segmenting/1, return_cause/1, reset_cause/1, segmentation/1, point_code/1,
		bcd/1, bcd/2, ssn/1]).

-export_type([sccp_message/0, party_address/0]).

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
-type party_address() :: #party_address{}.

-spec sccp(Message) -> Message
	when
		Message :: binary() | sccp_message().
%% @doc CODEC for SCCP messages.
%%
%% ITU-T Recommendation Q.713, SCCP formats and codes.
%%
sccp(<<?ConnectRequest, SrcLocalRef:24, Class, CalledPartyP, OptionalP, Rest/binary>> = _Message) ->
	CalledPartyL = binary:at(Rest, CalledPartyP - 1),
	CalledPartyB = binary:part(Rest, CalledPartyP, CalledPartyL),
	Address = party_address(CalledPartyB),
	Opts = optional_part(OptionalP, Rest),
	#sccp_connection_req{src_local_ref = SrcLocalRef,
			class = Class, called_party = Address,
			credit = get_option(?Credit, Opts),
			calling_party  = get_option(?CallingPartyAddress, Opts),
			data = get_option(?DATA, Opts),
			hop_counter = get_option(?HopCounter, Opts),
			importance = get_option(?Importance, Opts)};
sccp(<<?ConnectionConfirm, DestLocalRef:24, SrcLocalRef:24, Class, OptionalP, Rest/binary>>) ->
	Opts = optional_part(OptionalP, Rest),
	#sccp_connection_confirm{dest_local_ref = DestLocalRef,
			src_local_ref = SrcLocalRef, class = Class,
			credit = get_option(?Credit, Opts),
			called_party = get_option(?CalledPartyAddress, Opts),
			data = get_option(?DATA, Opts),
			importance = get_option(?Importance, Opts)};
sccp(<<?ConnectionRefused, DestLocalRef:24, Refuse, OptionalP, Rest/binary>>) ->
	Opts = optional_part(OptionalP, Rest),
	#sccp_connection_refused{dest_local_ref = DestLocalRef,
			refusal_cause = refusal_cause(Refuse),
			called_party = get_option(?CalledPartyAddress, Opts),
			data = get_option(?DATA, Opts),
			importance = get_option(?Importance, Opts)};
sccp(<<?Released, DestLocalRef:24, SrcLocalRef:24, Release, OptionalP, Rest/binary>>) ->
	Opts = optional_part(OptionalP, Rest),
	#sccp_released{dest_local_ref = DestLocalRef,
			src_local_ref = SrcLocalRef,
			release_cause = release_cause(Release),
			data = get_option(?DATA, Opts),
			importance = get_option(?Importance, Opts)};
sccp(<<?ReleaseComplete, DestLocalRef:24, SrcLocalRef:24/integer>>) ->
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
sccp(<<?DataAck, DestLocalRef:24, RecvSeq:1/binary, Credit:1/binary>>) ->
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
			calling_party = party_address(CallingPartyB),
			data = Data};
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
	Cause = reset_cause(Reset),
	#sccp_reset_request{dest_local_ref = DestLocalRef,
			src_local_ref = SrcLocalRef, reset_cause = Cause};
sccp(<<?ResetConfirmation, DestLocalRef:24, SrcLocalRef:24/integer>>) ->
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
sccp(<<?ExtendedUnitData, Class, Hops, CalledPartyP, CallingPartyP, DataP, OptionalP,
		Rest/binary>>) ->
	CalledPartyL = binary:at(Rest, CalledPartyP - 4),
	CallingPartyL = binary:at(Rest, CallingPartyP - 3),
	DataL = binary:at(Rest, DataP - 2),
	CalledPartyB = binary:part(Rest, CalledPartyP - 3, CalledPartyL),
	CallingPartyB = binary:part(Rest, CallingPartyP - 2, CallingPartyL),
	Data = binary:part(Rest, DataP - 1, DataL),
	Opts = optional_part(OptionalP, Rest), 
	#sccp_extended_unitdata{class = Class, hop_counter = Hops,
			called_party = party_address(CalledPartyB),
			calling_party = party_address(CallingPartyB), data = Data,
			segmentation = get_option(?Segmentation, Opts),
			importance = get_option(?Importance, Opts)};
sccp(<<?ExtendedUnitDataService, RC, Hops, CalledPartyP, CallingPartyP, DataP, OptionalP,
		Rest/binary>>) ->
	Return = return_cause(RC),
	CalledPartyL = binary:at(Rest, CalledPartyP - 4),
	CallingPartyL = binary:at(Rest, CallingPartyP - 3),
	DataL = binary:at(Rest, DataP - 2),
	CalledPartyB = binary:part(Rest, CalledPartyP - 3, CalledPartyL),
	CallingPartyB = binary:part(Rest, CallingPartyP - 2, CallingPartyL),
	Data = binary:part(Rest, DataP - 1, DataL),
	Opts = optional_part(OptionalP, Rest),
	#sccp_extended_unitdata_service{return_cause = Return, hop_counter = Hops,
			called_party = party_address(CalledPartyB),
			calling_party = party_address(CallingPartyB), data = Data,
			segmentation = get_option(?Segmentation, Opts),
			importance = get_option(?Importance, Opts)};
sccp(<<?LongUnitData, Class, Hops, CalledPartyP, CallingPartyP, LongDataP, OptionalP,
		Rest/binary>>) ->
	CalledPartyL = binary:at(Rest, CalledPartyP - 4),
	CallingPartyL = binary:at(Rest, CallingPartyP - 3),
	LongDataL = binary:at(Rest, LongDataP - 2),
	CalledPartyB = binary:part(Rest, CalledPartyP - 3, CalledPartyL),
	CallingPartyB = binary:part(Rest, CallingPartyP - 2, CallingPartyL),
	LongData = binary:part(Rest, LongDataP - 1, LongDataL),
	Opts = optional_part(OptionalP, Rest),
	#sccp_long_unitdata{class = Class, hop_counter = Hops,
			called_party = party_address(CalledPartyB),
			calling_party = party_address(CallingPartyB), 
			long_data = LongData,
			segmentation = get_option(?Segmentation, Opts),
			importance = get_option(?Importance, Opts)};
sccp(<<?LongUnitDataService, RC, Hops, CalledPartyP, CallingPartyP, LongDataP, OptionalP, 
		Rest/binary>>) ->
	Return = return_cause(RC),
	CalledPartyL = binary:at(Rest, CalledPartyP - 4),
	CallingPartyL = binary:at(Rest, CallingPartyP - 3),
	LongDataL = binary:at(Rest, LongDataP - 2),
	CalledPartyB = binary:part(Rest, CalledPartyP - 3, CalledPartyL),
	CallingPartyB = binary:part(Rest, CallingPartyP - 2, CallingPartyL),
	LongData = binary:part(Rest, LongDataP - 1, LongDataL),
	Opts = optional_part(OptionalP, Rest),
	#sccp_long_unitdata_service{return_cause = Return, hop_counter = Hops,
			called_party = party_address(CalledPartyB),
			calling_party = party_address(CallingPartyB), 
			long_data = LongData,
			segmentation = get_option(?Segmentation, Opts),
			importance = get_option(?Importance, Opts)};
sccp(#sccp_connection_req{} = S) ->
	sccp_connection_req(S);
sccp(#sccp_connection_confirm{} = S) ->
	sccp_connection_confirm(S);
sccp(#sccp_connection_refused{} = S) -> 
	sccp_connection_refused(S);
sccp(#sccp_released{} = S) ->
	sccp_released(S);
sccp(#sccp_release_complete{dest_local_ref = Dest, src_local_ref = Src} = _S) ->
	<<?ReleaseComplete, Dest:24, Src:24>>;
sccp(#sccp_data_form1{dest_local_ref = Dest, segmenting = S, data = Data}) ->
	Seg = segmenting(S),
	DataL = size(Data),
	DataP = 1,
	<<?DataForm1, Dest:24, Seg, DataP, DataL, Data/binary>>;
sccp(#sccp_data_form2{dest_local_ref = Dest, sequencing = S, data = Data}) ->
	Seq = segmenting(S),
	DataL = size(Data),
	DataP = 1,
	<<?DataForm2, Dest:24, Seq, DataP, DataL, Data/binary>>;
sccp(#sccp_data_ack{dest_local_ref = Dest, receive_seq_num = Seq, credit = Credit}) ->
	<<?DataAck, Dest:24, Seq/binary, Credit/binary>>;
sccp(#sccp_unitdata{class = Class, data = Data,
		called_party = #party_address{} = CalledParty,
		calling_party = #party_address{} = CallingParty})
		when is_integer(Class), is_binary(Data) ->
	CalledPartyB = party_address(CalledParty),
	CallingPartyB = party_address(CallingParty),
	CalledPartyL = size(CalledPartyB),
	CallingPartyL = size(CallingPartyB),
	DataL = size(Data),
	CalledPartyP = 3,
	CallingPartyP = CalledPartyP + CalledPartyL,
	DataP = CallingPartyP + CallingPartyL,
	<<?UnitData, Class, CalledPartyP, CallingPartyP, DataP,
			CalledPartyL, CalledPartyB/binary, CallingPartyL, CallingPartyB/binary, DataL, Data/binary>>;
sccp(#sccp_unitdata_service{return_cause = RC, called_party = CalledParty,
		calling_party = CallingParty, data = Data}) ->
	Return = return_cause(RC),
	CalledPartyB = party_address(CalledParty),
	CallingPartyB = party_address(CallingParty),
	CalledPartyL = size(CalledPartyB),
	CallingPartyL = size(CallingPartyB),
	DataL = size(Data),
	CalledPartyP = 3,
	CallingPartyP = CalledPartyP + CalledPartyL,
	DataP = CallingPartyP + CallingPartyL,
	<<?UnitDataService, Return/integer, CalledPartyP, CallingPartyP, DataP,
			CalledPartyL, CalledPartyB/binary, CallingPartyL, CallingPartyB/binary,
			DataL, Data/binary>>;
sccp(#sccp_expedited_data{dest_local_ref  = Dest, data = Data}) ->
	DataL = size(Data),
	DataP = 1,
	<<?ExpeditedData, Dest:24, DataP, DataL, Data/binary>>;
sccp(#sccp_expedited_ack{dest_local_ref = Dest}) ->
	<<?ExpeditedDataAck, Dest:24/integer>>;
sccp(#sccp_reset_request{dest_local_ref = Dest, src_local_ref = Src, reset_cause = Cause}) ->
	Reset = reset_cause(Cause),
	<<?ResetRequest, Dest:24, Src:24, Reset/integer>>;
sccp(#sccp_reset_confirmation{dest_local_ref = Dest, src_local_ref = Src}) ->
	<<?ResetConfirmation, Dest:24, Src:24/integer>>;
sccp(#sccp_protocol_data_unit_error{dest_local_ref = Dest, error_cause = Error}) ->
	<<?ProtocolDataUnitError, Dest:24, Error/integer>>;
sccp(#sccp_inactivity_test{dest_local_ref = Dest, src_local_ref = Src, class = Class,
		sequencing = S, credit = Credit}) ->
	Seq = segmenting(S),
	<<?InactivityTest, Dest:24, Src:24, Class, 0:15, Seq:1/integer, Credit/binary>>;
sccp(#sccp_extended_unitdata{} = S) ->
	sccp_extended_unitdata(S);
sccp(#sccp_extended_unitdata_service{} = S) ->
	sccp_extended_unitdata_service(S);
sccp(#sccp_long_unitdata{} = S) ->
	sccp_long_unitdata(S);
sccp(#sccp_long_unitdata_service{} = S) ->
	sccp_long_unitdata_service(S).

-spec party_address(Address) -> Address
	when
		Address :: binary() | party_address().
%% @doc CODEC for called party address.
%%
%% ITU-T Recommendation Q.713, section 3.4.
party_address(<<_:1, RI:1, GTI:4, SSNI:1, PCI:1, Address/binary>>) ->
	party_address1(PCI, SSNI, RI, GTI, Address);
party_address(#party_address{ri = RoutingIndicator,
		pc = undefined, ssn = SubSystemNumber} = P)
		when is_boolean(RoutingIndicator),
		is_integer(SubSystemNumber) ->
	RI = routing_indicator(RoutingIndicator),
	SSN = ssn(SubSystemNumber),
	party_address4(0, 1, RI, SSN, P);
party_address(#party_address{ri = RoutingIndicator,
		pc = PointCode, ssn = undefined} = P)
		when is_integer(PointCode) ->
	RI = routing_indicator(RoutingIndicator),
	PC = point_code(PointCode),
	party_address4(1, 0, RI, PC, P);
party_address(#party_address{ri = RoutingIndicator,
		pc = PointCode, ssn = SubSystemNumber} = P)
		when is_boolean(RoutingIndicator),
		is_integer(SubSystemNumber), is_integer(PointCode) ->
	RI = routing_indicator(RoutingIndicator),
	PC = point_code(PointCode),
	SSN = ssn(SubSystemNumber),
	party_address4(1, 1, RI, <<PC/binary, SSN/binary>>, P).
%% @hidden
party_address1(1, SSNI, RI, GTI, <<LSB, _:2, MSB:6, Address/binary>>) ->
	party_address2(SSNI, RI, GTI, Address, #party_address{pc = (MSB bsl 8) + LSB});
party_address1(0, SSNI, RI, GTI, Address) ->
	party_address2(SSNI, RI, GTI, Address, #party_address{}).
%% @hidden
party_address2(1, RI, GTI, <<SSN, Address/binary>>, P) ->
	party_address3(RI, GTI, Address, P#party_address{ssn = SSN});
party_address2(0, RI, GTI, Address, P) ->
	party_address3(RI, GTI, Address, P).
%% @hidden
party_address3(RI, 0,  _, P) ->
	P#party_address{ri = routing_indicator(RI)};
party_address3(RI, 1, <<OE:1, NAI:7, GT/binary>>, P) ->
	P#party_address{ri = routing_indicator(RI),
			nai = nai(NAI), gt = bcd(GT, OE)};
party_address3(RI, 2, <<TT, GT/binary>>, P) ->
	P#party_address{ri = routing_indicator(RI),
			translation_type = TT, gt = bcd(GT, 0)};
party_address3(RI, 3, <<TT, NP:4, ENC:4, GT/binary>>, P) ->
	Scheme  = encoding_scheme(ENC),
	Title = case Scheme of
		bcd_odd ->
			bcd(GT, 1);
		_ ->
			bcd(GT, 0)
	end,
	P#party_address{ri = routing_indicator(RI),
			translation_type = TT,
			numbering_plan = numbering_plan(NP),
			encoding_scheme = Scheme, gt = Title};
party_address3(RI, 4, <<TT, NP:4, ENC:4, _:1, NAI:7, GT/binary>>, P) ->
	Scheme  = encoding_scheme(ENC),
	Title = case Scheme of
		bcd_odd ->
			bcd(GT, 1);
		_ ->
			bcd(GT, 0)
	end,
	P#party_address{ri = routing_indicator(RI),
			translation_type = TT,
			numbering_plan = numbering_plan(NP),
			nai = nai(NAI), encoding_scheme = Scheme, gt = Title}.
%% @hidden
party_address4(PCI, SSNI, RI,  Address,
		#party_address{gt = undefined, translation_type = undefined,
		numbering_plan = undefined, encoding_scheme = undefined,
		nai = undefined}) ->
	<<0:1, RI:1, 0:4, SSNI:1, PCI:1, Address/binary>>;
party_address4(PCI, SSNI, RI,  Address,
		#party_address{gt = GlobalTitle, nai = NatureOfAddress,
		translation_type = undefined, numbering_plan = undefined,
		encoding_scheme = undefined}) when is_list(GlobalTitle),
		NatureOfAddress /= undefined ->
	OE = length(GlobalTitle) rem 2,
	NAI = nai(NatureOfAddress),
	GT = bcd(GlobalTitle),
	<<0:1, RI:1, 1:4, SSNI:1, PCI:1, Address/binary, OE:1, NAI:7, GT/binary>>;
party_address4(PCI, SSNI, RI, Address,
		#party_address{gt = GlobalTitle, translation_type = TT,
		numbering_plan = undefined, encoding_scheme = undefined,
		nai = undefined}) when TT /= undefined, is_list(GlobalTitle) ->
	GT = bcd(GlobalTitle),
	<<0:1, RI:1, 2:4, SSNI:1, PCI:1, Address/binary, TT, GT/binary>>;
party_address4(PCI, SSNI, RI, Address,
		#party_address{gt = GlobalTitle, translation_type = TT,
		numbering_plan = NumberingPlan, encoding_scheme = EncodingScheme,
		nai = undefined}) when NumberingPlan /= undefined,
		TT /= undefined, EncodingScheme /= undefined,
		is_list(GlobalTitle) ->
	NP = numbering_plan(NumberingPlan),
	ES = encoding_scheme(EncodingScheme),
	GT = bcd(GlobalTitle),
	<<0:1, RI:1, 3:4, SSNI:1, PCI:1, Address/binary,
			TT, NP:4, ES:4, GT/binary>>;
party_address4(PCI, SSNI, RI, Address,
		#party_address{gt = GlobalTitle, translation_type = TT,
		numbering_plan = NumberingPlan, encoding_scheme = EncodingScheme,
		nai = NatureOfAddress}) when TT /= undefined,
		NumberingPlan /= undefined, EncodingScheme /= undefined,
		NatureOfAddress /= undefined, is_list(GlobalTitle) ->
	NP = numbering_plan(NumberingPlan),
	ES = encoding_scheme(EncodingScheme),
	NAI = nai(NatureOfAddress),
	GT = bcd(GlobalTitle),
	<<0:1, RI:1, 4:4, SSNI:1, PCI:1, Address/binary,
			TT, NP:4, ES:4, 0:1, NAI:7, GT/binary>>.

-spec nai(Indicator) -> Indicator
	when
		Indicator :: unknown | subscriber | reserved_for_national | national
				| international | spare | reserved_for_national
				| reserved | 0..127.
%% @doc Values for network address indicator.
%%
%% ITU-T Recommendation Q.713, section 3.4.2.3.1.
nai(0 = _Indicator) -> unknown;
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

-spec numbering_plan(Plan) -> Plan
	when
		Plan :: unknown | isdn_tele | generic | data | telex | maritime
				| land_mobile | isdn_mobile | private_net | reserved | 0..15.
%% @doc Values for numbering plan.
%%
%% ITU-T Recommendation Q.713, section 3.4.2.3.3.
numbering_plan(0 = _Plan) -> unknown;
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

-spec encoding_scheme(Scheme) -> Scheme
	when
		Scheme :: unknown | bcd_odd | bcd_even | national | reserved | 0..15.
%% @doc Values for encoding scheme.
%%
%% ITU-T Recommendation Q.713, section 3.4.2.3.3.
encoding_scheme(0 = _Scheme) -> unknown;
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

-spec routing_indicator(Indicator) -> Indicator
	when
		Indicator  :: false | true | 0..1.
%% @doc Values for routing indicator.
%%
%% ITU-T Recommendation Q.713, section 3.4.1.
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
%% @private
get_option(K, Options) when is_integer(K) ->
	case lists:keyfind(K, 1, Options) of
		false ->
			undefined;
		{_, V} ->
			V
	end.

-spec optional_part(OptionalP, Rest) -> Q
	when
		OptionalP :: byte(),
		Rest :: binary(),
		Q :: [{K, V}],
		K :: byte(),
		V :: term().
%% @doc Extract optional part from a SCCP message.
%% @private
optional_part(OptionalP, Rest) when OptionalP > 0 ->
	Pos = OptionalP - 1,
	Len = size(Rest) - Pos,
	OptionalPart = binary:part(Rest, Pos, Len),
	optional_part1(OptionalPart, []);
optional_part(0, _Rest) ->
	[].

%% @hidden
optional_part1(<<?DestinationLocalRef, Len, Rest/binary>>, Acc) ->
	L = Len * 8,
	<<V:L/integer, Rest1/binary>> = Rest,
	optional_part1(Rest1, [{?DestinationLocalRef, V} | Acc]);
optional_part1(<<?SourceLocalRef, Len, Rest/binary>>, Acc) ->
	L = Len * 8,
	<<V:L/integer, Rest1/binary>> = Rest,
	optional_part1(Rest1, [{?SourceLocalRef, V} | Acc]);
optional_part1(<<?ProtocolClass, Len, Rest/binary>>, Acc) ->
	L = Len * 8,
	<<V:L/integer, Rest1/binary>> = Rest,
	optional_part1(Rest1, [{?ProtocolClass, V} | Acc]);
optional_part1(<<?ReceiveSequenceNum, Len, Rest/binary>>, Acc) ->
	L = Len * 8,
	<<V:L/integer, Rest1/binary>> = Rest,
	optional_part1(Rest1, [{?ReceiveSequenceNum, V} | Acc]);
optional_part1(<<?ReleaseCause, Len, Rest/binary>>, Acc) ->
	L = Len * 8,
	<<V:L/integer, Rest1/binary>> = Rest,
	optional_part1(Rest1, [{?ReleaseCause, V} | Acc]);
optional_part1(<<?ReturnCause, Len, Rest/binary>>, Acc) ->
	L = Len * 8,
	<<V:L/integer, Rest1/binary>> = Rest,
	optional_part1(Rest1, [{?ReturnCause, V} | Acc]);
optional_part1(<<?ResetCause, Len, Rest/binary>>, Acc) ->
	L = Len * 8,
	<<V:L/integer, Rest1/binary>> = Rest,
	optional_part1(Rest1, [{?ResetCause, V} | Acc]);
optional_part1(<<?ErrorCause, Len, Rest/binary>>, Acc) ->
	L = Len * 8,
	<<V:L/integer, Rest1/binary>> = Rest,
	optional_part1(Rest1, [{?ErrorCause, V} | Acc]);
optional_part1(<<?RefusalCause, Len, Rest/binary>>, Acc) ->
	L = Len * 8,
	<<V:L/integer, Rest1/binary>> = Rest,
	optional_part1(Rest1, [{?RefusalCause, V} | Acc]);
optional_part1(<<?HopCounter, Len, Rest/binary>>, Acc) ->
	L = Len * 8,
	<<V:L/integer, Rest1/binary>> = Rest,
	optional_part1(Rest1, [{?HopCounter, V} | Acc]);
optional_part1(<<?Importance, Len, Rest/binary>>, Acc) ->
	L = Len * 8,
	<<V:L/integer, Rest1/binary>> = Rest,
	optional_part1(Rest1, [{?Importance, V} | Acc]);
optional_part1(<<?CallingPartyAddress, Len, Rest/binary>>, Acc) ->
	V = binary:part(Rest, 0, Len),
	<<V:Len/binary, Rest1/binary>> = Rest,
	optional_part1(Rest1, [{?CallingPartyAddress, party_address(V)} | Acc]);
optional_part1(<<?CalledPartyAddress, Len, Rest/binary>>, Acc) ->
	V = binary:part(Rest, 0, Len),
	<<V:Len/binary, Rest1/binary>> = Rest,
	optional_part1(Rest1, [{?CalledPartyAddress, party_address(V)} | Acc]);
optional_part1(<<?Segmentation, Len, Rest/binary>>, Acc) ->
	V = binary:part(Rest, 0, Len),
	<<V:Len/binary, Rest1/binary>> = Rest,
	optional_part1(Rest1, [{?Segmentation, segmentation(V)} | Acc]);
optional_part1(<<0>>, Acc) ->
	Acc;
optional_part1(<<Name, Len, Rest/binary>>, Acc) ->
	V = binary:part(Rest, 0, Len),
	<<V:Len/binary, Rest1/binary>> = Rest,
	optional_part1(Rest1, [{Name, V} | Acc]).

-spec importance(Importance) -> Importance
	when
		Importance :: binary() | 0..7.
%% @doc Values for importance.
%%
%% ITU-T Recommendation Q.713, section 3.19.
importance(<<_:5, I/integer>> = _Importance) ->
	I;
importance(I) when I >= 0; I =< 7 ->
	integer_to_binary(I).

-spec refusal_cause(Cause) -> Cause
	when
		Cause :: enduser_orig | enduser_congestion | enduser_failure
				| sccp_user_oirg | dest_unknown | dest_inaccessible
				| qos_not_avail_non_transient | qos_not_avail_transient
				| access_failure | access_congestion | subsystem_failure
				| subsystem_congestion | connection_expire | incomp_userdata
				| unqualified | hcounter_violation | sccp_failure
				| no_translation_addr | unequipped_error | 0..255.
%% @doc Values for refusal cause.
%%
%% ITU-T Recommendation Q.713, section 3.15.
refusal_cause(0 = _Cause) -> enduser_orig;
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

-spec release_cause(Cause) -> Cause
	when
		Cause :: enduser_orig | enduser_congestion | enduser_failure
				| sccp_user_originated | rpc_error | inconsistant_data
				| access_failure | access_congestion | subsystem_failure
				| subsystem_congestion | mtp_failure | network_congestion
				| reset_timer_expire | inactivity_timer_expire
				| unqualified | sccp_failure | 0..255.
%% @doc Values for release cause.
%%
%% ITU-T Recommendation Q.713, section 3.11.
release_cause(0 = _Cause) -> enduser_orig;
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

-spec reset_cause(Cause) -> Cause
	when
		Cause :: enduser_orig | sccp_user_oirg | incorrect_ps
				| incorrect_pr | rpc_error | remote_end_user_operational
				| network_operational | access_opertional | network_congestion
				| unqualified | 0..255.
%% @doc Values for reset cause.
%%
%% ITU-T Recommendation Q.713, section 3.13.
reset_cause(0 = _Cause) -> enduser_orig;
reset_cause(1) -> sccp_user_oirg;
reset_cause(2) -> incorrect_ps;
reset_cause(3) -> incorrect_pr;
reset_cause(4) -> rpc_error;
reset_cause(5) -> rpc_error;
reset_cause(6) -> rpc_error;
reset_cause(7) -> remote_end_user_operational;
reset_cause(8) -> network_operational;
reset_cause(9) -> access_opertional;
reset_cause(10) -> network_congestion;
reset_cause(12) -> unqualified;
reset_cause(C) when (is_integer(C)) andalso
		(C == 11 orelse (C > 12 andalso C =< 255)) -> reserved;
reset_cause(enduser_orig) -> 0;
reset_cause(sccp_user_oirg) -> 1;
reset_cause(incorrect_ps) -> 2;
reset_cause(incorrect_pr) -> 3;
reset_cause(rpc_error) -> 4;
reset_cause(remote_end_user_operational) -> 7;
reset_cause(network_operational) -> 8;
reset_cause(access_opertional) -> 9;
reset_cause(network_congestion) -> 10;
reset_cause(unqualified) -> 12;
reset_cause(_) -> 255.

-spec segmenting(Segmenting) -> Segmenting
	when
		Segmenting:: false | true | 0..1.
%% @doc Values for segmenting/reassembling.
%%
%% ITU-T Recommendation Q.713, section 3.7.
segmenting(0 = _Segmenting) -> false;
segmenting(1) -> true;
segmenting(false) -> 0;
segmenting(true) -> 1.

-spec return_cause(Cause) -> Cause
	when
		Cause :: no_translation | subsystem_congestion | subsystem_failure
				| unequipped_error | mtp_failure | network_congestion
				| unqualified | transport_error | processing_error
				| reassembly_fail | sccp_failure | hcounter_violation
				| seg_not_supported | seg_failure | 0..255.
%% @doc Values for return cause.
%%
%% ITU-T Recommendation Q.713, section 3.12.
return_cause(N = _Cause) when N == 0; N == 1 -> no_translation;
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

-spec segmentation(Segmenting) -> Segmenting
	when
		Segmenting :: #segmentation{} | binary().
%% @doc Values for segmentation.
%%
%% ITU-T Recommendation Q.713, section 3.17.
segmentation(<<0:1, C:1, _:2, RemSeg:4, _Rest/binary>> = _Segmenting) ->
	#segmentation{first = false, class = C, remaning_seg = RemSeg};
segmentation(<<1:1, C:1, _:2, RemSeg:4, _Rest/binary>>) ->
	#segmentation{first = true, class = C, remaning_seg = RemSeg};
segmentation(#segmentation{first = false, class = C, remaning_seg = R}) ->
	<<0:1, C:1, 0:2, R:4, 0:24>>;
segmentation(#segmentation{first = true, class = C, remaning_seg = R}) ->
	<<1:1, C:1, 0:2, R:4, 0:24>>.

-spec point_code(Code) -> Code
	when
		Code :: undefined | 0..16383 | binary().
%% @doc Values for signalling point code (PC).
%%
%% ITU-T Recommendation Q.713, section 3.4.2.1.
point_code(undefined = _Code) ->
	<<>>;
point_code(<<>>) ->
	undefined;
point_code(<<LSB, 0:2, MSB:6>>) ->
	(MSB bsl 8) + LSB;
point_code(Code) when is_integer(Code) ->
	LSB = Code band 255,
	MSB = Code bsr 8,
	<<LSB, 0:2, MSB:6>>.

-spec ssn(SSN) -> SSN
	when
		SSN :: undefined | 0..255 | binary().
%% @doc Values for subsystem number (SSN).
%%
%% ITU-T Recommendation Q.713, section 3.4.2.2.
ssn(undefined = _SSN) ->
	<<>>;
ssn(<<>>) ->
	undefined;
ssn(<<N:8/integer>>) ->
	N;
ssn(N) when is_integer(N) ->
	<<N:8>>.

-spec bcd(Address) -> Data
	when
		Address :: [byte()],
		Data :: binary().
%% @doc Encode list of digits to a binary coded decimal value.
bcd(Address) when is_list(Address) ->
	bcd2(Address, <<>>).

-spec bcd(Data, OE) -> Address
	when
		Data :: binary(),
		OE :: 0..1,
		Address :: [byte()].
%% @doc Decode binary coded decimal value to a list of digits.
%%
%% `OE' indicates odd (1) or even (0) number of address
%%%     signals present in a global address information.
%%
%% ITU-T Recommendation Q.713, section 3.4.2.3.1.
bcd(Data, OE) when is_binary(Data) ->
	bcd1(Data, OE, []).

%% @hidden
bcd1(<<0:4/integer, Y:4/integer>>, 1, Acc) ->
	lists:reverse([Y | Acc]);
bcd1(<<X:4/integer, Y:4/integer>>, 0, Acc) ->
	lists:reverse([X, Y | Acc]);
bcd1(<<X:4/integer, Y:4/integer, Rest/binary>>, OE, Acc) ->
	bcd1(Rest, OE, [X, Y | Acc]).

%% @hidden
bcd2([X, Y | T], Acc) ->
	bcd2(T, <<Acc/binary, Y:4, X:4>>);
bcd2([X], Acc) ->
	<<Acc/binary, 0:4, X:4>>;
bcd2([], Acc) ->
	Acc.

%% @hidden
sccp_connection_req(#sccp_connection_req{src_local_ref = Src, class = Class,
		called_party = CalledParty} = S) ->
	CalledPartyB = party_address(CalledParty),
	CalledPartyP = 1,
	CalledPartyL = size(CalledPartyB),
	OptionalP = CalledPartyP + CalledPartyL,
	B = <<?ConnectRequest, Src:24, Class, CalledPartyP, OptionalP, CalledPartyL, CalledPartyB/binary>>,
	connection_req1(S, B).

%% @hidden
connection_req1(#sccp_connection_req{credit = undefined} = S, B) ->
	connection_req2(S, B, <<>>);
connection_req1(#sccp_connection_req{credit = C} = S, B) ->
	CL = size(C), 
	connection_req2(S, B, <<?Credit, CL, C/binary>>).

%% @hidden
connection_req2(#sccp_connection_req{calling_party = undefined} = S, B, O) ->
	connection_req3(S, B, O);
connection_req2(#sccp_connection_req{calling_party = CP} = S, B, O) ->
	CPB = party_address(CP),
	CPL = size(CPB),
	connection_req3(S, B, <<O/binary, ?CallingPartyAddress, CPL, CPB/binary>>).

%% @hidden
connection_req3(#sccp_connection_req{data = undefined} = S, B, O) ->
	connection_req4(S, B, O);
connection_req3(#sccp_connection_req{data = D} = S, B, O) ->
	DL = size(D),
	connection_req4(S, B, <<O/binary, ?DATA, DL, D/binary>>).

%% @hidden
connection_req4(#sccp_connection_req{hop_counter= undefined} = S, B, O) ->
	connection_req5(S, B, O);
connection_req4(#sccp_connection_req{hop_counter= H} = S, B, O) ->
	connection_req5(S, B, <<O/binary, ?HopCounter, 1, H/integer>>).

%% @hidden
connection_req5(#sccp_connection_req{importance= undefined} = _S, B, O) ->
	OL = size(O),
	connection_req6(<<B/binary, OL, O/binary>>, OL);
connection_req5(#sccp_connection_req{importance= I} = _S, B, O) ->
	Opt = <<O/binary, ?Importance, 1, I/integer>>,
	OptL = size(Opt),
	connection_req6(<<B/binary, OptL, Opt/binary>>, OptL).

%% @hidden
connection_req6(B, 0) ->
	B;
connection_req6(B, _) ->
	<<B/binary, 0>>.

%% @hidden
sccp_connection_confirm(#sccp_connection_confirm{dest_local_ref = Dest,
		src_local_ref = Src, class = Class} = S) ->
	OptionalP = 1,
	B = <<?ConnectionConfirm, Dest:24, Src:24, Class/integer, OptionalP>>,
	connection_confirm1(S, B).

%% @hidden
connection_confirm1(#sccp_connection_confirm{credit = undefined} = S, B) ->
	connection_confirm2(S,B, <<>>);
connection_confirm1(#sccp_connection_confirm{credit = C} = S, B) ->
	CL = size(C),
	connection_confirm2(S, B, <<?Credit, CL, C/binary>>).

%% @hidden
connection_confirm2(#sccp_connection_confirm{called_party = undefined} = S, B, O) ->
	connection_confirm3(S,B, O);
connection_confirm2(#sccp_connection_confirm{called_party= CP} = S, B, O) ->
	CPB = party_address(CP),
	CPL = size(CPB),
	connection_confirm3(S, B, <<O/binary, ?CalledPartyAddress, CPL, CPB/binary>>).

%% @hidden
connection_confirm3(#sccp_connection_confirm{data= undefined} = S, B, O) ->
	connection_confirm4(S,B, O);
connection_confirm3(#sccp_connection_confirm{data = D} = S, B, O) ->
	DL = size(D),
	connection_confirm4(S, B, <<O/binary, ?DATA, DL, D/binary>>).

%% @hidden
connection_confirm4(#sccp_connection_confirm{importance = undefined} = _S, B, O) ->
	OL = size(O),
	connection_confirm5(<<B/binary, OL, O/binary>>, OL);
connection_confirm4(#sccp_connection_confirm{importance = I} = _S, B, O) ->
	Opt = <<O/binary, ?Importance, 1, I/integer>>,
	OptL = size(Opt),
	connection_confirm5(<<B/binary, OptL, Opt/binary>>, OptL).

%% @hidden
connection_confirm5(B, 0) ->
	B;
connection_confirm5(B, _) ->
	<<B/binary, 0>>.

%% @hidden
sccp_connection_refused(#sccp_connection_refused{dest_local_ref = Dest, refusal_cause = RC} = S) ->
	Refuse = refusal_cause(RC),
	OptionalP = 1,
	B = <<?ConnectionRefused, Dest:24, Refuse, OptionalP>>,
	connection_refused1(S, B).

%% @hidden
connection_refused1(#sccp_connection_refused{called_party = undefined} = S, B) ->
	connection_refused2(S, B, <<>>);
connection_refused1(#sccp_connection_refused{called_party = CP} = S, B) ->
	CPB = party_address(CP),
	CPL = size(CPB),
	connection_refused2(S, B, <<?CalledPartyAddress, CPL, CPB/binary>>).

%% @hidden
connection_refused2(#sccp_connection_refused{data = undefined} = S, B, O) ->
	connection_refused3(S, B, O);
connection_refused2(#sccp_connection_refused{data = D} = S, B, O) ->
	DL = size(D),
	connection_refused3(S, B, <<O/binary, ?DATA, DL, D/binary>>).

%% @hidden
connection_refused3(#sccp_connection_refused{importance = undefined} = _S, B, O) ->
	OL = size(O),
	connection_refused4(<<B/binary, OL, O/binary>>, OL);
connection_refused3(#sccp_connection_refused{importance= I} = _S, B, O) ->
	Opt = <<O/binary, ?Importance, 1, I/integer>>,
	OptL = size(Opt),
	connection_refused4(<<B/binary, OptL, Opt/binary>>, OptL).

%% @hidden
connection_refused4(B, 0) ->
	B;
connection_refused4(B, _) ->
	<<B/binary, 0>>.

%% @hidden
sccp_released(#sccp_released{dest_local_ref = Dest, src_local_ref = Src, release_cause = RC} = S) ->
	Release = release_cause(RC),
	OptionalP = 1,
	B = <<?Released, Dest:24, Src:24, Release, OptionalP>>,
	released1(S, B).

%% @hidden
released1(#sccp_released{data = undefined} = S, B) ->
	released2(S, B, <<>>);
released1(#sccp_released{data = D} = S, B) ->
	DL = size(D),
	released2(S, B, <<?DATA, DL, D/binary>>).

%% @hidden
released2(#sccp_released{importance = undefined} = _S, B, O) ->
	OL = size(O),
	released3(<<B/binary, OL, O/binary>>, OL);
released2(#sccp_released{importance = I} = _S, B, O) ->
	Opt = <<O/binary, ?Importance, 1, I/integer>>,
	OptL = size(Opt),
	released3(<<B/binary, OptL, Opt/binary>>, OptL ).

%% @hidden
released3(B, 0) ->
	B;
released3(B, _) ->
	<<B/binary, 0>>.

%% @hidden
sccp_extended_unitdata(#sccp_extended_unitdata{class = Class, hop_counter = Hops,
		called_party = CalledParty, calling_party = CallingParty, data = Data} = S) ->
	CalledPartyB = party_address(CalledParty),
	CallingPartyB = party_address(CallingParty),
	CalledPartyL = size(CalledPartyB),
	CallingPartyL = size(CallingPartyB),
	DataL = size(Data),
	CalledPartyP = 4,
	CallingPartyP = CalledPartyP + CalledPartyL,
	DataP = CallingPartyP + CallingPartyL,
	OptionalP = DataP + DataL -1,
	B = <<?ExtendedUnitData, Class, Hops, CalledPartyP, CallingPartyP, DataP, OptionalP,
			CalledPartyL, CalledPartyB/binary, CallingPartyL, CallingPartyB/binary,
			DataL, Data/binary>>,
	extended_unitdata1(S, B).

%% @hidden
extended_unitdata1(#sccp_extended_unitdata{segmentation = undefined} = S, B) ->
	extended_unitdata2(S, B, <<>>);
extended_unitdata1(#sccp_extended_unitdata{segmentation = Seg} = S, B) ->
	SegB = segmentation(Seg),
	SegL = size(SegB),
	extended_unitdata2(S, B, <<?Segmentation, SegL, SegB/binary>>).

%% @hidden
extended_unitdata2(#sccp_extended_unitdata{importance = undefined} = _S, B, O) ->
	OL = size(O),
	extended_unitdata3(<<B/binary, OL, O/binary>>, OL);
extended_unitdata2(#sccp_extended_unitdata{importance = I} = _S, B, O) ->
	Opt = <<O/binary, ?Importance, 1, I/integer>>,
	OptL = size(Opt),
	extended_unitdata3(<<B/binary, OptL, Opt/binary>>, OptL).

%% @hidden
extended_unitdata3(B, 0) ->
	B;
extended_unitdata3(B, _) ->
	<<B/binary, 0>>.

%% @hidden
sccp_extended_unitdata_service(#sccp_extended_unitdata_service{return_cause = RC,
		hop_counter = Hops, called_party = CalledParty, calling_party = CallingParty,
		data = Data} = S) ->
	Cause = return_cause(RC),
	CalledPartyB = party_address(CalledParty),
	CallingPartyB = party_address(CallingParty),
	CalledPartyL = size(CalledPartyB),
	CallingPartyL = size(CallingPartyB),
	DataL = size(Data),
	CalledPartyP = 4,
	CallingPartyP = CalledPartyP + CalledPartyL,
	DataP = CallingPartyP + CallingPartyL,
	OptionalP = DataP + DataL - 1,
	B = <<?ExtendedUnitDataService, Cause, Hops, CalledPartyP, CallingPartyP, DataP, OptionalP,
			CalledPartyL, CalledPartyB/binary, CallingPartyL, CallingPartyB/binary,
			DataL, Data/binary>>,
	extended_unitdata_service1(S, B).

%% @hidden
extended_unitdata_service1(#sccp_extended_unitdata_service{segmentation = undefined} = S, B) ->
	extended_unitdata_service2(S, B, <<>>);
extended_unitdata_service1(#sccp_extended_unitdata_service{segmentation = Seg} = S, B) ->
	SegB = segmentation(Seg),
	SegL = size(SegB),
	extended_unitdata_service2(S, B, <<?Segmentation, SegL, SegB/binary>>).

%% @hidden
extended_unitdata_service2(#sccp_extended_unitdata_service{importance = undefined} = _S, B, O) ->
	OL = size(O),
	extended_unitdata_service3(<<B/binary, OL, O/binary>>, OL);
extended_unitdata_service2(#sccp_extended_unitdata_service{importance = I} = _S, B, O) ->
	Opt = <<O/binary, ?Importance, 1, I/integer>>,
	OptL = size(Opt),
	extended_unitdata_service3(<<B/binary, OptL, Opt/binary>>, OptL).

%% @hidden
extended_unitdata_service3(B, 0) ->
	B;
extended_unitdata_service3(B, _) ->
	<<B/binary, 0>>.

%% @hidden
sccp_long_unitdata(#sccp_long_unitdata{class = Class, hop_counter = Hops, called_party = CalledParty,
		calling_party = CallingParty, long_data = LongData} = S) ->
	CalledPartyB = party_address(CalledParty),
	CallingPartyB = party_address(CallingParty),
	CalledPartyL = size(CalledPartyB),
	CallingPartyL = size(CallingPartyB),
	LongDataL = size(LongData),
	CalledPartyP = 4,
	CallingPartyP = CalledPartyP + CalledPartyL,
	LongDataP = CallingPartyP + CallingPartyL,
	OptionalP = LongDataP + LongDataL - 1,
	B = <<?LongUnitData, Class, Hops, CalledPartyP, CallingPartyP, LongDataP, OptionalP,
			CalledPartyL, CalledPartyB/binary, CallingPartyL, CallingPartyB/binary,
			LongDataL, LongData/binary>>,
	long_unitdata1(S, B).

%% @hidden
long_unitdata1(#sccp_long_unitdata{segmentation = undefined} = S, B) ->
	long_unitdata2(S, B, <<>>);
long_unitdata1(#sccp_long_unitdata{segmentation = Seg} = S, B) ->
	SegB = segmentation(Seg),
	SegL = size(SegB),
	long_unitdata2(S, B, <<?Segmentation, SegL, SegB/binary>>).

%% @hidden
long_unitdata2(#sccp_long_unitdata{importance = undefined} = _S, B, O) ->
	OL = size(O),
	long_unitdata3(<<B/binary, OL, O/binary>>, OL);
long_unitdata2(#sccp_long_unitdata{importance = I} = _S, B, O) ->
	Opt = <<O/binary, ?Importance, 1, I/integer>>,
	OptL = size(Opt),
	long_unitdata3(<<B/binary, OptL, Opt/binary>>, OptL).

%% @hidden
long_unitdata3(B, 0) ->
	B;
long_unitdata3(B, _) ->
	<<B/binary, 0>>.

%% @hidden
sccp_long_unitdata_service(#sccp_long_unitdata_service{return_cause = RC, hop_counter = Hops,
		called_party = CalledParty, calling_party = CallingParty, long_data = LongData} = S) ->
	Cause = return_cause(RC),
	CalledPartyB = party_address(CalledParty),
	CallingPartyB = party_address(CallingParty),
	CalledPartyL = size(CalledPartyB),
	CallingPartyL = size(CallingPartyB),
	LongDataL = size(LongData),
	CalledPartyP = 4,
	CallingPartyP = CalledPartyP + CalledPartyL,
	LongDataP = CallingPartyP + CallingPartyL,
	OptionalP = LongDataP + LongDataL - 1,
	B = <<?LongUnitDataService, Cause/integer, Hops, CalledPartyP, CallingPartyP, LongDataP, OptionalP,
			CalledPartyL, CalledPartyB/binary, CallingPartyL, CallingPartyB/binary,
			LongDataL, LongData/binary>>,
	long_unitdata_service1(S, B).

%% @hidden
long_unitdata_service1(#sccp_long_unitdata_service{segmentation = undefined} = S, B) ->
	long_unitdata_service2(S, B, <<>>);
long_unitdata_service1(#sccp_long_unitdata_service{segmentation = Seg} = S, B) ->
	SegB = segmentation(Seg),
	SegL = size(SegB),
	long_unitdata_service2(S, B, <<?Segmentation, SegL, SegB/binary>>).

%% @hidden
long_unitdata_service2(#sccp_long_unitdata_service{importance = undefined} = _S, B, O) ->
	OL = size(O),
	long_unitdata_service3(<<B/binary, OL, O/binary>>, OL);
long_unitdata_service2(#sccp_long_unitdata_service{importance = I} = _S, B, O) ->
	Opt = <<O/binary, ?Importance, 1, I/integer>>,
	OptL = size(Opt),
	long_unitdata_service3(<<B/binary, OptL, Opt/binary>>, OptL).

%% @hidden
long_unitdata_service3(B, 0) ->
	B;
long_unitdata_service3(B, _) ->
	<<B/binary, 0>>.

