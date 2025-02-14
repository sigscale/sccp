%%% sccp_codec.erl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2015-2025 SigScale Global Inc.
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
-copyright('Copyright (c) 2015-2025 SigScale Global Inc.').
-author('vances@sigscale.org').

%% SCCP message codec funcion
-export([sccp/1]).
%% SCCP options codec funcion
-export([party_address/1, nai/1, numbering_plan/1, routing_indicator/1,
		importance/1, refusal_cause/1, release_cause/1, segmenting/1,
		return_cause/1, reset_cause/1, segmentation/1, point_code/1,
		global_title/1, bcd/1, bcd/2, ssn/1]).

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

%%
%% public api functionas
%%

-spec sccp(Message) -> Message
	when
		Message :: binary() | sccp_message().
%% @doc SCCP message CODEC.
%%
%% ITU-T Recommendation Q.713, SCCP formats and codes.
%%
sccp(<<?ConnectRequest, SrcLocalRef:24/little, Class,
		CalledPartyP, OptionalP, Rest/binary>> = _Message) ->
	CalledPartyL = binary:at(Rest, CalledPartyP - 2),
	CalledPartyB = binary:part(Rest, CalledPartyP - 1, CalledPartyL),
	Address = party_address(CalledPartyB),
	Opts = optional_part(OptionalP, Rest),
	#sccp_connection_req{src_local_ref = SrcLocalRef,
			class = Class, called_party = Address,
			credit = get_option(?Credit, Opts),
			calling_party = get_option(?CallingPartyAddress, Opts),
			data = get_option(?Data, Opts),
			hop_counter = get_option(?HopCounter, Opts),
			importance = get_option(?Importance, Opts)};
sccp(<<?ConnectionConfirm, DestLocalRef:24/little, SrcLocalRef:24/little,
		Class, OptionalP, Rest/binary>>) ->
	Opts = optional_part(OptionalP, Rest),
	#sccp_connection_confirm{dest_local_ref = DestLocalRef,
			src_local_ref = SrcLocalRef, class = Class,
			credit = get_option(?Credit, Opts),
			called_party = get_option(?CalledPartyAddress, Opts),
			data = get_option(?Data, Opts),
			importance = get_option(?Importance, Opts)};
sccp(<<?ConnectionRefused, DestLocalRef:24/little,
		Refuse, OptionalP, Rest/binary>>) ->
	Opts = optional_part(OptionalP, Rest),
	#sccp_connection_refused{dest_local_ref = DestLocalRef,
			refusal_cause = refusal_cause(Refuse),
			called_party = get_option(?CalledPartyAddress, Opts),
			data = get_option(?Data, Opts),
			importance = get_option(?Importance, Opts)};
sccp(<<?Released, DestLocalRef:24/little, SrcLocalRef:24/little,
		Release, OptionalP, Rest/binary>>) ->
	Opts = optional_part(OptionalP, Rest),
	#sccp_released{dest_local_ref = DestLocalRef,
			src_local_ref = SrcLocalRef,
			release_cause = release_cause(Release),
			data = get_option(?Data, Opts),
			importance = get_option(?Importance, Opts)};
sccp(<<?ReleaseComplete,
		DestLocalRef:24/little, SrcLocalRef:24/little>>) ->
	#sccp_release_complete{dest_local_ref = DestLocalRef,
			src_local_ref = SrcLocalRef};
sccp(<<?DataForm1, DestLocalRef:24/little,
		_:7, Seg:1, DataP, Rest/binary>>) ->
	DataL = binary:at(Rest, DataP - 1),
	Data = binary:part(Rest, DataP, DataL),
	#sccp_data_form1{dest_local_ref = DestLocalRef,
			segmenting = segmenting(Seg), data = Data};
sccp(<<?DataForm2, DestLocalRef:24/little,
		Sequencing:2/binary, DataP, Rest/binary>>) ->
	DataL = binary:at(Rest, DataP - 1),
	Data = binary:part(Rest, DataP, DataL),
	#sccp_data_form2{dest_local_ref = DestLocalRef,
			sequencing = sequencing(Sequencing), data = Data};
sccp(<<?DataAck, DestLocalRef:24/little,
		SequencingB:2/binary, Credit>>) ->
	Sequencing = sequencing(SequencingB),
	#sccp_data_ack{dest_local_ref = DestLocalRef,
			sequencing = Sequencing, credit = Credit};
sccp(<<?UnitData, Class, CalledPartyP, CallingPartyP,
		DataP, Rest/binary>>) ->
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
sccp(<<?UnitDataService, RC/integer, CalledPartyP, CallingPartyP,
		DataP, Rest/binary>>) ->
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
sccp(<<?ExpeditedData, DestLocalRef:24/little, DataP, Rest/binary>>) ->
	DataL = binary:at(Rest, DataP - 1),
	Data = binary:part(Rest, DataP, DataL),
	#sccp_expedited_data{dest_local_ref = DestLocalRef, data = Data };
sccp(<<?ExpeditedDataAck, DestLocalRef:24/little>>) ->
	#sccp_expedited_ack{dest_local_ref = DestLocalRef};
sccp(<<?ResetRequest,
		DestLocalRef:24/little, SrcLocalRef:24/little, Reset/integer, _/binary>>) ->
	Cause = reset_cause(Reset),
	#sccp_reset_request{dest_local_ref = DestLocalRef,
			src_local_ref = SrcLocalRef, reset_cause = Cause};
sccp(<<?ResetConfirmation,
		DestLocalRef:24/little, SrcLocalRef:24/little>>) ->
	#sccp_reset_confirmation{dest_local_ref = DestLocalRef,
			src_local_ref = SrcLocalRef};
sccp(<<?ProtocolDataUnitError, DestLocalRef:24/little, Error, _/binary>>) ->
	#sccp_protocol_data_unit_error{dest_local_ref = DestLocalRef,
			error_cause =  Error};
sccp(<<?InactivityTest, DestLocalRef:24/little, SrcLocalRef:24/little,
		Class, Sequencing:2/binary, Credit>>) ->
	#sccp_inactivity_test{dest_local_ref = DestLocalRef,
			src_local_ref = SrcLocalRef, class = Class,
			sequencing = sequencing(Sequencing), credit = Credit};
sccp(<<?ExtendedUnitData, Class, Hops, CalledPartyP, CallingPartyP,
		DataP, OptionalP, Rest/binary>>) ->
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
sccp(<<?ExtendedUnitDataService, RC, Hops, CalledPartyP, CallingPartyP,
		DataP, OptionalP, Rest/binary>>) ->
	Return = return_cause(RC),
	CalledPartyL = binary:at(Rest, CalledPartyP - 4),
	CallingPartyL = binary:at(Rest, CallingPartyP - 3),
	DataL = binary:at(Rest, DataP - 2),
	CalledPartyB = binary:part(Rest, CalledPartyP - 3, CalledPartyL),
	CallingPartyB = binary:part(Rest, CallingPartyP - 2, CallingPartyL),
	Data = binary:part(Rest, DataP - 1, DataL),
	Opts = optional_part(OptionalP, Rest),
	#sccp_extended_unitdata_service{return_cause = Return,
			hop_counter = Hops, called_party = party_address(CalledPartyB),
			calling_party = party_address(CallingPartyB), data = Data,
			segmentation = get_option(?Segmentation, Opts),
			importance = get_option(?Importance, Opts)};
sccp(<<?LongUnitData, Class, Hops, CalledPartyP:16/little,
		CallingPartyP:16/little, DataP:16/little, OptionalP:16/little,
		Rest/binary>>) ->
	CalledPartyL = binary:decode_unsigned(binary:part(Rest,
			CalledPartyP - 8, 2), little),
	CallingPartyL = binary:decode_unsigned(binary:part(Rest,
			CallingPartyP - 6, 2), little),
	DataL = binary:decode_unsigned(binary:part(Rest,
			DataP - 4, 2), little),
	CalledPartyB = binary:part(Rest, CalledPartyP - 6, CalledPartyL),
	CallingPartyB = binary:part(Rest, CallingPartyP - 4, CallingPartyL),
	Data = binary:part(Rest, DataP - 2, DataL),
	Opts = optional_part_long(OptionalP, Rest),
	#sccp_long_unitdata{class = Class, hop_counter = Hops,
			called_party = party_address(CalledPartyB),
			calling_party = party_address(CallingPartyB),
			data = Data,
			segmentation = get_option(?Segmentation, Opts),
			importance = get_option(?Importance, Opts)};
sccp(<<?LongUnitDataService, RC, Hops, CalledPartyP:16/little,
		CallingPartyP:16/little, DataP:16/little, OptionalP:16/little,
		Rest/binary>>) ->
	Return = return_cause(RC),
	CalledPartyL = binary:decode_unsigned(binary:part(Rest,
			CalledPartyP - 8, 2), little),
	CallingPartyL = binary:decode_unsigned(binary:part(Rest,
			CallingPartyP - 6, 2), little),
	DataL = binary:decode_unsigned(binary:part(Rest,
			DataP - 4, 2), little),
	CalledPartyB = binary:part(Rest, CalledPartyP - 6, CalledPartyL),
	CallingPartyB = binary:part(Rest, CallingPartyP - 4, CallingPartyL),
	Data = binary:part(Rest, DataP - 2, DataL),
	Opts = optional_part_long(OptionalP, Rest),
	#sccp_long_unitdata_service{return_cause = Return, hop_counter = Hops,
			called_party = party_address(CalledPartyB),
			calling_party = party_address(CallingPartyB),
			data = Data,
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
sccp(#sccp_release_complete{dest_local_ref = Dest,
		src_local_ref = Src} = _S) when is_integer(Dest),
		is_integer(Src) ->
	<<?ReleaseComplete, Dest:24/little, Src:24/little>>;
sccp(#sccp_data_form1{dest_local_ref = Dest,
		segmenting = S, data = Data}) when is_integer(Dest),
		is_boolean(S), is_binary(Data) ->
	Seg = segmenting(S),
	DataL = byte_size(Data),
	DataP = 1,
	<<?DataForm1, Dest:24/little, Seg, DataP, DataL, Data/binary>>;
sccp(#sccp_data_form2{dest_local_ref = Dest,
		sequencing = Sequencing, data = Data}) when is_integer(Dest),
		is_record(Sequencing, sequencing), is_binary(Data) ->
	SequencingB = sequencing(Sequencing),
	DataL = byte_size(Data),
	DataP = 1,
	<<?DataForm2, Dest:24/little,
			SequencingB/binary, DataP, DataL, Data/binary>>;
sccp(#sccp_data_ack{dest_local_ref = Dest,
		sequencing = Sequencing, credit = Credit})
		when is_integer(Dest), is_record(Sequencing, sequencing),
		is_integer(Credit) ->
		SequencingB = sequencing(Sequencing),
	<<?DataAck, Dest:24/little, SequencingB/binary, Credit>>;
sccp(#sccp_unitdata{class = Class, data = Data,
		called_party = CalledParty, calling_party = CallingParty})
		when is_integer(Class), is_binary(Data),
		is_record(CalledParty, party_address),
		is_record(CallingParty, party_address) ->
	CalledPartyB = party_address(CalledParty),
	CallingPartyB = party_address(CallingParty),
	CalledPartyL = byte_size(CalledPartyB),
	CallingPartyL = byte_size(CallingPartyB),
	DataL = byte_size(Data),
	CalledPartyP = 3,
	CallingPartyP = CalledPartyP + CalledPartyL,
	DataP = CallingPartyP + CallingPartyL,
	<<?UnitData, Class, CalledPartyP, CallingPartyP, DataP,
			CalledPartyL, CalledPartyB/binary, CallingPartyL,
			CallingPartyB/binary, DataL, Data/binary>>;
sccp(#sccp_unitdata_service{return_cause = RC,
		called_party = CalledParty, calling_party = CallingParty,
		data = Data}) when is_record(CalledParty, party_address),
		is_record(CallingParty, party_address),
		is_binary(Data) ->
	Return = return_cause(RC),
	CalledPartyB = party_address(CalledParty),
	CallingPartyB = party_address(CallingParty),
	CalledPartyL = byte_size(CalledPartyB),
	CallingPartyL = byte_size(CallingPartyB),
	DataL = byte_size(Data),
	CalledPartyP = 3,
	CallingPartyP = CalledPartyP + CalledPartyL,
	DataP = CallingPartyP + CallingPartyL,
	<<?UnitDataService, Return, CalledPartyP, CallingPartyP,
			DataP, CalledPartyL, CalledPartyB/binary, CallingPartyL,
			CallingPartyB/binary, DataL, Data/binary>>;
sccp(#sccp_expedited_data{dest_local_ref = Dest, data = Data})
		when is_integer(Dest), is_binary(Data) ->
	DataL = byte_size(Data),
	DataP = 1,
	<<?ExpeditedData, Dest:24/little, DataP, DataL, Data/binary>>;
sccp(#sccp_expedited_ack{dest_local_ref = Dest})
		when is_integer(Dest) ->
	<<?ExpeditedDataAck, Dest:24/little>>;
sccp(#sccp_reset_request{dest_local_ref = Dest,
		src_local_ref = Src, reset_cause = Cause})
		when is_integer(Dest), is_integer(Src) ->
	Reset = reset_cause(Cause),
	OptionalP = 1,
	<<?ResetRequest, Dest:24/little, Src:24/little, Reset, OptionalP>>;
sccp(#sccp_reset_confirmation{dest_local_ref = Dest,
		src_local_ref = Src}) when is_integer(Dest),
		is_integer(Src) ->
	<<?ResetConfirmation, Dest:24/little, Src:24/little>>;
sccp(#sccp_protocol_data_unit_error{dest_local_ref = Dest,
		error_cause = Error}) when is_integer(Dest),
		is_integer(Error) ->
	OptionalP = 1,
	<<?ProtocolDataUnitError, Dest:24/little, Error, OptionalP>>;
sccp(#sccp_inactivity_test{dest_local_ref = Dest,
		src_local_ref = Src, class = Class,
		sequencing = Sequencing, credit = Credit})
		when is_integer(Dest), is_integer(Src), is_integer(Class),
		is_record(Sequencing, sequencing), is_integer(Credit) ->
	SequencingB = sequencing(Sequencing),
	<<?InactivityTest, Dest:24/little, Src:24/little,
			Class, SequencingB/binary, Credit>>;
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
%% @doc Called/calling party address CODEC.
%%
%% ITU-T Recommendation Q.713, section 3.4.
%%
party_address(<<_:1, RI:1, GTI:4, SSNI:1, PCI:1,
		Address/binary>> = _Address) ->
	party_address1(PCI, SSNI, RI, GTI, Address);
party_address(#party_address{ri = RoutingIndicator,
		pc = undefined, ssn = SubSystemNumber} = P)
		when is_integer(SubSystemNumber),
		((RoutingIndicator == route_on_ssn)
				orelse (RoutingIndicator == route_on_gt)) ->
	RI = routing_indicator(RoutingIndicator),
	SSN = ssn(SubSystemNumber),
	party_address4(0, 1, RI, SSN, P);
party_address(#party_address{ri = RoutingIndicator,
		pc = PointCode, ssn = undefined} = P)
		when is_integer(PointCode),
		((RoutingIndicator == route_on_ssn)
				orelse (RoutingIndicator == route_on_gt)) ->
	RI = routing_indicator(RoutingIndicator),
	PC = point_code(PointCode),
	party_address4(1, 0, RI, PC, P);
party_address(#party_address{ri = RoutingIndicator,
		pc = PointCode, ssn = SubSystemNumber} = P)
		when is_integer(SubSystemNumber),
		is_integer(PointCode),
		((RoutingIndicator == route_on_ssn)
				orelse (RoutingIndicator == route_on_gt)) ->
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
party_address3(RI, 3, <<TT, NP:4, 1:4, GT/binary>>, P) ->
	Title = bcd(GT, 1),
	P#party_address{ri = routing_indicator(RI),
			translation_type = TT,
			numbering_plan = numbering_plan(NP),
			gt = Title};
party_address3(RI, 3, <<TT, NP:4, 2:4, GT/binary>>, P) ->
	Title = bcd(GT, 0),
	P#party_address{ri = routing_indicator(RI),
			translation_type = TT,
			numbering_plan = numbering_plan(NP),
			gt = Title};
party_address3(RI, 4, <<TT, NP:4, 1:4, _:1, NAI:7, GT/binary>>, P) ->
	Title = bcd(GT, 1),
	P#party_address{ri = routing_indicator(RI),
			translation_type = TT,
			numbering_plan = numbering_plan(NP),
			nai = nai(NAI), gt = Title};
party_address3(RI, 4, <<TT, NP:4, 2:4, _:1, NAI:7, GT/binary>>, P) ->
	Title = bcd(GT, 0),
	P#party_address{ri = routing_indicator(RI),
			translation_type = TT,
			numbering_plan = numbering_plan(NP),
			nai = nai(NAI), gt = Title}.
%% @hidden
party_address4(PCI, SSNI, RI,  Address,
		#party_address{gt = undefined, translation_type = undefined,
		numbering_plan = undefined, nai = undefined}) ->
	<<0:1, RI:1, 0:4, SSNI:1, PCI:1, Address/binary>>;
party_address4(PCI, SSNI, RI,  Address,
		#party_address{gt = GlobalTitle, nai = NatureOfAddress,
		translation_type = undefined, numbering_plan = undefined})
		when is_list(GlobalTitle), NatureOfAddress /= undefined ->
	OE = length(GlobalTitle) rem 2,
	NAI = nai(NatureOfAddress),
	GT = bcd(GlobalTitle),
	<<0:1, RI:1, 1:4, SSNI:1, PCI:1, Address/binary, OE:1, NAI:7, GT/binary>>;
party_address4(PCI, SSNI, RI, Address,
		#party_address{gt = GlobalTitle, translation_type = TT,
		numbering_plan = undefined, nai = undefined})
		when TT /= undefined, is_list(GlobalTitle) ->
	GT = bcd(GlobalTitle),
	<<0:1, RI:1, 2:4, SSNI:1, PCI:1, Address/binary, TT, GT/binary>>;
party_address4(PCI, SSNI, RI, Address,
		#party_address{gt = GlobalTitle, translation_type = TT,
		numbering_plan = NumberingPlan, nai = undefined})
		when NumberingPlan /= undefined, TT /= undefined,
		is_list(GlobalTitle) ->
	NP = numbering_plan(NumberingPlan),
	ES = case length(GlobalTitle) rem 2 of
		0 -> 2;
		1 -> 1
	end,
	GT = bcd(GlobalTitle),
	<<0:1, RI:1, 3:4, SSNI:1, PCI:1, Address/binary,
			TT, NP:4, ES:4, GT/binary>>;
party_address4(PCI, SSNI, RI, Address,
		#party_address{gt = GlobalTitle, translation_type = TT,
		numbering_plan = NumberingPlan, nai = NatureOfAddress})
		when TT /= undefined, NumberingPlan /= undefined,
		NatureOfAddress /= undefined, is_list(GlobalTitle) ->
	NP = numbering_plan(NumberingPlan),
	ES = case length(GlobalTitle) rem 2 of
		0 -> 2;
		1 -> 1
	end,
	NAI = nai(NatureOfAddress),
	GT = bcd(GlobalTitle),
	<<0:1, RI:1, 4:4, SSNI:1, PCI:1, Address/binary,
			TT, NP:4, ES:4, 0:1, NAI:7, GT/binary>>.

-spec nai(NAI) -> NAI
	when
		NAI :: unknown | subscriber | reserved_for_national | national
				| international | spare | reserved_for_national
				| reserved | 0..127.
%% @doc Network address indicator CODEC.
%%
%% ITU-T Recommendation Q.713, section 3.4.2.3.1.
%%
nai(0 = _NAI) -> unknown;
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

-spec numbering_plan(NP) -> NP
	when
		NP :: unknown | isdn_tele | generic | data | telex | maritime
				| land_mobile | isdn_mobile | private_net | reserved | 0..15.
%% @doc Numbering plan CODEC.
%%
%% ITU-T Recommendation Q.713, section 3.4.2.3.3.
%%
numbering_plan(0 = _NP) -> unknown;
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

-spec routing_indicator(RI) -> RI
	when
		RI:: route_on_ssn | route_on_gt | 0..1.
%% @doc Routing indicator CODEC.
%%
%% ITU-T Recommendation Q.713, section 3.4.1.
routing_indicator(0 = _RI) -> route_on_gt;
routing_indicator(1) -> route_on_ssn;
routing_indicator(route_on_gt) -> 0;
routing_indicator(route_on_ssn) -> 1.

-spec importance(Importance) -> Importance
	when
		Importance :: binary() | 0..7.
%% @doc Importance CODEC.
%%
%% ITU-T Recommendation Q.713, section 3.19.
%%
importance(<<_:5, I:3>> = _Importance) ->
	I;
importance(I) when I >= 0; I =< 7 ->
	<<0:5, I:3>>.

-spec refusal_cause(Cause) -> Cause
	when
		Cause :: enduser_orig | enduser_congestion | enduser_failure
				| sccp_user_oirg | dest_unknown | dest_inaccessible
				| qos_not_avail_non_transient | qos_not_avail_transient
				| access_failure | access_congestion | subsystem_failure
				| subsystem_congestion | connection_expire | incomp_userdata
				| unqualified | hcounter_violation | sccp_failure
				| no_translation_addr | unequipped_error | 0..255.
%% @doc Refusal cause CODEC.
%%
%% ITU-T Recommendation Q.713, section 3.15.
%%
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
%% @doc Release cause CODEC.
%%
%% ITU-T Recommendation Q.713, section 3.11.
%%
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
%% @doc Reset cause CODEC.
%%
%% ITU-T Recommendation Q.713, section 3.13.
%%
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
%% @doc Segmenting/reassembling CODEC.
%%
%% ITU-T Recommendation Q.713, section 3.7.
%%
segmenting(0 = _Segmenting) -> false;
segmenting(1) -> true;
segmenting(false) -> 0;
segmenting(true) -> 1.

-spec sequencing(Sequencing) -> Sequencing
	when
		Sequencing :: binary() | #sequencing{}.
%% @doc Sequencing/segmenting CODEC.
%%
%% ITU-T Recommendation Q.713, section 3.9.
%%
sequencing(<<SendSeqNum:7, _:1, ReceiveSeqNum:7,
		0:1>> = _Sequencing) ->
	#sequencing{send_seq_num = SendSeqNum,
			receive_seq_num = ReceiveSeqNum, more_data = false};
sequencing(<<SendSeqNum:7, _:1, ReceiveSeqNum:7, 1:1>>) ->
	#sequencing{send_seq_num = SendSeqNum,
			receive_seq_num = ReceiveSeqNum, more_data = true};
sequencing(#sequencing{send_seq_num = SendSeqNum,
		receive_seq_num = ReceiveSeqNum, more_data = false})
		when is_integer(ReceiveSeqNum), is_integer(ReceiveSeqNum) ->
	<<SendSeqNum:7, 0:1, ReceiveSeqNum:7, 0:1>>;
sequencing(#sequencing{send_seq_num = SendSeqNum,
		receive_seq_num = ReceiveSeqNum, more_data = true})
		when is_integer(ReceiveSeqNum), is_integer(ReceiveSeqNum) ->
	<<SendSeqNum:7, 0:1, ReceiveSeqNum:7, 1:1>>.

-spec return_cause(Cause) -> Cause
	when
		Cause :: no_translation | subsystem_congestion | subsystem_failure
				| unequipped_error | mtp_failure | network_congestion
				| unqualified | transport_error | processing_error
				| reassembly_fail | sccp_failure | hcounter_violation
				| seg_not_supported | seg_failure | 0..255.
%% @doc Return cause CODEC.
%%
%% ITU-T Recommendation Q.713, section 3.12.
%%
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
%% @doc Segmentation CODEC.
%%
%% ITU-T Recommendation Q.713, section 3.17.
%%
segmentation(<<0:1, C:1, _:2, RemSeg:4,
		SegLocalRef:24/little>> = _Segmenting) ->
	#segmentation{first = false, class = C,
			remaining_seg = RemSeg, seg_local_ref = SegLocalRef};
segmentation(<<1:1, C:1, _:2, RemSeg:4,
		SegLocalRef:24/little>>) ->
	#segmentation{first = true, class = C,
			remaining_seg = RemSeg, seg_local_ref = SegLocalRef};
segmentation(#segmentation{first = false, class = C,
		remaining_seg = R, seg_local_ref = SegLocalRef})
		when is_integer(R), is_integer(SegLocalRef) ->
	<<0:1, C:1, 0:2, R:4, SegLocalRef:24/little>>;
segmentation(#segmentation{first = true, class = C,
		remaining_seg = R, seg_local_ref = SegLocalRef})
		when is_integer(R), is_integer(SegLocalRef) ->
	<<1:1, C:1, 0:2, R:4, SegLocalRef:24/little>>.

-spec point_code(PC) -> PC
	when
		PC :: undefined | 0..16383 | binary().
%% @doc Signalling point code (PC) CODEC.
%%
%% ITU-T Recommendation Q.713, section 3.4.2.1.
%%
point_code(undefined = _PC) ->
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
%% @doc Subsystem number (SSN) CODEC.
%%
%% ITU-T Recommendation Q.713, section 3.4.2.2.
%%
ssn(undefined = _SSN) ->
	<<>>;
ssn(<<>>) ->
	undefined;
ssn(<<N:8/integer>>) ->
	N;
ssn(N) when is_integer(N) ->
	<<N:8>>.

-spec global_title(GT) -> GT
	when
		GT :: [Digit] | [NumChar],
		Digit :: 0..15,
		NumChar :: $0..$9.
%% @doc Global title address signal CODEC.
%%
%% ITU-T Recommendation Q.713, section 3.4.2.3.1
%%
global_title(GT) ->
	global_title1(GT, []).
%% @hidden
global_title1([$0 | T], Acc) ->
	global_title1(T, [0 | Acc]);
global_title1([$1 | T], Acc) ->
	global_title1(T, [1 | Acc]);
global_title1([$2 | T], Acc) ->
	global_title1(T, [2 | Acc]);
global_title1([$3 | T], Acc) ->
	global_title1(T, [3 | Acc]);
global_title1([$4 | T], Acc) ->
	global_title1(T, [4 | Acc]);
global_title1([$5 | T], Acc) ->
	global_title1(T, [5 | Acc]);
global_title1([$6 | T], Acc) ->
	global_title1(T, [6 | Acc]);
global_title1([$7 | T], Acc) ->
	global_title1(T, [7 | Acc]);
global_title1([$8 | T], Acc) ->
	global_title1(T, [8 | Acc]);
global_title1([$9 | T], Acc) ->
	global_title1(T, [9 | Acc]);
global_title1([0 | T], Acc) ->
	global_title1(T, [$0 | Acc]);
global_title1([1 | T], Acc) ->
	global_title1(T, [$1 | Acc]);
global_title1([2 | T], Acc) ->
	global_title1(T, [$2 | Acc]);
global_title1([3 | T], Acc) ->
	global_title1(T, [$3 | Acc]);
global_title1([4 | T], Acc) ->
	global_title1(T, [$4 | Acc]);
global_title1([5 | T], Acc) ->
	global_title1(T, [$5 | Acc]);
global_title1([6 | T], Acc) ->
	global_title1(T, [$6 | Acc]);
global_title1([7 | T], Acc) ->
	global_title1(T, [$7 | Acc]);
global_title1([8 | T], Acc) ->
	global_title1(T, [$8 | Acc]);
global_title1([9 | T], Acc) ->
	global_title1(T, [$9 | Acc]);
global_title1([], Acc) ->
	lists:reverse(Acc).

-spec bcd(Digits) -> BCD
	when
		Digits :: [0..15],
		BCD :: binary().
%% @doc Encode as binary coded decimal (BCD).
bcd(Digits) when is_list(Digits) ->
	bcd2(Digits, <<>>).

-spec bcd(BCD, OE) -> Digits
	when
		BCD :: binary(),
		OE :: Odd | Even,
		Odd :: 1 | bcd_odd,
		Even :: 0 | bcd_even,
		Digits :: [0..15].
%% @doc Decode binary coded decimal (BCD).
%%
%% `OE' indicates odd (1) or even (0) number of address
%%     signals present in a global address information.
%%
%% ITU-T Recommendation Q.713, section 3.4.2.3.1.
%%
bcd(BCD, bcd_odd = _OE) ->
	bcd(BCD, 1);
bcd(BCD, bcd_even) ->
	bcd(BCD, 0);
bcd(BCD, OE) when is_binary(BCD) ->
	bcd1(BCD, OE, []).
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

-spec get_option(Key, Options) -> Result
	when
		Key :: byte(),
		Options :: [{Key, Value}],
		Value :: term(),
		Result :: undefined | Value.
%% @doc Get option from SCCP optional parameters.
%% @private
get_option(Key, Options) ->
	case lists:keyfind(Key, 1, Options) of
		{_Key, Value} ->
			Value;
		false ->
			undefined
	end.

-spec optional_part(OptionalP, Rest) -> Options
	when
		OptionalP :: byte(),
		Rest :: binary(),
		Options :: [{Key, Value}],
		Key :: byte(),
		Value :: term().
%% @doc Extract optional part from an SCCP message.
%% @private
optional_part(OptionalP, Rest) when OptionalP > 0 ->
	Pos = OptionalP - 1,
	Len = byte_size(Rest) - Pos,
	OptionalPart = binary:part(Rest, Pos, Len),
	optional_part1(OptionalPart, []);
optional_part(OptionalP, _Rest) when OptionalP == 0 ->
	[].
%% @hidden
optional_part1(<<?CalledPartyAddress, Len, Rest/binary>>, Acc) ->
	<<V:Len/binary, Rest1/binary>> = Rest,
	optional_part1(Rest1, [{?CalledPartyAddress, party_address(V)} | Acc]);
optional_part1(<<?CallingPartyAddress, Len, Rest/binary>>, Acc) ->
	<<V:Len/binary, Rest1/binary>> = Rest,
	optional_part1(Rest1, [{?CallingPartyAddress, party_address(V)} | Acc]);
optional_part1(<<?Credit, 1, Rest/binary>>, Acc) ->
	<<V:8, Rest1/binary>> = Rest,
	optional_part1(Rest1, [{?Credit, V} | Acc]);
optional_part1(<<?Data, L, Rest/binary>>, Acc) ->
	<<V:L/binary, Rest1/binary>> = Rest,
	optional_part1(Rest1, [{?Data, V} | Acc]);
optional_part1(<<?Segmentation, 4, Rest/binary>>, Acc) ->
	<<V:4/binary, Rest1/binary>> = Rest,
	optional_part1(Rest1, [{?Segmentation, segmentation(V)} | Acc]);
optional_part1(<<?HopCounter, 1, Rest/binary>>, Acc) ->
	<<V:8, Rest1/binary>> = Rest,
	optional_part1(Rest1, [{?HopCounter, V} | Acc]);
optional_part1(<<?Importance, 1, Rest/binary>>, Acc) ->
	<<_:4, V:4, Rest1/binary>> = Rest,
	optional_part1(Rest1, [{?Importance, V} | Acc]);
optional_part1(<<?EndOfOptionalParameters, _Rest/binary>>, Acc) ->
	Acc.

-spec optional_part_long(OptionalP, Rest) -> Options
	when
		OptionalP :: integer(),
		Rest :: binary(),
		Options :: [{Key, Value}],
		Key :: byte(),
		Value :: term().
%% @doc Extract optional part from an LUDT/LUDT SSCCP message.
%% @private
optional_part_long(OptionalP, Rest) when OptionalP > 0 ->
	Pos = OptionalP - 2,
	Len = byte_size(Rest) - Pos,
	OptionalPart = binary:part(Rest, Pos, Len),
	optional_part_long1(OptionalPart, []);
optional_part_long(OptionalP, _Rest) when OptionalP == 0 ->
	[].

%% @hidden
%% @todo avoid creating new binaries
optional_part_long1(<<?CalledPartyAddress, Len:16/little, Rest/binary>>, Acc) ->
	<<V:Len/binary, Rest1/binary>> = Rest,
	optional_part_long1(Rest1, [{?CalledPartyAddress, party_address(V)} | Acc]);
optional_part_long1(<<?CallingPartyAddress, Len:16/little, Rest/binary>>, Acc) ->
	<<V:Len/binary, Rest1/binary>> = Rest,
	optional_part_long1(Rest1, [{?CallingPartyAddress, party_address(V)} | Acc]);
optional_part_long1(<<?Credit, 2:16/little, Rest/binary>>, Acc) ->
	<<V:8, Rest1/binary>> = Rest,
	optional_part_long1(Rest1, [{?Credit, V} | Acc]);
optional_part_long1(<<?Data, L:16/little, Rest/binary>>, Acc) ->
	<<V:L/binary, Rest1/binary>> = Rest,
	optional_part_long1(Rest1, [{?Data, V} | Acc]);
optional_part_long1(<<?Segmentation, 4:16/little, Rest/binary>>, Acc) ->
	<<V:4/binary, Rest1/binary>> = Rest,
	optional_part_long1(Rest1, [{?Segmentation, segmentation(V)} | Acc]);
optional_part_long1(<<?HopCounter, 1:16/little, Rest/binary>>, Acc) ->
	<<V:8, Rest1/binary>> = Rest,
	optional_part_long1(Rest1, [{?HopCounter, V} | Acc]);
optional_part_long1(<<?Importance, 1:16/little, Rest/binary>>, Acc) ->
	<<_:4, V:4, Rest1/binary>> = Rest,
	optional_part_long1(Rest1, [{?Importance, V} | Acc]);
optional_part_long1(<<?EndOfOptionalParameters, _Rest/binary>>, Acc) ->
	Acc.

%%
%% internal functionas
%%

%% @hidden
sccp_connection_req(#sccp_connection_req{src_local_ref = Src,
		class = Class, called_party = CalledParty} = R)
		when is_integer(Class), is_integer(Src),
		is_record(CalledParty, party_address) ->
	CalledPartyB = party_address(CalledParty),
	CalledPartyP = 2,
	CalledPartyL = byte_size(CalledPartyB),
	OptionalP = CalledPartyP + CalledPartyL,
	B = <<?ConnectRequest, Src:24/little, Class, CalledPartyP,
			OptionalP, CalledPartyL, CalledPartyB/binary>>,
	connection_req1(R, B).

%% @hidden
connection_req1(#sccp_connection_req{credit = undefined} = R, B) ->
	connection_req2(R, B);
connection_req1(#sccp_connection_req{credit = Credit} = R, B)
		when is_integer(Credit) ->
	connection_req2(R, <<B/binary, ?Credit, 1, Credit>>).

%% @hidden
connection_req2(#sccp_connection_req{calling_party = undefined} = R, B) ->
	connection_req3(R, B);
connection_req2(#sccp_connection_req{calling_party = CP} = R, B)
		when is_record(CP, party_address) ->
	CPB = party_address(CP),
	CPL = byte_size(CPB),
	connection_req3(R, <<B/binary, ?CallingPartyAddress, CPL, CPB/binary>>).

%% @hidden
connection_req3(#sccp_connection_req{data = undefined} = R, B) ->
	connection_req4(R, B);
connection_req3(#sccp_connection_req{data = D} = R, B)
		when is_binary(D) ->
	DL = byte_size(D),
	connection_req4(R, <<B/binary, ?Data, DL, D/binary>>).

%% @hidden
connection_req4(#sccp_connection_req{hop_counter = undefined} = R, B) ->
	connection_req5(R, B);
connection_req4(#sccp_connection_req{hop_counter = H} = R, B)
		when is_integer(H) ->
	connection_req5(R, <<B/binary, ?HopCounter, 1, H>>).

%% @hidden
connection_req5(#sccp_connection_req{importance = undefined}, B) ->
	<<B/binary, 0>>;
connection_req5(#sccp_connection_req{importance = I}, B)
		when is_integer(I) ->
	<<B/binary, ?Importance, 1, I, 0>>.

%% @hidden
sccp_connection_confirm(#sccp_connection_confirm{dest_local_ref = Dest,
		src_local_ref = Src, class = Class} = R)
		when is_integer(Dest), is_integer(Src), is_integer(Class) ->
	OptionalP = 1,
	B = <<?ConnectionConfirm, Dest:24/little, Src:24/little, Class/integer, OptionalP>>,
	connection_confirm1(R, B).

%% @hidden
connection_confirm1(#sccp_connection_confirm{credit = undefined} = R, B) ->
	connection_confirm2(R, B);
connection_confirm1(#sccp_connection_confirm{credit = Credit} = R, B) ->
	connection_confirm2(R, <<B/binary, ?Credit, 1, Credit>>).

%% @hidden
connection_confirm2(#sccp_connection_confirm{called_party = undefined} = R, B) ->
	connection_confirm3(R, B);
connection_confirm2(#sccp_connection_confirm{called_party= CP} = R, B) ->
	CPB = party_address(CP),
	CPL = byte_size(CPB),
	connection_confirm3(R, <<B/binary, ?CalledPartyAddress, CPL, CPB/binary>>).

%% @hidden
connection_confirm3(#sccp_connection_confirm{data= undefined} = R, B) ->
	connection_confirm4(R, B);
connection_confirm3(#sccp_connection_confirm{data = D} = R, B) ->
	DL = byte_size(D),
	connection_confirm4(R, <<B/binary, ?Data, DL, D/binary>>).

%% @hidden
connection_confirm4(#sccp_connection_confirm{importance = undefined}, B) ->
	<<B/binary, 0>>;
connection_confirm4(#sccp_connection_confirm{importance = I}, B) ->
	<<B/binary, ?Importance, 1, I, 0>>.

%% @hidden
sccp_connection_refused(#sccp_connection_refused{dest_local_ref = Dest,
		refusal_cause = RC} = R) when is_integer(Dest) ->
	Cause = refusal_cause(RC),
	OptionalP = 1,
	B = <<?ConnectionRefused, Dest:24/little, Cause, OptionalP>>,
	connection_refused1(R, B).

%% @hidden
connection_refused1(#sccp_connection_refused{called_party = undefined} = R, B) ->
	connection_refused2(R, B);
connection_refused1(#sccp_connection_refused{called_party = CP} = R, B)
		when is_record(CP, party_address) ->
	CPB = party_address(CP),
	CPL = byte_size(CPB),
	connection_refused2(R, <<B/binary, ?CalledPartyAddress, CPL, CPB/binary>>).

%% @hidden
connection_refused2(#sccp_connection_refused{data = undefined} = R, B) ->
	connection_refused3(R, B);
connection_refused2(#sccp_connection_refused{data = D} = R, B)
		when is_binary(D) ->
	DL = byte_size(D),
	connection_refused3(R, <<B/binary, ?Data, DL, D/binary>>).

%% @hidden
connection_refused3(#sccp_connection_refused{importance = undefined}, B) ->
	<<B/binary, 0>>;
connection_refused3(#sccp_connection_refused{importance= I}, B)
		when is_integer(I) ->
	<<B/binary, ?Importance, 1, I, 0>>.

%% @hidden
sccp_released(#sccp_released{dest_local_ref = Dest, src_local_ref = Src,
		release_cause = RC} = R) when is_integer(Dest), is_integer(Src) ->
	Cause = release_cause(RC),
	OptionalP = 1,
	B = <<?Released, Dest:24/little, Src:24/little, Cause, OptionalP>>,
	released1(R, B).

%% @hidden
released1(#sccp_released{data = undefined} = R, B) ->
	released2(R, B);
released1(#sccp_released{data = D} = R, B) when is_binary(D) ->
	DL = byte_size(D),
	released2(R, <<B/binary, ?Data, DL, D/binary>>).

%% @hidden
released2(#sccp_released{importance = undefined}, B) ->
	<<B/binary, 0>>;
released2(#sccp_released{importance = I}, B) when is_integer(I) ->
	<<B/binary, ?Importance, 1, I, 0>>.

%% @hidden
sccp_extended_unitdata(#sccp_extended_unitdata{class = Class,
		hop_counter = Hops, called_party = CalledParty,
		calling_party = CallingParty, data = Data} = R)
		when is_integer(Class), is_integer(Hops),
		is_record(CalledParty, party_address),
		is_record(CallingParty, party_address),
		is_binary(Data) ->
	CalledPartyB = party_address(CalledParty),
	CallingPartyB = party_address(CallingParty),
	CalledPartyL = byte_size(CalledPartyB),
	CallingPartyL = byte_size(CallingPartyB),
	DataL = byte_size(Data),
	CalledPartyP = 4,
	CallingPartyP = CalledPartyP + CalledPartyL,
	DataP = CallingPartyP + CallingPartyL,
	OptionalP = DataP + DataL,
	B = <<?ExtendedUnitData, Class, Hops, CalledPartyP, CallingPartyP, DataP, OptionalP,
			CalledPartyL, CalledPartyB/binary, CallingPartyL, CallingPartyB/binary,
			DataL, Data/binary>>,
	extended_unitdata1(R, B).

%% @hidden
extended_unitdata1(#sccp_extended_unitdata{segmentation = undefined} = R, B) ->
	extended_unitdata2(R, B);
extended_unitdata1(#sccp_extended_unitdata{segmentation = Seg} = R, B)
		when is_record(Seg, segmentation) ->
	SegB = segmentation(Seg),
	SegL = byte_size(SegB),
	extended_unitdata2(R, <<B/binary, ?Segmentation, SegL, SegB/binary>>).

%% @hidden
extended_unitdata2(#sccp_extended_unitdata{importance = undefined}, B) ->
	<<B/binary, 0>>;
extended_unitdata2(#sccp_extended_unitdata{importance = I}, B)
		when is_integer(I) ->
	<<B/binary, ?Importance, 1, I, 0>>.

%% @hidden
sccp_extended_unitdata_service(#sccp_extended_unitdata_service{return_cause = RC,
		hop_counter = Hops, called_party = CalledParty, calling_party = CallingParty,
		data = Data} = R) when is_integer(Hops), is_record(CalledParty, party_address),
		is_record(CallingParty, party_address), is_binary(Data) ->
	Cause = return_cause(RC),
	CalledPartyB = party_address(CalledParty),
	CallingPartyB = party_address(CallingParty),
	CalledPartyL = byte_size(CalledPartyB),
	CallingPartyL = byte_size(CallingPartyB),
	DataL = byte_size(Data),
	CalledPartyP = 4,
	CallingPartyP = CalledPartyP + CalledPartyL,
	DataP = CallingPartyP + CallingPartyL,
	OptionalP = DataP + DataL,
	B = <<?ExtendedUnitDataService, Cause, Hops, CalledPartyP, CallingPartyP, DataP, OptionalP,
			CalledPartyL, CalledPartyB/binary, CallingPartyL, CallingPartyB/binary,
			DataL, Data/binary>>,
	extended_unitdata_service1(R, B).

%% @hidden
extended_unitdata_service1(#sccp_extended_unitdata_service{segmentation = undefined} = R, B)->
	extended_unitdata_service2(R, B);
extended_unitdata_service1(#sccp_extended_unitdata_service{segmentation = Seg} = R, B)
		when is_record(Seg, segmentation) ->
	SegB = segmentation(Seg),
	SegL = byte_size(SegB),
	extended_unitdata_service2(R, <<B/binary, ?Segmentation, SegL, SegB/binary>>).

%% @hidden
extended_unitdata_service2(#sccp_extended_unitdata_service{importance = undefined}, B) ->
	<<B/binary, 0>>;
extended_unitdata_service2(#sccp_extended_unitdata_service{importance = I}, B)
		when is_integer(I) ->
	<<B/binary, ?Importance, 1, I, 0>>.

%% @hidden
sccp_long_unitdata(#sccp_long_unitdata{class = Class,
		hop_counter = Hops, called_party = CalledParty,
		calling_party = CallingParty, data = Data} = R)
		when is_integer(Class), is_integer(Hops),
		is_record(CalledParty, party_address),
		is_record(CallingParty, party_address), is_binary(Data) ->
	CalledPartyB = party_address(CalledParty),
	CallingPartyB = party_address(CallingParty),
	CalledPartyL = byte_size(CalledPartyB),
	CallingPartyL = byte_size(CallingPartyB),
	DataL = byte_size(Data),
	CalledPartyP = 8,
	CallingPartyP = CalledPartyP + CalledPartyL,
	DataP = CallingPartyP + CallingPartyL,
	OptionalP = DataP + DataL,
	B = <<?LongUnitData, Class, Hops,
			CalledPartyP:16/little, CallingPartyP:16/little,
			DataP:16/little, OptionalP:16/little,
			CalledPartyL:16/little, CalledPartyB/binary,
			CallingPartyL:16/little, CallingPartyB/binary,
			DataL:16/little, Data/binary>>,
	long_unitdata1(R, B).

%% @hidden
long_unitdata1(#sccp_long_unitdata{segmentation = undefined} = R, B) ->
	long_unitdata2(R, B);
long_unitdata1(#sccp_long_unitdata{segmentation = Seg} = R, B)
		when is_record(Seg, segmentation) ->
	SegB = segmentation(Seg),
	SegL = byte_size(SegB),
	long_unitdata2(R, <<B/binary, ?Segmentation,
			SegL:16/little, SegB/binary>>).

%% @hidden
long_unitdata2(#sccp_long_unitdata{importance = undefined}, B) ->
	<<B/binary, 0:16/little>>;
long_unitdata2(#sccp_long_unitdata{importance = I}, B)
		when is_integer(I) ->
	<<B/binary, ?Importance, 1:16/little, I, 0:16/little>>.

%% @hidden
sccp_long_unitdata_service(#sccp_long_unitdata_service{return_cause = RC,
		hop_counter = Hops, called_party = CalledParty,
		calling_party = CallingParty, data = Data} = R)
		when is_integer(Hops), is_record(CalledParty, party_address),
		is_record(CallingParty, party_address), is_binary(Data) ->
	Cause = return_cause(RC),
	CalledPartyB = party_address(CalledParty),
	CallingPartyB = party_address(CallingParty),
	CalledPartyL = byte_size(CalledPartyB),
	CallingPartyL = byte_size(CallingPartyB),
	DataL = byte_size(Data),
	CalledPartyP = 8,
	CallingPartyP = CalledPartyP + CalledPartyL,
	DataP = CallingPartyP + CallingPartyL,
	OptionalP = DataP + DataL,
	B = <<?LongUnitDataService, Cause, Hops,
			CalledPartyP:16/little, CallingPartyP:16/little,
			DataP:16/little, OptionalP:16/little,
			CalledPartyL:16/little, CalledPartyB/binary,
			CallingPartyL:16/little, CallingPartyB/binary,
			DataL:16/little, Data/binary>>,
	long_unitdata_service1(R, B).

%% @hidden
long_unitdata_service1(#sccp_long_unitdata_service{segmentation = undefined} = R, B) ->
	long_unitdata_service2(R, B);
long_unitdata_service1(#sccp_long_unitdata_service{segmentation = Seg} = R, B)
		when is_record(Seg, segmentation) ->
	SegB = segmentation(Seg),
	SegL = byte_size(SegB),
	long_unitdata_service2(R, <<B/binary, ?Segmentation,
			SegL:16/little, SegB/binary>>).

%% @hidden
long_unitdata_service2(#sccp_long_unitdata_service{importance = undefined}, B) ->
	<<B/binary, 0:16/little>>;
long_unitdata_service2(#sccp_long_unitdata_service{importance = I}, B)
		when is_integer(I) ->
	<<B/binary, ?Importance, 1:16/little, I, 0:16/little>>.

